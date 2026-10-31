# AI-assisted coding in the Text model (NVivo AI Assistant, ATLAS.ti AI Coding, MAXQDA AI Assist), through the user's own
# provider and key (ai.R). Unlike the results assistant, these requests send the documents' text, so they need their own
# consent. Suggested codes come back with verbatim quotes, which are located in the documents here: only text that is
# really in a document is ever coded, and quotes the model altered are reported as not found.

QDA_AI_MAX_CHARS <- 120000

qda_ai_docs <- function(proj, ids) {
  docs <- Filter(function(d) nzchar(d$text), text_docs(proj, ids))
  if (!length(docs)) api_error(422, "qda_ai_docs", "The chosen documents have no text (transcribe recordings first).")
  per <- floor(QDA_AI_MAX_CHARS / length(docs))
  cut <- vapply(docs, function(d) nchar(d$text) > per, TRUE)
  block <- paste(vapply(seq_along(docs), function(i) sprintf("=== DOCUMENT %d: %s ===\n%s", i, docs[[i]]$name, substr(docs[[i]]$text, 1, per)), ""), collapse = "\n\n")
  list(docs = docs, block = block, truncated = names(docs)[cut])
}

#' JSON from a model reply (code fences and text around it removed).
qda_ai_json <- function(txt) {
  t <- sub("^[^{]*", "", sub("[^}]*$", "", gsub("```(json)?", "", txt)))
  tryCatch(jsonlite::fromJSON(t, simplifyVector = FALSE), error = function(e) api_error(502, "qda_ai_format", "The AI reply could not be read.", "Try again, or choose a more capable model."))
}

#' Where a quote is in a text: exact, then with any whitespace between its words (UTF-16 offsets), or NULL.
qda_find_quote <- function(text, quote) {
  quote <- trimws(gsub("^[\"“‘']+|[\"”’']+$", "", trimws(quote)))
  if (nchar(quote) < 3) return(NULL)
  p <- regexpr(quote, text, fixed = TRUE)
  if (p < 0) {
    words <- strsplit(quote, "\\s+")[[1]]
    re <- paste(vapply(words, function(w) gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", w), ""), collapse = "\\s+")
    p <- regexpr(re, text, perl = TRUE, ignore.case = TRUE)
    if (p < 0) return(NULL)
  }
  qda_cp2js(text, c(as.integer(p) - 1L, as.integer(p) - 1L + attr(p, "match.length")))
}

qda_ai <- function(proj, q, b) {
  if (!isTRUE(b$consent)) api_error(400, "ai_consent", "Confirm that the documents' text may be sent to the AI provider first.")
  kind <- as.character(b$kind %||% "suggest")
  d <- qda_ai_docs(proj, b$documentIds)
  focus <- substr(trimws(as.character(b$focus %||% "")), 1, 1000)
  codebook <- paste(vapply(q$codes, function(c) paste0("- ", c$name, if (nzchar(c$description %||% "")) paste0(": ", c$description) else ""), ""), collapse = "\n")
  rules <- "Work only from the documents below. Quote them verbatim: copy the words exactly as written, without changes, ellipses or corrections."
  if (kind == "suggest") {
    n <- max(3, min(20, as.integer(b$n %||% 8)))
    system <- paste0("You are a qualitative research assistant doing thematic coding (Braun & Clarke). Propose up to ", n, " codes (themes) grounded in the documents",
                     if (nzchar(focus)) paste0(", focused on this research question: ", focus) else "", ". ",
                     if (nzchar(codebook)) paste0("Reuse these existing codes where they fit (same names):\n", codebook, "\n") else "",
                     rules, " For each code give a one-sentence definition and 2 to 8 short quotes (5 to 40 words each) from any of the documents. ",
                     "Reply with JSON only: {\"codes\": [{\"name\": \"...\", \"description\": \"...\", \"quotes\": [{\"doc\": 1, \"quote\": \"...\"}]}]}")
    r <- ai_complete(proj, b$provider, b$model, system, list(list(role = "user", content = d$block)), b$maxTokens %||% 4000, "text coding")
    j <- qda_ai_json(r$text)
    groups <- lapply(j$codes %||% list(), function(cd) {
      segs <- list(); miss <- character(0)
      for (qt in cd$quotes %||% list()) {
        i <- suppressWarnings(as.integer(qt$doc)); txt <- as.character(qt$quote %||% "")
        doc <- if (!is.na(i) && i >= 1 && i <= length(d$docs)) d$docs[[i]] else NULL
        pos <- if (!is.null(doc)) qda_find_quote(doc$text, txt) else NULL
        if (is.null(pos)) { miss <- c(miss, txt); next }
        segs[[length(segs) + 1]] <- list(docId = doc$id, start = pos[1], end = pos[2])
      }
      list(name = substr(as.character(cd$name %||% "Theme"), 1, 120), description = substr(as.character(cd$description %||% ""), 1, 1000), segments = segs,
           nSegments = length(segs), notFound = as.list(miss), examples = lapply(utils::head(segs, 2), function(s) substr(qda_slice(proj$documents[[s$docId]]$text, s$start, s$end), 1, 160)))
    })
    return(list(kind = kind, groups = Filter(function(g) g$nSegments > 0 || length(g$notFound), groups), truncated = as.list(d$truncated), usage = r$usage, model = r$model))
  }
  if (kind == "summarise") {
    system <- paste0("You are a qualitative research assistant. Summarise each document in 3 to 6 sentences", if (nzchar(focus)) paste0(", focused on: ", focus) else "",
                     ". Stay close to what the participants or authors say; do not interpret beyond the text. Reply with JSON only: {\"summaries\": [{\"doc\": 1, \"summary\": \"...\"}]}")
    r <- ai_complete(proj, b$provider, b$model, system, list(list(role = "user", content = d$block)), b$maxTokens %||% 4000, "text summary")
    j <- qda_ai_json(r$text)
    sums <- lapply(j$summaries %||% list(), function(s) { i <- suppressWarnings(as.integer(s$doc)); if (is.na(i) || i < 1 || i > length(d$docs)) return(NULL)
      list(docId = d$docs[[i]]$id, document = d$docs[[i]]$name, summary = as.character(s$summary %||% "")) })
    return(list(kind = kind, summaries = Filter(Negate(is.null), sums), truncated = as.list(d$truncated), usage = r$usage, model = r$model))
  }
  if (kind == "ask") {
    if (!nzchar(focus)) api_error(400, "qda_ai_question", "Type a question about the documents.")
    system <- paste0("You are a qualitative research assistant. Answer the user's question from the documents only; if they do not answer it, say so. ",
                     "Support each point with a short verbatim quote and its document number, as: \"quote\" [Document n]. Use short Markdown paragraphs or lists.")
    r <- ai_complete(proj, b$provider, b$model, system, list(list(role = "user", content = paste0(d$block, "\n\nQUESTION: ", focus))), b$maxTokens %||% 3000, "text question")
    names_map <- lapply(seq_along(d$docs), function(i) list(n = i, docId = d$docs[[i]]$id, document = d$docs[[i]]$name))
    return(list(kind = kind, answer = r$text, documents = names_map, truncated = as.list(d$truncated), usage = r$usage, model = r$model))
  }
  api_error(400, "qda_ai_kind", "Unknown AI request.")
}
