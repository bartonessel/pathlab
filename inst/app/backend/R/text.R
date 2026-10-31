# Textual analysis, Part A: documents (sources) and the preprocessing pipeline. Documents are imported from .txt/.md,
# .pdf (pdftools), .docx (officer), .csv/.xlsx (one document per row of a text column, other columns as attributes)
# or a text column of the project dataset (open-ended answers, linked to the respondent's row). Like datasets, they
# live in the server session; the uploaded files are never changed. Preprocessing uses quanteda; every step reports
# the tokens and types that remain, and the matching R code is returned.

TEXT_MAX_DOCS <- 5000L
TEXT_MAX_CHARS <- 5e6

text_utf8 <- function(x) {
  x <- sub("^﻿", "", x)
  bad <- !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "")
  gsub("\r\n?", "\n", x)
}
text_count_words <- function(x) lengths(regmatches(x, gregexpr("[[:alnum:]][[:alnum:]'’-]*", x)))

#' Text of one uploaded file (not tabular). Returns list(text, attributes).
text_extract <- function(raw, filename) {
  ext <- tolower(tools::file_ext(filename))
  tmp <- tempfile(fileext = paste0(".", ext)); writeBin(raw, tmp); on.exit(unlink(tmp))
  switch(ext,
    txt = , md = , text = list(text = text_utf8(rawToChar(raw[raw != as.raw(0)])), attributes = list()),
    pdf = {
      if (!requireNamespace("pdftools", quietly = TRUE)) api_error(501, "text_unavailable", "The pdftools R package is not installed on the analysis server.")
      pages <- tryCatch(pdftools::pdf_text(tmp), error = function(e) api_error(422, "text_pdf", paste("The PDF could not be read:", sanitize_message(conditionMessage(e)))))
      txt <- paste(trimws(pages), collapse = "\n\n")
      if (!nzchar(gsub("\\s", "", txt))) api_error(422, "text_pdf_scanned", "This PDF has no text layer (it is a scanned image).", "Run OCR in a PDF tool first, or paste the text into a .txt file.")
      list(text = text_utf8(txt), attributes = list(pages = length(pages)))
    },
    docx = {
      doc <- tryCatch(officer::read_docx(tmp), error = function(e) api_error(422, "text_docx", paste("The Word file could not be read:", sanitize_message(conditionMessage(e)))))
      s <- officer::docx_summary(doc)
      s <- s[order(s$doc_index), ]
      par <- s[s$content_type == "paragraph", c("doc_index", "text")]
      tab <- s[s$content_type == "table cell", , drop = FALSE]
      if (nrow(tab)) {
        rows <- stats::aggregate(text ~ doc_index + row_id, data = tab, FUN = function(v) paste(v, collapse = " | "))
        par <- rbind(par, data.frame(doc_index = rows$doc_index + rows$row_id / 1e4, text = rows$text))
        par <- par[order(par$doc_index), ]
      }
      list(text = text_utf8(paste(par$text[nzchar(trimws(par$text))], collapse = "\n\n")), attributes = list())
    },
    api_error(400, "text_format", sprintf("'.%s' files are not supported for text.", ext), "Use .txt, .md, .pdf, .docx, .csv or .xlsx."))
}

text_new_doc <- function(proj, name, source, kind, text, attributes = list(), dataset = NULL) {
  if (nchar(text) > TEXT_MAX_CHARS) api_error(413, "text_too_long", sprintf("'%s' has more than %s characters.", name, format(TEXT_MAX_CHARS, big.mark = ",")))
  if (length(proj$documents %||% list()) >= TEXT_MAX_DOCS) api_error(413, "text_too_many", sprintf("A project can hold at most %d documents.", TEXT_MAX_DOCS))
  id <- new_id("t")
  doc <- list(id = id, name = substr(name, 1, 200), source = source, kind = kind, text = text, attributes = attributes,
              datasetId = dataset$id, datasetRow = dataset$row, createdAt = now_iso())
  proj$documents <- c(proj$documents %||% list(), setNames(list(doc), id))
  doc
}

#' Documents embedded in a project file, restored with their original ids (codings refer to them). Existing ids are skipped.
text_restore <- function(proj, docs, dataset_id = NULL) {
  out <- list()
  for (d in docs %||% list()) {
    id <- as.character(d$id %||% ""); if (!grepl("^[A-Za-z0-9_-]{2,40}$", id) || !is.null(proj$documents[[id]])) next
    txt <- text_utf8(as.character(d$text %||% "")); if (!nzchar(txt)) next   # a recording that was never transcribed has nothing to restore
    if (nchar(txt) > TEXT_MAX_CHARS || length(proj$documents %||% list()) >= TEXT_MAX_DOCS) api_error(413, "text_too_long", "The project file has more text than a session can hold.")
    row <- suppressWarnings(as.integer(d$datasetRow %||% NA))
    doc <- list(id = id, name = substr(as.character(d$name %||% id), 1, 200), source = as.character(d$source %||% ""), kind = as.character(d$kind %||% "txt"), text = txt,
                attributes = Filter(Negate(is.null), d$attributes %||% list()), datasetId = if (!is.na(row) && !is.null(dataset_id)) dataset_id, datasetRow = if (!is.na(row) && !is.null(dataset_id)) row, createdAt = as.character(d$createdAt %||% now_iso()))
    # A transcript keeps its timestamps; the recording itself is not in the project file.
    if (length(d$segments)) {
      doc$segments <- lapply(d$segments, function(s) list(start = as.numeric(s$start %||% NA), end = as.numeric(s$end %||% NA), text = as.character(s$text %||% ""), speaker = as.character(s$speaker %||% ""),
                                                          from = as.integer(s$from %||% 0L), to = as.integer(s$to %||% 0L)))
      m <- d$media %||% list()
      doc$media <- list(ext = as.character(m$ext %||% doc$kind), mime = as.character(m$mime %||% ""), duration = as.numeric(m$duration %||% NA), video = isTRUE(m$video),
                        engine = m$engine, model = m$model, language = m$language, transcribedAt = m$transcribedAt)
    }
    proj$documents[[id]] <- doc
    out[[length(out) + 1]] <- text_meta(doc)
  }
  out
}

text_meta <- function(d) list(id = d$id, name = d$name, source = d$source, kind = d$kind, nChars = nchar(d$text), nWords = text_count_words(d$text),
                              excerpt = substr(gsub("\\s+", " ", d$text), 1, 180), attributes = d$attributes %||% setNames(list(), character(0)),
                              datasetId = d$datasetId, datasetRow = d$datasetRow, createdAt = d$createdAt,
                              media = media_meta(d), nSegments = length(d$segments %||% list()), visual = visual_meta(d))
text_list <- function(proj) unname(lapply(proj$documents %||% list(), text_meta))

#' Documents from the rows of a table: one per non-empty cell of `text_col`; `id_col` names them; `attr_cols` become attributes.
#' With several text columns (a survey with several open questions): one document per respondent with a "# question"
#' section per answered question, which Autocode → Sections codes by question.
text_from_table <- function(proj, df, source, text_col, id_col = NULL, attr_cols = character(0), dataset = NULL) {
  text_col <- unique(as.character(text_col))
  miss <- setdiff(text_col, names(df))
  if (!length(text_col) || length(miss)) api_error(400, "text_column", sprintf("There is no column '%s'.", c(miss, "")[1]))
  attr_cols <- setdiff(intersect(attr_cols, names(df)), text_col)
  if (length(attr_cols) > 50) api_error(400, "text_attributes", "Choose at most 50 attribute columns.")
  txt <- if (length(text_col) == 1) as.character(df[[text_col]]) else {
    parts <- lapply(text_col, function(cn) { v <- as.character(df[[cn]]); ifelse(!is.na(v) & nzchar(trimws(v)), paste0("# ", cn, "\n", trimws(v)), NA_character_) })
    apply(do.call(cbind, parts), 1, function(r) { r <- r[!is.na(r)]; if (length(r)) paste(r, collapse = "\n\n") else NA_character_ })
  }
  keep <- which(!is.na(txt) & nzchar(trimws(txt)))
  if (!length(keep)) api_error(422, "text_empty", sprintf("The column '%s' has no text.", paste(text_col, collapse = "', '")))
  if (length(keep) + length(proj$documents %||% list()) > TEXT_MAX_DOCS) api_error(413, "text_too_many", sprintf("A project can hold at most %d documents.", TEXT_MAX_DOCS))
  out <- vector("list", length(keep))
  for (k in seq_along(keep)) {
    i <- keep[k]
    nm <- if (!is.null(id_col) && id_col %in% names(df) && !is.na(df[[id_col]][i])) paste0(id_col, " ", df[[id_col]][i]) else paste("Row", i)
    at <- lapply(setNames(attr_cols, attr_cols), function(cn) { v <- df[[cn]][i]; if (is.factor(v)) as.character(v) else if (is.na(v)) NULL else v })
    out[[k]] <- text_new_doc(proj, nm, source, if (is.null(dataset)) "row" else "dataset", text_utf8(txt[i]), Filter(Negate(is.null), at),
                             if (!is.null(dataset)) list(id = dataset$id, row = i))
  }
  out
}

#' Columns of a table that look like text (for the import dialog): name, share of non-empty cells, mean length, example.
text_column_candidates <- function(df) unname(lapply(names(df), function(cn) {
  v <- df[[cn]]; s <- if (is.character(v) || is.factor(v)) as.character(v) else character(0)
  list(name = cn, type = class(v)[1], text = length(s) > 0, meanChars = if (length(s)) round(mean(nchar(s[!is.na(s)])), 1) else 0,
       example = if (length(s)) substr(s[!is.na(s) & nzchar(s)][1] %||% "", 1, 100) else "")
}))

text_import_upload <- function(proj, raw, filename, text_col = NULL, id_col = NULL, attr_cols = character(0), sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% REF_EXTS) return(list(documents = text_import_refs(proj, raw, filename)))   # Zotero / EndNote / Mendeley / PubMed exports
  if (ext %in% c("csv", "tsv", "xlsx", "xls")) {
    tmp <- tempfile(fileext = paste0(".", ext)); writeBin(raw, tmp); on.exit(unlink(tmp))
    parsed <- read_dataset(tmp, filename, sheet)
    df <- parsed$data
    if (!length(text_col) || !any(nzchar(text_col))) return(list(needsColumn = list(filename = filename, rows = nrow(df), columns = text_column_candidates(df))))
    docs <- text_from_table(proj, df, filename, text_col, id_col, attr_cols)
    return(list(documents = lapply(docs, text_meta)))
  }
  x <- text_extract(raw, filename)
  d <- text_new_doc(proj, tools::file_path_sans_ext(basename(filename)), filename, ext, x$text, x$attributes)
  if (ext == "pdf") d <- text_attach_pdf(proj, d, raw)   # its pages can be coded as regions
  list(documents = list(text_meta(d)))
}

# ---- Preprocessing (quanteda) -------------------------------------------------------------------------------------
TEXT_LANGS <- c(en = "English", fr = "French", de = "German", es = "Spanish", pt = "Portuguese", it = "Italian", nl = "Dutch",
                sv = "Swedish", da = "Danish", no = "Norwegian", fi = "Finnish", ru = "Russian", hu = "Hungarian", ro = "Romanian", tr = "Turkish")
TEXT_STEM_LANG <- c(en = "english", fr = "french", de = "german", es = "spanish", pt = "portuguese", it = "italian", nl = "dutch", sv = "swedish",
                    da = "danish", no = "norwegian", fi = "finnish", ru = "russian", hu = "hungarian", ro = "romanian", tr = "turkish")

normalize_text_options <- function(raw) {
  raw <- raw %||% list()
  lang <- as.character(raw$language %||% "en")[1]; if (!lang %in% names(TEXT_LANGS)) lang <- "en"
  custom <- unique(tolower(trimws(as_chr_vec(raw$customStopwords))))
  list(lower = !isFALSE(raw$lower), punct = !isFALSE(raw$punct), numbers = !isFALSE(raw$numbers), symbols = !isFALSE(raw$symbols), urls = !isFALSE(raw$urls),
       stopwords = !isFALSE(raw$stopwords), language = lang, customStopwords = head(custom[nzchar(custom)], 2000), stem = isTRUE(raw$stem),
       ngrams = { n <- suppressWarnings(as.integer(raw$ngrams %||% 1L)); if (is.na(n) || n < 1) 1L else min(n, 3L) },
       minFreq = { n <- suppressWarnings(as.integer(raw$minFreq %||% 1L)); if (is.na(n) || n < 1) 1L else min(n, 1000L) },
       minDocs = { n <- suppressWarnings(as.integer(raw$minDocs %||% 1L)); if (is.na(n) || n < 1) 1L else min(n, 1000L) })
}

text_docs <- function(proj, ids) {
  all <- proj$documents %||% list()
  if (!length(all)) api_error(422, "text_no_docs", "Import at least one document first.")
  ids <- as_chr_vec(ids); if (!length(ids)) ids <- names(all)
  miss <- setdiff(ids, names(all)); if (length(miss)) api_error(404, "text_doc_missing", "Some documents are no longer in this session.", "Import them again.")
  all[ids]
}

#' Tokens and document-feature matrix after the pipeline; `steps` reports tokens / types after each step.
text_pipeline <- function(docs, o) {
  if (!requireNamespace("quanteda", quietly = TRUE)) api_error(501, "text_unavailable", "The quanteda R package is not installed on the analysis server.", "Run Rscript backend/install_packages.R.")
  texts <- vapply(docs, `[[`, "", "text"); names(texts) <- vapply(docs, `[[`, "", "id")
  corp <- quanteda::corpus(texts, docvars = data.frame(doc_name = vapply(docs, `[[`, "", "name")))
  steps <- list()
  rec <- function(label, tk) { steps[[length(steps) + 1]] <<- list(step = label, tokens = sum(quanteda::ntoken(tk)), types = length(quanteda::types(tk))); tk }
  tk <- rec("Tokenised (words, punctuation, numbers)", quanteda::tokens(corp, what = "word"))
  if (o$punct || o$numbers || o$symbols || o$urls)
    tk <- rec(paste("Removed", rp_list(c(if (o$punct) "punctuation", if (o$numbers) "numbers", if (o$symbols) "symbols", if (o$urls) "URLs"))),
              quanteda::tokens(tk, remove_punct = o$punct, remove_numbers = o$numbers, remove_symbols = o$symbols, remove_url = o$urls))
  if (o$lower) tk <- rec("Lower-cased", quanteda::tokens_tolower(tk))
  sw <- character(0)
  if (o$stopwords) sw <- stopwords::stopwords(o$language, source = "snowball")
  sw <- unique(c(sw, o$customStopwords))
  if (length(sw)) tk <- rec(sprintf("Removed %d stop words (%s%s)", length(sw), if (o$stopwords) paste(TEXT_LANGS[[o$language]], "Snowball list") else "", if (length(o$customStopwords)) paste0(if (o$stopwords) " + " else "", length(o$customStopwords), " custom") else ""),
                            quanteda::tokens_remove(tk, sw, padding = FALSE))
  if (o$stem) tk <- rec(paste0("Stemmed (Snowball, ", TEXT_LANGS[[o$language]], ")"), quanteda::tokens_wordstem(tk, language = TEXT_STEM_LANG[[o$language]]))
  if (o$ngrams > 1) tk <- rec(sprintf("Added n-grams up to %d words", o$ngrams), quanteda::tokens_ngrams(tk, n = seq_len(o$ngrams), concatenator = " "))
  dm <- quanteda::dfm(tk, tolower = FALSE)
  if (o$minFreq > 1 || o$minDocs > 1) {
    dm <- quanteda::dfm_trim(dm, min_termfreq = o$minFreq, min_docfreq = o$minDocs)
    steps[[length(steps) + 1]] <- list(step = sprintf("Kept features used at least %d time(s) in at least %d document(s)", o$minFreq, o$minDocs), tokens = sum(Matrix::rowSums(dm)), types = quanteda::nfeat(dm))
  }
  list(corpus = corp, tokens = tk, dfm = dm, steps = steps, stopwords = sw)
}

text_preprocess_code <- function(o) {
  q <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  c("library(quanteda)", "# texts: a named character vector with one element per document", "corp <- corpus(texts)",
    sprintf("toks <- tokens(corp, remove_punct = %s, remove_numbers = %s, remove_symbols = %s, remove_url = %s)", o$punct, o$numbers, o$symbols, o$urls),
    if (o$lower) "toks <- tokens_tolower(toks)",
    if (o$stopwords || length(o$customStopwords)) sprintf("toks <- tokens_remove(toks, c(%s%s))", if (o$stopwords) sprintf("stopwords::stopwords(%s, source = \"snowball\")", q(o$language)) else "",
                                                           if (length(o$customStopwords)) paste0(if (o$stopwords) ", " else "", q(o$customStopwords)) else ""),
    if (o$stem) sprintf("toks <- tokens_wordstem(toks, language = %s)", q(TEXT_STEM_LANG[[o$language]])),
    if (o$ngrams > 1) sprintf("toks <- tokens_ngrams(toks, n = 1:%d, concatenator = \" \")", o$ngrams),
    "dfmat <- dfm(toks)", if (o$minFreq > 1 || o$minDocs > 1) sprintf("dfmat <- dfm_trim(dfmat, min_termfreq = %d, min_docfreq = %d)", o$minFreq, o$minDocs),
    "quanteda.textstats::textstat_frequency(dfmat, n = 50)")
}

text_preprocess <- function(proj, ids, raw_opts) {
  o <- normalize_text_options(raw_opts)
  docs <- text_docs(proj, ids)
  pp <- text_pipeline(docs, o)
  fr <- quanteda.textstats::textstat_frequency(pp$dfm, n = 60)
  per <- lapply(seq_along(docs), function(i) list(id = docs[[i]]$id, name = docs[[i]]$name, tokensRaw = unname(quanteda::ntoken(quanteda::tokens(pp$corpus[i]))),
                                                   tokens = unname(quanteda::ntoken(pp$tokens)[i]), features = unname(Matrix::rowSums(pp$dfm[i, ] > 0))))
  list(options = o, nDocs = length(docs), steps = pp$steps,
       top = unname(lapply(seq_len(nrow(fr)), function(i) list(feature = fr$feature[i], frequency = fr$frequency[i], docfreq = fr$docfreq[i], rank = fr$rank[i]))),
       perDoc = per, nFeatures = quanteda::nfeat(pp$dfm), sparsity = quanteda::sparsity(pp$dfm),
       languages = lapply(names(TEXT_LANGS), function(k) list(id = k, label = TEXT_LANGS[[k]])),
       rCode = paste(Filter(Negate(is.null), text_preprocess_code(o)), collapse = "\n"),
       packages = list(quanteda = as.character(utils::packageVersion("quanteda")), stopwords = as.character(utils::packageVersion("stopwords"))))
}
