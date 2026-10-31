# Textual analysis, Part B (continued): the coding tools of NVivo, ATLAS.ti and MAXQDA that work on positions in the
# text. Autocoding (search hits as the hit, its sentence or its paragraph; speaker turns of transcripts; sections under
# headings; the terms of a dictionary category), Boolean and proximity coding queries (AND, OR, NOT, NEAR, PRECEDES on
# coded segments, sub-codes included), and the code × document matrix (MAXQDA's Code Matrix Browser).
#
# Positions sent to the browser are UTF-16 offsets (qda_cp2js); codings arrive in UTF-16 offsets, as everywhere in
# Part B. Nothing here changes the codebook: autocoding returns the segments, and the browser adds them on confirmation.

QDA_AUTOCODE_MAX <- 20000L

#' A search pattern as a PCRE regular expression on whole words: terms separated by commas or new lines (any of them),
#' words of a phrase separated by spaces, `*` for any ending, `?` for one letter.
qda_pattern_regex <- function(pattern) {
  terms <- trimws(unlist(strsplit(pattern %||% "", "[,;\n]")))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) api_error(400, "qda_pattern", "Enter a word, phrase or wildcard (fee*) to search for.")
  one <- function(w) paste(vapply(strsplit(w, "")[[1]], function(ch) {
    if (ch == "*") "[\\p{L}\\p{N}'’-]*" else if (ch == "?") "[\\p{L}\\p{N}]" else if (grepl("[\\p{L}\\p{N}]", ch, perl = TRUE)) ch else paste0("\\", ch)
  }, ""), collapse = "")
  alt <- vapply(terms, function(t) paste(vapply(strsplit(t, "\\s+")[[1]], one, ""), collapse = "\\s+"), "")
  paste0("(?i)(?<![\\p{L}\\p{N}])(?:", paste(alt, collapse = "|"), ")(?![\\p{L}\\p{N}])")
}

#' Matches of a regular expression: 0-based code-point starts and (exclusive) ends.
qda_matches <- function(text, re) {
  m <- gregexpr(re, text, perl = TRUE)[[1]]
  if (m[1] < 0) return(data.frame(start = integer(0), end = integer(0)))
  data.frame(start = as.integer(m) - 1L, end = as.integer(m) - 1L + attr(m, "match.length"))
}

#' Sentences or paragraphs (one per line) of a text, trimmed, as code-point spans.
qda_unit_spans <- function(text, unit) {
  re <- if (unit == "sentence") "\\S[^.!?\\n]*(?:[.!?]+[\"'”’)]*|(?=\\n)|$)" else "\\S(?:[^\\n]*\\S)?"
  qda_matches(text, re)
}

#' Sorted, merged spans (overlapping or touching spans become one).
qda_merge <- function(s) {
  if (!nrow(s)) return(s)
  s <- s[order(s$start, s$end), , drop = FALSE]
  out <- s[1, , drop = FALSE]
  for (i in seq_len(nrow(s))[-1]) {
    k <- nrow(out)
    if (s$start[i] <= out$end[k]) out$end[k] <- max(out$end[k], s$end[i]) else out <- rbind(out, s[i, , drop = FALSE])
  }
  out
}

#' Autocoding. `mode`: "search" (pattern, unit = hit / sentence / paragraph), "speaker" (transcript turns "Name: ..."),
#' "heading" (Markdown headings or "Q1." lines; the section below each), "dictionary" (categories = list(list(name, terms))).
#' Returns the groups of segments a code would get, for the user to confirm.
qda_autocode <- function(proj, ids, b) {
  docs <- text_docs(proj, ids)
  mode <- as.character(b$mode %||% "search")
  unit <- if (as.character(b$unit %||% "hit") %in% c("hit", "sentence", "paragraph")) as.character(b$unit %||% "hit") else "hit"
  groups <- list()
  add <- function(name, d, spans) {
    if (!nrow(spans)) return()
    spans <- qda_merge(spans)
    js <- lapply(seq_len(nrow(spans)), function(i) { p <- qda_cp2js(d$text, c(spans$start[i], spans$end[i])); list(docId = d$id, start = p[1], end = p[2]) })
    groups[[name]] <<- c(groups[[name]] %||% list(), js)
  }
  by_search <- function(name, pattern) for (d in docs) {
    hits <- qda_matches(d$text, qda_pattern_regex(pattern))
    if (!nrow(hits)) next
    if (unit != "hit") {
      u <- qda_unit_spans(d$text, unit)
      k <- vapply(hits$start, function(s) { j <- which(u$start <= s & u$end > s); if (length(j)) j[1] else NA_integer_ }, 0L)
      hits <- rbind(u[unique(k[!is.na(k)]), , drop = FALSE], hits[is.na(k), , drop = FALSE])
    }
    add(name, d, hits)
  }
  if (mode == "search") {
    pattern <- as.character(b$pattern %||% "")
    by_search(substr(trimws(pattern), 1, 60), pattern)
  } else if (mode == "dictionary") {
    cats <- Filter(function(c) nzchar(trimws(as.character(c$name %||% ""))) && nzchar(trimws(paste(unlist(c$terms), collapse = ""))), b$categories %||% list())
    if (!length(cats)) api_error(400, "qda_dictionary", "The dictionary has no categories with terms.")
    for (c in cats) by_search(trimws(as.character(c$name)), paste(unlist(c$terms), collapse = ","))
  } else if (mode == "speaker") {
    # A turn starts with "Name:" at the beginning of a line (at most five words before the colon) and runs to the next turn.
    re <- "(?m)^[ \\t]*([\\p{Lu}][^:\\n]{0,40}?)[ \\t]*:[ \\t]*"
    for (d in docs) {
      m <- gregexpr(re, d$text, perl = TRUE)[[1]]
      if (m[1] < 0) next
      cs <- attr(m, "capture.start")[, 1]; cl <- attr(m, "capture.length")[, 1]
      who <- substring(d$text, cs, cs + cl - 1L)
      ok <- lengths(strsplit(trimws(who), "\\s+")) <= 5
      st <- as.integer(m)[ok] - 1L + attr(m, "match.length")[ok]; who <- trimws(who[ok])
      nxt <- c(as.integer(m)[ok][-1] - 1L, nchar(d$text))
      for (i in seq_along(st)) {
        seg <- substr(d$text, st[i] + 1L, nxt[i]); lead <- nchar(seg) - nchar(sub("^\\s+", "", seg)); trail <- nchar(seg) - nchar(sub("\\s+$", "", seg))
        if (nchar(seg) - lead - trail > 0) add(who[i], d, data.frame(start = st[i] + lead, end = nxt[i] - trail))
      }
    }
  } else if (mode == "heading") {
    re <- "(?m)^[ \\t]*(?:#{1,6}[ \\t]+(.+?)|((?:Q|Question)[ \\t]*\\d+[.:)][^\\n]*))[ \\t]*$"
    for (d in docs) {
      m <- gregexpr(re, d$text, perl = TRUE)[[1]]
      if (m[1] < 0) next
      cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
      title <- vapply(seq_along(m), function(i) { j <- if (cl[i, 1] > 0) 1 else 2; trimws(substring(d$text, cs[i, j], cs[i, j] + cl[i, j] - 1L)) }, "")
      st <- as.integer(m) - 1L + attr(m, "match.length"); nxt <- c(as.integer(m)[-1] - 1L, nchar(d$text))
      for (i in seq_along(st)) {
        seg <- substr(d$text, st[i] + 1L, nxt[i]); lead <- nchar(seg) - nchar(sub("^\\s+", "", seg)); trail <- nchar(seg) - nchar(sub("\\s+$", "", seg))
        if (nchar(seg) - lead - trail > 0) add(substr(title[i], 1, 80), d, data.frame(start = st[i] + lead, end = nxt[i] - trail))
      }
    }
  } else api_error(400, "qda_autocode", "Unknown autocoding mode.")
  total <- sum(lengths(groups))
  if (total > QDA_AUTOCODE_MAX) api_error(422, "qda_autocode_size", sprintf("Autocoding would create %s segments (at most %s).", format(total, big.mark = ","), format(QDA_AUTOCODE_MAX, big.mark = ",")),
                                          "Search for something more specific or code fewer documents at a time.")
  list(mode = mode, unit = unit, groups = unname(lapply(names(groups), function(n) {
    g <- groups[[n]]
    list(name = n, segments = g, nSegments = length(g), nDocs = length(unique(vapply(g, `[[`, "", "docId"))),
         examples = lapply(utils::head(g, 3), function(x) substr(qda_slice(docs[[x$docId]]$text, x$start, x$end), 1, 160)))
  })), nSegments = total)
}

#' Segments (UTF-16 offsets) of a code and, with `sub`, of its sub-codes, in the given documents.
qda_code_segments <- function(q, code_id, docs, sub = TRUE) {
  ids <- code_id
  if (sub) for (i in 1:50) { more <- vapply(Filter(function(c) c$parentId %in% ids && !c$id %in% ids, q$codes), `[[`, "", "id"); if (!length(more)) break; ids <- c(ids, more) }
  Filter(function(x) x$codeId %in% ids && x$docId %in% docs, q$codings)
}

#' Boolean and proximity coding queries (NVivo's Coding query, ATLAS.ti's Query Tool, MAXQDA's Complex Coding Query).
#' op: and (overlapping text: the overlap), or (text coded with either), not (A segments that do not overlap B; with
#' scope "document", A segments in documents without B), near (A segments with a B segment within `distance` words),
#' precedes (A segments followed by a B segment within `distance` words).
qda_boolean <- function(proj, q, b) {
  all_docs <- proj$documents %||% list()
  ids <- as_chr_vec(b$documentIds); docs <- if (length(ids)) intersect(ids, names(all_docs)) else names(all_docs)
  op <- as.character(b$op %||% "and"); if (!op %in% c("and", "or", "not", "near", "precedes")) api_error(400, "qda_op", "Unknown operator.")
  ca <- as.character(b$codeA %||% ""); cb <- as.character(b$codeB %||% "")
  known <- vapply(q$codes, `[[`, "", "id")
  if (!ca %in% known || !cb %in% known) api_error(422, "qda_query_codes", "Choose two codes.")
  sub <- !isFALSE(b$sub); dist <- max(0L, min(500L, as.integer(b$distance %||% 10L)))
  scope <- if (identical(b$scope, "document")) "document" else "segment"
  A <- qda_code_segments(q, ca, docs, sub); B <- qda_code_segments(q, cb, docs, sub)
  by_doc <- function(x) split(x, vapply(x, `[[`, "", "docId"))
  Ad <- by_doc(A); Bd <- by_doc(B)
  span <- function(xs) if (length(xs)) data.frame(start = vapply(xs, `[[`, 0L, "start"), end = vapply(xs, `[[`, 0L, "end")) else data.frame(start = integer(0), end = integer(0))
  gap_words <- function(text, a, bb) { s <- min(a$end, bb$end); e <- max(a$start, bb$start); if (e <= s) 0L else text_count_words(qda_slice(text, s, e)) }
  res <- list()
  for (d in docs) {
    a <- Ad[[d]] %||% list(); bb <- Bd[[d]] %||% list(); text <- all_docs[[d]]$text
    out <- switch(op,
      and = if (scope == "document") { if (length(a) && length(bb)) qda_merge(rbind(span(a), span(bb))) else span(list()) } else {
        ov <- do.call(rbind, c(list(span(list())), lapply(a, function(x) do.call(rbind, c(list(span(list())), lapply(bb, function(y) if (x$start < y$end && y$start < x$end) data.frame(start = max(x$start, y$start), end = min(x$end, y$end))))))))
        qda_merge(ov) },
      or = qda_merge(rbind(span(a), span(bb))),
      not = if (scope == "document") { if (length(bb)) span(list()) else qda_merge(span(a)) } else qda_merge(span(Filter(function(x) !any(vapply(bb, function(y) x$start < y$end && y$start < x$end, TRUE)), a))),
      near = qda_merge(span(Filter(function(x) any(vapply(bb, function(y) gap_words(text, x, y) <= dist, TRUE)), a))),
      precedes = qda_merge(span(Filter(function(x) any(vapply(bb, function(y) y$start >= x$end && gap_words(text, x, y) <= dist, TRUE)), a))))
    if (nrow(out)) for (i in seq_len(nrow(out))) res[[length(res) + 1]] <- list(docId = d, document = all_docs[[d]]$name, start = out$start[i], end = out$end[i], text = qda_slice(text, out$start[i], out$end[i]))
  }
  paths <- qda_paths(q$codes)
  label <- switch(op, and = "AND", or = "OR", not = "NOT", near = sprintf("NEAR (%d words)", dist), precedes = sprintf("PRECEDES (%d words)", dist))
  list(op = op, scope = scope, label = paste(paths[[ca]], label, paths[[cb]]), segments = utils::head(res, 2000), nSegments = length(res), nDocs = length(unique(vapply(res, `[[`, "", "docId"))))
}

#' Code × document matrix: coded segments of each code in each document (MAXQDA's Code Matrix Browser).
qda_matrix <- function(proj, q, b) {
  all_docs <- proj$documents %||% list()
  ids <- as_chr_vec(b$documentIds); docs <- if (length(ids)) intersect(ids, names(all_docs)) else names(all_docs)
  paths <- qda_paths(q$codes)
  rows <- lapply(q$codes, function(c) {
    cs <- Filter(function(x) x$codeId == c$id && x$docId %in% docs, q$codings)
    n <- table(factor(vapply(cs, `[[`, "", "docId"), levels = docs))
    list(codeId = c$id, code = paths[[c$id]], color = c$color, counts = as.list(unname(as.integer(n))), total = length(cs))
  })
  list(documents = lapply(docs, function(d) list(id = d, name = all_docs[[d]]$name)), rows = rows)
}
