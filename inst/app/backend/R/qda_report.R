# Codebook and coded-segment exports of the Text model (NVivo / ATLAS.ti / MAXQDA reports): an Excel workbook (codebook
# with frequencies, coded segments with weights, paraphrases and memos, Summary Grid, memos, documents and attributes)
# and a Word report (each code with its definition, coded segments as quotes, paraphrases and summaries). Every row is
# built from the project's codings; nothing is computed in the browser.

qda_report_tables <- function(proj, q, b) {
  docs <- proj$documents %||% list()
  paths <- qda_paths(q$codes)
  ord <- order(vapply(q$codes, function(c) paths[[c$id]], ""))
  codes <- q$codes[ord]
  cs <- Filter(function(x) x$docId %in% names(docs), q$codings)
  seg_text <- function(x) if (!is.null(x$region)) sprintf("[region on page %d]", x$region$page) else qda_slice(docs[[x$docId]]$text, x$start, x$end)
  codebook <- data.frame(
    Code = vapply(codes, function(c) paths[[c$id]], ""), Name = vapply(codes, `[[`, "", "name"), Definition = vapply(codes, function(c) c$description %||% "", ""),
    Colour = vapply(codes, `[[`, "", "color"),
    Segments = vapply(codes, function(c) sum(vapply(cs, function(x) x$codeId == c$id, TRUE)), 0L),
    Documents = vapply(codes, function(c) length(unique(vapply(Filter(function(x) x$codeId == c$id, cs), `[[`, "", "docId"))), 0L),
    stringsAsFactors = FALSE, check.names = FALSE)
  segs <- if (length(cs)) do.call(rbind, lapply(cs, function(x) data.frame(
    Code = paths[[x$codeId]] %||% "", Document = docs[[x$docId]]$name, Coder = x$coder, Start = x$start, End = x$end,
    Weight = x$weight %||% NA_real_, Text = seg_text(x), Paraphrase = x$paraphrase %||% "", Memo = x$memo %||% "", stringsAsFactors = FALSE))) else
    data.frame(Code = character(0), Document = character(0), Coder = character(0), Start = integer(0), End = integer(0), Weight = numeric(0), Text = character(0), Paraphrase = character(0), Memo = character(0))
  if (nrow(segs)) segs <- segs[order(segs$Code, segs$Document, segs$Start), , drop = FALSE]
  sums <- b$summaries %||% list()
  summaries <- if (length(sums)) do.call(rbind, lapply(names(sums), function(k) { p <- strsplit(k, "|", fixed = TRUE)[[1]]
    data.frame(Code = paths[[p[1]]] %||% p[1], Document = docs[[p[2]]]$name %||% p[2], Summary = as.character(sums[[k]]), stringsAsFactors = FALSE) })) else
    data.frame(Code = character(0), Document = character(0), Summary = character(0))
  memos <- b$memos %||% list()
  memo_df <- data.frame(Title = vapply(memos, function(m) as.character(m$title %||% ""), ""), Text = vapply(memos, function(m) as.character(m$text %||% ""), ""), stringsAsFactors = FALSE)
  attrs <- unique(unlist(lapply(docs, function(d) names(d$attributes))))
  doc_df <- data.frame(Document = vapply(docs, `[[`, "", "name"), Type = vapply(docs, `[[`, "", "kind"), Words = vapply(docs, function(d) text_count_words(d$text), 0L), stringsAsFactors = FALSE, check.names = FALSE)
  for (a in attrs) doc_df[[a]] <- vapply(docs, function(d) { v <- d$attributes[[a]]; if (is.null(v)) "" else as.character(v) }, "")
  list(codebook = codebook, segments = segs, summaries = summaries, memos = memo_df, documents = doc_df)
}

qda_report <- function(proj, q, b, format = "xlsx", project_name = "PathLab project") {
  tb <- qda_report_tables(proj, q, b)
  path <- tempfile(fileext = paste0(".", format))
  if (format == "xlsx") {
    writexl::write_xlsx(list(Codebook = tb$codebook, `Coded segments` = tb$segments, `Summary grid` = tb$summaries, Memos = tb$memos, Documents = tb$documents), path)
    return(path)
  }
  doc <- officer::read_docx()
  par <- function(d, txt, style = "Normal") officer::body_add_par(d, txt, style = style)
  quote <- function(d, txt) officer::body_add_fpar(d, officer::fpar(officer::ftext(txt, officer::fp_text(italic = TRUE, font.size = 10)), fp_p = officer::fp_par(padding.left = 24, padding.top = 2)))
  doc <- par(doc, sprintf("%s: codebook and coded segments", project_name), "heading 1")
  doc <- par(doc, sprintf("%d codes, %d coded segments in %d documents. Exported from PathLab SEM %s on %s.", nrow(tb$codebook), nrow(tb$segments), nrow(tb$documents), PATHLAB_RELEASE, format(Sys.Date())))
  for (i in seq_len(nrow(tb$codebook))) {
    cb <- tb$codebook[i, ]
    doc <- par(doc, cb$Code, "heading 2")
    if (nzchar(cb$Definition)) doc <- par(doc, paste("Definition:", cb$Definition))
    doc <- par(doc, sprintf("%d segment(s) in %d document(s).", cb$Segments, cb$Documents))
    ss <- tb$segments[tb$segments$Code == cb$Code, , drop = FALSE]
    for (j in seq_len(nrow(ss))) {
      doc <- quote(doc, sprintf("“%s” (%s%s)", ss$Text[j], ss$Document[j], if (!is.na(ss$Weight[j]) && ss$Weight[j] > 0) sprintf(", weight %g", ss$Weight[j]) else ""))
      if (nzchar(ss$Paraphrase[j])) doc <- par(doc, paste("Paraphrase:", ss$Paraphrase[j]))
      if (nzchar(ss$Memo[j])) doc <- par(doc, paste("Memo:", ss$Memo[j]))
    }
    su <- tb$summaries[tb$summaries$Code == cb$Code, , drop = FALSE]
    for (j in seq_len(nrow(su))) doc <- par(doc, sprintf("Summary, %s: %s", su$Document[j], su$Summary[j]))
  }
  if (nrow(tb$memos)) { doc <- par(doc, "Memos", "heading 2"); for (j in seq_len(nrow(tb$memos))) doc <- par(doc, sprintf("%s: %s", tb$memos$Title[j], tb$memos$Text[j])) }
  print(doc, target = path)
  path
}
