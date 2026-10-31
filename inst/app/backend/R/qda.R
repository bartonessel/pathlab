# Textual analysis, Part B: qualitative coding. The codebook, the coded segments (codings) and memos are kept in the
# project document (the browser); the documents are in the server session. Here, in R: retrieval of coded segments,
# code frequencies, code co-occurrence (overlap and document level, with the c-coefficient), code × attribute matrices,
# text search (quanteda KWIC), inter-coder agreement (irr: Cohen's kappa, Krippendorff's alpha, percent agreement),
# code presence as dataset variables (mixed methods), and REFI-QDA exchange (.qdpx projects, .qdc codebooks) — the open
# standard that NVivo, ATLAS.ti, MAXQDA and others import and export.
#
# Positions: the browser counts UTF-16 code units, R and REFI-QDA count characters (code points); qda_js2cp / qda_cp2js
# convert, so quotations stay exact with any characters.

qda_units <- function(text) { cp <- utf8ToInt(text); if (!length(cp)) integer(0) else ifelse(cp > 0xFFFF, 2L, 1L) }
qda_js2cp <- function(text, pos) { u <- c(0L, cumsum(qda_units(text))); vapply(pos, function(p) max(0L, findInterval(p, u) - 1L), 0L) }
qda_cp2js <- function(text, pos) { u <- c(0L, cumsum(qda_units(text))); vapply(pos, function(p) u[min(length(u), p + 1L)], 0L) }
qda_slice <- function(text, start, end) { cp <- qda_js2cp(text, c(start, end)); substr(text, cp[1] + 1L, cp[2]) }

normalize_qda <- function(b) {
  codes <- lapply(b$codes %||% list(), function(c) list(id = as.character(c$id %||% ""), name = substr(as.character(c$name %||% ""), 1, 200), color = as.character(c$color %||% "#2f6db5"),
                                                         parentId = as.character(c$parentId %||% ""), description = substr(as.character(c$description %||% ""), 1, 4000)))
  codes <- Filter(function(c) nzchar(c$id) && nzchar(c$name), codes)
  if (length(codes) > 2000) api_error(400, "qda_codes", "A codebook can have at most 2000 codes.")
  ids <- vapply(codes, `[[`, "", "id")
  num <- function(v) { v <- suppressWarnings(as.numeric(v %||% NA))[1]; if (is.na(v)) NULL else v }
  codings <- lapply(b$codings %||% list(), function(x) {
    out <- list(id = as.character(x$id %||% ""), docId = as.character(x$docId %||% ""), codeId = as.character(x$codeId %||% ""),
                start = suppressWarnings(as.integer(x$start %||% NA)), end = suppressWarnings(as.integer(x$end %||% NA)),
                coder = substr(as.character(x$coder %||% "Coder 1"), 1, 80), memo = substr(as.character(x$memo %||% ""), 1, 4000),
                weight = num(x$weight), paraphrase = substr(as.character(x$paraphrase %||% ""), 1, 4000))
    # A region of an image or a PDF page (relative coordinates); its text span is empty.
    r <- x$region
    if (is.list(r) && !is.null(num(r$x)) && !is.null(num(r$w))) out$region <- list(page = as.integer(num(r$page) %||% 1), x = num(r$x), y = num(r$y), w = num(r$w), h = num(r$h))
    out
  })
  codings <- Filter(function(x) x$codeId %in% ids && (!is.null(x$region) || (!is.na(x$start) && !is.na(x$end) && x$end > x$start)), codings)
  if (length(codings) > 1e5) api_error(400, "qda_codings", "At most 100,000 coded segments.")
  list(codes = codes, codings = codings)
}

qda_code_names <- function(codes) setNames(vapply(codes, `[[`, "", "name"), vapply(codes, `[[`, "", "id"))
#' Full path of each code in the hierarchy (Parent > Child).
qda_paths <- function(codes) {
  nm <- qda_code_names(codes); par <- setNames(vapply(codes, `[[`, "", "parentId"), names(nm))
  vapply(names(nm), function(id) { p <- nm[[id]]; seen <- id; k <- par[[id]]; while (nzchar(k) && k %in% names(nm) && !k %in% seen) { p <- paste(nm[[k]], ">", p); seen <- c(seen, k); k <- par[[k]] }; p }, "")
}

#' Code frequencies: segments, documents, coded words, per coder.
qda_overview <- function(proj, q) {
  docs <- proj$documents %||% list()
  paths <- qda_paths(q$codes)
  rows <- lapply(q$codes, function(c) {
    cs <- Filter(function(x) x$codeId == c$id && x$docId %in% names(docs), q$codings)
    words <- sum(vapply(cs, function(x) if (!is.null(x$region)) 0 else text_count_words(qda_slice(docs[[x$docId]]$text, x$start, x$end)), 0))
    list(codeId = c$id, code = paths[[c$id]], segments = length(cs), documents = length(unique(vapply(cs, `[[`, "", "docId"))), words = words,
         coders = length(unique(vapply(cs, `[[`, "", "coder"))))
  })
  list(rows = rows, nDocuments = length(docs), nCoded = length(unique(vapply(Filter(function(x) x$docId %in% names(docs), q$codings), `[[`, "", "docId"))))
}

#' Coded segments of the chosen codes, with their text (retrieval).
qda_retrieve <- function(proj, q, code_ids, max = 2000L) {
  docs <- proj$documents %||% list(); paths <- qda_paths(q$codes)
  cs <- Filter(function(x) x$codeId %in% code_ids && x$docId %in% names(docs), q$codings)
  cs <- cs[order(match(vapply(cs, `[[`, "", "docId"), names(docs)), vapply(cs, `[[`, 0L, "start"))]   # document order, then position
  lapply(utils::head(cs, max), function(x) list(id = x$id, codeId = x$codeId, code = paths[[x$codeId]], docId = x$docId, document = docs[[x$docId]]$name,
                                                start = x$start, end = x$end, coder = x$coder, memo = x$memo, weight = x$weight, paraphrase = x$paraphrase, region = x$region,
                                                text = if (!is.null(x$region)) sprintf("[region on page %d]", x$region$page) else qda_slice(docs[[x$docId]]$text, x$start, x$end)))
}

#' Co-occurrence of codes: overlapping segments in the same document, or presence in the same document.
qda_cooccurrence <- function(proj, q, level = "overlap") {
  docs <- names(proj$documents %||% list())
  cs <- Filter(function(x) x$docId %in% docs, q$codings)
  ids <- vapply(q$codes, `[[`, "", "id"); paths <- qda_paths(q$codes)
  n <- length(ids); m <- matrix(0L, n, n, dimnames = list(ids, ids))
  if (level == "document") {
    pres <- sapply(ids, function(id) docs %in% vapply(Filter(function(x) x$codeId == id, cs), `[[`, "", "docId"))
    if (is.null(dim(pres))) pres <- matrix(pres, ncol = n)
    m[] <- crossprod(pres * 1L)
  } else {
    by_doc <- split(cs, vapply(cs, `[[`, "", "docId"))
    for (d in by_doc) for (i in seq_along(d)) for (j in seq_along(d)) if (i < j) {
      a <- d[[i]]; b <- d[[j]]
      if (a$codeId != b$codeId && qda_overlap(a, b)) { m[a$codeId, b$codeId] <- m[a$codeId, b$codeId] + 1L; m[b$codeId, a$codeId] <- m[b$codeId, a$codeId] + 1L }
    }
    for (id in ids) m[id, id] <- sum(vapply(cs, function(x) x$codeId == id, TRUE))
  }
  diag_n <- diag(m)
  cc <- m / pmax(1, outer(diag_n, diag_n, "+") - m)   # ATLAS.ti c-coefficient n12 / (n1 + n2 - n12)
  diag(cc) <- NA
  list(level = level, codes = lapply(ids, function(id) list(id = id, code = paths[[id]])), counts = lapply(seq_len(n), function(i) as.list(unname(m[i, ]))),
       coefficient = lapply(seq_len(n), function(i) as.list(unname(cc[i, ]))))
}

#' Code × attribute: number of documents (and segments) with each code for each attribute value.
qda_attributes <- function(proj, q, attribute) {
  docs <- proj$documents %||% list()
  vals <- vapply(docs, function(d) { v <- d$attributes[[attribute]]; if (is.null(v)) "(missing)" else as.character(v) }, "")
  lev <- sort(unique(vals)); paths <- qda_paths(q$codes)
  rows <- lapply(q$codes, function(c) {
    cs <- Filter(function(x) x$codeId == c$id && x$docId %in% names(docs), q$codings)
    dd <- unique(vapply(cs, `[[`, "", "docId"))
    list(codeId = c$id, code = paths[[c$id]], documents = as.list(unname(vapply(lev, function(l) sum(vals[dd] == l), 0L))),
         segments = as.list(unname(vapply(lev, function(l) sum(vals[vapply(cs, `[[`, "", "docId")] == l), 0L))))
  })
  list(attribute = attribute, values = as.list(lev), totals = as.list(unname(vapply(lev, function(l) sum(vals == l), 0L))), rows = rows)
}

#' Text search: keywords in context (quanteda::kwic), with character positions for jumping to the passage.
qda_kwic <- function(proj, ids, pattern, window = 6L, case = FALSE) {
  if (!nzchar(trimws(pattern %||% ""))) api_error(400, "qda_pattern", "Enter a word or phrase to search for.")
  docs <- text_docs(proj, ids)
  texts <- vapply(docs, `[[`, "", "text"); names(texts) <- names(docs)
  tk <- quanteda::tokens(quanteda::corpus(texts))
  pat <- if (grepl("\\s", trimws(pattern))) quanteda::phrase(trimws(pattern)) else trimws(pattern)
  kw <- as.data.frame(quanteda::kwic(tk, pattern = pat, window = window, case_insensitive = !isTRUE(case), valuetype = "glob"))
  if (!nrow(kw)) return(list(pattern = pattern, hits = list(), nDocs = 0L))
  hits <- lapply(seq_len(min(nrow(kw), 2000L)), function(i) list(docId = kw$docname[i], document = docs[[kw$docname[i]]]$name, pre = kw$pre[i], keyword = kw$keyword[i], post = kw$post[i], from = kw$from[i]))
  list(pattern = pattern, hits = hits, nHits = nrow(kw), nDocs = length(unique(kw$docname)))
}

#' Inter-coder agreement on code presence per document (documents coded by both coders).
qda_agreement <- function(proj, q, coder1, coder2) {
  if (!requireNamespace("irr", quietly = TRUE)) api_error(501, "qda_unavailable", "The irr R package is not installed on the analysis server.")
  docs <- names(proj$documents %||% list())
  by <- function(cd) Filter(function(x) x$coder == cd && x$docId %in% docs, q$codings)
  c1 <- by(coder1); c2 <- by(coder2)
  units <- intersect(unique(vapply(c1, `[[`, "", "docId")), unique(vapply(c2, `[[`, "", "docId")))
  if (length(units) < 2) api_error(422, "qda_agreement", sprintf("At least two documents must be coded by both %s and %s.", coder1, coder2))
  paths <- qda_paths(q$codes)
  rows <- lapply(q$codes, function(c) {
    p1 <- as.integer(units %in% vapply(Filter(function(x) x$codeId == c$id, c1), `[[`, "", "docId"))
    p2 <- as.integer(units %in% vapply(Filter(function(x) x$codeId == c$id, c2), `[[`, "", "docId"))
    if (!any(c(p1, p2) == 1L)) return(NULL)
    r <- cbind(p1, p2)
    k <- suppressWarnings(irr::kappa2(r)); a <- irr::agree(r); ka <- suppressWarnings(irr::kripp.alpha(t(r), method = "nominal"))
    list(codeId = c$id, code = paths[[c$id]], units = length(units), agreement = a$value / 100, kappa = if (is.finite(k$value)) k$value else NULL, kappaP = if (is.finite(k$p.value)) k$p.value else NULL,
         alpha = if (is.finite(ka$value)) ka$value else NULL, coder1 = sum(p1), coder2 = sum(p2))
  })
  rows <- Filter(Negate(is.null), rows)
  pooled <- if (length(rows)) {
    P <- do.call(rbind, lapply(q$codes, function(c) cbind(as.integer(units %in% vapply(Filter(function(x) x$codeId == c$id, c1), `[[`, "", "docId")),
                                                         as.integer(units %in% vapply(Filter(function(x) x$codeId == c$id, c2), `[[`, "", "docId")))))
    k <- suppressWarnings(irr::kappa2(P)); ka <- suppressWarnings(irr::kripp.alpha(t(P), method = "nominal"))
    list(kappa = k$value, alpha = ka$value, agreement = irr::agree(P)$value / 100, decisions = nrow(P))
  } else NULL
  list(coder1 = coder1, coder2 = coder2, units = length(units), rows = rows, pooled = pooled,
       rCode = paste(c("library(irr)", "# ratings: one row per document x code decision (1 = code applied), one column per coder",
                       "kappa2(ratings)              # Cohen's kappa", "kripp.alpha(t(ratings), method = \"nominal\")   # Krippendorff's alpha", "agree(ratings)               # percent agreement"), collapse = "\n"))
}

#' Code presence per respondent as derived variables of the dataset (1 = coded, 0 = answered but not coded, NA = no document).
qda_to_dataset <- function(proj, did, q, code_ids) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.")
  docs <- Filter(function(d) identical(d$datasetId, did) && !is.null(d$datasetRow), proj$documents %||% list())
  if (!length(docs)) api_error(422, "qda_no_link", "No documents are linked to this dataset.", "Create the documents with “From data” so that each answer is linked to its respondent.")
  n <- nrow(ds$data); paths <- qda_paths(q$codes); nm <- qda_code_names(q$codes)
  ds$derived <- ds$derived %||% list()
  made <- character(0)
  for (cid in intersect(code_ids, names(nm))) {
    v <- rep(NA_integer_, n)
    for (d in docs) v[d$datasetRow] <- 0L
    coded <- unique(vapply(Filter(function(x) x$codeId == cid && x$docId %in% names(docs), q$codings), `[[`, "", "docId"))
    for (d in docs[coded]) v[d$datasetRow] <- 1L
    name <- substr(paste0("code_", gsub("[^A-Za-z0-9]+", "_", nm[[cid]])), 1, 60)
    if (name %in% names(ds$data)) name <- paste0(name, "_2")
    ds$derived[[name]] <- list(name = name, values = v, kind = "text_codes", kindLabel = "Code presence (text analysis)", analysisId = "", analysisName = "",
                               resultId = "", set = "", from = cid, label = paths[[cid]], nValues = sum(!is.na(v)), createdAt = now_iso())
    made <- c(made, name)
  }
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  list(dataset = dataset_with_derived(ds), variables = as.list(made))
}

# ---- REFI-QDA (.qdpx / .qdc) ---------------------------------------------------------------------------------------
qda_guid <- function() { h <- sample(c(0:9, letters[1:6]), 32, TRUE); paste0(paste(h[1:8], collapse = ""), "-", paste(h[9:12], collapse = ""), "-4", paste(h[14:16], collapse = ""), "-", sample(c("8", "9", "a", "b"), 1), paste(h[18:20], collapse = ""), "-", paste(h[21:32], collapse = "")) }

qda_codebook_xml <- function(parent, codes, guid_of) {
  kids <- function(pid) Filter(function(c) identical(c$parentId, pid), codes)
  add <- function(node, pid) for (c in kids(pid)) {
    e <- xml2::xml_add_child(node, "Code", guid = guid_of[[c$id]], name = c$name, isCodable = "true", color = toupper(c$color))
    if (nzchar(c$description)) xml2::xml_add_child(e, "Description", c$description)
    add(e, c$id)
  }
  # Codes whose parent is not in the codebook become top-level codes.
  codes <- lapply(codes, function(c) { if (nzchar(c$parentId) && !c$parentId %in% vapply(codes, `[[`, "", "id")) c$parentId <- ""; c })
  add(xml2::xml_add_child(parent, "Codes"), "")
}

qda_export <- function(proj, q, memos, ids, format = "qdpx", project_name = "PathLab project") {
  if (!requireNamespace("xml2", quietly = TRUE)) api_error(501, "qda_unavailable", "The xml2 R package is not installed.")
  guid_of <- setNames(lapply(q$codes, function(c) qda_guid()), vapply(q$codes, `[[`, "", "id"))
  if (format == "qdc") {
    x <- xml2::read_xml("<CodeBook xmlns=\"urn:QDA-XML:codebook:1.0\" origin=\"PathLab SEM\"/>")
    qda_codebook_xml(x, q$codes, guid_of)
    path <- tempfile(fileext = ".qdc"); xml2::write_xml(x, path, options = c("format", "as_xml")); return(path)
  }
  docs <- text_docs(proj, ids)
  x <- xml2::read_xml(sprintf("<Project xmlns=\"urn:QDA-XML:project:1.0\" name=\"%s\" origin=\"PathLab SEM %s\" creationDateTime=\"%s\"/>",
                              gsub("[\"<>&]", "", project_name), PATHLAB_VERSION, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  coders <- unique(c("Coder 1", vapply(q$codings, `[[`, "", "coder"))); user_guid <- setNames(lapply(coders, function(u) qda_guid()), coders)
  us <- xml2::xml_add_child(x, "Users"); for (u in coders) xml2::xml_add_child(us, "User", guid = user_guid[[u]], name = u)
  qda_codebook_xml(xml2::xml_add_child(x, "CodeBook"), q$codes, guid_of)
  dir <- tempfile("qdpx"); dir.create(file.path(dir, "sources"), recursive = TRUE)
  src <- xml2::xml_add_child(x, "Sources")
  for (d in docs) {
    g <- qda_guid()
    if (identical(d$visual$type, "image") && file.exists(d$visual$file %||% "")) {   # a picture and its coded regions (in pixels)
      file.copy(d$visual$file, file.path(dir, "sources", paste0(g, ".", d$visual$ext)))
      ps <- xml2::xml_add_child(src, "PictureSource", guid = g, name = d$name, path = paste0("internal://", g, ".", d$visual$ext))
      for (cg in Filter(function(z) z$docId == d$id && !is.null(z$region), q$codings)) {
        r <- cg$region; W <- d$visual$width; H <- d$visual$height
        sel <- xml2::xml_add_child(ps, "PictureSelection", guid = qda_guid(), name = "Region", firstX = round(r$x * W), firstY = round(r$y * H), secondX = round((r$x + r$w) * W), secondY = round((r$y + r$h) * H),
                                   creatingUser = user_guid[[cg$coder]])
        if (nzchar(cg$memo)) xml2::xml_add_child(sel, "Description", cg$memo)
        xml2::xml_add_child(xml2::xml_add_child(sel, "Coding", guid = qda_guid(), creatingUser = user_guid[[cg$coder]]), "CodeRef", targetGUID = guid_of[[cg$codeId]])
      }
      next
    }
    writeLines(enc2utf8(d$text), file.path(dir, "sources", paste0(g, ".txt")), useBytes = TRUE, sep = "")
    ts <- xml2::xml_add_child(src, "TextSource", guid = g, name = d$name, plainTextPath = paste0("internal://", g, ".txt"))
    for (cg in Filter(function(z) z$docId == d$id && is.null(z$region), q$codings)) {   # PDF page regions are not part of REFI-QDA text sources
      cp <- qda_js2cp(d$text, c(cg$start, cg$end))
      sel <- xml2::xml_add_child(ts, "PlainTextSelection", guid = qda_guid(), name = substr(qda_slice(d$text, cg$start, cg$end), 1, 60), startPosition = cp[1], endPosition = cp[2],
                                 creatingUser = user_guid[[cg$coder]])
      if (nzchar(cg$memo)) xml2::xml_add_child(sel, "Description", cg$memo)
      cod <- xml2::xml_add_child(sel, "Coding", guid = qda_guid(), creatingUser = user_guid[[cg$coder]])
      xml2::xml_add_child(cod, "CodeRef", targetGUID = guid_of[[cg$codeId]])
    }
  }
  if (length(memos)) { nt <- xml2::xml_add_child(x, "Notes"); for (m in memos) { n <- xml2::xml_add_child(nt, "Note", guid = qda_guid(), name = substr(as.character(m$title %||% "Memo"), 1, 100)); xml2::xml_add_child(n, "PlainTextContent", as.character(m$text %||% "")) } }
  xml2::write_xml(x, file.path(dir, "project.qde"), options = c("format", "as_xml"))
  path <- tempfile(fileext = ".qdpx")
  zip::zip(path, files = c("project.qde", file.path("sources", list.files(file.path(dir, "sources")))), root = dir, mode = "mirror")
  path
}

#' Import a .qdpx project (documents, codebook, coded segments, memos) or a .qdc codebook. With `merge` (a colleague's
#' coding of the same documents), a source whose name and text match a document already in the session is coded there
#' instead of being added again; `matched` counts them.
qda_import <- function(proj, raw, filename, merge = FALSE) {
  ext <- tolower(tools::file_ext(filename))
  tmp <- tempfile(fileext = paste0(".", ext)); writeBin(raw, tmp)
  read_codes <- function(x) {
    ns <- xml2::xml_ns_rename(xml2::xml_ns(x), d1 = "q")
    out <- list()
    walk <- function(node, parent) for (cn in xml2::xml_find_all(node, "./q:Code", ns)) {
      id <- paste0("c_", substr(gsub("-", "", xml2::xml_attr(cn, "guid")), 1, 12))
      desc <- xml2::xml_text(xml2::xml_find_first(cn, "./q:Description", ns)); if (is.na(desc)) desc <- ""
      col <- xml2::xml_attr(cn, "color"); if (is.na(col) || !grepl("^#[0-9A-Fa-f]{6}", col)) col <- "#2F6DB5"
      out[[length(out) + 1]] <<- list(id = id, guid = xml2::xml_attr(cn, "guid"), name = xml2::xml_attr(cn, "name"), color = tolower(substr(col, 1, 7)), parentId = parent, description = desc)
      walk(cn, id)
    }
    cb <- xml2::xml_find_first(x, ".//q:Codes", ns)
    if (!inherits(cb, "xml_missing")) walk(cb, "")
    list(codes = out, ns = ns)
  }
  if (ext == "qdc") { x <- xml2::read_xml(tmp); r <- read_codes(x); return(list(codes = lapply(r$codes, function(c) c[names(c) != "guid"]), codings = list(), documents = list(), memos = list())) }
  if (ext != "qdpx") api_error(400, "qda_format", "Choose a REFI-QDA project (.qdpx) or codebook (.qdc).")
  dir <- tempfile("qdpx_in"); zip::unzip(tmp, exdir = dir)
  qde <- list.files(dir, pattern = "\\.qde$", recursive = TRUE, full.names = TRUE)[1]
  if (is.na(qde)) api_error(422, "qda_format", "The .qdpx file has no project.qde.")
  x <- xml2::read_xml(qde); r <- read_codes(x); ns <- r$ns
  code_by_guid <- setNames(vapply(r$codes, `[[`, "", "id"), vapply(r$codes, `[[`, "", "guid"))
  users <- xml2::xml_find_all(x, ".//q:Users/q:User", ns); user_by_guid <- setNames(xml2::xml_attr(users, "name"), xml2::xml_attr(users, "guid"))
  docs <- list(); codings <- list(); matched <- 0L
  for (ts in xml2::xml_find_all(x, ".//q:Sources/q:TextSource", ns)) {
    ptc <- xml2::xml_find_first(ts, "./q:PlainTextContent", ns)
    text <- if (!inherits(ptc, "xml_missing")) xml2::xml_text(ptc) else {
      p <- sub("^internal://", "", xml2::xml_attr(ts, "plainTextPath") %||% "")
      f <- list.files(dir, pattern = paste0("^", gsub("([.])", "\\\\\\1", basename(p)), "$"), recursive = TRUE, full.names = TRUE)[1]
      if (is.na(f)) "" else paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    }
    if (!nzchar(text)) next
    text <- text_utf8(text); nm <- xml2::xml_attr(ts, "name")
    same <- if (merge) Filter(function(o) identical(o$name, nm) && identical(o$text, text), proj$documents %||% list()) else list()
    if (length(same)) { d <- same[[1]]; matched <- matched + 1L } else {
      d <- text_new_doc(proj, nm, filename, "qdpx", text)
      docs[[length(docs) + 1]] <- text_meta(d)
    }
    for (sel in xml2::xml_find_all(ts, "./q:PlainTextSelection", ns)) {
      s <- as.integer(xml2::xml_attr(sel, "startPosition")); e <- as.integer(xml2::xml_attr(sel, "endPosition"))
      js <- qda_cp2js(d$text, c(s, e))
      memo <- xml2::xml_text(xml2::xml_find_first(sel, "./q:Description", ns)); if (is.na(memo)) memo <- ""
      coder <- user_by_guid[xml2::xml_attr(sel, "creatingUser")]; coder <- if (is.na(coder)) "Coder 1" else unname(coder)
      for (cr in xml2::xml_find_all(sel, ".//q:CodeRef", ns)) {
        cid <- code_by_guid[xml2::xml_attr(cr, "targetGUID")]
        if (!is.na(cid)) codings[[length(codings) + 1]] <- list(id = paste0("g_", substr(gsub("-", "", qda_guid()), 1, 12)), docId = d$id, codeId = unname(cid), start = js[1], end = js[2], coder = coder, memo = memo)
      }
    }
  }
  memos <- lapply(xml2::xml_find_all(x, ".//q:Notes/q:Note", ns), function(n) list(title = xml2::xml_attr(n, "name") %||% "Memo", text = xml2::xml_text(xml2::xml_find_first(n, ".//q:PlainTextContent", ns))))
  list(codes = lapply(r$codes, function(c) c[names(c) != "guid"]), codings = codings, documents = docs, memos = memos, matched = matched)
}
