# More document sources for the Text model, as NVivo, ATLAS.ti and MAXQDA import them: reference-manager exports (RIS
# from Zotero, EndNote and Mendeley; BibTeX; PubMed / MEDLINE .nbib), one document per reference (title and abstract,
# with authors, year, journal, DOI and keywords as attributes), and web pages by address (the main text of the page,
# with its address and title). Surveys with several open questions are handled in text_from_table (one document per
# respondent, a section per question).

REF_EXTS <- c("ris", "bib", "nbib", "enw")

#' References of a RIS / MEDLINE file (tagged lines "TAG  - value", continuation lines indented).
ref_parse_tagged <- function(lines, medline = FALSE) {
  recs <- list(); cur <- list(); last <- NULL
  flush <- function() { if (length(cur)) recs[[length(recs) + 1]] <<- cur; cur <<- list(); last <<- NULL }
  for (ln in lines) {
    m <- regmatches(ln, regexec("^([A-Z][A-Z0-9]{1,3})\\s*-\\s?(.*)$", ln))[[1]]
    if (length(m)) {
      tag <- m[2]; val <- trimws(m[3])
      if (!medline && tag == "ER") { flush(); next }
      if (medline && tag == "PMID" && length(cur)) flush()
      cur[[tag]] <- c(cur[[tag]], val); last <- tag
    } else if (!is.null(last) && grepl("^\\s{2,}\\S", ln)) {
      k <- length(cur[[last]]); cur[[last]][k] <- paste(cur[[last]][k], trimws(ln))
    } else if (!nzchar(trimws(ln)) && medline) { if (length(cur)) flush() }
  }
  flush()
  pick <- function(r, tags) { for (t in tags) if (length(r[[t]])) return(r[[t]]); character(0) }
  lapply(recs, function(r) list(
    title = paste(pick(r, c("TI", "T1", "CT", "BT")), collapse = " "), abstract = paste(pick(r, c("AB", "N2")), collapse = " "),
    authors = pick(r, c("AU", "A1", "FAU", "A2")), year = substr(paste(pick(r, c("PY", "Y1", "DP", "DA")), collapse = ""), 1, 4),
    journal = paste(pick(r, c("JO", "JF", "T2", "JT", "TA", "J2")), collapse = "; ")[1], doi = sub("\\s.*$", "", paste(c(pick(r, c("DO")), sub(" \\[doi\\]$", "", grep("\\[doi\\]", pick(r, "AID"), value = TRUE)))[1], collapse = "")),
    keywords = pick(r, c("KW", "MH", "OT")), url = pick(r, c("UR", "L2"))[1] %||% "", type = pick(r, c("TY", "PT"))[1] %||% ""))
}

#' References of a BibTeX file (entries @type{key, field = {value} | "value" | number, ...}).
ref_parse_bibtex <- function(txt) {
  out <- list()
  starts <- gregexpr("@[A-Za-z]+\\s*\\{", txt)[[1]]
  if (starts[1] < 0) return(out)
  chars <- strsplit(txt, "")[[1]]
  clean <- function(v) trimws(gsub("\\s+", " ", gsub("[{}]", "", gsub("\\\\[a-zA-Z]+\\s*|\\\\.", "", v))))
  for (s in starts) {
    open <- s + attr(regexpr("@[A-Za-z]+\\s*\\{", substring(txt, s)), "match.length") - 1
    depth <- 1; i <- open + 1
    while (i <= length(chars) && depth > 0) { if (chars[i] == "{") depth <- depth + 1 else if (chars[i] == "}") depth <- depth - 1; i <- i + 1 }
    body <- substr(txt, open + 1, i - 2)
    type <- tolower(sub("^@([A-Za-z]+).*$", "\\1", substr(txt, s, open)))
    if (type %in% c("comment", "string", "preamble")) next
    f <- list(); j <- regexpr(",", body); rest <- if (j > 0) substring(body, j + 1) else ""
    repeat {
      m <- regexpr("^\\s*([A-Za-z_-]+)\\s*=\\s*", rest); if (m < 0) break
      key <- tolower(sub("^\\s*([A-Za-z_-]+)\\s*=\\s*$", "\\1", regmatches(rest, m))); rest <- substring(rest, attr(m, "match.length") + 1)
      ch <- substr(rest, 1, 1)
      if (ch == "{") { d <- 0; k <- 1; rc <- strsplit(rest, "")[[1]]; repeat { if (rc[k] == "{") d <- d + 1 else if (rc[k] == "}") { d <- d - 1; if (d == 0) break }; k <- k + 1; if (k > length(rc)) break }; val <- substr(rest, 2, k - 1); rest <- substring(rest, k + 1) }
      else if (ch == "\"") {
        if (substr(rest, 2, 2) == "\"") { val <- ""; rest <- substring(rest, 3) }
        else { k <- regexpr("[^\\\\]\"", substring(rest, 2)); val <- substr(rest, 2, k + 1); rest <- substring(rest, k + 3) }
      }
      else { k <- regexpr("[,}]|$", rest); val <- substr(rest, 1, k - 1); rest <- substring(rest, k) }
      f[[key]] <- clean(val)
      rest <- sub("^\\s*,", "", rest)
    }
    out[[length(out) + 1]] <- list(title = f$title %||% "", abstract = f$abstract %||% "", authors = trimws(strsplit(f$author %||% "", "\\s+and\\s+")[[1]]),
                                   year = substr(f$year %||% "", 1, 4), journal = f$journal %||% f$booktitle %||% "", doi = f$doi %||% "",
                                   keywords = trimws(strsplit(f$keywords %||% "", "[,;]")[[1]]), url = f$url %||% "", type = type)
  }
  out
}

#' One document per reference with a title or abstract.
text_import_refs <- function(proj, raw, filename) {
  ext <- tolower(tools::file_ext(filename))
  txt <- text_utf8(rawToChar(raw[raw != as.raw(0)]))
  refs <- if (ext == "bib") ref_parse_bibtex(txt) else ref_parse_tagged(strsplit(txt, "\n")[[1]], medline = ext == "nbib" || grepl("^PMID-", txt))
  refs <- Filter(function(r) nzchar(r$title) || nzchar(r$abstract), refs)
  if (!length(refs)) api_error(422, "refs_empty", sprintf("No references with a title or abstract were found in %s.", filename), "Export RIS, BibTeX or PubMed (MEDLINE) with abstracts.")
  if (length(refs) + length(proj$documents %||% list()) > TEXT_MAX_DOCS) api_error(413, "text_too_many", sprintf("A project can hold at most %d documents.", TEXT_MAX_DOCS))
  lapply(refs, function(r) {
    first <- if (length(r$authors)) sub(",.*$", "", r$authors[1]) else ""
    nm <- substr(trimws(paste0(if (nzchar(first)) paste0(first, if (length(r$authors) > 1) " et al." else "", " ") else "", if (nzchar(r$year)) paste0("(", r$year, ") ") else "", r$title)), 1, 200)
    at <- Filter(function(v) !is.null(v) && nzchar(v), list(authors = paste(r$authors, collapse = "; "), year = r$year, journal = r$journal, doi = r$doi, keywords = paste(r$keywords, collapse = "; "), url = r$url, type = r$type))
    d <- text_new_doc(proj, if (nzchar(nm)) nm else "Reference", filename, "ref", trimws(paste(r$title, r$abstract, sep = "\n\n")), at)
    text_meta(d)
  })
}

#' A web page: its main text (scripts, menus, headers and footers removed), with its address and title as attributes.
text_import_url <- function(proj, url) {
  url <- trimws(as.character(url %||% ""))
  if (!grepl("^https?://[^/\\s]+", url)) api_error(400, "url_invalid", "Enter a web address starting with http:// or https://.")
  host <- tolower(sub("^https?://([^/:?#]+).*$", "\\1", url))
  if (grepl("^(localhost|127\\.|10\\.|192\\.168\\.|169\\.254\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.|0\\.|\\[?::1)", host)) api_error(400, "url_local", "Addresses on this computer or a private network are not fetched.")
  resp <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_user_agent(httr2::req_timeout(httr2::request(url), 30), "PathLab SEM text import"), is_error = function(r) FALSE)),
                   error = function(e) api_error(502, "url_unreachable", paste("The page could not be fetched:", sanitize_message(conditionMessage(e)))))
  if (httr2::resp_status(resp) >= 400) api_error(502, "url_status", sprintf("The page returned HTTP %d.", httr2::resp_status(resp)))
  type <- httr2::resp_content_type(resp) %||% ""
  if (!grepl("html|xml", type)) api_error(422, "url_type", sprintf("The address is not a web page (%s).", type), "Download the file and import it instead.")
  raw <- httr2::resp_body_raw(resp); if (length(raw) > 10 * 1024^2) api_error(413, "url_too_large", "The page is larger than 10 MB.")
  page <- xml2::read_html(raw)
  for (x in xml2::xml_find_all(page, "//script|//style|//noscript|//nav|//header|//footer|//aside|//form|//svg|//iframe")) xml2::xml_remove(x)
  title <- trimws(xml2::xml_text(xml2::xml_find_first(page, "//title")))
  root <- xml2::xml_find_first(page, "//article"); if (inherits(root, "xml_missing")) root <- xml2::xml_find_first(page, "//main")
  if (inherits(root, "xml_missing")) root <- xml2::xml_find_first(page, "//body")
  blocks <- xml2::xml_find_all(root, ".//h1|.//h2|.//h3|.//h4|.//p|.//li|.//blockquote|.//pre|.//td")
  txt <- trimws(gsub("[ \t\r\n]+", " ", xml2::xml_text(blocks)))
  txt <- txt[nzchar(txt) & !duplicated(txt)]
  if (!length(txt)) txt <- trimws(gsub("\\s+", " ", xml2::xml_text(root)))
  text <- paste(txt, collapse = "\n")
  if (!nzchar(text)) api_error(422, "url_empty", "No text was found on the page.")
  d <- text_new_doc(proj, if (nzchar(title)) substr(title, 1, 200) else host, url, "web", text_utf8(text), list(url = url, site = host, retrieved = format(Sys.Date())))
  text_meta(d)
}
