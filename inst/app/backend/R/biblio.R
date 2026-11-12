# Bibliometric analysis: Biblioshiny-style performance and science-mapping analyses (bibliometrix) and
# VOSviewer-style network maps (co-authorship, co-occurrence, citation, bibliographic coupling, co-citation) with
# association-strength normalisation, force-directed layout and Leiden clustering (igraph).
# The bibliographic collection (bibliometrix data frame M) lives in the project session on the server; every view is
# computed from it after the user's filters and thesaurus. Uploaded files are never changed.

BIBLIO_DBS <- c(wos = "Web of Science", scopus = "Scopus", dimensions = "Dimensions", pubmed = "PubMed", lens = "Lens.org",
                openalex = "OpenAlex", cochrane = "Cochrane Library")

# Analysis fields: label and the bibliometrix tag.
BIBLIO_FIELDS <- c(DE = "Author keywords", ID = "Keywords Plus", KW = "All keywords", TI = "Title terms", AB = "Abstract terms",
                   AU = "Authors", SO = "Sources", AU_UN = "Affiliations", AU_CO = "Countries", CR = "Cited references")

bq <- function(e) suppressWarnings(suppressMessages(e))

#' bibliometrix looks up its own data sets (country list, logo) with data(), which needs the package attached.
biblio_attach <- function() {
  biblio_need()
  if (!"package:bibliometrix" %in% search()) suppressPackageStartupMessages(attachNamespace("bibliometrix"))
}

#' A clear message when the bibliometric packages are not installed (an optional feature in smaller builds).
biblio_need <- function(extra = character(0)) need_packages(c("bibliometrix", extra), "Bibliometric analysis", "biblio_unavailable")

# ---- Import ---------------------------------------------------------------------------------------

#' Database and format of an export file from its name and first bytes.
biblio_detect <- function(raw, filename) {
  ext <- tolower(tools::file_ext(filename))
  head <- tryCatch(rawToChar(raw[seq_len(min(length(raw), 6000))]), error = function(e) "")
  head <- iconv(head, "", "UTF-8", sub = "")
  has <- function(p) grepl(p, head, ignore.case = TRUE)
  if (ext %in% c("xlsx", "xls")) return(list(db = "dimensions", format = "excel"))
  if (ext == "bib") return(list(db = if (has("scopus")) "scopus" else "wos", format = "bibtex"))
  if (ext == "ris" || ext == "ciw") return(list(db = "wos", format = if (ext == "ciw") "endnote" else "plaintext"))
  if (ext %in% c("txt", "nbib", "")) {
    if (grepl("^\\s*(FN |PT |VR )", head) || has("\nPT [JBSCP]")) return(list(db = "wos", format = "plaintext"))
    if (has("PMID- ")) return(list(db = "pubmed", format = "pubmed"))
    if (has("Record #") || has("\nAN ")) return(list(db = "cochrane", format = "plaintext"))
    return(list(db = "wos", format = "plaintext"))
  }
  if (ext == "csv") {
    if (has("Lens ID")) return(list(db = "lens", format = "csv"))
    if (has("Dimensions URL") || has("Publication ID")) return(list(db = "dimensions", format = "csv"))
    if (has("openalex") || grepl("^\"?id\"?,", head)) return(list(db = "openalex", format = "csv"))
    if (has("Source title") || has("EID") || has("Link") && has("Authors")) return(list(db = "scopus", format = "csv"))
    return(list(db = "scopus", format = "csv"))
  }
  api_error(415, "biblio_format", sprintf("%s is not a supported bibliographic export.", filename),
            "Export from Scopus (CSV or BibTeX), Web of Science (plain text or BibTeX), Dimensions (CSV or Excel), PubMed (text), Lens.org or OpenAlex (CSV).")
}

#' Read one export file into a bibliometrix data frame.
biblio_read <- function(raw, filename, db = NULL) {
  biblio_need()
  det <- biblio_detect(raw, filename)
  if (!is.null(db) && nzchar(db) && db %in% names(BIBLIO_DBS)) det$db <- db
  ext <- tools::file_ext(filename)
  tmp <- tempfile(fileext = paste0(".", if (nzchar(ext)) ext else "txt"))
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw, tmp)
  M <- tryCatch(bq(bibliometrix::convert2df(tmp, dbsource = det$db, format = det$format, remove.duplicates = TRUE)),
                error = function(e) api_error(422, "biblio_read", sprintf("%s could not be read as a %s export: %s", filename, BIBLIO_DBS[[det$db]], sanitize_message(conditionMessage(e), 200)),
                                              "Check the database and the export format (full record with cited references)."))
  if (!is.data.frame(M) || !nrow(M)) api_error(422, "biblio_empty", sprintf("No records were found in %s.", filename))
  list(M = M, db = det$db, format = det$format)
}

#' Merge collections (several files or databases) and remove duplicates (DOI, then title + year).
biblio_merge <- function(parts) {
  if (length(parts) == 1) return(parts[[1]])
  M <- tryCatch(bq(do.call(bibliometrix::mergeDbSources, c(parts, list(remove.duplicated = TRUE, verbose = FALSE)))),
                error = function(e) NULL)
  if (is.null(M)) {
    cols <- Reduce(union, lapply(parts, names))
    M <- do.call(rbind, lapply(parts, function(m) { for (k in setdiff(cols, names(m))) m[[k]] <- NA; m[, cols] }))
    key <- ifelse(!is.na(M$DI) & nzchar(M$DI), toupper(M$DI), paste(toupper(M$TI), M$PY))
    M <- M[!duplicated(key), , drop = FALSE]
    class(M) <- c("bibliometrixDB", "data.frame")
  }
  M
}

#' The collection with the fields every view needs (countries, affiliations, combined keywords, numeric year/citations).
biblio_prepare <- function(M) {
  biblio_attach()
  M$PY <- suppressWarnings(as.numeric(M$PY))
  M$TC <- suppressWarnings(as.numeric(M$TC)); M$TC[is.na(M$TC)] <- 0
  for (f in c("AU", "DE", "ID", "SO", "DT", "LA", "CR", "TI", "AB", "C1")) if (is.null(M[[f]])) M[[f]] <- NA_character_
  empty <- function(f) is.null(M[[f]]) || all(is.na(M[[f]]) | !nzchar(as.character(M[[f]])))
  if (empty("AU_CO")) M <- tryCatch(bq(bibliometrix::metaTagExtraction(M, Field = "AU_CO", sep = ";")), error = function(e) { M$AU_CO <- NA; M })
  if (empty("AU1_CO")) M <- tryCatch(bq(bibliometrix::metaTagExtraction(M, Field = "AU1_CO", sep = ";")), error = function(e) { M$AU1_CO <- NA; M })
  if (empty("AU_UN")) M <- tryCatch(bq(bibliometrix::metaTagExtraction(M, Field = "AU_UN", sep = ";")), error = function(e) { M$AU_UN <- NA; M })
  # All keywords: author keywords and Keywords Plus together (VOSviewer's "all keywords"), without duplicates per document.
  M$KW <- vapply(seq_len(nrow(M)), function(i) {
    k <- unique(trimws(toupper(unlist(strsplit(paste(M$DE[i] %||% "", M$ID[i] %||% "", sep = ";"), ";")))))
    k <- k[nzchar(k) & k != "NA"]
    if (length(k)) paste(k, collapse = ";") else NA_character_
  }, "")
  if (!"SR" %in% names(M)) M <- tryCatch(bq(bibliometrix::metaTagExtraction(M, Field = "SR")), error = function(e) { M$SR <- paste(M$AU, M$PY); M })
  class(M) <- c("bibliometrixDB", "data.frame")
  M
}

#' Store the collection in the project session.
biblio_set <- function(proj, M, files) {
  M <- biblio_prepare(M)
  proj$biblio <- list(M = M, files = files, thesaurus = proj$biblio$thesaurus %||% list(), loadedAt = now_iso())
  biblio_info(proj)
}

biblio_get <- function(proj) {
  biblio_need()
  b <- proj$biblio
  if (is.null(b) || is.null(b$M)) api_error(404, "biblio_none", "No bibliographic data are loaded.", "Import an export file (Scopus, Web of Science, Dimensions, PubMed, Lens.org or OpenAlex) or load the sample collection.")
  b
}

#' Summary of the stored collection (for the Data view and the filters).
biblio_info <- function(proj) {
  b <- biblio_get(proj); M <- b$M
  cnt <- function(x, k = 50) { t <- sort(table(trimws(unlist(strsplit(as.character(x[!is.na(x)]), ";")))), decreasing = TRUE); t <- utils::head(t[nzchar(names(t))], k); lapply(names(t), function(n) list(value = n, n = as.integer(t[[n]]))) }
  has <- function(f) { x <- M[[f]]; !is.null(x) && mean(!is.na(x) & nzchar(as.character(x))) > 0.05 }
  list(
    nDocuments = nrow(M),
    databases = as.list(unique(vapply(unique(toupper(M$DB)), function(d) switch(d, ISI = , WOS = "Web of Science", SCOPUS = "Scopus", DIMENSIONS = "Dimensions",
                                                                                 PUBMED = "PubMed", LENS = "Lens.org", OPENALEX = "OpenAlex", COCHRANE = "Cochrane Library", d), ""))),
    years = if (any(!is.na(M$PY))) list(min = min(M$PY, na.rm = TRUE), max = max(M$PY, na.rm = TRUE)) else NULL,
    documentTypes = cnt(M$DT, 30), languages = cnt(M$LA, 30), sources = cnt(M$SO, 60), countries = cnt(M$AU_CO, 60),
    fields = list(DE = has("DE"), ID = has("ID"), AB = has("AB"), CR = has("CR"), C1 = has("C1"), AU_CO = has("AU_CO"), AU_UN = has("AU_UN"), TC = any(M$TC > 0)),
    files = b$files, thesaurus = b$thesaurus, loadedAt = b$loadedAt
  )
}

# ---- Filters and thesaurus ------------------------------------------------------------------------

normalize_biblio_filters <- function(raw) {
  raw <- raw %||% list()
  num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) && is.finite(v[1])) v[1] else NULL }
  list(yearFrom = num(raw$yearFrom), yearTo = num(raw$yearTo), documentTypes = as_chr_vec(raw$documentTypes),
       languages = as_chr_vec(raw$languages), minCitations = num(raw$minCitations) %||% 0,
       sources = as_chr_vec(raw$sources), countries = as_chr_vec(raw$countries), bradfordZones = intersect(as_chr_vec(raw$bradfordZones), c("Zone 1", "Zone 2", "Zone 3")))
}

biblio_filter <- function(M, f) {
  keep <- rep(TRUE, nrow(M))
  if (!is.null(f$yearFrom)) keep <- keep & !is.na(M$PY) & M$PY >= f$yearFrom
  if (!is.null(f$yearTo)) keep <- keep & !is.na(M$PY) & M$PY <= f$yearTo
  if (length(f$documentTypes)) keep <- keep & toupper(trimws(M$DT)) %in% toupper(f$documentTypes)
  if (length(f$languages)) keep <- keep & toupper(trimws(M$LA)) %in% toupper(f$languages)
  if ((f$minCitations %||% 0) > 0) keep <- keep & M$TC >= f$minCitations
  if (length(f$sources)) keep <- keep & toupper(trimws(M$SO)) %in% toupper(f$sources)
  if (length(f$countries)) keep <- keep & vapply(M$AU_CO, function(x) any(toupper(split_items(x)) %in% toupper(f$countries)), TRUE)
  if (length(f$bradfordZones)) keep <- keep & biblio_bradford_keep(M, f$bradfordZones)
  out <- M[keep, , drop = FALSE]
  class(out) <- class(M)
  if (nrow(out) < 3) api_error(422, "biblio_filtered", sprintf("Only %d document(s) match the filters.", nrow(out)), "Widen the filters.")
  out
}

#' Thesaurus (VOSviewer format: "label" -> "replace by"; an empty replacement removes the term) applied to the
#' keyword and term fields, author names and sources. Matching ignores case.
normalize_thesaurus <- function(raw) {
  rows <- Filter(function(r) nzchar(trimws(as.character(r$label %||% ""))), raw %||% list())
  lapply(rows, function(r) list(label = trimws(as.character(r$label)), replaceBy = trimws(as.character(r$replaceBy %||% ""))))
}

biblio_apply_thesaurus <- function(M, th, fields = c("DE", "ID", "KW", "AU", "SO", "AU_UN", "AU_CO")) {
  if (!length(th)) return(M)
  map <- setNames(toupper(vapply(th, `[[`, "", "replaceBy")), toupper(vapply(th, `[[`, "", "label")))
  for (f in intersect(fields, names(M))) {
    M[[f]] <- vapply(M[[f]], function(x) {
      if (is.na(x)) return(NA_character_)
      it <- trimws(unlist(strsplit(x, ";"))); up <- toupper(it)
      hit <- up %in% names(map)
      it[hit] <- map[up[hit]]
      it <- unique(it[nzchar(it)])
      if (length(it)) paste(it, collapse = ";") else NA_character_
    }, "", USE.NAMES = FALSE)
  }
  M
}

#' Collection for an analysis: filters, then thesaurus. Title/abstract terms are extracted on demand.
biblio_working <- function(proj, filters) {
  biblio_attach()
  b <- biblio_get(proj)
  M <- biblio_filter(b$M, normalize_biblio_filters(filters))
  biblio_apply_thesaurus(M, b$thesaurus)
}

biblio_terms <- function(M, field, ngrams = 1) {
  tag <- paste0(field, "_TM")
  M <- bq(bibliometrix::termExtraction(M, Field = field, ngrams = ngrams, remove.numbers = TRUE, remove.terms = NULL, keep.terms = NULL, verbose = FALSE))
  M[[field]] <- NULL
  names(M)[names(M) == tag] <- field
  M
}

# ---- Helpers --------------------------------------------------------------------------------------

split_items <- function(x) trimws(unlist(strsplit(as.character(x[!is.na(x)]), ";")))
top_items <- function(x, n = 20) {
  t <- sort(table(unlist(lapply(as.character(x[!is.na(x)]), function(d) unique(trimws(unlist(strsplit(d, ";"))))))), decreasing = TRUE)
  t <- t[nzchar(names(t)) & names(t) != "NA"]
  utils::head(t, n)
}
title_case <- function(x) { x <- tolower(x); gsub("(^|[[:space:]-])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE) }
df_rows <- function(df) if (is.null(df) || !nrow(df)) list() else lapply(seq_len(nrow(df)), function(i) lapply(as.list(df[i, , drop = FALSE]), function(v) if (is.factor(v)) as.character(v) else if (length(v) && is.numeric(v) && !is.finite(v)) NULL else v))
current_year <- function() as.numeric(format(Sys.Date(), "%Y"))

# ---- Biblioshiny-style views ---------------------------------------------------------------------

biblio_overview <- function(M, o) {
  R <- bq(bibliometrix::biblioAnalysis(M, sep = ";"))
  S <- bq(summary(R, k = 10, verbose = FALSE, pause = FALSE))
  mi <- S$MainInformationDF
  main <- lapply(seq_len(nrow(mi)), function(i) {
    d <- trimws(as.character(mi$Description[i])); v <- trimws(as.character(mi$Results[i]))
    list(label = d, value = v, heading = !nzchar(v))
  })
  yrs <- M$PY[!is.na(M$PY)]
  years <- if (length(yrs)) seq(min(yrs), max(yrs)) else integer(0)
  cy <- current_year()
  annual <- lapply(years, function(y) {
    sel <- !is.na(M$PY) & M$PY == y
    list(year = y, documents = sum(sel), meanTC = if (any(sel)) mean(M$TC[sel]) else 0,
         meanTCperYear = if (any(sel)) mean(M$TC[sel]) / max(1, cy - y + 1) else 0, citableYears = cy - y + 1)
  })
  list(main = main, growthRate = num_or_null(S$AnnualGrowthRate), annual = annual, lifeCycle = biblio_life_cycle(M),
       threeField = biblio_three_field(M, o$left %||% "AU", o$middle %||% "DE", o$right %||% "SO", o$nItems %||% 15))
}

#' Three-field plot (Sankey): the most frequent items of three fields and how many documents connect them.
biblio_three_field <- function(M, left, middle, right, n = 15) {
  fields <- c(left, middle, right)
  for (f in fields) if (f %in% c("TI", "AB")) M <- biblio_terms(M, f)
  tops <- lapply(fields, function(f) names(top_items(M[[f]], n)))
  # Document-by-item incidence of the top items of each field.
  inc <- lapply(seq_along(fields), function(k) {
    items <- lapply(M[[fields[k]]], function(x) if (is.na(x)) character(0) else unique(trimws(unlist(strsplit(x, ";")))))
    ii <- unlist(lapply(seq_along(items), function(i) rep(i, sum(items[[i]] %in% tops[[k]]))))
    jj <- unlist(lapply(items, function(x) match(x[x %in% tops[[k]]], tops[[k]])))
    Matrix::sparseMatrix(i = ii, j = jj, x = 1, dims = c(nrow(M), length(tops[[k]])))
  })
  nodes <- unlist(lapply(seq_along(fields), function(k) lapply(tops[[k]], function(it) list(id = paste0(k, ":", it), label = it, column = k - 1, field = fields[k]))), recursive = FALSE)
  links <- list()
  for (k in 1:2) {
    W <- as.matrix(Matrix::crossprod(inc[[k]], inc[[k + 1]]))
    for (a in seq_along(tops[[k]])) for (b in seq_along(tops[[k + 1]])) if (W[a, b] > 0)
      links[[length(links) + 1]] <- list(source = paste0(k, ":", tops[[k]][a]), target = paste0(k + 1, ":", tops[[k + 1]][b]), value = W[a, b])
  }
  list(fields = as.list(unname(BIBLIO_FIELDS[fields])), nodes = nodes, links = links)
}

biblio_sources <- function(M, o) {
  n <- o$nItems %||% 15
  rel <- top_items(M$SO, n)
  br <- bq(bibliometrix::bradford(M))
  bt <- br$table
  zones <- lapply(split(bt, bt$Zone), function(z) list(zone = as.character(z$Zone[1]), sources = nrow(z), documents = sum(z$Freq)))
  H <- bq(bibliometrix::Hindex(M, field = "source", sep = ";", years = Inf))$H
  H <- utils::head(H[order(-H$h_index, -H$TC), , drop = FALSE], n)
  topS <- names(top_items(M$SO, 5))
  yrs <- sort(unique(M$PY[!is.na(M$PY)]))
  dyn <- lapply(topS, function(s) list(source = s, cumulative = as.list(cumsum(vapply(yrs, function(y) sum(M$SO == s & M$PY == y, na.rm = TRUE), 0)))))
  lcs <- tryCatch({ Ms <- bq(bibliometrix::metaTagExtraction(M, Field = "CR_SO", sep = ";")); t <- top_items(Ms$CR_SO, n); lapply(names(t), function(s) list(source = s, citations = as.integer(t[[s]]))) }, error = function(e) list())
  list(relevant = lapply(names(rel), function(s) list(source = s, documents = as.integer(rel[[s]]))), localCited = lcs,
       bradford = list(table = df_rows(utils::head(bt, 300)), zones = unname(zones)),
       impact = df_rows(H), dynamics = list(years = as.list(yrs), series = dyn))
}

biblio_authors <- function(M, o) {
  n <- o$nItems %||% 15
  R <- bq(bibliometrix::biblioAnalysis(M, sep = ";"))
  frac <- R$AuthorsFrac
  rel <- utils::head(R$Authors, n)
  lo <- bq(bibliometrix::lotka(M))
  ap <- bq(bibliometrix::authorProdOverTime(M, k = min(n, 10), graph = FALSE))$dfAU
  H <- bq(bibliometrix::Hindex(M, field = "author", sep = ";", years = Inf))$H
  H <- utils::head(H[order(-H$h_index, -H$TC), , drop = FALSE], n)
  aff <- top_items(M$AU_UN, n)
  # Corresponding author's country: single-country (SCP) and multiple-country (MCP) publications.
  co1 <- trimws(M$AU1_CO)
  multi <- vapply(M$AU_CO, function(x) length(unique(split_items(x))) > 1, TRUE)
  cc <- sort(table(co1[!is.na(co1) & nzchar(co1)]), decreasing = TRUE)
  countries <- lapply(utils::head(names(cc), n), function(k) { s <- !is.na(co1) & co1 == k; list(country = k, documents = sum(s), scp = sum(s & !multi), mcp = sum(s & multi)) })
  prod <- table(split_items(M$AU_CO))
  cited <- tapply(M$TC, co1, sum)
  cited <- utils::head(sort(cited[!is.na(names(cited)) & nzchar(names(cited))], decreasing = TRUE), n)
  lca <- tryCatch({ Ma <- bq(bibliometrix::metaTagExtraction(M, Field = "CR_AU", sep = ";")); t <- top_items(Ma$CR_AU, n); lapply(names(t), function(a) list(author = a, citations = as.integer(t[[a]]))) }, error = function(e) list())
  list(relevant = lapply(names(rel), function(a) list(author = a, documents = as.integer(rel[[a]]), fractionalized = num_or_null(frac$Frequency[match(a, frac$Author)]))),
       localCited = lca, collaboration = biblio_collab_indicators(M),
       countryTime = biblio_field_over_time(M, "AU_CO"), affiliationTime = biblio_field_over_time(M, "AU_UN"),
       lotka = list(table = df_rows(setNames(lo$AuthorProd, c("documents", "authors", "proportion"))), beta = num_or_null(lo$Beta), c = num_or_null(lo$C), r2 = num_or_null(lo$R2), p = num_or_null(lo$p.value)),
       production = df_rows(ap), hindex = df_rows(H),
       affiliations = lapply(names(aff), function(a) list(affiliation = a, documents = as.integer(aff[[a]]))),
       countries = countries,
       countryProduction = lapply(names(prod), function(k) list(country = k, frequency = as.integer(prod[[k]]))),
       citedCountries = lapply(names(cited), function(k) list(country = k, citations = num_or_null(cited[[k]]), average = num_or_null(cited[[k]] / sum(co1 == k, na.rm = TRUE)))))
}

biblio_documents <- function(M, o) {
  n <- o$nItems %||% 15
  cy <- current_year()
  ord <- order(-M$TC)[seq_len(min(n, nrow(M)))]
  ntc <- vapply(seq_len(nrow(M)), function(i) { s <- !is.na(M$PY) & M$PY == M$PY[i]; m <- mean(M$TC[s]); if (m > 0) M$TC[i] / m else NA_real_ }, 0)
  global <- lapply(ord, function(i) list(document = M$SR[i] %||% M$TI[i], title = title_case(M$TI[i] %||% ""), doi = M$DI[i] %||% NULL, year = M$PY[i],
                                         citations = M$TC[i], perYear = M$TC[i] / max(1, cy - M$PY[i] + 1), normalized = num_or_null(ntc[i])))
  lc <- tryCatch(bq(bibliometrix::localCitations(M, sep = ";")), error = function(e) NULL)
  local <- if (is.null(lc)) list() else df_rows(utils::head(lc$Papers, n))
  ci <- tryCatch(bq(bibliometrix::citations(M, field = "article", sep = ";")), error = function(e) NULL)
  refs <- if (is.null(ci)) list() else { cc <- utils::head(ci$Cited, n); lapply(names(cc), function(r) list(reference = r, citations = as.integer(cc[[r]]))) }
  rp <- tryCatch(bq(bibliometrix::rpys(M, sep = ";", graph = FALSE))$rpysTable, error = function(e) NULL)
  list(global = global, local = local, references = refs, rpys = df_rows(rp))
}

biblio_words <- function(M, o) {
  field <- o$field %||% "DE"
  n <- o$nItems %||% 50
  if (field %in% c("TI", "AB")) M <- biblio_terms(M, field, o$ngrams %||% 1)
  tw <- top_items(M[[field]], n)
  words <- lapply(names(tw), function(w) list(word = w, frequency = as.integer(tw[[w]])))
  yrs <- sort(unique(M$PY[!is.na(M$PY)]))
  top10 <- utils::head(names(tw), 8)
  growth <- lapply(top10, function(w) list(word = w, cumulative = as.list(cumsum(vapply(yrs, function(y) sum(vapply(M[[field]][!is.na(M$PY) & M$PY == y], function(x) w %in% split_items(x), TRUE)), 0)))))
  trend <- tryCatch({
    fb <- bq(bibliometrix::fieldByYear(M, field = field, timespan = NULL, min.freq = o$minFreq %||% 5, n.items = o$itemsPerYear %||% 3, graph = FALSE))$df_graph
    df_rows(as.data.frame(fb))
  }, error = function(e) list())
  list(field = field, fieldLabel = unname(BIBLIO_FIELDS[field]), words = words, growth = list(years = as.list(yrs), series = growth), trend = trend)
}

biblio_thematic_map <- function(M, o) {
  field <- o$field %||% "DE"
  if (!field %in% c("DE", "ID", "TI", "AB")) field <- "DE"
  tm <- bq(bibliometrix::thematicMap(M, field = field, n = o$n %||% 250, minfreq = o$minFreq %||% 5, ngrams = o$ngrams %||% 1, stemming = FALSE, size = 0.5,
                                      n.labels = 3, repel = TRUE, cluster = o$clustering %||% "louvain"))
  cl <- tm$clusters
  words <- tm$words
  clusters <- lapply(seq_len(nrow(cl)), function(i) {
    w <- words[words$Cluster == cl$groups[i] | words$Cluster_Label == cl$name[i], , drop = FALSE]
    w <- utils::head(w[order(-w$Occurrences), , drop = FALSE], 12)
    list(id = as.integer(cl$groups[i]), label = as.character(cl$name[i]), size = as.integer(cl$n[i]), frequency = num_or_null(cl$freq[i]),
         centrality = cl$centrality[i], density = cl$density[i], rcentrality = cl$rcentrality[i], rdensity = cl$rdensity[i],
         words = lapply(seq_len(nrow(w)), function(j) list(word = w$Words[j], occurrences = w$Occurrences[j])))
  })
  perf <- theme_performance(tm$documentToClusters)
  clusters <- lapply(clusters, function(c) { p <- perf[[toupper(c$label)]] %||% perf[[c$label]] %||% perf[[tolower(c$label)]]; if (is.null(p)) c else c(c, p) })
  list(field = field, fieldLabel = unname(BIBLIO_FIELDS[field]), clusters = clusters, origin = list(x = tm$axisOrigin[1], y = tm$axisOrigin[2]))
}

biblio_thematic_evolution <- function(M, o) {
  field <- o$field %||% "DE"
  if (!field %in% c("DE", "ID", "TI", "AB")) field <- "DE"
  yrs <- sort(unique(M$PY[!is.na(M$PY)]))
  cuts <- as.numeric(unlist(o$cuts %||% list()))
  if (!length(cuts)) cuts <- unname(round(stats::quantile(yrs, c(1/3, 2/3), type = 1)))
  cuts <- sort(unique(cuts[cuts > min(yrs) & cuts < max(yrs)]))
  if (!length(cuts)) api_error(422, "biblio_cuts", "Choose cutting years inside the timespan of the collection.")
  te <- bq(bibliometrix::thematicEvolution(M, field = field, years = cuts, n = o$n %||% 250, minFreq = o$minFreq %||% 2, ngrams = o$ngrams %||% 1))
  perf <- lapply(te$TM, function(m) theme_performance(m$documentToClusters))
  nodes <- lapply(seq_len(nrow(te$Nodes)), function(i) {
    p <- perf[[as.integer(te$Nodes$slice[i])]]; nm <- te$Nodes$name[i]
    p <- if (is.null(p)) NULL else p[[nm]] %||% p[[toupper(nm)]] %||% p[[tolower(nm)]]
    c(list(id = as.integer(te$Nodes$id[i]), label = nm, period = as.character(te$Nodes$group[i]),
           slice = as.integer(te$Nodes$slice[i]), frequency = num_or_null(te$Nodes$freq[i])), p)
  })
  edges <- lapply(seq_len(nrow(te$Edges)), function(i) list(source = as.integer(te$Edges$from[i]), target = as.integer(te$Edges$to[i]),
                                                              inclusion = te$Edges$Inclusion[i], weighted = te$Edges$Inc_Weighted[i], stability = te$Edges$Stability[i]))
  periods <- unique(as.character(te$Nodes$group[order(te$Nodes$slice)]))
  list(field = field, fieldLabel = unname(BIBLIO_FIELDS[field]), cuts = as.list(cuts), periods = as.list(periods), nodes = nodes, edges = edges)
}

biblio_factorial <- function(M, o) {
  field <- o$field %||% "ID"
  method <- if (identical(o$method, "MCA")) "MCA" else if (identical(o$method, "MDS")) "MDS" else "CA"
  if (!field %in% c("DE", "ID", "TI", "AB")) field <- "ID"
  cs <- bq(bibliometrix::conceptualStructure(M, field = field, ngrams = o$ngrams %||% 1, method = method, minDegree = o$minDegree %||% 10, clust = o$clusters %||% "auto", k.max = 8,
                                             stemming = FALSE, labelsize = 10, documents = o$documents %||% 10, graph = FALSE))
  dc <- cs$km.res$data.clust
  eig <- tryCatch(if (method == "CA") cs$res$sv^2 / sum(cs$res$sv^2) else NULL, error = function(e) NULL)
  docs <- cs$docCoord
  list(field = field, method = method, inertia = if (is.null(eig)) list() else as.list(utils::head(eig, 2)),
       words = lapply(seq_len(nrow(dc)), function(i) list(word = rownames(dc)[i], x = dc[i, 1], y = dc[i, 2], cluster = as.integer(dc$clust[i]))),
       documents = lapply(seq_len(min(nrow(docs), o$documents %||% 10)), function(i) list(document = rownames(docs)[i], x = docs$dim1[i], y = docs$dim2[i], citations = docs$TC[i], cluster = as.integer(docs$Cluster[i]))))
}

biblio_historiograph <- function(M, o) {
  n <- o$nItems %||% 20
  hn <- bq(bibliometrix::histNetwork(M, min.citations = 1, sep = ";", network = TRUE, verbose = FALSE))
  hd <- hn$histData
  keep <- utils::head(order(-hd$LCS), n)
  keep <- keep[hd$LCS[keep] > 0]
  A <- hn$NetMatrix[keep, keep, drop = FALSE]
  nodes <- lapply(seq_along(keep), function(k) { i <- keep[k]; list(id = k - 1L, label = hd$Paper[i], title = title_case(hd$Title[i] %||% ""), doi = hd$DOI[i] %||% NULL, year = hd$Year[i], lcs = hd$LCS[i], gcs = hd$GCS[i]) })
  ed <- which(A > 0, arr.ind = TRUE)
  # NetMatrix[i, j] > 0: document i cites document j.
  edges <- lapply(seq_len(nrow(ed)), function(r) list(source = ed[r, 1] - 1L, target = ed[r, 2] - 1L))
  list(nodes = nodes, edges = edges)
}

biblio_collaboration <- function(M, o) {
  co <- lapply(M$AU_CO, function(x) sort(unique(split_items(x))))
  prod <- table(unlist(co))
  pairs <- list()
  for (cs in co) if (length(cs) > 1) { cmb <- utils::combn(cs, 2); for (j in seq_len(ncol(cmb))) { k <- paste(cmb[1, j], cmb[2, j], sep = "\r"); pairs[[k]] <- (pairs[[k]] %||% 0) + 1 } }
  pv <- unlist(pairs)
  pv <- utils::head(sort(pv, decreasing = TRUE), o$nLinks %||% 60)
  list(production = lapply(names(prod), function(k) list(country = k, documents = as.integer(prod[[k]]))),
       links = lapply(names(pv), function(k) { s <- strsplit(k, "\r")[[1]]; list(from = s[1], to = s[2], documents = as.integer(pv[[k]])) }))
}

BIBLIO_VIEWS <- list(overview = biblio_overview, sources = biblio_sources, authors = biblio_authors, documents = biblio_documents,
                     words = biblio_words, thematicMap = biblio_thematic_map, thematicEvolution = biblio_thematic_evolution,
                     factorial = biblio_factorial, historiograph = biblio_historiograph, collaboration = biblio_collaboration)

biblio_view <- function(proj, view, options, filters) {
  f <- BIBLIO_VIEWS[[view %||% ""]]
  if (is.null(f)) api_error(400, "biblio_view", "Unknown bibliometric view.")
  M <- biblio_working(proj, filters)
  t0 <- Sys.time()
  out <- tryCatch(if ("proj" %in% names(formals(f))) f(M, options %||% list(), proj = proj, filters = filters) else f(M, options %||% list()), pathlab_error = function(e) stop(e), error = function(e)
    api_error(422, "biblio_failed", paste("The analysis could not be computed:", sanitize_message(conditionMessage(e), 200)),
              "The collection may lack the fields this analysis needs (for example cited references, keywords or affiliations)."))
  c(list(view = view, nDocuments = nrow(M), durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))), out)
}

# ---- VOSviewer-style networks ---------------------------------------------------------------------

#' Network types and units, as in VOSviewer's "Create map" wizard.
BIBLIO_NETWORKS <- list(
  coauthorship = c(AU = "Authors", AU_UN = "Organizations", AU_CO = "Countries"),
  cooccurrence = c(KW = "All keywords", DE = "Author keywords", ID = "Keywords Plus", TI = "Title terms", AB = "Abstract terms"),
  citation = c(documents = "Documents"),
  coupling = c(documents = "Documents", AU = "Authors", SO = "Sources", AU_UN = "Organizations", AU_CO = "Countries"),
  cocitation = c(CR = "Cited references", CR_AU = "Cited authors", CR_SO = "Cited sources")
)

normalize_network_options <- function(raw) {
  raw <- raw %||% list()
  type <- as.character(raw$type %||% "cooccurrence")[1]
  if (!type %in% names(BIBLIO_NETWORKS)) type <- "cooccurrence"
  unit <- as.character(raw$unit %||% names(BIBLIO_NETWORKS[[type]])[1])[1]
  if (!unit %in% names(BIBLIO_NETWORKS[[type]])) unit <- names(BIBLIO_NETWORKS[[type]])[1]
  num <- function(x, d, lo, hi) { v <- suppressWarnings(as.numeric(x)); if (length(v) && is.finite(v[1])) min(hi, max(lo, v[1])) else d }
  list(type = type, unit = unit, counting = if (identical(raw$counting, "fractional")) "fractional" else "full",
       normalization = if (identical(raw$normalization, "none")) "none" else "association",
       minOccurrences = round(num(raw$minOccurrences, 5, 1, 1000)), maxItems = round(num(raw$maxItems, 150, 5, 1000)),
       resolution = num(raw$resolution, 1, 0.05, 5), minClusterSize = round(num(raw$minClusterSize, 2, 1, 200)), seed = round(num(raw$seed, 2026, 1, 1e9)),
       layout = if (identical(raw$layout, "fr")) "fr" else "vos", relevance = round(num(raw$relevance, 60, 10, 100)),
       exclude = utils::head(unique(as_chr_vec(raw$exclude)), 2000))
}

#' Document-by-item incidence (sparse) for a unit; title/abstract terms are extracted on demand.
biblio_incidence <- function(M, unit) {
  if (unit %in% c("TI", "AB")) M <- biblio_terms(M, unit)
  fld <- switch(unit, CR_AU = "CR_AU", CR_SO = "CR_SO", unit)
  if (fld %in% c("CR_AU", "CR_SO")) M <- bq(bibliometrix::metaTagExtraction(M, Field = fld, sep = ";"))
  A <- bq(bibliometrix::cocMatrix(M, Field = fld, sep = ";", binary = TRUE))
  A <- methods::as(A, "CsparseMatrix")
  bad <- colnames(A) %in% c("", "NA", "NO TITLE CAPTURED", "ANONYMOUS")
  if (any(bad)) A <- A[, !bad, drop = FALSE]
  list(A = A, M = M)
}

#' Co-occurrence of items in documents: full counting (A'A) or fractional counting (each document's links weighted
#' 1 / (n_d - 1), so a document contributes the same total link strength however many items it has).
co_matrix <- function(A, counting) {
  if (counting == "fractional") {
    k <- Matrix::rowSums(A)
    w <- ifelse(k > 1, 1 / (k - 1), 0)
    C <- Matrix::crossprod(A * w, A)
  } else C <- Matrix::crossprod(A)
  C
}

biblio_network <- function(proj, opts, filters) {
  o <- normalize_network_options(opts)
  M <- biblio_working(proj, filters)
  t0 <- Sys.time()
  occ_label <- "Occurrences"
  if (o$type %in% c("coauthorship", "cooccurrence", "cocitation")) {
    inc <- biblio_incidence(M, o$unit); A <- inc$A
    occ <- Matrix::colSums(A)
    occ_label <- switch(o$type, coauthorship = "Documents", cooccurrence = "Occurrences", cocitation = "Citations")
    keep <- which(occ >= o$minOccurrences)
    if (length(keep) < 2) api_error(422, "biblio_threshold", sprintf("Only %d item(s) reach the threshold of %d.", length(keep), o$minOccurrences), "Lower the minimum number of occurrences.")
    A <- A[, keep, drop = FALSE]; occ <- occ[keep]
    if (length(o$exclude)) { ex <- !toupper(colnames(A)) %in% toupper(o$exclude); A <- A[, ex, drop = FALSE]; occ <- occ[ex] }
    # Title and abstract terms: keep the most relevant share (VOSviewer's relevance score), dropping general terms.
    if (o$type == "cooccurrence" && o$unit %in% c("TI", "AB") && o$relevance < 100 && ncol(A) > 10) {
      rel <- term_relevance(Matrix::crossprod(A)); ok <- rel >= stats::quantile(rel, 1 - o$relevance / 100)
      A <- A[, ok, drop = FALSE]; occ <- occ[ok]
    }
    if (ncol(A) < 2) api_error(422, "biblio_threshold", "Fewer than two items remain.", "Lower the threshold or exclude fewer items.")
    C <- co_matrix(A, o$counting)
    docs_of <- A
  } else if (o$type == "coupling") {
    R <- biblio_incidence(M, "CR")$A                                   # documents x references
    if (o$unit == "documents") {
      U <- Matrix::sparseMatrix(i = seq_len(nrow(M)), j = seq_len(nrow(M)), x = 1, dims = c(nrow(M), nrow(M)), dimnames = list(NULL, M$SR))
      occ <- M$TC; occ_label <- "Citations"                             # documents: threshold on citations (as in VOSviewer)
    } else {
      U <- biblio_incidence(M, o$unit)$A
      occ <- Matrix::colSums(U); occ_label <- "Documents"
    }
    keep <- which(occ >= o$minOccurrences)
    if (length(keep) < 2) api_error(422, "biblio_threshold", sprintf("Only %d item(s) reach the threshold of %d.", length(keep), o$minOccurrences), "Lower the threshold.")
    U <- U[, keep, drop = FALSE]; occ <- occ[keep]
    if (length(o$exclude)) { ex <- !toupper(colnames(U) %||% M$SR[keep]) %in% toupper(o$exclude); U <- U[, ex, drop = FALSE]; occ <- occ[ex] }
    # Coupling strength: number of shared cited references (fractional: each citing document's references weighted 1 / n_refs).
    Rw <- if (o$counting == "fractional") { k <- Matrix::rowSums(R); R * ifelse(k > 0, 1 / k, 0) } else R
    UR <- Matrix::crossprod(U, Rw)                                      # units x references
    C <- Matrix::tcrossprod(UR)
    docs_of <- U
  } else {                                                              # direct citation among documents
    hn <- bq(bibliometrix::histNetwork(M, min.citations = 0, sep = ";", network = TRUE, verbose = FALSE))
    M <- hn$M                                                           # histNetwork orders the documents by year
    D <- methods::as(hn$NetMatrix, "CsparseMatrix")
    C <- D + Matrix::t(D)
    occ <- M$TC; occ_label <- "Citations"
    keep <- which(occ >= o$minOccurrences)
    if (length(keep) < 2) api_error(422, "biblio_threshold", sprintf("Only %d document(s) reach the threshold of %d citations.", length(keep), o$minOccurrences), "Lower the threshold.")
    C <- C[keep, keep, drop = FALSE]; occ <- occ[keep]
    docs_of <- Matrix::sparseMatrix(i = keep, j = seq_along(keep), x = 1, dims = c(nrow(M), length(keep)), dimnames = list(NULL, M$SR[keep]))
  }
  Matrix::diag(C) <- 0
  C <- Matrix::drop0(C)
  tls <- Matrix::rowSums(C)
  links_n <- Matrix::rowSums(C > 0)
  # VOSviewer keeps the items with the greatest total link strength (connected items only).
  ord <- order(-tls, -occ)
  sel <- utils::head(ord[tls[ord] > 0], o$maxItems)
  if (length(sel) < 2) api_error(422, "biblio_unconnected", "The selected items are not connected to each other.", "Lower the threshold or choose another unit.")
  C <- C[sel, sel, drop = FALSE]; occ <- occ[sel]; docs_of <- docs_of[, sel, drop = FALSE]
  tls <- Matrix::rowSums(C); links_n <- Matrix::rowSums(C > 0)
  labels <- colnames(docs_of) %||% rownames(C)
  if (is.null(labels)) labels <- paste0("item", seq_along(sel))
  # Association strength (van Eck & Waltman, 2009): s_ij = c_ij / (w_i w_j), w = total link strength.
  S <- if (o$normalization == "association") { w <- pmax(tls, 1e-12); Matrix::Diagonal(x = 1 / w) %*% C %*% Matrix::Diagonal(x = 1 / w) } else C
  S <- methods::as(S, "CsparseMatrix")
  ed <- Matrix::summary(Matrix::triu(S, 1))
  cnt <- Matrix::summary(Matrix::triu(C, 1))
  g <- igraph::graph_from_data_frame(data.frame(from = ed$i, to = ed$j, weight = ed$x / max(ed$x)), directed = FALSE, vertices = data.frame(name = seq_along(sel)))
  set.seed(o$seed)
  cl <- igraph::cluster_leiden(g, objective_function = "modularity", weights = igraph::E(g)$weight, resolution = o$resolution, n_iterations = -1)
  memb <- igraph::membership(cl)
  # Small clusters join the cluster they are most strongly linked to.
  sizes <- table(memb)
  small <- as.integer(names(sizes)[sizes < o$minClusterSize])
  if (length(small) && length(small) < length(sizes)) for (i in which(memb %in% small)) {
    nb <- which(C[i, ] > 0 & !(memb %in% small))
    if (length(nb)) memb[i] <- as.integer(names(which.max(tapply(as.numeric(C[i, nb]), memb[nb], sum))))
  }
  # Clusters numbered by size (1 = largest), as in VOSviewer.
  rk <- rank(-table(memb)[as.character(sort(unique(memb)))], ties.method = "first")
  memb <- as.integer(rk[as.character(memb)])
  lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$weight, niter = 1500)
  lay <- scale(lay, center = TRUE, scale = FALSE); lay <- lay / max(abs(lay))
  # VOS mapping technique (VOSviewer's layout), started from the force-directed layout.
  if (o$layout == "vos") lay <- tryCatch(vos_layout(S, lay), error = function(e) lay)
  # Overlay scores: average publication year, average citations and average normalized citations of the documents.
  ntc <- vapply(seq_len(nrow(M)), function(i) { s <- !is.na(M$PY) & M$PY == M$PY[i]; m <- mean(M$TC[s]); if (m > 0) M$TC[i] / m else NA_real_ }, 0)
  avg <- function(v) { D <- docs_of; vapply(seq_len(ncol(D)), function(k) { idx <- which(D[, k] > 0); x <- v[idx]; if (length(x) && any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_ }, 0) }
  ay <- avg(M$PY); ac <- avg(M$TC); an <- avg(ntc)
  # CiteSpace measures: betweenness centrality, burst strength, sigma; cluster silhouette and LLR labels from titles.
  q <- tryCatch(biblio_cluster_quality(S, memb, g, M, docs_of, tryCatch(biblio_year_counts(M, docs_of), error = function(e) NULL)), error = function(e) NULL)
  nodes <- lapply(seq_along(sel), function(k) list(id = k - 1L, label = labels[k], x = lay[k, 1], y = lay[k, 2], cluster = memb[k],
                                                   weight = num_or_null(occ[k]), links = as.integer(links_n[k]), strength = num_or_null(tls[k]),
                                                   avgYear = num_or_null(ay[k]), avgCitations = num_or_null(ac[k]), avgNormCitations = num_or_null(an[k]),
                                                   betweenness = if (is.null(q)) NULL else num_or_null(q$betweenness[k]), burst = if (is.null(q)) NULL else num_or_null(q$burst[k]),
                                                   sigma = if (is.null(q)) NULL else num_or_null(q$sigma[k])))
  edges <- lapply(seq_len(nrow(cnt)), function(r) list(source = cnt$i[r] - 1L, target = cnt$j[r] - 1L, strength = cnt$x[r]))
  clusters <- lapply(sort(unique(memb)), function(k) {
    idx <- which(memb == k)
    top <- idx[order(-occ[idx])][seq_len(min(5, length(idx)))]
    qc <- if (is.null(q)) NULL else Filter(function(x) x$id == k, q$clusters)
    qc <- if (length(qc)) qc[[1]] else NULL
    list(id = k, size = length(idx), top = as.list(labels[top]), silhouette = qc$silhouette, labels = qc$labels %||% list(), documents = qc$documents)
  })
  list(options = o, unitLabel = unname(BIBLIO_NETWORKS[[o$type]][o$unit]), weightLabel = occ_label, nDocuments = nrow(M),
       nodes = nodes, edges = edges, clusters = clusters, modularity = num_or_null(igraph::modularity(g, memb, weights = igraph::E(g)$weight)),
       meanSilhouette = if (is.null(q)) NULL else q$meanSilhouette,
       durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# ---- World map (for country production and collaboration maps) ------------------------------------

biblio_world <- function() {
  if (!is.null(.store$world)) return(.store$world)
  need_packages("maps", "The world map", "biblio_unavailable")
  w <- maps::map("world", plot = FALSE, fill = TRUE)
  breaks <- c(0, which(is.na(w$x)), length(w$x) + 1)
  polys <- list()
  for (k in seq_len(length(breaks) - 1)) {
    idx <- (breaks[k] + 1):(breaks[k + 1] - 1)
    if (length(idx) < 4) next
    step <- max(1, floor(length(idx) / 120))
    idx <- idx[seq(1, length(idx), by = step)]
    nm <- toupper(sub(":.*$", "", w$names[k]))
    polys[[length(polys) + 1]] <- list(country = nm, x = round(w$x[idx], 2), y = round(w$y[idx], 2))
  }
  # Name differences between maps and bibliometrix country names.
  alias <- c("UK" = "UNITED KINGDOM", "USA" = "USA", "SOUTH KOREA" = "KOREA", "CZECH REPUBLIC" = "CZECH REPUBLIC")
  for (i in seq_along(polys)) if (polys[[i]]$country %in% names(alias)) polys[[i]]$country <- unname(alias[polys[[i]]$country])
  .store$world <- list(polygons = polys)
  .store$world
}

# ---- Export ----------------------------------------------------------------------------------------

#' The (filtered) collection as an Excel file that Biblioshiny can load (bibliometrix field tags as columns).
biblio_export_xlsx <- function(proj, filters, path) {
  M <- biblio_working(proj, filters)
  keep <- names(M)[vapply(M, function(x) is.atomic(x), TRUE)]
  openxlsx::write.xlsx(as.data.frame(M[, keep, drop = FALSE]), path)
  path
}

#' Sample collection: bibliometric approaches in business and management (898 Web of Science records; bibliometrixData).
biblio_sample <- function() {
  biblio_need("bibliometrixData")
  e <- new.env()
  utils::data("management", package = "bibliometrixData", envir = e)
  e$management
}

# ---- Report --------------------------------------------------------------------------------------

BIBLIO_REPORT_SECTIONS <- c(collection = "Data and collection", production = "Annual scientific production", sources = "Most relevant sources",
                            authors = "Most relevant authors", countries = "Countries", documents = "Most cited documents",
                            keywords = "Most frequent keywords", themes = "Thematic map")

#' Figures captured in the browser (PNG, base64) with a caption and the report section they belong to.
normalize_report_figures <- function(raw) {
  raw <- Filter(function(g) is.list(g) && is.character(g$png), raw %||% list())
  if (length(raw) > 24) api_error(413, "biblio_report_figures", "A report can include at most 24 visuals.")
  out <- lapply(raw, function(g) {
    png <- gsub("[[:space:]]", "", g$png[1])
    if (!grepl("^[A-Za-z0-9+/]+=*$", png) || !startsWith(png, "iVBOR")) api_error(400, "biblio_report_figures", "A visual is not a PNG image.")
    if (nchar(png) > 12e6) api_error(413, "biblio_report_figures", "A visual is too large.", "Choose fewer or smaller visuals.")
    list(png = png, caption = substr(trimws(as.character(g$caption %||% "Figure")[1]), 1, 300), section = as.character(g$section %||% "")[1])
  })
  if (sum(vapply(out, function(g) nchar(g$png), 0)) > 60e6) api_error(413, "biblio_report_figures", "The visuals are too large together.", "Choose fewer visuals.")
  out
}

#' Bar chart as inline SVG for the report: horizontal (ranked items) or vertical (years).
biblio_svg_bars <- function(labels, values, vertical = FALSE, color = "#2f5f98") {
  n <- length(values); if (!n) return("")
  vmax <- max(values, 1)
  esc <- html_escape
  svg <- function(W, H, inner) sprintf("<div class=\"diagram\"><svg viewBox=\"0 0 %d %d\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" font-family=\"Segoe UI, sans-serif\">%s</svg></div>", W, H, inner)
  if (vertical) {
    W <- 900; H <- 240; l <- 40; b <- 28; t <- 10; bw <- (W - l - 10) / n
    step <- max(1, ceiling(n / 15))
    bars <- vapply(seq_len(n), function(i) {
      h <- (H - b - t) * values[i] / vmax; x <- l + (i - 1) * bw
      lab <- if ((i - 1) %% step == 0) sprintf("<text x=\"%.1f\" y=\"%d\" font-size=\"10\" text-anchor=\"middle\" fill=\"#556270\">%s</text>", x + bw / 2, H - b + 14, esc(labels[i])) else ""
      sprintf("<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" rx=\"2\" fill=\"%s\"><title>%s: %s</title></rect>%s", x + bw * 0.12, H - b - h, bw * 0.76, h, color, esc(labels[i]), format(values[i]), lab)
    }, "")
    axis <- sprintf("<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#c9d2dd\"/><text x=\"%d\" y=\"%d\" font-size=\"10\" text-anchor=\"end\" fill=\"#556270\">%s</text>", l, H - b, W - 10, H - b, l - 6, t + 8, format(vmax))
    return(svg(W, H, paste0(axis, paste(bars, collapse = ""))))
  }
  W <- 900; rowH <- 22; l <- 330; H <- n * rowH + 8
  bars <- vapply(seq_len(n), function(i) {
    y <- 4 + (i - 1) * rowH; w <- (W - l - 60) * values[i] / vmax
    lab <- labels[i]; if (nchar(lab) > 52) lab <- paste0(substr(lab, 1, 50), "…")
    sprintf("<text x=\"%d\" y=\"%.1f\" font-size=\"12\" text-anchor=\"end\" fill=\"#1d2733\">%s</text><rect x=\"%d\" y=\"%.1f\" width=\"%.1f\" height=\"%d\" rx=\"3\" fill=\"%s\"/><text x=\"%.1f\" y=\"%.1f\" font-size=\"11\" fill=\"#556270\">%s</text>",
            l - 8, y + 14, esc(lab), l, y + 3, max(w, 1), rowH - 7, color, l + w + 6, y + 14, format(round(values[i], 2)))
  }, "")
  svg(W, H, paste(bars, collapse = ""))
}

#' Bibliometric report (HTML) of the filtered collection: the chosen sections, each computed with the same functions
#' as the views. A section the collection cannot support (for example no affiliations) is replaced by a note.
biblio_report_html <- function(proj, filters, sections = names(BIBLIO_REPORT_SECTIONS), project_name = "Bibliometric analysis", n = 10, figures = list()) {
  sections <- intersect(names(BIBLIO_REPORT_SECTIONS), as.character(unlist(sections)))
  figures <- normalize_report_figures(figures)
  if (!length(sections) && !length(figures)) api_error(400, "biblio_report_sections", "Choose at least one section or visual for the report.")
  M <- biblio_working(proj, filters)
  b <- biblio_get(proj)
  f <- normalize_biblio_filters(filters)
  esc <- html_escape
  na <- function(x) is.null(x) || !length(x) || is.na(suppressWarnings(as.numeric(x[1])))
  num <- function(x, d = 2) if (na(x)) "&ndash;" else formatC(as.numeric(x[1]), format = "f", digits = d, big.mark = ",")
  int <- function(x) if (na(x)) "&ndash;" else formatC(round(as.numeric(x[1])), format = "d", big.mark = ",")
  yr <- function(x) if (na(x)) "&ndash;" else as.character(round(as.numeric(x[1])))
  tabno <- 0L
  tab <- function(caption, headers, rows, num_cols) { tabno <<- tabno + 1L; html_table(headers, rows, paste0("Table ", tabno, ". ", caption), num_cols) }
  figno <- 0L
  fig <- function(svg, caption) { figno <<- figno + 1L; paste0(svg, "<p class=\"note\"><strong>Figure ", figno, ".</strong> ", caption, "</p>") }
  safe <- function(expr) tryCatch(expr, pathlab_error = function(e) paste0("<div class=\"warn\">", esc(conditionMessage(e)), "</div>"),
                                  error = function(e) paste0("<div class=\"warn\">This section could not be computed from the collection: ",
                                                             esc(sanitize_message(conditionMessage(e), 200)), ".</div>"))
  kw_field <- if (isTRUE(mean(!is.na(M$DE) & nzchar(M$DE)) > 0.05)) "DE" else "ID"
  ov <- NULL
  overview <- function() { if (is.null(ov)) ov <<- biblio_overview(M, list()); ov }
  au <- NULL
  authors <- function() { if (is.null(au)) au <<- biblio_authors(M, list(nItems = n)); au }
  body <- character(0)
  for (k in seq_along(sections)) {
    s <- sections[k]
    html <- safe(switch(s,
      collection = {
        o <- overview()
        dbs <- unique(vapply(b$files %||% list(), function(x) as.character(x$database %||% ""), ""))
        flt <- c(if (!is.null(f$yearFrom) || !is.null(f$yearTo)) paste0("years ", f$yearFrom %||% "&hellip;", "&ndash;", f$yearTo %||% "&hellip;"),
                 if (length(f$documentTypes)) paste0("document types: ", esc(paste(f$documentTypes, collapse = ", "))),
                 if (length(f$languages)) paste0("languages: ", esc(paste(f$languages, collapse = ", "))),
                 if ((f$minCitations %||% 0) > 0) paste0("at least ", f$minCitations, " citations"))
        files <- vapply(b$files %||% list(), function(x) paste0(esc(x$name %||% ""), if (!is.null(x$records)) paste0(" (", int(x$records), " records)") else ""), "")
        kv <- paste0("<dl class=\"kv\"><dt>Source files</dt><dd>", if (length(files)) paste(files, collapse = "<br>") else "&ndash;", "</dd>",
                     "<dt>Databases</dt><dd>", esc(paste(dbs[nzchar(dbs)], collapse = ", ")), "</dd>",
                     "<dt>Filters</dt><dd>", if (length(flt)) paste(flt, collapse = "; ") else "none (the whole collection)", "</dd>",
                     "<dt>Thesaurus</dt><dd>", length(b$thesaurus %||% list()), " replacement(s)</dd>",
                     "<dt>Documents analysed</dt><dd>", int(nrow(M)), "</dd></dl>")
        rows <- lapply(o$main, function(r) if (isTRUE(r$heading)) c(paste0("<strong>", esc(r$label), "</strong>"), "") else c(esc(r$label), esc(r$value)))
        paste0(kv, tab("Main information about the collection", c("Description", "Results"), rows, 2))
      },
      production = {
        o <- overview()
        yrs <- vapply(o$annual, function(r) as.numeric(r$year), 0); docs <- vapply(o$annual, function(r) as.numeric(r$documents), 0)
        paste0("<p>The collection spans ", min(yrs), "&ndash;", max(yrs), " with an annual growth rate of ", num(o$growthRate), "%.</p>",
               fig(biblio_svg_bars(as.character(yrs), docs, vertical = TRUE), "Annual scientific production (documents per year)."),
               tab("Annual scientific production and citations", c("Year", "Documents", "Mean citations per document", "Mean citations per year"),
                   lapply(o$annual, function(r) c(r$year, int(r$documents), num(r$meanTC), num(r$meanTCperYear))), 2:4))
      },
      sources = {
        x <- biblio_sources(M, list(nItems = n))
        rel <- x$relevant
        paste0(fig(biblio_svg_bars(vapply(rel, function(r) title_case(r$source), ""), vapply(rel, function(r) as.numeric(r$documents), 0), color = "#c2410c"),
                   "Most relevant sources (number of documents)."),
               tab("Bradford's law: source zones", c("Zone", "Sources", "Documents"), lapply(x$bradford$zones, function(z) c(esc(z$zone), int(z$sources), int(z$documents))), 2:3),
               tab("Source impact", c("Source", "h-index", "g-index", "m-index", "Total citations", "Documents", "First year"),
                   lapply(x$impact, function(r) c(esc(title_case(r$Element)), int(r$h_index), int(r$g_index), num(r$m_index, 3), int(r$TC), int(r$NP), yr(r$PY_start))), 2:7))
      },
      authors = {
        x <- authors()
        rel <- x$relevant
        lo <- x$lotka
        paste0(fig(biblio_svg_bars(vapply(rel, `[[`, "", "author"), vapply(rel, function(r) as.numeric(r$documents), 0), color = "#7c3aed"), "Most relevant authors (number of documents)."),
               tab("Most relevant authors", c("Author", "Documents", "Fractionalized documents"),
                   lapply(rel, function(r) c(esc(r$author), int(r$documents), num(r$fractionalized))), 2:3),
               tab("Author impact", c("Author", "h-index", "g-index", "Total citations", "Documents"),
                   lapply(x$hindex, function(r) c(esc(r$Element), int(r$h_index), int(r$g_index), int(r$TC), int(r$NP))), 2:5),
               "<p>Lotka's law: ", if (!na(lo$beta)) paste0("estimated exponent &beta; = ", num(lo$beta), ", constant C = ", num(lo$c, 3), ", R&sup2; = ", num(lo$r2, 3), ".") else "could not be estimated.", "</p>",
               tab("Author productivity (Lotka's law)", c("Documents written", "Authors", "Proportion of authors"),
                   lapply(lo$table, function(r) c(int(r$documents), int(r$authors), num(r$proportion, 3))), 1:3))
      },
      countries = {
        x <- authors()
        if (!length(x$countries)) stop("no author countries were found (the affiliations are missing)")
        paste0(tab("Corresponding author's country: single-country (SCP) and multiple-country (MCP) publications",
                   c("Country", "Documents", "SCP", "MCP", "MCP ratio"),
                   lapply(x$countries, function(r) c(esc(title_case(r$country)), int(r$documents), int(r$scp), int(r$mcp), num(r$mcp / max(1, r$documents), 3))), 2:5),
               tab("Most cited countries", c("Country", "Total citations", "Average citations per document"),
                   lapply(x$citedCountries, function(r) c(esc(title_case(r$country)), int(r$citations), num(r$average))), 2:3))
      },
      documents = {
        x <- biblio_documents(M, list(nItems = n))
        paste0(tab("Most globally cited documents", c("Document", "Title", "Year", "Citations", "Citations per year", "Normalized citations"),
                   lapply(x$global, function(r) c(esc(r$document), esc(r$title), r$year, int(r$citations), num(r$perYear), num(r$normalized))), 3:6),
               if (length(x$references)) tab("Most cited references in the collection", c("Cited reference", "Citations"),
                                             lapply(x$references, function(r) c(esc(r$reference), int(r$citations))), 2) else "")
      },
      keywords = {
        x <- biblio_words(M, list(field = kw_field, nItems = 20))
        w <- x$words
        tr <- x$trend
        paste0(fig(biblio_svg_bars(vapply(w, function(r) title_case(r$word), ""), vapply(w, function(r) as.numeric(r$frequency), 0), color = "#0f766e"),
                   paste0("Most frequent ", tolower(x$fieldLabel), " (occurrences).")),
               if (length(tr) && all(c("item", "freq", "year_med") %in% names(tr[[1]])))
                 tab("Trend topics (years of use)", c("Term", "Frequency", "First quartile year", "Median year", "Third quartile year"),
                     lapply(utils::head(tr, 30), function(r) c(esc(title_case(r$item)), int(r$freq), yr(r$year_q1), yr(r$year_med), yr(r$year_q3))), 2:5) else "")
      },
      themes = {
        x <- biblio_thematic_map(M, list(field = kw_field, minFreq = 5))
        cl <- x$clusters
        if (!length(cl)) stop("no themes reached the minimum frequency")
        xs <- vapply(cl, function(c) as.numeric(c$rcentrality), 0); ys <- vapply(cl, function(c) as.numeric(c$rdensity), 0)
        ox <- (min(xs) + max(xs)) / 2; oy <- (min(ys) + max(ys)) / 2
        quad <- ifelse(xs >= ox, ifelse(ys >= oy, "Motor theme", "Basic theme"), ifelse(ys >= oy, "Niche theme", "Emerging or declining theme"))
        paste0("<p>Clusters of ", tolower(x$fieldLabel), " placed by centrality (relevance) and density (development), following Cobo et al. (2011). ",
               "Quadrants are split at the middle of each axis, as in the thematic map view.</p>",
               tab("Themes of the thematic map", c("Theme", "Quadrant", "Words", "Frequency", "Centrality rank", "Density rank", "Main words"),
                   lapply(seq_along(cl), function(i) c(esc(title_case(cl[[i]]$label)), quad[i], int(cl[[i]]$size), int(cl[[i]]$frequency), num(xs[i], 1), num(ys[i], 1),
                                                       esc(paste(vapply(utils::head(cl[[i]]$words, 6), function(w) as.character(w$word), ""), collapse = ", ")))), 3:6))
      }))
    mine <- Filter(function(g) identical(g$section, s), figures)
    extra <- paste(vapply(mine, function(g) fig(paste0("<div class=\"diagram\"><img alt=\"", esc(g$caption), "\" src=\"data:image/png;base64,", g$png, "\" style=\"max-width:100%;height:auto\"></div>"), esc(g$caption)), ""), collapse = "")
    body <- c(body, paste0("<h2>", k, ". ", BIBLIO_REPORT_SECTIONS[[s]], "</h2>"), html, extra)
  }
  rest <- Filter(function(g) !g$section %in% sections, figures)
  if (length(rest)) body <- c(body, paste0("<h2>", length(sections) + 1, ". Figures</h2>"),
                              vapply(rest, function(g) fig(paste0("<div class=\"diagram\"><img alt=\"", esc(g$caption), "\" src=\"data:image/png;base64,", g$png, "\" style=\"max-width:100%;height:auto\"></div>"), esc(g$caption)), ""))
  intro <- paste0("<style>.kv dd{word-break:normal;overflow-wrap:anywhere}</style><div class=\"info\">Bibliometric analysis of ", int(nrow(M)), " documents, computed with bibliometrix ",
                  esc(as.character(utils::packageVersion("bibliometrix"))), " (Aria &amp; Cuccurullo, 2017). Every number comes from the collection after the filters and thesaurus listed under Data and collection.</div>")
  report_doc(project_name, paste0("Bibliometric analysis report &middot; PathLab SEM ", PATHLAB_RELEASE, " &middot; generated ", esc(now_iso()), " (UTC)"),
             paste(c(intro, body), collapse = "\n"))
}
