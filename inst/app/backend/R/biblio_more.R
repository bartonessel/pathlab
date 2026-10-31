# Bibliometric analyses beyond Biblioshiny's menu: CiteSpace's burst detection (Kleinberg, 2003), timeline and timezone
# views with cluster silhouette, betweenness centrality, sigma and log-likelihood cluster labels (Chen, 2006; Chen et
# al., 2010); Biblioshiny's clustering by coupling and life cycle; theme performance (SciMAT, Cobo et al., 2012);
# collaboration indicators and Price's law; the VOS mapping technique and VOSviewer's relevance-based term selection
# (van Eck & Waltman, 2010); a PRISMA 2020 flow of the collection; and record retrieval from OpenAlex and PubMed.
# The views are registered in BIBLIO_VIEWS (biblio.R); all numbers are computed here or by bibliometrix / igraph.

# ---- Burst detection ------------------------------------------------------------------------------

#' Kleinberg's two-state burst detection on yearly counts (batched model, as in CiteSpace). `r` = documents with the
#' item per year, `d` = documents per year; the burst state has rate s x the base rate and entering it costs
#' gamma * ln(years). Returns the state of each year and the bursts with their strength (cost saved by the burst state).
kleinberg_bursts <- function(r, d, s = 2, gamma = 1) {
  n <- length(r); ok <- d > 0
  p0 <- sum(r) / sum(d)
  if (!is.finite(p0) || p0 <= 0 || p0 >= 1 || n < 2) return(list(state = integer(n), bursts = list()))
  p1 <- min(0.9999, s * p0)
  cost <- function(p) ifelse(ok, -stats::dbinom(r, d, p, log = TRUE), 0)
  c0 <- cost(p0); c1 <- cost(p1); tau <- gamma * log(n)
  C <- matrix(Inf, n, 2); B <- matrix(1L, n, 2)
  C[1, ] <- c(c0[1], c1[1] + tau)
  for (t in seq_len(n)[-1]) {
    a <- C[t - 1, 1]; b <- C[t - 1, 2]
    if (a <= b) { C[t, 1] <- a + c0[t]; B[t, 1] <- 1L } else { C[t, 1] <- b + c0[t]; B[t, 1] <- 2L }
    if (a + tau <= b) { C[t, 2] <- a + tau + c1[t]; B[t, 2] <- 1L } else { C[t, 2] <- b + c1[t]; B[t, 2] <- 2L }
  }
  st <- integer(n); st[n] <- which.min(C[n, ])
  if (n > 1) for (t in n:2) st[t - 1] <- B[t, st[t]]
  state <- st - 1L
  runs <- rle(state); ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
  bursts <- lapply(which(runs$values == 1), function(k) { i <- starts[k]:ends[k]; list(from = starts[k], to = ends[k], strength = sum(c0[i] - c1[i])) })
  list(state = state, bursts = Filter(function(b) b$strength > 0, bursts))
}

#' Years x items counts of documents (by publication year of the citing document) for an incidence matrix.
biblio_year_counts <- function(M, A) {
  py <- M$PY; ok <- which(!is.na(py))
  if (!length(ok)) api_error(422, "biblio_years", "The documents have no publication years.")
  yrs <- seq(min(py[ok]), max(py[ok]))
  Y <- Matrix::sparseMatrix(i = match(py[ok], yrs), j = ok, x = 1, dims = c(length(yrs), nrow(M)))
  list(years = yrs, R = as.matrix(Y %*% A), d = tabulate(match(py[ok], yrs), length(yrs)))
}

#' Strongest burst of each column of a years x items count matrix (strength 0 when there is none).
biblio_item_bursts <- function(yc, s = 2, gamma = 1) {
  lapply(seq_len(ncol(yc$R)), function(j) {
    kb <- kleinberg_bursts(yc$R[, j], yc$d, s, gamma)
    if (!length(kb$bursts)) return(list(strength = 0, begin = NULL, end = NULL))
    b <- kb$bursts[[which.max(vapply(kb$bursts, `[[`, 0, "strength"))]]
    list(strength = b$strength, begin = yc$years[b$from], end = yc$years[b$to])
  })
}

biblio_field_label <- function(f) { l <- unname(c(BIBLIO_FIELDS, CR_AU = "Cited authors", CR_SO = "Cited sources")[f]); if (is.na(l)) f else l }

#' Publication year of a cited reference ("AUTHOR A, 2015, JOURNAL ..."), NA when it has none.
cr_year <- function(x) { y <- suppressWarnings(as.numeric(sub("^.*?,\\s*((1[89]|20)\\d{2})\\b.*$", "\\1", x, perl = TRUE))); y[!grepl(",\\s*(1[89]|20)\\d{2}\\b", x)] <- NA; y }

biblio_bursts <- function(M, o) {
  field <- o$field %||% "DE"
  if (!field %in% c("DE", "ID", "KW", "TI", "AB", "CR", "CR_AU", "CR_SO", "AU", "SO", "AU_CO", "AU_UN")) field <- "DE"
  inc <- biblio_incidence(M, field); A <- inc$A; M <- inc$M
  minf <- max(1, o$minFreq %||% 5); s <- min(5, max(1.1, o$s %||% 2)); gamma <- min(5, max(0.1, o$gamma %||% 1))
  occ <- Matrix::colSums(A)
  cand <- which(occ >= minf)
  if (!length(cand)) api_error(422, "biblio_threshold", sprintf("No item occurs in %d or more documents.", minf), "Lower the minimum frequency.")
  cand <- utils::head(cand[order(-occ[cand])], 2000)
  yc <- biblio_year_counts(M, A[, cand, drop = FALSE])
  b <- biblio_item_bursts(yc, s, gamma)
  labels <- colnames(A)[cand]
  first <- if (field == "CR") cr_year(labels) else rep(NA_real_, length(cand))
  appear <- vapply(seq_along(cand), function(j) { k <- which(yc$R[, j] > 0); if (length(k)) yc$years[k[1]] else NA_real_ }, 0)
  first[is.na(first)] <- appear[is.na(first)]
  hit <- which(vapply(b, `[[`, 0, "strength") > 0)
  hit <- utils::head(hit[order(-vapply(b[hit], `[[`, 0, "strength"))], o$nItems %||% 25)
  items <- lapply(hit, function(j) list(item = labels[j], year = num_or_null(first[j]), strength = b[[j]]$strength, begin = b[[j]]$begin, end = b[[j]]$end,
                                        frequency = as.integer(occ[cand[j]]), counts = as.list(as.integer(yc$R[, j]))))
  list(field = field, fieldLabel = biblio_field_label(field), years = as.list(yc$years), items = items, candidates = length(cand),
       params = list(s = s, gamma = gamma, minFreq = minf))
}

# ---- Cluster quality: silhouette, betweenness, sigma, log-likelihood labels ----------------------------------------

#' Dunning's log-likelihood ratio (G2) of each title term in a cluster's documents against the other documents; the
#' highest over-represented terms label the cluster (CiteSpace's LLR labels).
llr_labels <- function(terms_by_doc, in_cluster, k = 3) {
  inside <- unlist(lapply(terms_by_doc[in_cluster], unique)); outside <- unlist(lapply(terms_by_doc[!in_cluster], unique))
  if (!length(inside)) return(list())
  ti <- table(inside); to <- table(outside)
  n1 <- sum(in_cluster); n2 <- sum(!in_cluster)
  a <- as.numeric(ti); b <- as.numeric(to[names(ti)]); b[is.na(b)] <- 0
  # A label must appear in at least 5% of the cluster's documents (at least 3), so rare exclusive words do not win.
  keep <- a >= max(3, ceiling(0.05 * n1)) & a / n1 > b / max(1, n2)
  if (!any(keep)) return(list())
  a <- a[keep]; b <- b[keep]; w <- names(ti)[keep]
  g2 <- function(o, e) ifelse(o > 0, o * log(o / e), 0)
  N <- n1 + n2; c1 <- n1 - a; d1 <- n2 - b
  E <- function(r, c) r * c / N
  G <- 2 * (g2(a, E(a + b, n1)) + g2(b, E(a + b, n2)) + g2(c1, E(c1 + d1, n1)) + g2(d1, E(c1 + d1, n2)))
  o <- order(-G)[seq_len(min(k, length(G)))]
  lapply(o, function(i) list(term = w[i], llr = G[i], p = stats::pchisq(G[i], 1, lower.tail = FALSE)))
}

#' Title terms of each document (for cluster labels).
biblio_title_terms <- function(M) {
  # Single words and two-word phrases (CiteSpace labels clusters with noun phrases such as "citation analysis").
  sp <- function(x) if (is.na(x)) character(0) else unique(tolower(trimws(unlist(strsplit(x, ";")))))
  T1 <- tryCatch(biblio_terms(M, "TI", 1)$TI, error = function(e) tolower(M$TI))
  T2 <- tryCatch(biblio_terms(M, "TI", 2)$TI, error = function(e) rep(NA_character_, nrow(M)))
  lapply(seq_len(nrow(M)), function(i) unique(c(sp(as.character(T2[i])), sp(as.character(T1[i])))))
}

#' Silhouette of each cluster (dissimilarity 1 - normalised similarity), betweenness centrality (distances = inverse
#' link weights), burst strength and sigma = (betweenness + 1)^burst of each item, and LLR labels of each cluster from
#' the titles of the documents that use (or cite) its items.
biblio_cluster_quality <- function(S, memb, g, M, docs_of, yc = NULL, s = 2, gamma = 1) {
  # Dissimilarity: 1 - cosine similarity of the items' link profiles (items linked to the same items are similar).
  Sd <- as.matrix(S); diag(Sd) <- 0
  nr <- sqrt(rowSums(Sd^2)); nr[nr == 0] <- 1
  D <- 1 - (Sd %*% t(Sd)) / outer(nr, nr); D[D < 0] <- 0; diag(D) <- 0
  k <- length(unique(memb))
  sil <- if (k >= 2) tryCatch({ si <- cluster::silhouette(as.integer(memb), dmatrix = D); summary(si)$clus.avg.widths }, error = function(e) NULL) else NULL
  # Betweenness on the unweighted network (as CiteSpace reports it), normalised to 0-1.
  btw <- tryCatch(igraph::betweenness(g, weights = NA, normalized = TRUE), error = function(e) rep(0, length(memb)))
  bursts <- if (is.null(yc)) NULL else biblio_item_bursts(yc, s, gamma)
  burst <- if (is.null(bursts)) rep(0, length(memb)) else vapply(bursts, `[[`, 0, "strength")
  terms <- biblio_title_terms(M)
  # Each document belongs to the cluster holding most of its items (documents without items belong to none).
  cls <- sort(unique(memb))
  H <- as.matrix(docs_of %*% Matrix::sparseMatrix(i = seq_along(memb), j = match(memb, cls), x = 1, dims = c(length(memb), length(cls))))
  dom <- ifelse(rowSums(H) > 0, cls[max.col(H, ties.method = "first")], NA)
  clusters <- lapply(cls, function(cl) {
    used <- !is.na(dom) & dom == cl
    lab <- llr_labels(terms, used)
    list(id = cl, silhouette = if (is.null(sil)) NULL else num_or_null(unname(sil[as.character(cl)])), documents = sum(used), labels = lab)
  })
  list(betweenness = as.numeric(btw), burst = burst, sigma = (as.numeric(btw) + 1)^burst,
       burstSpan = if (is.null(bursts)) NULL else lapply(bursts, function(b) if (b$strength > 0) list(begin = b$begin, end = b$end) else NULL),
       clusters = clusters, meanSilhouette = if (is.null(sil)) NULL else num_or_null(stats::weighted.mean(sil, as.numeric(table(memb)[names(sil)]), na.rm = TRUE)))
}

# ---- Timeline and timezone views (CiteSpace) ----------------------------------------------------------

biblio_timeline <- function(M, o) {
  field <- o$field %||% "DE"
  if (!field %in% c("DE", "ID", "KW", "TI", "AB", "CR", "CR_AU", "CR_SO")) field <- "DE"
  inc <- biblio_incidence(M, field); A <- inc$A; M <- inc$M
  occ <- Matrix::colSums(A)
  keep <- which(occ >= max(1, o$minFreq %||% 5))
  keep <- utils::head(keep[order(-occ[keep])], min(400, max(20, o$nItems %||% 150)))
  if (length(keep) < 5) api_error(422, "biblio_threshold", "Fewer than five items reach the minimum frequency.", "Lower the minimum frequency.")
  A <- A[, keep, drop = FALSE]; occ <- occ[keep]
  C <- Matrix::crossprod(A); Matrix::diag(C) <- 0; C <- Matrix::drop0(C)
  tls <- Matrix::rowSums(C)
  conn <- which(tls > 0)
  if (length(conn) < 5) api_error(422, "biblio_unconnected", "The items are not connected to each other.", "Lower the minimum frequency or choose another field.")
  A <- A[, conn, drop = FALSE]; occ <- occ[conn]; C <- C[conn, conn, drop = FALSE]; tls <- tls[conn]
  w <- pmax(tls, 1e-12)
  S <- methods::as(Matrix::Diagonal(x = 1 / w) %*% C %*% Matrix::Diagonal(x = 1 / w), "CsparseMatrix")
  ed <- Matrix::summary(Matrix::triu(S, 1))
  g <- igraph::graph_from_data_frame(data.frame(from = ed$i, to = ed$j, weight = ed$x / max(ed$x)), directed = FALSE, vertices = data.frame(name = seq_len(ncol(A))))
  set.seed(o$seed %||% 2026)
  memb <- igraph::membership(igraph::cluster_leiden(g, objective_function = "modularity", weights = igraph::E(g)$weight, resolution = o$resolution %||% 1, n_iterations = -1))
  rk <- rank(-table(memb)[as.character(sort(unique(memb)))], ties.method = "first"); memb <- as.integer(rk[as.character(memb)])
  yc <- biblio_year_counts(M, A)
  q <- biblio_cluster_quality(S, memb, g, M, A, yc)
  labels <- colnames(A)
  first <- vapply(seq_len(ncol(A)), function(j) { k <- which(yc$R[, j] > 0); if (length(k)) yc$years[k[1]] else NA_real_ }, 0)
  if (field == "CR") { cy <- cr_year(labels); first[!is.na(cy)] <- cy[!is.na(cy)] }
  nodes <- lapply(seq_along(labels), function(k) list(id = k - 1L, label = labels[k], cluster = memb[k], year = num_or_null(first[k]), frequency = as.integer(occ[k]),
                                                      betweenness = q$betweenness[k], burst = q$burst[k], sigma = q$sigma[k]))
  cnt <- Matrix::summary(Matrix::triu(C, 1))
  cnt <- cnt[order(-cnt$x), , drop = FALSE][seq_len(min(nrow(cnt), 1500)), , drop = FALSE]
  edges <- lapply(seq_len(nrow(cnt)), function(r) list(source = cnt$i[r] - 1L, target = cnt$j[r] - 1L, strength = cnt$x[r]))
  clusters <- lapply(q$clusters, function(cl) {
    idx <- which(memb == cl$id); yrs <- first[idx]
    top <- idx[order(-occ[idx])][seq_len(min(5, length(idx)))]
    c(cl, list(size = length(idx), label = if (length(cl$labels)) cl$labels[[1]]$term else labels[top[1]], meanYear = num_or_null(mean(yrs, na.rm = TRUE)),
               top = as.list(labels[top])))
  })
  list(field = field, fieldLabel = biblio_field_label(field), years = as.list(yc$years), nodes = nodes, edges = edges, clusters = clusters,
       modularity = num_or_null(igraph::modularity(g, memb, weights = igraph::E(g)$weight)), meanSilhouette = q$meanSilhouette)
}

# ---- Clustering by coupling (Biblioshiny) -------------------------------------------------------------

biblio_coupling_map <- function(M, o) {
  an <- o$analysis %||% "documents"; if (!an %in% c("documents", "authors", "sources")) an <- "documents"
  imp <- if (identical(o$impact, "global")) "global" else "local"
  cm <- bq(bibliometrix::couplingMap(M, analysis = an, field = "CR", n = o$n %||% 250, impact.measure = imp, minfreq = o$minFreq %||% 5,
                                     size = 0.5, n.labels = 3, repel = TRUE))
  cl <- as.data.frame(cm$clusters); dat <- as.data.frame(cm$data)
  item_col <- names(dat)[1]; score_col <- intersect(c("NormalizedLocalCitationScore", "NormalizedGlobalCitationScore", "LCS", "GCS"), names(dat))[1]
  # Cluster labels: log-likelihood title terms of the cluster's documents (of its authors or sources).
  terms <- biblio_title_terms(M)
  docs_in <- function(items) {
    it <- toupper(trimws(items))
    switch(an, documents = toupper(trimws(M$SR)) %in% it, sources = toupper(trimws(M$SO)) %in% it,
           authors = vapply(M$AU, function(x) any(toupper(split_items(x)) %in% it), TRUE))
  }
  clusters <- lapply(seq_len(nrow(cl)), function(i) {
    d <- dat[dat$Cluster == cl$group[i], , drop = FALSE]
    lab <- llr_labels(terms, docs_in(as.character(d[[item_col]])))
    if (!is.na(score_col)) d <- d[order(-d[[score_col]]), , drop = FALSE]
    d <- utils::head(d, 10)
    list(id = as.integer(cl$group[i]), label = if (length(lab)) paste(vapply(utils::head(lab, 2), `[[`, "", "term"), collapse = " / ") else paste("Cluster", cl$group[i]),
         labels = lab, size = as.integer(cl$freq[i]),
         centrality = num_or_null(cl$centrality[i]), impact = num_or_null(cl$impact[i]),
         items = lapply(seq_len(nrow(d)), function(j) list(item = title_case(as.character(d[[item_col]][j])), score = if (is.na(score_col)) NULL else num_or_null(d[[score_col]][j]))))
  })
  list(analysis = an, impact = imp, clusters = clusters)
}

# ---- Life cycle, collaboration indicators, Price's law ---------------------------------------------------

#' Logistic life cycle of the field (bibliometrix::lifeCycle): saturation, peak year and the years by which 10%,
#' 90% and 99% of the eventual output are reached.
biblio_life_cycle <- function(M) {
  tb <- as.data.frame(table(PY = M$PY[!is.na(M$PY)])); if (nrow(tb) < 5) return(NULL)
  tb$PY <- as.numeric(as.character(tb$PY)); names(tb)[2] <- "n"
  lc <- tryCatch(bq(bibliometrix::lifeCycle(tb, forecast_years = 10, plot = FALSE, verbose = FALSE)), error = function(e) NULL)
  if (is.null(lc)) return(NULL)
  p <- lc$parameters_real_years
  list(K = num_or_null(p$K), peakYear = num_or_null(p$tm_year), peakAnnual = num_or_null(p$peak_annual), year10 = num_or_null(p$year_10_pct),
       year90 = num_or_null(p$year_90_pct), year99 = num_or_null(p$year_99_pct), r2 = num_or_null(lc$metrics$R_squared),
       observed = lapply(seq_len(nrow(lc$data)), function(i) list(year = lc$data$PY[i], annual = lc$data$n[i], fitted = lc$data$fitted_annual[i])),
       forecast = lapply(seq_len(nrow(lc$forecast)), function(i) list(year = lc$forecast$PY[i], annual = lc$forecast$predicted_annual[i])))
}

#' Collaboration indicators: degree of collaboration (share of multi-authored documents; Subramanyam, 1983),
#' collaboration index (authors per multi-authored document; Lawani, 1980) and collaboration coefficient (Ajiferuke et
#' al., 1988); Price's law (the square root of all authors writes half of the documents; Price, 1963).
biblio_collab_indicators <- function(M) {
  au <- lapply(M$AU, function(x) unique(split_items(x)))
  na <- lengths(au); na <- na[na > 0]; N <- length(na)
  if (!N) return(NULL)
  multi <- na[na > 1]
  cnt <- sort(table(unlist(au)), decreasing = TRUE)
  top <- names(cnt)[seq_len(min(length(cnt), ceiling(sqrt(length(cnt)))))]
  share <- mean(vapply(au, function(a) length(a) > 0 && any(a %in% top), TRUE))
  docs_by_top <- sum(vapply(au, function(a) sum(a %in% top) / max(1, length(a)), 0)) / N   # fractional
  list(documents = N, degree = length(multi) / N, index = if (length(multi)) mean(multi) else NA_real_, coefficient = 1 - mean(1 / na),
       price = list(authors = length(cnt), core = length(top), share = share, fractionalShare = docs_by_top, holds = docs_by_top >= 0.5))
}

#' Cumulative documents per year of the most productive items of a field (countries, affiliations).
biblio_field_over_time <- function(M, field, k = 5) {
  top <- names(top_items(M[[field]], k)); yrs <- sort(unique(M$PY[!is.na(M$PY)]))
  list(years = as.list(yrs), series = lapply(top, function(it) list(item = it, cumulative = as.list(cumsum(vapply(yrs, function(y)
    sum(vapply(M[[field]][!is.na(M$PY) & M$PY == y], function(x) it %in% split_items(x), TRUE)), 0))))))
}

#' Theme performance (SciMAT): documents, citations, average citations and h-index of the documents of each theme.
theme_performance <- function(docs, cluster_col = "Assigned_cluster") {
  if (is.null(docs) || !nrow(docs)) return(list())
  docs <- as.data.frame(docs)
  h <- function(tc) { tc <- sort(tc[!is.na(tc)], decreasing = TRUE); sum(tc >= seq_along(tc)) }
  out <- lapply(split(docs, as.character(docs[[cluster_col]])), function(d) list(documents = nrow(d), citations = sum(d$TC, na.rm = TRUE),
                                                                                  avgCitations = mean(d$TC, na.rm = TRUE), hIndex = h(d$TC)))
  out
}

# ---- PRISMA 2020 flow of the collection ---------------------------------------------------------------

biblio_prisma <- function(M, o, proj = NULL, filters = NULL) {
  b <- biblio_get(proj); full <- b$M
  files <- b$files %||% list()
  ident <- lapply(files, function(f) {
    db <- f$database %||% ""; nm <- f$name %||% ""
    list(source = if (!nzchar(nm)) db else if (startsWith(nm, db)) nm else paste0(db, " (", nm, ")"), records = as.integer(f$records %||% 0))
  })
  total <- sum(vapply(ident, function(x) x$records, 0L))
  if (!total) total <- nrow(full)
  f <- normalize_biblio_filters(filters)
  steps <- list()
  cur <- rep(TRUE, nrow(full))
  step <- function(reason, keep) { n <- sum(cur & !keep); cur <<- cur & keep; if (n > 0) steps[[length(steps) + 1]] <<- list(reason = reason, n = n) }
  if (!is.null(f$yearFrom) || !is.null(f$yearTo)) step(sprintf("Published outside %s–%s", f$yearFrom %||% "", f$yearTo %||% ""),
                                                       !is.na(full$PY) & full$PY >= (f$yearFrom %||% -Inf) & full$PY <= (f$yearTo %||% Inf))
  if (length(f$documentTypes)) step("Other document types", toupper(trimws(full$DT)) %in% toupper(f$documentTypes))
  if (length(f$languages)) step("Other languages", toupper(trimws(full$LA)) %in% toupper(f$languages))
  if (length(f$sources)) step("Other sources", toupper(trimws(full$SO)) %in% toupper(f$sources))
  if (length(f$countries)) step("No author from the chosen countries", vapply(full$AU_CO, function(x) any(toupper(split_items(x)) %in% toupper(f$countries)), TRUE))
  if (length(f$bradfordZones)) step("Sources outside the chosen Bradford zones", biblio_bradford_keep(full, f$bradfordZones))
  if ((f$minCitations %||% 0) > 0) step(sprintf("Fewer than %s citations", f$minCitations), full$TC >= f$minCitations)
  list(identified = ident, total = total, duplicates = max(0L, total - nrow(full)), screened = nrow(full), excluded = steps, included = sum(cur))
}

#' Documents whose source lies in the chosen Bradford zones ("Zone 1" ... "Zone 3").
biblio_bradford_keep <- function(M, zones) {
  bt <- tryCatch(bq(bibliometrix::bradford(M))$table, error = function(e) NULL)
  if (is.null(bt)) return(rep(TRUE, nrow(M)))
  ok <- toupper(as.character(bt$SO[as.character(bt$Zone) %in% zones]))
  toupper(trimws(M$SO)) %in% ok
}

# ---- VOS mapping technique and relevance-based term selection --------------------------------------------

#' VOS layout (van Eck & Waltman, 2010; van Eck et al., 2010): minimises sum s_ij d_ij^2 - sum d_ij (attraction 2,
#' repulsion 1) by majorisation, starting from `init`; then centred, rotated to the principal axes and scaled.
vos_layout <- function(S, init, iterations = 200) {
  S <- as.matrix(S); n <- nrow(S); diag(S) <- 0
  pos <- S[upper.tri(S)]; m <- mean(pos[pos > 0]); if (is.finite(m) && m > 0) S <- S / m
  L <- -S; diag(L) <- rowSums(S)
  Li <- MASS::ginv(L + 1 / n)
  X <- scale(as.matrix(init), scale = FALSE)
  for (it in seq_len(iterations)) {
    D <- as.matrix(stats::dist(X)); D[D < 1e-9] <- 1e-9
    Bm <- -1 / D; diag(Bm) <- 0; diag(Bm) <- -rowSums(Bm)
    Xn <- 0.5 * Li %*% (Bm %*% X)
    Xn <- scale(Xn, scale = FALSE)
    if (max(abs(Xn - X)) < 1e-6 * max(abs(X))) { X <- Xn; break }
    X <- Xn
  }
  pc <- stats::prcomp(X, center = TRUE)$x[, 1:2, drop = FALSE]
  pc / max(abs(pc))
}

#' VOSviewer's relevance of each term: how unevenly its co-occurrences spread over the other terms (Kullback-Leibler
#' divergence from the overall distribution). General terms score low; the most relevant `pct` percent are kept.
term_relevance <- function(C) {
  C <- as.matrix(C); diag(C) <- 0
  tot <- colSums(C); pj <- tot / sum(tot)
  vapply(seq_len(nrow(C)), function(i) { r <- C[i, ]; if (sum(r) <= 0) return(0); pi <- r / sum(r); k <- pi > 0 & pj > 0; sum(pi[k] * log(pi[k] / pj[k])) }, 0)
}

# ---- Retrieval from OpenAlex and PubMed ----------------------------------------------------------------

#' Search OpenAlex (openalexR) or PubMed (pubmedR) and convert the records to a bibliometrix collection. Only these
#' two public APIs are contacted; the query and the optional e-mail (OpenAlex's polite pool) are the only data sent.
biblio_search <- function(source, query, limit = 500, yearFrom = NULL, yearTo = NULL, email = NULL) {
  query <- trimws(as.character(query %||% ""))[1]
  if (!nzchar(query) || nchar(query) > 1000) api_error(400, "biblio_query", "Enter a search query (up to 1000 characters).")
  limit <- as.integer(max(10, min(5000, suppressWarnings(as.numeric(limit)) %||% 500)))
  biblio_attach()
  if (identical(source, "pubmed")) {
    if (!requireNamespace("pubmedR", quietly = TRUE)) api_error(501, "biblio_search", "The pubmedR package is not installed.", "Run Rscript backend/install_packages.R.")
    q <- query
    if (!is.null(yearFrom) || !is.null(yearTo)) q <- sprintf("(%s) AND %s:%s[dp]", q, yearFrom %||% 1800, yearTo %||% 3000)
    res <- tryCatch(bq(pubmedR::pmApiRequest(query = q, limit = limit, api_key = NULL)), error = function(e) api_error(502, "biblio_search", paste("PubMed could not be searched:", sanitize_message(conditionMessage(e), 200))))
    M <- tryCatch(bq(bibliometrix::convert2df(res, dbsource = "pubmed", format = "api")), error = function(e) api_error(422, "biblio_search", paste("The PubMed records could not be read:", sanitize_message(conditionMessage(e), 200))))
    db <- "PubMed"
  } else {
    if (!requireNamespace("openalexR", quietly = TRUE)) api_error(501, "biblio_search", "The openalexR package is not installed.", "Run Rscript backend/install_packages.R.")
    if (!is.null(email) && nzchar(email) && grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) options(openalexR.mailto = email)
    # Raw records (output = "list"): bibliometrix reads them from an .RData file (dbsource "openalex_api"). The list
    # form also avoids openalexR's data-frame conversion, which fails on the current API ("doi" returned twice).
    args <- list(entity = "works", search = query, per_page = 200, pages = seq_len(ceiling(limit / 200)), output = "list", verbose = FALSE)
    if (!is.null(yearFrom)) args$from_publication_date <- sprintf("%d-01-01", as.integer(yearFrom))
    if (!is.null(yearTo)) args$to_publication_date <- sprintf("%d-12-31", as.integer(yearTo))
    works <- tryCatch(bq(do.call(openalexR::oa_fetch, args)), error = function(e) api_error(502, "biblio_search", paste("OpenAlex could not be searched:", sanitize_message(conditionMessage(e), 200)),
                                                                                           "Check the internet connection; OpenAlex may ask for an API key (OPENALEX_API_KEY) for large requests."))
    if (!length(works)) api_error(422, "biblio_empty", "OpenAlex found no records for this query.")
    works <- utils::head(works, limit)
    tmp <- tempfile(fileext = ".RData"); on.exit(unlink(tmp), add = TRUE)
    save(works, file = tmp)
    M <- tryCatch(bq(bibliometrix::convert2df(tmp, dbsource = "openalex_api", format = "api")), error = function(e) api_error(422, "biblio_search", paste("The OpenAlex records could not be read:", sanitize_message(conditionMessage(e), 200))))
    db <- "OpenAlex"
  }
  if (!is.data.frame(M) || !nrow(M)) api_error(422, "biblio_empty", "The search returned no records.")
  list(M = M, file = list(name = sprintf("%s search: %s", db, substr(query, 1, 80)), database = db, format = "api", records = nrow(M)))
}

BIBLIO_VIEWS$bursts <- biblio_bursts
BIBLIO_VIEWS$timeline <- biblio_timeline
BIBLIO_VIEWS$couplingMap <- biblio_coupling_map
BIBLIO_VIEWS$prisma <- biblio_prisma
