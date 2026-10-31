# FIMIX-PLS (finite mixture partial least squares; Hahn, Johnson, Herrmann & Huber, 2002; Sarstedt & Ringle, 2010).
# Construct scores come from SEMinR (the same estimation as the main analysis). The structural equations are then
# estimated as a finite mixture of normal regressions with flexmix: one regression per endogenous construct, all
# sharing the same latent segment membership, without intercepts (construct scores are standardised), fitted by EM
# with several random starts (flexmix::stepFlexmix). No R package implements FIMIX-PLS itself; this module assembles it
# from SEMinR and flexmix and is validated by recovering known segments from simulated data (test-fimix.R).
# Segment-retention criteria follow Sarstedt, Becker, Ringle & Schwaiger (2011).

normalize_fimix <- function(raw) {
  raw <- raw %||% list()
  mp <- suppressWarnings(as.numeric(raw[["minPrior"]] %||% 0.05))
  if (is.na(mp) || mp < 0.01 || mp > 0.25) api_error(400, "invalid_fimix", "The minimum segment size must be between 1% and 25%.")
  list(
    kMax = int_in(raw[["kMax"]], 4L, 2L, 8L, "The largest number of segments"),
    nrep = int_in(raw[["nrep"]], 10L, 1L, 30L, "The number of random starts"),
    iterMax = int_in(raw[["iterMax"]], 1000L, 50L, 5000L, "The maximum number of EM iterations"),
    seed = int_in(raw[["seed"]], 2026L, 1L, .Machine$integer.max, "The seed"),
    minPrior = mp
  )
}

fimix_criteria <- function(ll, npar, n, post) {
  k <- ncol(post)
  ent <- if (k == 1) NA_real_ else { p <- pmax(post, 1e-300); 1 - sum(-p * log(p)) / (n * log(k)) }
  list(logLik = ll, npar = npar,
       AIC = -2 * ll + 2 * npar, AIC3 = -2 * ll + 3 * npar, AIC4 = -2 * ll + 4 * npar,
       BIC = -2 * ll + log(n) * npar, CAIC = -2 * ll + (log(n) + 1) * npar,
       HQ = -2 * ll + 2 * log(log(n)) * npar, MDL5 = -2 * ll + 5 * log(n) * npar, EN = ent)
}

#' Posterior-weighted R^2 of one equation in one segment.
weighted_r2 <- function(y, yhat, w) {
  if (sum(w) <= 0) return(NA_real_)
  ybar <- sum(w * y) / sum(w)
  1 - sum(w * (y - yhat)^2) / sum(w * (y - ybar)^2)
}

run_fimix <- function(df, model, settings, opts) {
  if (!length(model$paths)) api_error(422, "fimix_no_paths", "FIMIX-PLS needs a structural model: draw at least one path.")
  if (length(model$moderations %||% list())) api_error(422, "fimix_moderation", "FIMIX-PLS is available for models without moderating effects.",
                                                       "Run it on a copy of the model without interaction terms (Duplicate the analysis, then delete the moderations).")
  if (!requireNamespace("flexmix", quietly = TRUE)) api_error(501, "fimix_unavailable", "The flexmix R package is not installed on the analysis server.",
                                                             "Run Rscript backend/install_packages.R.")
  t0 <- Sys.time()
  prep <- prepare_data(df, model, settings)
  iw <- if (settings$innerWeights == "path_factorial") seminr::path_factorial else seminr::path_weighting
  miss_fun <- if (settings$missing$treatment == "mean_replacement") seminr::mean_replacement else stats::na.omit
  m <- tryCatch(suppressMessages(seminr::estimate_pls(
    data = prep$data, measurement_model = build_measurement_model(model), structural_model = build_structural_model(model),
    inner_weights = iw, missing = miss_fun, missing_value = NA, maxIt = settings$maxIt, stopCriterion = settings$stopCriterion)),
    error = function(e) api_error(422, "fimix_estimation", paste("The PLS model could not be estimated:", conditionMessage(e))))
  sc <- as.data.frame(m$construct_scores)
  cons <- colnames(sc)
  safe <- setNames(make.names(cons, unique = TRUE), cons)
  names(sc) <- unname(safe)
  smx <- m$smMatrix
  endo <- unique(as.character(smx[, "target"]))
  eqs <- lapply(endo, function(y) list(target = y, predictors = as.character(smx[smx[, "target"] == y, "source"])))
  models <- lapply(eqs, function(e) flexmix::FLXMRglm(stats::as.formula(paste(safe[[e$target]], "~", paste(safe[e$predictors], collapse = " + "), "- 1"))))
  n <- nrow(sc)
  if (n < 30 * opts$kMax) warn_small <- sprintf("With n = %d, solutions with many segments rest on few cases per segment.", n) else warn_small <- NULL

  set.seed(opts$seed)
  fits <- tryCatch(suppressWarnings(flexmix::stepFlexmix(~ 1, data = sc, k = seq_len(opts$kMax), nrep = opts$nrep, model = models,
                                                         control = list(iter.max = opts$iterMax, minprior = opts$minPrior), verbose = FALSE)),
                   error = function(e) api_error(422, "fimix_failed", paste("FIMIX-PLS failed:", conditionMessage(e))))
  solutions <- lapply(seq_len(opts$kMax), function(k) {
    f <- flexmix::getModel(fits, as.character(k))
    post <- flexmix::posterior(f)
    ll <- as.numeric(f@logLik)   # slots: stats::logLik only dispatches to flexmix when the package is attached
    npar <- as.numeric(f@df)
    k0 <- f@k
    crit <- fimix_criteria(ll, npar, n, post)
    cl <- flexmix::clusters(f)
    segs <- lapply(seq_len(k0), function(s) {
      w <- post[, s]
      paths <- list(); r2 <- list()
      for (j in seq_along(eqs)) {
        e <- eqs[[j]]
        p <- flexmix::parameters(f, component = s, model = j)
        sel <- grepl("^coef\\.", rownames(p))
        coefs <- setNames(p[sel, 1], sub("^coef\\.", "", rownames(p)[sel]))   # names kept even for a single predictor
        x <- as.matrix(sc[, safe[e$predictors], drop = FALSE])
        yhat <- as.vector(x %*% coefs[safe[e$predictors]])
        for (pr in e$predictors) paths[[length(paths) + 1]] <- list(from = pr, to = e$target, coefficient = unname(coefs[[safe[[pr]]]]))
        r2[[length(r2) + 1]] <- list(construct = e$target, rSquared = weighted_r2(sc[[safe[[e$target]]]], yhat, w),
                                     sigma = unname(p["sigma", 1]))
      }
      list(segment = s, prior = unname(f@prior[s]), size = sum(cl == s), paths = paths, rSquared = r2)
    })
    list(k = k, kRetained = k0, converged = isTRUE(f@converged), iterations = f@iter, criteria = crit, segments = segs)
  })
  crit_names <- c("AIC", "AIC3", "AIC4", "BIC", "CAIC", "HQ", "MDL5")
  best <- lapply(setNames(crit_names, crit_names), function(cn) {
    v <- vapply(solutions, function(s) s$criteria[[cn]], 0)
    solutions[[which.min(v)]]$k
  })
  warn <- c(warn_small, unlist(lapply(solutions, function(s) if (s$kRetained < s$k) sprintf("With %d segments, %d segment(s) fell below the minimum size and were dropped by the EM algorithm.", s$k, s$k - s$kRetained))),
            if (any(!vapply(solutions, `[[`, TRUE, "converged"))) "Some solutions did not converge within the iteration limit; increase the maximum number of iterations.")
  list(
    options = opts,
    n = list(input = prep$nInput, used = n),
    equations = lapply(eqs, function(e) list(target = e$target, predictors = as.list(e$predictors))),
    solutions = solutions,
    best = best,
    warnings = as.list(warn),
    rCode = fimix_code(model, settings, opts, eqs, safe),
    packages = list(seminr = seminr_version(), flexmix = as.character(utils::packageVersion("flexmix"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = list(nInput = prep$nInput, rows = prep$rows, tables = setNames(lapply(seq_len(opts$kMax), function(k) {
      f <- flexmix::getModel(fits, as.character(k))
      data.frame(segment = flexmix::clusters(f), probability = apply(flexmix::posterior(f), 1, max))
    }), paste0("k", seq_len(opts$kMax))))
  )
}

#' R code: the SEMinR script (construct scores), then the FIMIX step with flexmix.
fimix_code <- function(model, settings, opts, eqs, safe) {
  base <- tryCatch(generate_r_code(model, settings, list(filename = "your_data.csv")), error = function(e) "# (SEMinR model code unavailable)")
  eq_lines <- vapply(eqs, function(e) sprintf("  FLXMRglm(%s ~ %s - 1)", safe[[e$target]], paste(safe[e$predictors], collapse = " + ")), "")
  paste(c(base, "",
          "# ---- FIMIX-PLS (PathLab SEM): finite mixture regression of the structural equations on the construct scores ----",
          "library(flexmix)",
          "pls <- pls_model   # the model estimated by the script above",
          "scores <- as.data.frame(pls$construct_scores)",
          "names(scores) <- make.names(names(scores), unique = TRUE)",
          "equations <- list(", paste(eq_lines, collapse = ",\n"), ")",
          sprintf("set.seed(%d)", opts$seed),
          sprintf("fimix <- stepFlexmix(~ 1, data = scores, k = 1:%d, nrep = %d, model = equations,", opts$kMax, opts$nrep),
          sprintf("                     control = list(iter.max = %d, minprior = %s), verbose = FALSE)", opts$iterMax, format(opts$minPrior)),
          "fimix                                   # log-likelihood, AIC, BIC per number of segments",
          "fit2 <- getModel(fimix, \"2\")            # e.g. the two-segment solution",
          "parameters(fit2, model = 1)             # segment-specific path coefficients of the first equation",
          "table(clusters(fit2))                   # segment sizes"),
        collapse = "\n")
}

# ---- Report, workbook and APA tables ----------------------------------------------------------------

FIMIX_CRIT <- c(AIC = "AIC", AIC3 = "AIC3", AIC4 = "AIC4", BIC = "BIC", CAIC = "CAIC", HQ = "HQ", MDL5 = "MDL5", logLik = "LnL", EN = "EN")

fimix_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>FIMIX-PLS segmentation</h2>")
  a("<p>Finite mixture regression of the structural equations on the SEMinR construct scores (n = ", res$n$used, "), ",
    "1 to ", res$options$kMax, " segments, ", res$options$nrep, " random starts each (seed ", res$options$seed, "), minimum segment size ",
    round(100 * res$options$minPrior), "%. Estimated with flexmix ", esc(res$packages$flexmix), " on SEMinR ", esc(res$packages$seminr), " scores.</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  crit <- names(FIMIX_CRIT)
  rows <- lapply(res$solutions, function(s) c(as.character(s$k), vapply(crit, function(cn) {
    v <- s$criteria[[cn]]
    txt <- if (cn == "EN") (if (is.null(v) || is.na(num_of(v))) "&ndash;" else fmt(v)) else fmt(v, 2)
    if (!is.null(res$best[[cn]]) && identical(as.integer(res$best[[cn]]), as.integer(s$k))) paste0("<strong>", txt, "</strong>") else txt
  }, "")))
  a(html_table(c("Segments", unname(FIMIX_CRIT)), rows, "Segment retention criteria (bold = lowest value of each information criterion)", num_cols = 2:(length(crit) + 1)))
  a("<p class=\"note\">Sarstedt et al. (2011) recommend considering AIC3 and CAIC jointly, and an EN above 0.50 for a clear segmentation. FIMIX-PLS identifies the number of segments; explaining them (for example with observed variables, then MGA) is a separate step.</p>")
  for (s in res$solutions) {
    if (s$k < 2) next
    segs <- s$segments
    hdr <- c("Path", vapply(segs, function(g) sprintf("Segment %d (%.0f%%)", g$segment, 100 * g$prior), ""))
    paths <- segs[[1]]$paths
    prow <- lapply(seq_along(paths), function(i) c(paste0(esc(paths[[i]]$from), " &rarr; ", esc(paths[[i]]$to)), vapply(segs, function(g) fmt(g$paths[[i]]$coefficient), "")))
    rrow <- lapply(seq_along(segs[[1]]$rSquared), function(i) c(paste0("R&sup2; ", esc(segs[[1]]$rSquared[[i]]$construct)), vapply(segs, function(g) fmt(g$rSquared[[i]]$rSquared), "")))
    a(html_table(hdr, c(prow, rrow), sprintf("%d-segment solution: segment-specific path coefficients and R&sup2;", s$k), num_cols = 2:length(hdr)))
  }
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

fimix_sheets <- function(res, project_name) {
  crit <- do.call(rbind, lapply(res$solutions, function(s) {
    d <- data.frame(Segments = s$k, Retained = s$kRetained, Converged = s$converged)
    for (cn in names(FIMIX_CRIT)) d[[FIMIX_CRIT[[cn]]]] <- num_of(s$criteria[[cn]])
    d
  }))
  seg <- do.call(rbind, lapply(res$solutions, function(s) do.call(rbind, lapply(s$segments, function(g) rbind(
    do.call(rbind, lapply(g$paths, function(p) data.frame(Solution = s$k, Segment = g$segment, Prior = g$prior, Size = g$size, Statistic = "Path coefficient",
                                                          From = p$from, To = p$to, Value = p$coefficient, stringsAsFactors = FALSE))),
    do.call(rbind, lapply(g$rSquared, function(r) data.frame(Solution = s$k, Segment = g$segment, Prior = g$prior, Size = g$size, Statistic = "R2",
                                                             From = "", To = r$construct, Value = num_of(r$rSquared), stringsAsFactors = FALSE))))))))
  list(Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "SEMinR", "flexmix", "n", "Random starts", "Seed", "Minimum segment size"),
                         Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$packages$seminr, res$packages$flexmix, res$n$used, res$options$nrep, res$options$seed, res$options$minPrior),
                         stringsAsFactors = FALSE),
       Criteria = crit, Segments = seg)
}

fimix_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  crit <- c("AIC", "AIC3", "BIC", "CAIC", "EN")
  rows <- lapply(res$solutions, function(s) c(as.character(s$k), vapply(crit, function(cn) {
    v <- num_of(s$criteria[[cn]])
    if (is.na(v)) "—" else if (cn == "EN") pt_num(v, d, TRUE, lz) else pt_num(v, 2)
  }, "")))
  bold <- list()
  for (j in seq_along(crit)) if (crit[j] != "EN") bold[[length(bold) + 1]] <- c(res$best[[crit[j]]], j + 1)
  tabs <- list(pt_table("fimix_criteria", "FIMIX-PLS Segment Retention Criteria", c("Segments", crit), rows,
                        notes = list(sprintf("N = %d. Lowest value of each information criterion in bold. EN = normed entropy. %d random starts per solution (seed %s).",
                                             res$n$used, res$options$nrep, res$options$seed)), bold = bold))
  # Segment-specific coefficients for the solution favoured by AIC3 and CAIC (or AIC3 when they differ and it has >= 2 segments).
  pick <- as.integer(res$best$AIC3)
  if (pick < 2) pick <- as.integer(res$best$CAIC)
  if (pick >= 2) {
    s <- res$solutions[[pick]]
    segs <- s$segments
    paths <- segs[[1]]$paths
    rows2 <- c(lapply(seq_along(paths), function(i) c(paste0(paths[[i]]$from, " → ", paths[[i]]$to), vapply(segs, function(g) pt_num(g$paths[[i]]$coefficient, d), ""))),
               lapply(seq_along(segs[[1]]$rSquared), function(i) c(paste0("R² (", segs[[1]]$rSquared[[i]]$construct, ")"), vapply(segs, function(g) pt_num(g$rSquared[[i]]$rSquared, d, TRUE, lz), ""))))
    tabs[[2]] <- pt_table("fimix_segments", sprintf("FIMIX-PLS: Segment-Specific Path Coefficients (%d Segments)", pick),
                          c("Path", vapply(segs, function(g) sprintf("Segment %d (%.1f%%)", g$segment, 100 * g$prior), "")), rows2,
                          notes = list("Path coefficients from the finite mixture regression of the structural equations on the standardised construct scores; R² weighted by the segment membership probabilities."))
  }
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}
