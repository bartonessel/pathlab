# Covariance-based SEM with lavaan (and semTools for reliability and HTMT).
# Two-step practice (Anderson & Gerbing, 1988): the measurement model is assessed in a CFA with all factors
# correlated (fit, standardised loadings, composite reliability, AVE, HTMT, Fornell-Larcker); the structural model
# (paths, R², indirect effects, global fit) is estimated with lavaan::sem(). Every number comes from lavaan/semTools.
# The data are never changed: variables are renamed only in the model copy (lavaan needs syntactic names), the
# project missing-value code is treated as missing, and the missing-data choice (listwise or FIML) is reported.

CB_ESTIMATORS <- c(ML = "Maximum likelihood (ML)", MLR = "Robust maximum likelihood (MLR)", WLSMV = "Diagonally weighted least squares (WLSMV)")

normalize_cb_settings <- function(raw) {
  raw <- raw %||% list()
  pick <- function(v, allowed, default) { v <- as.character(v %||% default)[1]; if (v %in% allowed) v else default }
  est <- pick(raw[["estimator"]], names(CB_ESTIMATORS), "ML")
  b <- raw[["bootstrap"]] %||% list()
  ci <- suppressWarnings(as.numeric(raw[["ciLevel"]] %||% 0.95))
  if (is.na(ci) || ci <= 0.5 || ci >= 1) ci <- 0.95
  boot_on <- isTRUE(b$enabled) && est == "ML"
  list(
    estimator = est,
    missing = if (est == "WLSMV") "listwise" else pick(raw[["missing"]], c("listwise", "fiml"), "listwise"),
    ordinal = isTRUE(raw[["ordinal"]]) && est == "WLSMV",
    bootstrap = list(enabled = boot_on, nboot = int_in(b$nboot, 1000L, 100L, 5000L, "The number of bootstrap samples"),
                     seed = int_in(b$seed, 2026L, 1L, .Machine$integer.max, "The bootstrap seed")),
    ciLevel = ci
  )
}

#' The CB-SEM model: constructs (common factors), paths and covariances (factor and residual).
normalize_cb_model <- function(raw) {
  base <- normalize_model(raw)
  if (length(raw$moderations %||% list())) api_error(422, "cb_moderation", "CB-SEM analyses do not support moderating (interaction) effects yet.",
                                                     "Remove the moderations, or estimate the moderation in a PLS-SEM analysis.")
  covs <- lapply(raw$covariances %||% list(), function(cv) list(
    id = as.character(cv$id %||% ""), kind = if (identical(cv$kind, "residual")) "residual" else "factor",
    a = as.character(cv$a %||% ""), b = as.character(cv$b %||% ""), dataDriven = isTRUE(cv$dataDriven)))
  base$covariances <- covs
  base$moderations <- list()
  base
}

safe_names <- function(x, prefix) {
  s <- gsub("[^A-Za-z0-9_]", "_", x)
  s <- ifelse(grepl("^[A-Za-z]", s), s, paste0(prefix, s))
  make.unique(s, sep = "_")
}

#' lavaan syntax plus the name maps (display name <-> lavaan name).
cb_syntax <- function(model, structural = TRUE) {
  cons <- model$constructs
  byid <- setNames(cons, vapply(cons, `[[`, "", "id"))
  cn <- setNames(safe_names(vapply(cons, `[[`, "", "name"), "F_"), vapply(cons, `[[`, "", "id"))
  inds <- unique(unlist(lapply(cons, `[[`, "indicators")))
  vn <- setNames(safe_names(inds, "x_"), inds)
  # A factor name must not clash with a variable name.
  clash <- cn %in% vn
  if (any(clash)) cn[clash] <- paste0(cn[clash], "_f")
  lines <- character(0); single <- character(0)
  for (c in cons) {
    if (length(c$dimensions %||% character(0))) {   # second-order factor measured by first-order factors
      lines <- c(lines, sprintf("%s =~ %s", cn[[c$id]], paste(cn[c$dimensions], collapse = " + ")))
      next
    }
    if (length(c$indicators) == 1) {
      single <- c(single, c$name)
      lines <- c(lines, sprintf("%s =~ 1*%s", cn[[c$id]], vn[[c$indicators]]), sprintf("%s ~~ 0*%s", vn[[c$indicators]], vn[[c$indicators]]))
    } else lines <- c(lines, sprintf("%s =~ %s", cn[[c$id]], paste(vn[c$indicators], collapse = " + ")))
  }
  labels <- list()
  if (structural) {
    k <- 0
    for (to in unique(vapply(model$paths, `[[`, "", "to"))) {
      ps <- Filter(function(p) p$to == to, model$paths)
      rhs <- vapply(ps, function(p) { k <<- k + 1; lab <- paste0("p", k); labels[[lab]] <<- p; paste0(lab, "*", cn[[p$from]]) }, "")
      lines <- c(lines, sprintf("%s ~ %s", cn[[to]], paste(rhs, collapse = " + ")))
    }
  }
  for (cv in model$covariances %||% list()) {
    if (cv$kind == "factor" && cv$a %in% names(cn) && cv$b %in% names(cn)) lines <- c(lines, sprintf("%s ~~ %s", cn[[cv$a]], cn[[cv$b]]))
    if (cv$kind == "residual" && cv$a %in% names(vn) && cv$b %in% names(vn)) lines <- c(lines, sprintf("%s ~~ %s", vn[[cv$a]], vn[[cv$b]]))
  }
  # Indirect effects as defined parameters (products of path labels along each chain of length >= 2).
  defined <- list()
  if (structural && length(model$paths)) {
    lab_of <- setNames(names(labels), vapply(labels, function(p) paste(p$from, p$to), ""))
    name_to_id <- setNames(vapply(cons, `[[`, "", "id"), vapply(cons, `[[`, "", "name"))
    for (ch in indirect_chains(model)) {
      ids <- name_to_id[ch]
      labs <- vapply(seq_len(length(ids) - 1), function(i) lab_of[[paste(ids[i], ids[i + 1])]], "")
      nm <- paste0("ind", length(defined) + 1)
      lines <- c(lines, sprintf("%s := %s", nm, paste(labs, collapse = "*")))
      defined[[nm]] <- ch
    }
  }
  list(syntax = paste(lines, collapse = "\n"), factorNames = cn, varNames = vn, labels = labels, defined = defined, singleIndicator = single,
       byLavaan = c(setNames(vapply(cons, `[[`, "", "name"), unname(cn)), setNames(names(vn), unname(vn))))
}

validate_cbsem <- function(model, df) {
  out <- list()
  add <- function(sev, code, msg, hint = NULL) out[[length(out) + 1]] <<- list(severity = sev, code = code, message = msg, hint = hint)
  if (!length(model$constructs)) add("error", "cb_empty", "Add at least one latent variable (construct) with indicators.")
  for (c in model$constructs) {
    if (length(c$dimensions %||% character(0))) {
      if (length(c$dimensions) < 2) add("error", "cb_hoc_dimensions", sprintf("Second-order factor '%s' needs at least two first-order factors.", c$name))
      if (length(c$dimensions) == 2) add("warning", "cb_hoc_two", sprintf("Second-order factor '%s' has two first-order factors; it is identified only with constraints or through its relations with other factors.", c$name))
      next
    }
    if (!length(c$indicators)) add("error", "cb_no_indicators", sprintf("'%s' has no indicators.", c$name))
    if (length(c$indicators) == 2) add("warning", "cb_two_indicators", sprintf("'%s' has only two indicators; it is identified only through its correlations with other factors.", c$name))
    for (v in c$indicators) {
      if (!v %in% names(df)) add("error", "cb_missing_column", sprintf("Indicator '%s' is not in the dataset.", v))
      else if (!is.numeric(df[[v]])) add("error", "cb_not_numeric", sprintf("Indicator '%s' is not numeric.", v))
    }
  }
  dup <- unique(unlist(lapply(model$constructs, `[[`, "indicators")))
  if (length(unlist(lapply(model$constructs, `[[`, "indicators"))) != length(dup)) add("error", "cb_shared_indicator", "An indicator is assigned to more than one construct.")
  out
}

cb_rows <- function(df, cols) lapply(seq_len(nrow(df)), function(i) { r <- as.list(df[i, cols, drop = FALSE]); lapply(r, function(v) if (is.factor(v)) as.character(v) else v) })

run_cbsem <- function(df, model, cb, missing_code = NULL, progress = function(...) NULL) {
  issues <- validate_cbsem(model, df)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    stop(analysis_failure("validation", issues[[which(vapply(issues, function(i) i$severity == "error", TRUE))[1]]]$message,
                          "Fix the listed model problems and run again."))
  t0 <- Sys.time()
  warn <- vapply(Filter(function(i) i$severity == "warning", issues), `[[`, "", "message")
  sx <- cb_syntax(model, structural = TRUE)
  mx <- cb_syntax(model, structural = FALSE)
  inds <- names(sx$varNames)
  d <- df[, inds, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in inds) { hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code; recoded <- recoded + sum(hit); d[[cn]][hit] <- NA }
  names(d) <- unname(sx$varNames[inds])
  n_input <- nrow(d)
  rows_cb <- if (cb$missing == "listwise") which(stats::complete.cases(d)) else seq_len(nrow(d))
  if (cb$missing == "listwise") d <- d[stats::complete.cases(d), , drop = FALSE]
  n_used <- nrow(d)
  if (n_used < 50) stop(analysis_failure("data", sprintf("Only %d usable rows.", n_used), "CB-SEM needs a reasonably large sample (usually 200 or more)."))
  if (n_used < 200) warn <- c(warn, sprintf("n = %d. CB-SEM estimates and fit indices are more reliable with 200 or more cases.", n_used))
  args <- list(data = d, estimator = cb$estimator, missing = if (cb$missing == "fiml") "fiml" else "listwise")
  if (cb$ordinal) args$ordered <- TRUE
  # WLSMV on continuous indicators: lavaan (0.6-17+) refuses unless told explicitly that nothing is ordered.
  else if (cb$estimator == "WLSMV") args$ordered <- FALSE
  lav_warn <- character(0)
  fit_with <- function(f, syntax, extra = list()) withCallingHandlers(
    tryCatch(do.call(f, c(list(model = syntax), args, extra)),
             error = function(e) stop(analysis_failure("estimation", paste("lavaan could not estimate the model:", sanitize_message(conditionMessage(e))),
                                                     "Check that every factor is identified (three or more indicators, or two with correlated factors) and that indicators vary."))),
    warning = function(w) { lav_warn <<- c(lav_warn, sanitize_message(conditionMessage(w))); invokeRestart("muffleWarning") })

  progress("cfa", "Estimating the measurement model (CFA)", 0.15)
  cfa <- fit_with(lavaan::cfa, mx$syntax)
  progress("sem", if (cb$bootstrap$enabled) sprintf("Estimating the structural model with %d bootstrap samples", cb$bootstrap$nboot) else "Estimating the structural model", 0.4)
  if (cb$bootstrap$enabled) set.seed(cb$bootstrap$seed)
  sem <- fit_with(lavaan::sem, sx$syntax, if (cb$bootstrap$enabled) list(se = "bootstrap", bootstrap = cb$bootstrap$nboot) else list())
  if (!isTRUE(lavaan::lavInspect(sem, "converged"))) stop(analysis_failure("estimation", "lavaan did not converge.", "Simplify the model or check the data."))
  if (!isTRUE(suppressWarnings(lavaan::lavInspect(sem, "post.check")))) warn <- c(warn, "The solution is not admissible (for example a negative residual variance or a correlation above 1). Interpret with caution.")
  warn <- c(warn, unique(lav_warn))
  progress("results", "Collecting the results", 0.85)

  nm <- function(x) unname(ifelse(x %in% names(sx$byLavaan), sx$byLavaan[x], x))
  robust <- cb$estimator != "ML"
  fit_of <- function(f) {
    keys <- c("chisq", "df", "pvalue", "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr",
              "cfi.robust", "tli.robust", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "aic", "bic", "ntotal")
    fm <- suppressWarnings(lavaan::fitMeasures(f, intersect(keys, lavaan::fitMeasures(f, "all") |> names())))
    g <- function(k) if (k %in% names(fm) && is.finite(fm[[k]])) unname(fm[[k]]) else NULL
    if (robust) list(chisq = g("chisq.scaled"), df = g("df.scaled") %||% g("df"), p = g("pvalue.scaled"), cfi = g("cfi.robust") %||% g("cfi"),
                     tli = g("tli.robust") %||% g("tli"), rmsea = g("rmsea.robust") %||% g("rmsea"), rmseaLower = g("rmsea.ci.lower.robust") %||% g("rmsea.ci.lower"),
                     rmseaUpper = g("rmsea.ci.upper.robust") %||% g("rmsea.ci.upper"), srmr = g("srmr"), aic = g("aic"), bic = g("bic"), scaled = TRUE,
                     chisqStandard = g("chisq"))
    else list(chisq = g("chisq"), df = g("df"), p = g("pvalue"), cfi = g("cfi"), tli = g("tli"), rmsea = g("rmsea"), rmseaLower = g("rmsea.ci.lower"),
              rmseaUpper = g("rmsea.ci.upper"), srmr = g("srmr"), aic = g("aic"), bic = g("bic"), scaled = FALSE)
  }
  std <- function(f, ops, boot = FALSE) {
    s <- lavaan::standardizedSolution(f, level = cb$ciLevel)
    s <- s[s$op %in% ops, , drop = FALSE]
    pe <- lavaan::parameterEstimates(f, level = cb$ciLevel, boot.ci.type = if (boot) "perc" else "norm")
    key <- function(x) paste(x$lhs, x$op, x$rhs)
    est <- setNames(pe$est, key(pe)); se <- setNames(pe$se, key(pe)); lo <- setNames(pe$ci.lower, key(pe)); hi <- setNames(pe$ci.upper, key(pe))
    p <- setNames(pe$pvalue, key(pe))
    lapply(seq_len(nrow(s)), function(i) {
      k <- key(s[i, ])
      list(lhs = nm(s$lhs[i]), op = s$op[i], rhs = nm(s$rhs[i]), std = s$est.std[i], est = unname(est[k]), se = unname(se[k]),
           z = if (is.finite(unname(est[k] / se[k]))) unname(est[k] / se[k]) else NULL, p = unname(p[k]), ciLower = unname(lo[k]), ciUpper = unname(hi[k]),
           stdCiLower = s$ci.lower[i], stdCiUpper = s$ci.upper[i])
    })
  }

  # Reliability and validity from the CFA.
  # compRelSEM refits an unrestricted model internally; its warnings duplicate those of the CFA collected above.
  rel <- tryCatch(suppressWarnings(suppressMessages(semTools::compRelSEM(cfa))), error = function(e) NULL)
  ave <- tryCatch(suppressWarnings(suppressMessages(semTools::AVE(cfa))), error = function(e) NULL)
  relv <- if (is.null(rel)) NULL else vapply(rel, function(v) as.numeric(v)[1], 0)
  facs <- unname(mx$factorNames)
  reliability <- lapply(facs, function(f) list(construct = nm(f), omega = if (!is.null(relv) && f %in% names(relv)) unname(relv[[f]]) else NULL,
                                               ave = if (!is.null(ave) && f %in% names(ave)) unname(ave[[f]]) else NULL))
  multi <- facs[vapply(model$constructs, function(c) length(c$indicators) >= 2, TRUE)]
  htmt <- tryCatch({
    if (length(multi) >= 2) {
      sub_syn <- paste(grep(paste0("^(", paste(multi, collapse = "|"), ") =~"), strsplit(mx$syntax, "\n")[[1]], value = TRUE), collapse = "\n")
      h <- suppressWarnings(suppressMessages(semTools::htmt(sub_syn, data = d, missing = if (cb$missing == "fiml") "ml" else "listwise", ordered = if (cb$ordinal) TRUE else NULL)))
      dimnames(h) <- list(nm(rownames(h)), nm(colnames(h)))
      lapply(rownames(h), function(r) c(list(construct = r), as.list(setNames(as.numeric(h[r, ]), colnames(h)))))
    } else NULL
  }, error = function(e) NULL)
  cor_lv <- lavaan::lavInspect(cfa, "cor.lv")
  dimnames(cor_lv) <- list(nm(rownames(cor_lv)), nm(colnames(cor_lv)))
  sqrt_ave <- setNames(vapply(reliability, function(r) if (is.null(r$ave)) NA_real_ else sqrt(r$ave), 0), vapply(reliability, `[[`, "", "construct"))
  fornell <- lapply(rownames(cor_lv), function(r) c(list(construct = r), as.list(setNames(vapply(colnames(cor_lv), function(cc) if (cc == r) sqrt_ave[[r]] else cor_lv[r, cc], 0), colnames(cor_lv)))))

  r2 <- lavaan::lavInspect(sem, "r2")
  latent_r2 <- r2[names(r2) %in% unname(sx$factorNames)]
  mi <- tryCatch({
    m <- suppressWarnings(lavaan::modindices(sem, sort. = TRUE, minimum.value = 3.84))
    m <- utils::head(m[m$op %in% c("=~", "~~", "~"), , drop = FALSE], 20)
    lapply(seq_len(nrow(m)), function(i) list(lhs = nm(m$lhs[i]), op = m$op[i], rhs = nm(m$rhs[i]), mi = m$mi[i], epc = m$epc[i], sepcAll = m$sepc.all[i]))
  }, error = function(e) list())
  ind <- if (length(sx$defined)) {
    pe <- lavaan::parameterEstimates(sem, level = cb$ciLevel, boot.ci.type = if (cb$bootstrap$enabled) "perc" else "norm", standardized = TRUE)
    pe <- pe[pe$op == ":=", , drop = FALSE]
    lapply(seq_len(nrow(pe)), function(i) list(chain = as.list(sx$defined[[pe$lhs[i]]]), est = pe$est[i], std = pe$std.all[i], se = pe$se[i],
                                                p = pe$pvalue[i], ciLower = pe$ci.lower[i], ciUpper = pe$ci.upper[i]))
  } else list()
  covs <- Filter(function(x) x$lhs != x$rhs, std(sem, "~~", boot = cb$bootstrap$enabled))
  # Loadings of the estimated model (unstandardised and standardised) for the diagram; the first indicator of
  # every factor is the marker (loading fixed to 1, lavaan's default scaling).
  sem_loadings <- lapply(std(sem, "=~", boot = cb$bootstrap$enabled), function(l) c(l, list(marker = is.null(l$z))))
  residual_vars <- Filter(function(x) x$lhs == x$rhs && x$lhs %in% names(sx$varNames), std(sem, "~~", boot = cb$bootstrap$enabled))
  progress("final", "Collecting matrices, residuals and factor scores", 0.92)
  final <- tryCatch(cb_final_results(sem, d, nm, cb$ciLevel), error = function(e) NULL)
  fit_table <- tryCatch(cb_fit_table(sem, args, robust), error = function(e) NULL)

  list(
    status = "completed",
    kind = "cbsem",
    settings = cb,
    estimatorLabel = unname(CB_ESTIMATORS[cb$estimator]),
    n = list(input = n_input, used = n_used, rowsRemoved = n_input - n_used, cellsRecoded = recoded,
             missing = if (cb$missing == "fiml") "Full information maximum likelihood (FIML)" else "Listwise deletion"),
    syntax = sx$syntax,
    singleIndicator = as.list(sx$singleIndicator),
    dataDrivenResiduals = lapply(Filter(function(cv) cv$kind == "residual" && isTRUE(cv$dataDriven), model$covariances %||% list()), function(cv) list(cv$a, cv$b)),
    fit = fit_of(sem),
    cfaFit = fit_of(cfa),
    loadings = std(cfa, "=~"),
    reliability = reliability,
    htmt = htmt,
    fornellLarcker = fornell,
    paths = std(sem, "~", boot = cb$bootstrap$enabled),
    rSquared = lapply(names(latent_r2), function(k) list(construct = nm(k), rSquared = unname(latent_r2[[k]]))),
    covariances = covs,
    semLoadings = sem_loadings,
    residualVariances = residual_vars,
    fitTable = fit_table,
    final = final,
    indirect = ind,
    modIndices = mi,
    warnings = as.list(unique(warn)),
    rCode = cb_code(sx, mx, cb, inds, missing_code),
    environment = list(rVersion = R.version.string, lavaan = as.character(utils::packageVersion("lavaan")),
                       semTools = as.character(utils::packageVersion("semTools"))),
    completedAt = now_iso(),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = tryCatch({
      idx <- lavaan::lavInspect(sem, "case.idx")
      tabs <- cb_scores(sem, d, nm)
      if (!length(tabs)) NULL else list(nInput = n_input, rows = rows_cb[idx], tables = tabs)
    }, error = function(e) NULL)
  )
}

#' Goodness-of-fit index family for ML-type fits (Jöreskog & Sörbom): GFI = 1 - tr[(Σ⁻¹S - I)²] / tr[(Σ⁻¹S)²],
#' AGFI = 1 - p*/df (1 - GFI), PGFI = df/p* GFI, with p* = p(p + 1)/2. Computed here because lavaan 0.7.3 returns
#' GFI = 1 for some models while its AGFI and PGFI follow the formula (checked in the tests).
cb_gfi <- function(f) {
  tryCatch({
    S <- lavaan::lavInspect(f, "sampstat")$cov
    Sig <- lavaan::lavInspect(f, "implied")$cov
    if (is.null(S) || is.null(Sig)) return(NULL)
    A <- solve(Sig) %*% S; p <- nrow(S); I <- diag(p)
    gfi <- 1 - sum(diag((A - I) %*% (A - I))) / sum(diag(A %*% A))
    df <- as.numeric(lavaan::fitMeasures(f, "df")); pstar <- p * (p + 1) / 2
    list(gfi = gfi, agfi = if (df > 0) 1 - pstar / df * (1 - gfi) else NULL, pgfi = df / pstar * gfi)
  }, error = function(e) NULL)
}

#' Model fit of the estimated model next to the null (independence) model, as in SmartPLS's CB-SEM report.
#' The null model has only the indicator variances; it is fitted with the same data and options (its chi-square
#' equals lavaan's baseline chi-square).
cb_fit_table <- function(f, args, robust) {
  fm <- suppressWarnings(lavaan::fitMeasures(f))
  g <- function(k) if (k %in% names(fm) && is.finite(fm[[k]])) unname(fm[[k]]) else NULL
  ch <- if (robust) g("chisq.scaled") %||% g("chisq") else g("chisq")
  df <- if (robust) g("df.scaled") %||% g("df") else g("df")
  gf <- cb_gfi(f)
  est <- list(chisq = ch, npar = g("npar"), nobs = g("ntotal"), df = df, p = if (robust) g("pvalue.scaled") %||% g("pvalue") else g("pvalue"),
              chisqDf = if (!is.null(ch) && !is.null(df) && df > 0) ch / df else NULL,
              rmsea = if (robust) g("rmsea.robust") %||% g("rmsea") else g("rmsea"),
              rmseaLower = if (robust) g("rmsea.ci.lower.robust") %||% g("rmsea.ci.lower") else g("rmsea.ci.lower"),
              rmseaUpper = if (robust) g("rmsea.ci.upper.robust") %||% g("rmsea.ci.upper") else g("rmsea.ci.upper"),
              gfi = gf$gfi, agfi = gf$agfi, pgfi = gf$pgfi, srmr = g("srmr"), nfi = g("nfi"),
              tli = if (robust) g("tli.robust") %||% g("tli") else g("tli"), cfi = if (robust) g("cfi.robust") %||% g("cfi") else g("cfi"),
              aic = g("aic"), bic = g("bic"))
  null <- tryCatch({
    ov <- lavaan::lavNames(f, "ov")
    nf <- suppressWarnings(do.call(lavaan::lavaan, c(list(model = paste(ov, "~~", ov, collapse = "\n")), args)))
    nm <- suppressWarnings(lavaan::fitMeasures(nf))
    h <- function(k) if (k %in% names(nm) && is.finite(nm[[k]])) unname(nm[[k]]) else NULL
    nch <- if (robust) h("chisq.scaled") %||% h("chisq") else h("chisq")
    ndf <- h("df")
    list(chisq = nch, npar = h("npar"), df = ndf, p = if (robust) h("pvalue.scaled") %||% h("pvalue") else h("pvalue"),
         chisqDf = if (!is.null(nch) && !is.null(ndf) && ndf > 0) nch / ndf else NULL,
         rmsea = if (robust) h("rmsea.robust") %||% h("rmsea") else h("rmsea"),
         rmseaLower = if (robust) h("rmsea.ci.lower.robust") %||% h("rmsea.ci.lower") else h("rmsea.ci.lower"),
         rmseaUpper = if (robust) h("rmsea.ci.upper.robust") %||% h("rmsea.ci.upper") else h("rmsea.ci.upper"))
  }, error = function(e) NULL)
  list(estimated = est, null = null, scaled = robust)
}

#' The "final results" of the estimated (structural) model: loadings, factor and error-term parameters and
#' matrices, indicator covariance/correlation matrices (empirical and model-implied), residual covariances and
#' Mardia's multivariate normality test. `nm` maps lavaan names back to display names.
cb_final_results <- function(f, d, nm, ci) {
  rename <- function(m) { m <- unclass(m); dimnames(m) <- list(nm(rownames(m)), nm(colnames(m))); m }
  mt <- function(m, key = "name") { if (is.null(m)) return(NULL); m <- rename(m); m[!is.finite(m)] <- NA; matrix_to_table(m, key, "") }
  sym <- function(m) { m <- as.matrix(m); m[upper.tri(m)] <- t(m)[upper.tri(m)]; m }
  est <- lavaan::lavInspect(f, "est"); stdm <- lavaan::lavInspect(f, "std")
  # Higher-order models: lambda rows are indicators, columns factors; transposed to indicator x factor as in SmartPLS.
  lam <- est$lambda; lam_s <- stdm$lambda
  pe <- lavaan::parameterEstimates(f, level = ci, standardized = TRUE)
  ov <- lavaan::lavNames(f, "ov"); lv <- lavaan::lavNames(f, "lv")
  par_rows <- function(sel) lapply(which(sel), function(i) list(
    lhs = nm(pe$lhs[i]), op = pe$op[i], rhs = nm(pe$rhs[i]), est = pe$est[i], std = pe$std.all[i], se = num_or_null(pe$se[i]),
    z = num_or_null(pe$z[i]), p = num_or_null(pe$pvalue[i]), ciLower = num_or_null(pe$ci.lower[i]), ciUpper = num_or_null(pe$ci.upper[i]),
    fixed = !is.finite(pe$se[i]) || pe$se[i] == 0))
  S <- tryCatch(lavaan::lavInspect(f, "sampstat")$cov, error = function(e) NULL)
  Sig <- tryCatch(lavaan::lavInspect(f, "implied")$cov, error = function(e) NULL)
  res_raw <- tryCatch(lavaan::resid(f, type = "raw")$cov, error = function(e) NULL)
  res_std <- tryCatch(lavaan::resid(f, type = "standardized")$cov, error = function(e) NULL)
  theta <- sym(est$theta); theta_s <- sym(stdm$theta)
  psi <- tryCatch(lavaan::lavInspect(f, "cov.lv"), error = function(e) NULL)
  cc <- d[stats::complete.cases(d), ov, drop = FALSE]
  mardia <- tryCatch({
    sk <- semTools::mardiaSkew(cc); ku <- semTools::mardiaKurtosis(cc)
    list(n = nrow(cc), skewness = unname(sk["b1d"]), skewChi = unname(sk["chi"]), skewDf = unname(sk["df"]), skewP = unname(sk["p"]),
         kurtosis = unname(ku["b2d"]), kurtosisZ = unname(ku["z"]), kurtosisP = unname(ku["p"]), expectedKurtosis = length(ov) * (length(ov) + 2))
  }, error = function(e) NULL)
  list(
    loadingMatrix = mt(lam), loadingMatrixStd = mt(lam_s),
    factorParams = par_rows(pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% lv),
    factorCov = mt(psi), factorCor = mt(if (is.null(psi)) NULL else stats::cov2cor(psi)),
    errorParams = par_rows(pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% ov),
    errorCov = mt(theta), errorCor = mt(theta_s),
    empiricalCov = mt(S), empiricalCor = mt(if (is.null(S)) NULL else stats::cov2cor(S)),
    impliedCov = mt(Sig), impliedCor = mt(if (is.null(Sig)) NULL else stats::cov2cor(Sig)),
    residualCov = mt(res_raw), residualCovStd = mt(res_std),
    mardia = mardia
  )
}

#' Factor scores of the estimated model: Thurstone (regression), Bartlett and ten Berge (psych; first-order models).
cb_scores <- function(f, d, nm) {
  out <- list()
  reg <- tryCatch(suppressWarnings(lavaan::lavPredict(f, method = "regression")), error = function(e) NULL)
  if (!is.null(reg)) { s <- as.data.frame(reg); names(s) <- nm(colnames(reg)); out$scores <- s }
  bar <- tryCatch(suppressWarnings(lavaan::lavPredict(f, method = "Bartlett")), error = function(e) NULL)
  if (!is.null(bar)) { s <- as.data.frame(bar); names(s) <- nm(colnames(bar)); out$scoresBartlett <- s }
  tb <- tryCatch({
    ov <- lavaan::lavNames(f, "ov"); idx <- lavaan::lavInspect(f, "case.idx")
    X <- scale(as.matrix(d[idx, ov, drop = FALSE]))
    L <- lavaan::lavInspect(f, "std")$lambda
    if (any(is.na(X))) NULL else {
      s <- psych::factor.scores(X, L, Phi = lavaan::lavInspect(f, "cor.lv"), method = "tenBerge")$scores
      s <- as.data.frame(s); names(s) <- nm(colnames(L)); s
    }
  }, error = function(e) NULL)
  if (!is.null(tb)) out$scoresTenBerge <- tb
  out
}

cb_code <- function(sx, mx, cb, inds, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  args <- paste0("estimator = \"", cb$estimator, "\", missing = \"", if (cb$missing == "fiml") "fiml" else "listwise", "\"",
                 if (cb$ordinal) ", ordered = TRUE" else if (cb$estimator == "WLSMV") ", ordered = FALSE" else "")
  c_lines <- c("# CB-SEM generated by PathLab SEM (lavaan + semTools)",
               "library(lavaan)", "library(semTools)",
               "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
               "# lavaan needs syntactic names: the model uses these names for your variables",
               paste0("d <- data.frame(\n", paste(sprintf("  %s = data[[%s]]", unname(sx$varNames), vapply(inds, q, "")), collapse = ",\n"), "\n)"))
  if (!is.null(missing_code)) c_lines <- c(c_lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  if (cb$missing == "listwise") c_lines <- c(c_lines, "d <- d[complete.cases(d), ]")
  c_lines <- c(c_lines,
               "# Step 1: measurement model (CFA, all factors correlated)",
               paste0("cfa_model <- '\n", mx$syntax, "\n'"),
               paste0("cfa_fit <- cfa(cfa_model, data = d, ", args, ")"),
               "summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)",
               "compRelSEM(cfa_fit)   # composite reliability (omega)",
               "AVE(cfa_fit)",
               "# Step 2: structural model",
               paste0("sem_model <- '\n", sx$syntax, "\n'"),
               if (cb$bootstrap$enabled) paste0("set.seed(", cb$bootstrap$seed, ")") else NULL,
               paste0("sem_fit <- sem(sem_model, data = d, ", args, if (cb$bootstrap$enabled) paste0(", se = \"bootstrap\", bootstrap = ", cb$bootstrap$nboot) else "", ")"),
               "summary(sem_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)",
               paste0("parameterEstimates(sem_fit, level = ", cb$ciLevel, if (cb$bootstrap$enabled) ", boot.ci.type = \"perc\"" else "", ")"),
               "modindices(sem_fit, sort. = TRUE, minimum.value = 3.84)")
  paste(c_lines, collapse = "\n")
}

# ---- Report, workbook and APA tables -------------------------------------------------------------

cb_fit_rows <- function(f) list(
  c("&chi;&sup2;", paste0(fmt(f$chisq, 2), if (isTRUE(f$scaled)) " (scaled)" else "", ", df = ", fmt(f$df, 0), ", p ", fmt_p(f$p))),
  c("CFI", flag(fmt(f$cfi), lt(f$cfi, 0.90))), c("TLI", flag(fmt(f$tli), lt(f$tli, 0.90))),
  c("RMSEA [90% CI]", paste0(flag(fmt(f$rmsea), gt(f$rmsea, 0.08)), " [", fmt(f$rmseaLower), ", ", fmt(f$rmseaUpper), "]")),
  c("SRMR", flag(fmt(f$srmr), gt(f$srmr, 0.08))), c("AIC / BIC", paste0(fmt(f$aic, 1), " / ", fmt(f$bic, 1))))

cb_body_html <- function(res) {
  esc <- html_escape
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>CB-SEM (lavaan)</h2>")
  a("<p>", esc(res$estimatorLabel), "; ", esc(res$n$missing), "; n = ", res$n$used, if (res$n$rowsRemoved > 0) sprintf(" (%d of %d rows removed)", res$n$rowsRemoved, res$n$input) else "",
    ". lavaan ", esc(res$environment$lavaan), ", semTools ", esc(res$environment$semTools), ". The measurement model was assessed in a CFA with all factors correlated; ",
    "the structural model was estimated separately (two-step approach). Red values miss common guidelines (CFI/TLI &lt; 0.90, RMSEA or SRMR &gt; 0.08, loadings &lt; 0.70, &omega; &lt; 0.70, AVE &lt; 0.50, HTMT &gt; 0.85).</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  if (!is.null(res$fitTable)) {
    ft <- res$fitTable; e <- ft$estimated; n <- ft$null %||% list()
    rows <- list(c("Chi-square", fmt(e$chisq), fmt(n$chisq)), c("Number of model parameters", fmt(e$npar, 0), fmt(n$npar, 0)),
                 c("Number of observations", fmt(e$nobs, 0), "n/a"), c("Degrees of freedom", fmt(e$df, 0), fmt(n$df, 0)),
                 c("P value", fmt_p(e$p), fmt_p(n$p)), c("ChiSqr/df", fmt(e$chisqDf), fmt(n$chisqDf)),
                 c("RMSEA", flag(fmt(e$rmsea), gt(e$rmsea, 0.08)), fmt(n$rmsea)), c("RMSEA LOW 90% CI", fmt(e$rmseaLower), fmt(n$rmseaLower)),
                 c("RMSEA HIGH 90% CI", fmt(e$rmseaUpper), fmt(n$rmseaUpper)), c("GFI", flag(fmt(e$gfi), lt(e$gfi, 0.90)), "n/a"),
                 c("AGFI", flag(fmt(e$agfi), lt(e$agfi, 0.80)), "n/a"), c("PGFI", fmt(e$pgfi), "n/a"), c("SRMR", flag(fmt(e$srmr), gt(e$srmr, 0.08)), "n/a"),
                 c("NFI", flag(fmt(e$nfi), lt(e$nfi, 0.90)), "n/a"), c("TLI", flag(fmt(e$tli), lt(e$tli, 0.90)), "n/a"), c("CFI", flag(fmt(e$cfi), lt(e$cfi, 0.90)), "n/a"),
                 c("AIC", fmt(e$aic), "n/a"), c("BIC", fmt(e$bic), "n/a"))
    a(html_table(c("", "Estimated model", "Null model"), rows,
                 paste0("Model fit of the structural model", if (isTRUE(ft$scaled)) " (scaled chi-square and robust indices)" else ""), num_cols = 2:3))
  } else a(html_table(c("Index", "Structural model"), cb_fit_rows(res$fit), "Global fit of the structural model", num_cols = 2))
  a(html_table(c("Index", "Measurement model (CFA)"), cb_fit_rows(res$cfaFit), "Global fit of the measurement model", num_cols = 2))
  rel <- setNames(res$reliability, vapply(res$reliability, `[[`, "", "construct"))
  a(html_table(c("Factor", "Indicator", "Std. loading", "p", "&omega;", "AVE"), lapply(seq_along(res$loadings), function(i) {
    l <- res$loadings[[i]]; first <- i == 1 || res$loadings[[i - 1]]$lhs != l$lhs; r <- rel[[l$lhs]]
    c(if (first) esc(l$lhs) else "", esc(l$rhs), flag(fmt(l$std), lt(abs(num_of(l$std)), 0.70)), fmt_p(l$p),
      if (first) flag(fmt(r$omega), lt(r$omega, 0.70)) else "", if (first) flag(fmt(r$ave), lt(r$ave, 0.50)) else "")
  }), "Measurement model (CFA)", num_cols = 3:6))
  if (!is.null(res$htmt)) {
    nms <- vapply(res$htmt, `[[`, "", "construct")
    a(html_table(c("", esc(nms)), lapply(res$htmt, function(r) c(esc(r$construct), vapply(nms, function(n) if (n == r$construct) "&ndash;" else flag(fmt(r[[n]]), gt(r[[n]], 0.85)), ""))),
                 "Discriminant validity: HTMT", num_cols = seq_along(nms) + 1))
  }
  a(html_table(c("Path", "&beta; (std.)", "Estimate", "SE", "z", "p", "CI"), lapply(res$paths, function(p) c(paste0(esc(p$rhs), " &rarr; ", esc(p$lhs)),
    fmt(p$std), fmt(p$est), fmt(p$se), fmt(p$z, 2), flag(fmt_p(p$p), gt(p$p, 0.05 - 1e-12)), paste0("[", fmt(p$ciLower), ", ", fmt(p$ciUpper), "]"))), "Structural paths", num_cols = 2:7))
  if (length(res$rSquared)) a(html_table(c("Endogenous factor", "R&sup2;"), lapply(res$rSquared, function(x) c(esc(x$construct), fmt(x$rSquared))), "Explained variance", num_cols = 2))
  if (length(res$indirect)) a(html_table(c("Indirect effect", "Estimate", "Std.", "p", "CI"), lapply(res$indirect, function(x) c(esc(paste(ul(x$chain), collapse = " -> ")),
    fmt(x$est), fmt(x$std), fmt_p(x$p), paste0("[", fmt(x$ciLower), ", ", fmt(x$ciUpper), "]"))),
    if (isTRUE(res$settings$bootstrap$enabled)) sprintf("Indirect effects (bootstrap percentile CI, %d samples)", res$settings$bootstrap$nboot) else "Indirect effects (delta method)", num_cols = 2:5))
  if (length(res$residualVariances)) a(html_table(c("Indicator", "Residual variance", "Standardised", "SE", "p"), lapply(res$residualVariances, function(x)
    c(esc(x$lhs), fmt(x$est), fmt(x$std), fmt(x$se), fmt_p(x$p))), "Indicator error terms (residual variances, structural model)", num_cols = 2:5))
  mar <- res$final$mardia
  if (!is.null(mar)) a(html_table(c("Test", "Statistic", "Test statistic", "p"), list(
    c("Mardia's multivariate skewness", fmt(mar$skewness), paste0("&chi;&sup2;(", fmt(mar$skewDf, 0), ") = ", fmt(mar$skewChi, 2)), fmt_p(mar$skewP)),
    c("Mardia's multivariate kurtosis", paste0(fmt(mar$kurtosis), " (expected ", fmt(mar$expectedKurtosis, 0), ")"), paste0("z = ", fmt(mar$kurtosisZ, 2)), fmt_p(mar$kurtosisP))),
    sprintf("Multivariate normality of the indicators (semTools, n = %d)", mar$n), num_cols = 2:4))
  dd <- res$dataDrivenResiduals %||% list()
  if (length(dd)) a("<div class=\"warn\">Residual covariances added from modification indices (data-driven): ", esc(paste(vapply(dd, function(x) paste(ul(x), collapse = " ~~ "), ""), collapse = ", ")), ". Report and justify them.</div>")
  if (length(res$covariances)) a(html_table(c("Covariance", "Correlation", "p"), lapply(res$covariances, function(x) c(paste0(esc(x$lhs), " &harr; ", esc(x$rhs)), fmt(x$std), fmt_p(x$p))), "Covariances", num_cols = 2:3))
  a("<h3>lavaan code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

cb_sheets <- function(res, project_name) {
  pdf <- function(rows) do.call(rbind, lapply(rows, function(p) data.frame(lhs = p$lhs, op = p$op, rhs = p$rhs, Std = num_of(p$std), Estimate = num_of(p$est), SE = num_of(p$se),
                                                                           z = num_of(p$z), p = num_of(p$p), CILower = num_of(p$ciLower), CIUpper = num_of(p$ciUpper), stringsAsFactors = FALSE)))
  fitdf <- function(f, label) data.frame(Model = label, ChiSquare = num_of(f$chisq), df = num_of(f$df), p = num_of(f$p), CFI = num_of(f$cfi), TLI = num_of(f$tli),
                                          RMSEA = num_of(f$rmsea), RMSEALower = num_of(f$rmseaLower), RMSEAUpper = num_of(f$rmseaUpper), SRMR = num_of(f$srmr), AIC = num_of(f$aic), BIC = num_of(f$bic), Scaled = isTRUE(f$scaled))
  out <- list(
    Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "lavaan", "semTools", "Estimator", "Missing data", "Rows used", "Rows in dataset"),
                      Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$environment$lavaan, res$environment$semTools, res$estimatorLabel, res$n$missing, res$n$used, res$n$input), stringsAsFactors = FALSE),
    Fit = rbind(fitdf(res$fit, "Structural model"), fitdf(res$cfaFit, "Measurement model (CFA)")),
    Loadings = pdf(res$loadings),
    Reliability = data.frame(Construct = vapply(res$reliability, `[[`, "", "construct"), Omega = vapply(res$reliability, function(r) num_of(r$omega), 0), AVE = vapply(res$reliability, function(r) num_of(r$ave), 0)),
    Paths = pdf(res$paths),
    RSquared = data.frame(Construct = vapply(res$rSquared, `[[`, "", "construct"), R2 = vapply(res$rSquared, function(x) num_of(x$rSquared), 0)),
    Syntax = data.frame(lavaan = strsplit(res$syntax, "\n")[[1]])
  )
  if (length(res$covariances)) out$Covariances <- pdf(res$covariances)
  if (length(res$residualVariances)) out$ErrorTerms <- pdf(res$residualVariances)
  if (!is.null(res$fitTable)) {
    keys <- c(chisq = "Chi-square", npar = "Number of model parameters", nobs = "Number of observations", df = "Degrees of freedom", p = "P value",
              chisqDf = "ChiSqr/df", rmsea = "RMSEA", rmseaLower = "RMSEA LOW 90% CI", rmseaUpper = "RMSEA HIGH 90% CI", gfi = "GFI", agfi = "AGFI",
              pgfi = "PGFI", srmr = "SRMR", nfi = "NFI", tli = "TLI", cfi = "CFI", aic = "AIC", bic = "BIC")
    out$ModelFitTable <- data.frame(Measure = unname(keys), Estimated = vapply(names(keys), function(k) num_of(res$fitTable$estimated[[k]]), 0),
                                    Null = vapply(names(keys), function(k) num_of(res$fitTable$null[[k]]), 0), row.names = NULL)
  }
  # Matrices of the final results, one sheet each (sheet name = matrix).
  mats <- c(LoadingsUnstd = "loadingMatrix", LoadingsStd = "loadingMatrixStd", FactorCov = "factorCov", FactorCor = "factorCor",
            ErrorCov = "errorCov", ErrorCor = "errorCor", EmpiricalCov = "empiricalCov", EmpiricalCor = "empiricalCor",
            ImpliedCov = "impliedCov", ImpliedCor = "impliedCor", ResidualCov = "residualCov", ResidualCovStd = "residualCovStd")
  for (sheet in names(mats)) if (!is.null(res$final[[mats[[sheet]]]])) out[[sheet]] <- table_to_df(res$final[[mats[[sheet]]]])
  if (length(res$indirect)) out$Indirect <- do.call(rbind, lapply(res$indirect, function(x) data.frame(Chain = paste(ul(x$chain), collapse = " -> "), Estimate = num_of(x$est), Std = num_of(x$std),
                                                                                                      SE = num_of(x$se), p = num_of(x$p), CILower = num_of(x$ciLower), CIUpper = num_of(x$ciUpper), stringsAsFactors = FALSE)))
  if (!is.null(res$htmt)) { nms <- vapply(res$htmt, `[[`, "", "construct"); h <- data.frame(Construct = nms); for (n in nms) h[[n]] <- vapply(res$htmt, function(r) num_of(r[[n]]), 0); out$HTMT <- h }
  if (length(res$modIndices)) out$ModificationIndices <- do.call(rbind, lapply(res$modIndices, function(m) data.frame(lhs = m$lhs, op = m$op, rhs = m$rhs, MI = m$mi, EPC = m$epc, SEPCall = num_of(m$sepcAll))))
  out
}

cb_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero; stars <- identical(opts$pStyle, "stars")
  f <- res$fit; m <- res$cfaFit
  fitrow <- function(x, label) c(label, pt_num(x$chisq, 2), as.character(round(num_of(x$df))), pt_p(x$p, lz), pt_num(x$cfi, d, TRUE, lz), pt_num(x$tli, d, TRUE, lz),
                                 sprintf("%s [%s, %s]", pt_num(x$rmsea, d, TRUE, lz), pt_num(x$rmseaLower, d, TRUE, lz), pt_num(x$rmseaUpper, d, TRUE, lz)), pt_num(x$srmr, d, TRUE, lz))
  dd <- res$dataDrivenResiduals %||% list()
  note_est <- paste0(sprintf("N = %d. %s; %s.", res$n$used, res$estimatorLabel, tolower(res$n$missing)),
                     if (length(dd)) sprintf(" Residual covariances added after inspecting modification indices: %s.", paste(vapply(dd, function(x) paste(ul(x), collapse = " with "), ""), collapse = "; ")) else "")
  rel <- setNames(res$reliability, vapply(res$reliability, `[[`, "", "construct"))
  lrows <- lapply(seq_along(res$loadings), function(i) {
    l <- res$loadings[[i]]; first <- i == 1 || res$loadings[[i - 1]]$lhs != l$lhs; r <- rel[[l$lhs]]
    c(if (first) l$lhs else "", l$rhs, paste0(pt_num(l$std, d, TRUE, lz), if (stars && !is.null(l$p)) pt_stars(l$p) else ""),
      if (first) pt_num(r$omega, d, TRUE, lz) else "", if (first) pt_num(r$ave, d, TRUE, lz) else "")
  })
  prows <- lapply(res$paths, function(p) c(paste0(p$rhs, " → ", p$lhs), paste0(pt_num(p$std, d), if (stars) pt_stars(p$p) else ""), pt_num(p$est, d), pt_num(p$se, d),
                                           pt_num(p$z, 2), pt_p(p$p, lz), pt_ci(p$ciLower, p$ciUpper, d)))
  r2_note <- paste0("R²: ", paste(vapply(res$rSquared, function(x) sprintf("%s = %s", x$construct, pt_num(x$rSquared, d, TRUE, lz)), ""), collapse = "; "), ".")
  tabs <- list(
    pt_table("cb_fit", "Model Fit Indices", c("Model", "χ²", "df", "p", "CFI", "TLI", "RMSEA [90% CI]", "SRMR"), list(fitrow(m, "Measurement model (CFA)"), fitrow(f, "Structural model")),
             notes = list(paste0(note_est, if (isTRUE(f$scaled)) " Scaled χ² and robust CFI, TLI and RMSEA." else ""))),
    pt_table("cb_measurement", "Confirmatory Factor Analysis: Standardised Loadings, Composite Reliability and AVE", c("Factor", "Indicator", "λ", "ω", "AVE"), lrows,
             notes = list(paste(note_est, "λ = standardised loading; ω = composite reliability (semTools::compRelSEM); AVE = average variance extracted. The first loading of each factor is the fixed marker (unstandardised loading = 1), so it has no significance test.")), merge = 1L, label_cols = 2L),
    pt_table("cb_paths", "Structural Model Estimates", c("Path", "β", "B", "SE", "z", "p", sprintf("%d%% CI", round(100 * res$settings$ciLevel))), prows,
             notes = c(list(paste(note_est, r2_note)), if (stars) list(if (identical(lz, "apa")) "*p < .05. **p < .01. ***p < .001." else "*p < 0.05. **p < 0.01. ***p < 0.001.") else list()))
  )
  if (!is.null(res$htmt)) {
    nms <- vapply(res$htmt, `[[`, "", "construct")
    hrows <- lapply(seq_along(res$htmt), function(i) c(nms[i], vapply(seq_along(nms), function(j) if (j >= i) "" else pt_num(res$htmt[[i]][[nms[j]]], d, TRUE, lz), "")))
    tabs[[length(tabs) + 1]] <- pt_table("cb_htmt", "Discriminant Validity: Heterotrait–Monotrait Ratio (HTMT)", c("", nms), hrows, notes = list("HTMT computed with semTools::htmt; values below .85 support discriminant validity."))
  }
  if (length(res$indirect)) {
    irows <- lapply(res$indirect, function(x) c(paste(ul(x$chain), collapse = " → "), pt_num(x$est, d), pt_num(x$std, d), pt_p(x$p, lz), pt_ci(x$ciLower, x$ciUpper, d)))
    tabs[[length(tabs) + 1]] <- pt_table("cb_indirect", "Indirect Effects", c("Indirect effect", "B", "β", "p", sprintf("%d%% CI", round(100 * res$settings$ciLevel))), irows,
                                         notes = list(if (isTRUE(res$settings$bootstrap$enabled)) sprintf("Bootstrap percentile confidence intervals (%d samples, seed %s).", res$settings$bootstrap$nboot, res$settings$bootstrap$seed) else "Delta-method standard errors and confidence intervals."))
  }
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}
