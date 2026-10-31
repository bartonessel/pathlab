# Latent profile analysis (tidyLPA with the mclust engine). Every estimate comes from tidyLPA / mclust:
# estimate_profiles() fits each model structure for each number of profiles (Mclust), with the bootstrapped
# likelihood ratio test (mclustBootstrapLRT) and bootstrap standard errors (MclustBootstrap); fit indices come
# from tidyLPA's fit table and the recommended solution from compare_solutions() (analytic hierarchy process).
# The data are never changed: the project missing-value code is treated as missing for this analysis only,
# rows with missing values are left out (mclust needs complete data), and standardising only affects the copy.

# tidyLPA model numbers available with mclust (models 4 and 5 need Mplus).
LPA_MODELS <- list(
  "1" = list(label = "Equal variances, covariances fixed to zero", mclust = "EEI"),
  "2" = list(label = "Varying variances, covariances fixed to zero", mclust = "VVI"),
  "3" = list(label = "Equal variances, equal covariances", mclust = "EEE"),
  "6" = list(label = "Varying variances, varying covariances", mclust = "VVV")
)
LPA_FIT <- c("LogLik", "parameters", "n", "AIC", "AWE", "BIC", "CAIC", "CLC", "KIC", "SABIC", "ICL", "Entropy",
             "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p", "LMR_val", "LMR_p")

normalize_lpa <- function(raw) {
  raw <- raw %||% list()
  models <- unique(as.integer(unlist(raw$models %||% list(1L, 2L))))
  models <- sort(models[!is.na(models) & as.character(models) %in% names(LPA_MODELS)])
  if (!length(models)) api_error(400, "invalid_lpa", "Choose at least one model structure (1, 2, 3 or 6).")
  k_min <- int_in(raw$kMin, 1L, 1L, 10L, "The smallest number of profiles")
  k_max <- int_in(raw$kMax, 5L, 1L, 10L, "The largest number of profiles")
  if (k_max < k_min) api_error(400, "invalid_lpa", "The largest number of profiles must be at least the smallest.")
  list(
    variables = unique(as_chr_vec(raw$variables)),
    auxiliary = unique(as_chr_vec(raw$auxiliary)),
    covariates = unique(as_chr_vec(raw$covariates)),
    models = models,
    kMin = k_min,
    kMax = k_max,
    nboot = int_in(raw$nboot, 100L, 20L, 1000L, "The number of bootstrap samples"),
    standardize = isTRUE(raw$standardize),
    seed = int_in(raw$seed, 2026L, 1L, .Machine$integer.max, "The seed")
  )
}

lpa_issue <- function(severity, code, message, hint = NULL) list(severity = severity, code = code, message = message, hint = hint)

#' Problems that stop the analysis (errors) or deserve attention (warnings).
validate_lpa <- function(df, o) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- lpa_issue(...)
  v <- o$variables
  if (length(v) < 2) add("error", "lpa_variables", "Choose at least two indicator variables for the latent profile analysis.")
  cv <- o$covariates %||% character(0)
  miss <- setdiff(c(v, o$auxiliary, cv), names(df))
  if (length(miss)) add("error", "lpa_missing_column", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  bad <- Filter(function(cn) cn %in% names(df) && !is.numeric(df[[cn]]), c(v, o$auxiliary, cv))
  if (length(bad)) add("error", "lpa_not_numeric", sprintf("Latent profile analysis needs numeric variables; not numeric: %s.", paste(bad, collapse = ", ")))
  const <- Filter(function(cn) cn %in% names(df) && is.numeric(df[[cn]]) && length(unique(stats::na.omit(df[[cn]]))) < 2, v)
  if (length(const)) add("error", "lpa_constant", sprintf("These variables have no variance: %s.", paste(const, collapse = ", ")))
  both <- intersect(v, o$auxiliary)
  if (length(both)) add("error", "lpa_aux_overlap", sprintf("%s cannot be both a profile indicator and a comparison variable.", paste(both, collapse = ", ")))
  both <- intersect(v, cv)
  if (length(both)) add("error", "lpa_cov_overlap", sprintf("%s cannot be both a profile indicator and a predictor of profile membership.", paste(both, collapse = ", ")))
  out
}

lpa_rows <- function(df, o, missing_code) {
  d <- df[, o$variables, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in names(d)) {
    hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code
    recoded <- recoded + sum(hit)
    d[[cn]][hit] <- NA
  }
  keep <- stats::complete.cases(d)
  list(data = d[keep, , drop = FALSE], rows = which(keep), nInput = nrow(d), cellsRecoded = recoded)
}

lpa_key <- function(model, classes) paste0("model_", model, "_class_", classes)

#' One fitted solution of tidyLPA as plain lists (fit indices, profile sizes, estimates, classification quality).
lpa_solution <- function(x, model, classes) {
  ok <- !is.null(x$estimates) && is.data.frame(x$fit) && !is.na(x$fit$LogLik[1])
  out <- list(key = lpa_key(model, classes), model = model, classes = classes, modelLabel = LPA_MODELS[[as.character(model)]]$label,
              mclust = LPA_MODELS[[as.character(model)]]$mclust, ok = ok, warnings = as.list(unique(as.character(unlist(x$warnings)))))
  if (!ok) return(out)
  fit <- x$fit[1, , drop = FALSE]
  out$fit <- setNames(lapply(LPA_FIT, function(f) if (f %in% names(fit)) unname(as.numeric(fit[[f]])) else NULL), LPA_FIT)
  cls <- as.integer(x$model$classification)
  z <- x$model$z
  n <- length(cls)
  out$sizes <- lapply(seq_len(classes), function(k) {
    nk <- sum(cls == k)
    # Mean posterior probability of the assigned profile (diagonal of the classification table).
    list(profile = k, n = nk, proportion = nk / n, meanPosterior = if (nk) mean(z[cls == k, k]) else NA_real_,
         modelProportion = unname(x$model$parameters$pro[k]))
  })
  # Average posterior probabilities: rows = assigned profile, columns = profile.
  out$posterior <- lapply(seq_len(classes), function(k) {
    vals <- if (any(cls == k)) colMeans(z[cls == k, , drop = FALSE]) else rep(NA_real_, classes)
    list(profile = k, probs = as.list(unname(vals)))
  })
  e <- x$estimates
  out$estimates <- lapply(seq_len(nrow(e)), function(i) list(
    category = as.character(e$Category[i]), parameter = as.character(e$Parameter[i]), profile = as.integer(e$Class[i]),
    estimate = unname(e$Estimate[i]), se = unname(e$se[i]), p = unname(e$p[i])))
  out
}

#' Compare the profiles of one solution on auxiliary variables (classify-analyse: one-way Welch ANOVA on the
#' modal assignment, effect size eta-squared from the linear model). Ignores classification uncertainty.
lpa_auxiliary <- function(df, rows, cls, vars, missing_code) {
  lapply(vars, function(v) {
    y <- df[[v]][rows]
    if (!is.null(missing_code)) y[!is.na(y) & y == missing_code] <- NA
    g <- factor(cls)
    ok <- !is.na(y)
    prof <- lapply(sort(unique(cls)), function(k) {
      yk <- y[ok & cls == k]
      list(profile = k, n = length(yk), mean = if (length(yk)) mean(yk) else NA_real_, sd = if (length(yk) > 1) stats::sd(yk) else NA_real_)
    })
    test <- NULL
    gk <- droplevels(g[ok])
    if (nlevels(gk) >= 2 && all(table(gk) >= 2)) {
      w <- tryCatch(stats::oneway.test(y[ok] ~ gk, var.equal = FALSE), error = function(e) NULL)
      eta <- summary(stats::lm(y[ok] ~ gk))$r.squared
      if (!is.null(w)) test <- list(F = unname(w$statistic), df1 = unname(w$parameter[1]), df2 = unname(w$parameter[2]), p = unname(w$p.value), eta2 = eta)
    }
    list(variable = v, nMissing = sum(!ok), profiles = prof, test = test)
  })
}

#' Run the latent profile analysis. Returns the result list; `.derived` (modal profile and its posterior probability
#' for every solution, by dataset row) is split off by the worker and stored next to the result.
run_lpa <- function(df, o, missing_code = NULL, progress = function(...) NULL) {
  issues <- validate_lpa(df, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    stop(analysis_failure("validation", issues[[which(vapply(issues, function(i) i$severity == "error", TRUE))[1]]]$message,
                          "Fix the listed problems and run again."))
  t0 <- Sys.time()
  warn <- character(0)
  dat <- lpa_rows(df, o, missing_code)
  d <- dat$data
  n <- nrow(d)
  p <- ncol(d)
  if (n < 2 * o$kMax + 10) stop(analysis_failure("data", sprintf("Only %d complete rows remain for up to %d profiles.", n, o$kMax),
                                                 "Use fewer profiles or variables with fewer missing values."))
  if (n < 200) warn <- c(warn, sprintf("The sample is small for latent profile analysis (n = %d). Small profiles and the BLRT are unstable below about 300 cases.", n))
  if (n < nrow(df)) warn <- c(warn, sprintf("%d row(s) with missing values in the indicators were left out (mclust needs complete data).", nrow(df) - n))
  few <- names(d)[vapply(d, function(x) length(unique(x)) < 10, TRUE)]
  if (length(few)) warn <- c(warn, sprintf("Some indicators have fewer than 10 distinct values (%s). LPA assumes continuous indicators; avoid complex models with many profiles, or use scale scores.", paste(few, collapse = ", ")))
  desc <- lapply(names(d), function(v) list(variable = v, mean = mean(d[[v]]), sd = stats::sd(d[[v]]), min = min(d[[v]]), max = max(d[[v]])))
  if (o$standardize) d <- as.data.frame(scale(d))

  suppressPackageStartupMessages(requireNamespace("tidyLPA", quietly = TRUE))
  fits <- list()
  for (i in seq_along(o$models)) {
    mi <- o$models[i]
    progress("lpa", sprintf("Model %d (%s): %d to %d profiles, %d bootstrap samples", mi, LPA_MODELS[[as.character(mi)]]$label, o$kMin, o$kMax, o$nboot),
             0.05 + 0.85 * (i - 1) / length(o$models))
    set.seed(o$seed)
    m <- withCallingHandlers(
      tryCatch(tidyLPA::estimate_profiles(d, n_profiles = o$kMin:o$kMax, models = mi, nboot = o$nboot),
               error = function(e) stop(analysis_failure("estimation", paste("tidyLPA could not estimate the profiles:", sanitize_message(conditionMessage(e))),
                                                         "Try fewer profiles, a simpler model structure (1 or 2) or other indicators."))),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage"))
    fits <- c(fits, unclass(m))
  }
  progress("results", "Comparing the solutions", 0.92)
  grid <- expand.grid(classes = o$kMin:o$kMax, model = o$models)
  sols <- lapply(seq_len(nrow(grid)), function(i) {
    key <- lpa_key(grid$model[i], grid$classes[i])
    lpa_solution(fits[[key]] %||% list(), grid$model[i], grid$classes[i])
  })
  # Lo-Mendell-Rubin adjusted likelihood ratio test of k vs. k - 1 profiles of the same model structure (tidyLPA::calc_lrt).
  for (i in seq_along(sols)) {
    s <- sols[[i]]
    if (!s$ok || s$classes < 2) next
    prev <- Filter(function(x) x$ok && x$model == s$model && x$classes == s$classes - 1, sols)
    if (!length(prev)) next
    lr <- tryCatch(tidyLPA::calc_lrt(n, prev[[1]]$fit$LogLik, prev[[1]]$fit$parameters, s$classes - 1, s$fit$LogLik, s$fit$parameters, s$classes), error = function(e) NULL)
    if (!is.null(lr)) { sols[[i]]$fit$LMR_val <- unname(lr[["lmr_lr"]]); sols[[i]]$fit$LMR_p <- unname(lr[["lmr_p"]]) }
  }
  ok_keys <- vapply(Filter(function(s) s$ok, sols), `[[`, "", "key")
  if (!length(ok_keys)) stop(analysis_failure("estimation", "mclust could not estimate any of the requested solutions.",
                                              "Try a simpler model structure (1 or 2), fewer profiles, or standardised indicators."))
  failed <- vapply(Filter(function(s) !s$ok, sols), function(s) sprintf("model %d with %d profiles", s$model, s$classes), "")
  if (length(failed)) warn <- c(warn, sprintf("mclust could not estimate: %s.", paste(failed, collapse = ", ")))

  # Recommended solution: tidyLPA's analytic hierarchy process (Akogul & Erisoglu, 2017) over the estimated
  # solutions; best solution by each fit index as reported by compare_solutions().
  best <- list()
  recommended <- NULL
  if (length(ok_keys) >= 2) {
    sub <- structure(fits[ok_keys], class = c("tidyLPA", "list"))
    cs <- tryCatch(suppressWarnings(suppressMessages(tidyLPA::compare_solutions(sub))), error = function(e) NULL)
    if (!is.null(cs)) {
      pick <- function(idx) { s <- Filter(function(s) s$key == ok_keys[idx], sols)[[1]]; list(model = s$model, classes = s$classes) }
      for (f in names(cs$best)) best[[f]] <- pick(cs$best[[f]])
      if (!is.null(cs$AHP) && !is.na(cs$AHP)) recommended <- c(pick(cs$AHP), list(method = "AHP"))
    }
  }
  if (is.null(recommended)) {
    s <- Filter(function(s) s$ok, sols)
    bic <- vapply(s, function(x) x$fit$BIC, 0)
    recommended <- list(model = s[[which.min(bic)]]$model, classes = s[[which.min(bic)]]$classes, method = "BIC")
  }
  if (recommended$classes == o$kMax && o$kMax > 1)
    warn <- c(warn, "The recommended solution has the largest number of profiles considered; consider estimating more profiles.")

  # Membership of every solution by original row, and the auxiliary comparisons on the modal assignment.
  derived <- list(nInput = dat$nInput, rows = dat$rows, tables = list())
  for (i in seq_along(sols)) {
    s <- sols[[i]]
    if (!s$ok) next
    x <- fits[[s$key]]
    cls <- as.integer(x$model$classification)
    derived$tables[[s$key]] <- data.frame(profile = cls, probability = apply(x$model$z, 1, max))
    if (length(o$auxiliary)) sols[[i]]$auxiliary <- lpa_auxiliary(df, dat$rows, cls, o$auxiliary, missing_code)
    # Three-step analyses that correct for classification error: BCH for the comparison variables (distal outcomes),
    # ML three-step for the predictors of profile membership.
    if (s$classes >= 2 && (length(o$auxiliary) || length(o$covariates))) {
      grab <- function(vars) { if (!length(vars)) return(NULL); g <- df[dat$rows, vars, drop = FALSE]
        if (!is.null(missing_code)) for (cn in names(g)) g[[cn]][!is.na(g[[cn]]) & g[[cn]] == missing_code] <- NA; g }
      sols[[i]]$threeStep <- tryCatch(ts_tables(x$model$z, cls, grab(o$auxiliary), grab(o$covariates), unit = "Profile", prefix = "lpa"),
        error = function(e) { warn <<- c(warn, paste("The three-step analyses could not be computed:", conditionMessage(e))); list() })
    }
  }
  for (s in Filter(function(s) s$ok, sols)) {
    if (s$fit$n_min < 0.05 && s$key == lpa_key(recommended$model, recommended$classes))
      warn <- c(warn, sprintf("The recommended solution has a profile with %.1f%% of the cases; small profiles may not replicate.", 100 * s$fit$n_min))
  }

  list(
    status = "completed",
    kind = "lpa",
    completedAt = now_iso(),
    # Lists keep one-element vectors as JSON arrays.
    options = utils::modifyList(o, list(variables = as.list(o$variables), auxiliary = as.list(o$auxiliary), covariates = as.list(o$covariates %||% character(0)), models = as.list(o$models))),
    n = list(input = dat$nInput, used = n, rowsRemoved = dat$nInput - n, cellsRecoded = dat$cellsRecoded),
    descriptives = desc,
    solutions = sols,
    recommended = recommended,
    bestBy = best,
    selected = list(model = recommended$model, classes = recommended$classes),
    warnings = as.list(unique(warn)),
    rCode = lpa_code(o, missing_code),
    packages = list(tidyLPA = as.character(utils::packageVersion("tidyLPA")), mclust = as.character(utils::packageVersion("mclust"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = derived
  )
}

#' R code reproducing the analysis (dataset object `data`).
lpa_code <- function(o, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  lines <- c("# Latent profile analysis generated by PathLab SEM",
             "library(tidyLPA)   # mclust engine",
             "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
             paste0("d <- data[, c(", paste(vapply(o$variables, q, ""), collapse = ", "), ")]"))
  if (!is.null(missing_code)) lines <- c(lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  lines <- c(lines, "d <- d[complete.cases(d), ]   # mclust needs complete data")
  if (o$standardize) lines <- c(lines, "d <- as.data.frame(scale(d))   # standardised indicators (z-scores)")
  lines <- c(lines,
             "fits <- list()",
             paste0("for (mi in c(", paste(o$models, collapse = ", "), ")) {   # one call per model structure, each with the same seed"),
             paste0("  set.seed(", o$seed, ")"),
             paste0("  fits <- c(fits, unclass(estimate_profiles(d, n_profiles = ", o$kMin, ":", o$kMax, ", models = mi, nboot = ", o$nboot, ")))"),
             "}",
             "m <- structure(fits, class = c(\"tidyLPA\", \"list\"))",
             "get_fit(m)            # fit indices, entropy, BLRT",
             "# Lo-Mendell-Rubin test of k vs. k - 1 profiles: tidyLPA::calc_lrt(n, ll0, par0, k - 1, ll1, par1, k)",
             "compare_solutions(m)  # best solution by each index and the AHP recommendation",
             "# Estimates and membership of one solution, e.g. model 1 with 3 profiles:",
             "get_estimates(m[[\"model_1_class_3\"]])",
             "head(get_data(m[[\"model_1_class_3\"]]))",
             if (length(o$auxiliary) || length(o$covariates)) "# Three-step analyses (BCH distal outcomes, ML three-step covariates) use the posterior probabilities m[[...]]$model$z and the classification m[[...]]$model$classification; PathLab computes them as in Vermunt (2010) and Bakk & Vermunt (2016).")
  paste(lines, collapse = "\n")
}

#' Store the chosen solution in a stored LPA result (used by reports, tables and membership variables).
set_lpa_selection <- function(proj, rid, model, classes) {
  path <- proj$results[[rid]]
  if (is.null(path) || !file.exists(path)) api_error(404, "result_not_found", "The analysis result was not found.", "Run the analysis again.")
  res <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(res$kind, "lpa")) api_error(400, "not_lpa", "This result is not a latent profile analysis.")
  model <- as.integer(model); classes <- as.integer(classes)
  hit <- Filter(function(s) identical(as.integer(s$model), model) && identical(as.integer(s$classes), classes) && isTRUE(s$ok), res$solutions)
  if (!length(hit)) api_error(400, "invalid_solution", "That solution was not estimated.")
  res$selected <- list(model = model, classes = classes)
  tmp <- paste0(path, ".tmp")
  writeLines(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", na = "null", digits = NA), tmp, useBytes = TRUE)
  file.rename(tmp, path)
  res$selected
}

lpa_selected <- function(res) {
  sel <- res$selected %||% res$recommended
  Filter(function(s) as.integer(s$model) == as.integer(sel$model) && as.integer(s$classes) == as.integer(sel$classes), res$solutions)[[1]]
}

# ---- Report, workbook and APA tables (from the stored result; the chosen solution is `selected`) ----------

lpa_means <- function(s, vars) {
  est <- s$estimates %||% list()
  lapply(seq_len(as.integer(s$classes)), function(k) {
    get <- function(v, what) {
      e <- Filter(function(x) identical(x$category, "Means") && identical(x$parameter, v) && as.integer(x$profile) == k, est)
      if (length(e)) num_of(e[[1]][[what]]) else NA_real_
    }
    list(profile = k, mean = vapply(vars, get, 0, what = "estimate"), se = vapply(vars, get, 0, what = "se"))
  })
}

LPA_COLORS <- c("#2f6db5", "#c2410c", "#0f766e", "#7c3aed", "#b45309", "#be185d", "#4d7c0f", "#0e7490", "#475569", "#9f1239")
LPA_DASH <- c("", "6 4", "2 3", "8 3 2 3", "1 4", "10 4", "4 2", "6 2 2 2", "3 5", "12 3")

#' Profile plot as inline SVG (means of the chosen solution with +-1.96 bootstrap SE; numbers from tidyLPA).
lpa_plot_svg <- function(s, vars) {
  pm <- lpa_means(s, vars)
  W <- max(420, min(760, 120 + 70 * length(vars))); H <- 300; ml <- 56; mr <- 18; mt <- 14; mb <- 46
  vals <- unlist(lapply(pm, function(p) c(p$mean - 1.96 * p$se, p$mean + 1.96 * p$se, p$mean)))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return("")
  lo <- min(vals); hi <- max(vals); pad <- (hi - lo) * 0.08; if (pad == 0) pad <- 1
  lo <- lo - pad; hi <- hi + pad
  n <- length(vars)
  sx <- function(i) ml + (if (n > 1) (i - 1) / (n - 1) else 0.5) * (W - ml - mr)
  sy <- function(v) H - mb - (v - lo) / (hi - lo) * (H - mt - mb)
  ticks <- pretty(c(lo, hi), 5); ticks <- ticks[ticks >= lo & ticks <= hi]
  g <- c(sprintf("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" role=\"img\" aria-label=\"Profile plot\" style=\"max-width:%dpx;width:100%%\">", W, H, W),
         vapply(ticks, function(t) sprintf("<line x1=\"%d\" x2=\"%d\" y1=\"%.1f\" y2=\"%.1f\" stroke=\"#e3e8ee\"/><text x=\"%d\" y=\"%.1f\" text-anchor=\"end\" font-size=\"11\" fill=\"#5b6675\">%s</text>",
                                           ml, W - mr, sy(t), sy(t), ml - 8, sy(t) + 4, format(t)), ""),
         vapply(seq_along(vars), function(i) sprintf("<text x=\"%.1f\" y=\"%d\" text-anchor=\"middle\" font-size=\"11\" fill=\"#5b6675\">%s</text>", sx(i), H - mb + 16, html_escape(vars[i])), ""),
         sprintf("<line x1=\"%d\" x2=\"%d\" y1=\"%d\" y2=\"%d\" stroke=\"#5b6675\"/>", ml, W - mr, H - mb, H - mb))
  for (p in pm) {
    k <- p$profile; col <- LPA_COLORS[(k - 1) %% 10 + 1]
    ok <- which(is.finite(p$mean))
    for (i in ok) if (is.finite(p$se[i])) {
      g <- c(g, sprintf("<line x1=\"%.1f\" x2=\"%.1f\" y1=\"%.1f\" y2=\"%.1f\" stroke=\"%s\" stroke-opacity=\"0.7\"/>", sx(i), sx(i), sy(p$mean[i] - 1.96 * p$se[i]), sy(p$mean[i] + 1.96 * p$se[i]), col))
    }
    g <- c(g, sprintf("<polyline fill=\"none\" stroke=\"%s\" stroke-width=\"2\" stroke-dasharray=\"%s\" points=\"%s\"/>", col, LPA_DASH[(k - 1) %% 10 + 1],
                      paste(sprintf("%.1f,%.1f", sx(ok), sy(p$mean[ok])), collapse = " ")),
           vapply(ok, function(i) sprintf("<circle cx=\"%.1f\" cy=\"%.1f\" r=\"3.8\" fill=\"%s\"/>", sx(i), sy(p$mean[i]), col), ""))
  }
  g <- c(g, sprintf("<text x=\"%.1f\" y=\"%d\" text-anchor=\"middle\" font-size=\"12\">Indicator</text>", (ml + W - mr) / 2, H - 8),
         sprintf("<text transform=\"translate(14 %.1f) rotate(-90)\" text-anchor=\"middle\" font-size=\"12\">Mean</text>", (mt + H - mb) / 2), "</svg>")
  sizes <- s$sizes %||% list()
  legend <- paste(vapply(pm, function(p) {
    sz <- if (length(sizes) >= p$profile) sprintf(" (%.1f%%)", 100 * num_of(sizes[[p$profile]]$proportion)) else ""
    sprintf("<span style=\"display:inline-flex;align-items:center;gap:6px;margin-right:14px\"><svg width=\"26\" height=\"10\"><line x1=\"0\" y1=\"5\" x2=\"26\" y2=\"5\" stroke=\"%s\" stroke-width=\"2\" stroke-dasharray=\"%s\"/></svg>Profile %d%s</span>",
            LPA_COLORS[(p$profile - 1) %% 10 + 1], LPA_DASH[(p$profile - 1) %% 10 + 1], p$profile, sz)
  }, ""), collapse = "")
  paste0("<figure>", paste(g, collapse = ""), "<figcaption class=\"note\">", legend, "<br>Error bars: &plusmn;1.96 bootstrap standard errors.</figcaption></figure>")
}

lpa_fit_val <- function(s, k) num_of(s$fit[[k]])
lpa_is <- function(x, sel) !is.null(sel) && as.integer(x$model) == as.integer(sel$model) && as.integer(x$classes) == as.integer(sel$classes)

lpa_body_html <- function(res) {
  esc <- html_escape
  o <- res$options
  vars <- ul(o$variables)
  s <- lpa_selected(res)
  S <- character(0); a <- function(...) S <<- c(S, paste0(...))
  a("<h2>Latent profile analysis</h2>")
  a("<p>Gaussian mixture models of ", length(vars), " indicator(s) (", esc(paste(vars, collapse = ", ")), ") for ", o$kMin, " to ", o$kMax,
    " profiles and model structure(s) ", esc(paste(ul(o$models), collapse = ", ")), " (n = ", res$n$used,
    if (res$n$rowsRemoved > 0) paste0("; ", res$n$rowsRemoved, " row(s) with missing values left out") else "",
    if (isTRUE(o$standardize)) "; standardised indicators" else "",
    "). Estimated with tidyLPA ", esc(res$packages$tidyLPA), " (mclust ", esc(res$packages$mclust), "); BLRT and standard errors from ",
    o$nboot, " bootstrap samples (seed ", o$seed, ").</p>")
  for (w in ul(res$warnings)) a("<div class=\"warn\">", esc(w), "</div>")
  has_lmr <- any(vapply(res$solutions, function(x) !is.null(x$fit$LMR_p), TRUE))
  cell <- function(x, idx, d = 2) { t <- fmt(lpa_fit_val(x, idx), d); if (lpa_is(x, res$bestBy[[idx]])) paste0("<strong>", t, "</strong>") else t }
  rows <- lapply(res$solutions, function(x) {
    if (!isTRUE(x$ok)) return(c(as.character(x$model), as.character(x$classes), rep("&ndash;", 8 + has_lmr), "not estimated"))
    tag <- c(if (lpa_is(x, res$recommended)) "recommended", if (!is.null(s) && identical(x$key, s$key)) "interpreted")
    c(as.character(x$model), as.character(x$classes), fmt(lpa_fit_val(x, "LogLik"), 2), cell(x, "AIC"), cell(x, "BIC"), cell(x, "SABIC"),
      fmt(lpa_fit_val(x, "Entropy")), flag(fmt(lpa_fit_val(x, "prob_min")), lt(lpa_fit_val(x, "prob_min"), 0.7)),
      flag(paste0(fmt(100 * lpa_fit_val(x, "n_min"), 1), "%"), lt(lpa_fit_val(x, "n_min"), 0.05)), fmt_p(lpa_fit_val(x, "BLRT_p")),
      if (has_lmr) fmt_p(x$fit$LMR_p), paste(tag, collapse = ", "))
  })
  a(html_table(c("Model", "Profiles", "LogLik", "AIC", "BIC", "SABIC", "Entropy", "Min. post. prob.", "Smallest profile", "BLRT p", if (has_lmr) "LMR p", ""), rows,
               "Fit of the estimated solutions (bold = best value of the index; red = average posterior probability below .70 or a profile under 5% of cases)", num_cols = 2:(10 + has_lmr)))
  a("<p class=\"note\">Model structures: ", esc(paste(vapply(ul(o$models), function(m) sprintf("%s = %s", m, LPA_MODELS[[as.character(m)]]$label), ""), collapse = "; ")),
    ". Recommended solution: ", sprintf("model %s with %s profiles", res$recommended$model, res$recommended$classes),
    if (identical(res$recommended$method, "AHP")) " (analytic hierarchy process over AIC, AWE, BIC, CLC and KIC; tidyLPA)" else " (lowest BIC)",
    ". The BLRT and the Lo-Mendell-Rubin adjusted LRT (LMR) compare k with k &minus; 1 profiles of the same model structure. Choose the number of profiles from several criteria together and from whether the profiles are substantively meaningful.</p>")
  if (!is.null(s)) {
    a("<h3>Interpreted solution: model ", s$model, " (", esc(s$modelLabel), "), ", s$classes, " profiles</h3>")
    a(lpa_plot_svg(s, vars))
    pm <- lpa_means(s, vars)
    sizes <- s$sizes %||% list()
    hdr <- c("Indicator", vapply(pm, function(p) sprintf("Profile %d (n = %s)", p$profile, if (length(sizes) >= p$profile) sizes[[p$profile]]$n else "&ndash;"), ""))
    mrows <- lapply(seq_along(vars), function(j) c(esc(vars[j]), vapply(pm, function(p) paste0(fmt(p$mean[j]), " (", fmt(p$se[j]), ")"), "")))
    a(html_table(hdr, mrows, "Profile means (bootstrap standard errors)", num_cols = 2:length(hdr)))
    srows <- lapply(sizes, function(z) c(paste("Profile", z$profile), as.character(z$n), paste0(fmt(100 * num_of(z$proportion), 1), "%"),
                                          paste0(fmt(100 * num_of(z$modelProportion), 1), "%"), flag(fmt(z$meanPosterior), lt(z$meanPosterior, 0.7))))
    a(html_table(c("Profile", "n (modal)", "% (modal)", "% (estimated)", "Avg. posterior prob."), srows, "Profile sizes and classification quality", num_cols = 2:5))
    if (length(s$auxiliary)) {
      arows <- lapply(s$auxiliary, function(x) {
        tst <- x$test
        c(esc(x$variable), vapply(x$profiles, function(p) paste0(fmt(p$mean, 2), " (", fmt(p$sd, 2), ")"), ""),
          if (is.null(tst)) rep("&ndash;", 3) else c(paste0(fmt(tst$F, 2), " (", tst$df1, ", ", fmt(tst$df2, 1), ")"), fmt_p(tst$p), fmt(tst$eta2)))
      })
      a(html_table(c("Variable", vapply(seq_len(as.integer(s$classes)), function(k) sprintf("Profile %d M (SD)", k), ""), "Welch F (df)", "p", "&eta;&sup2;"), arows,
                   "Profiles compared on other variables", num_cols = 2:(as.integer(s$classes) + 4)))
      a("<p class=\"note\">Welch's one-way ANOVA on the most likely profile of each case (classify-analyse). It ignores classification uncertainty, so treat it as descriptive, especially when entropy is low.</p>")
    }
    if (length(s$threeStep)) {
      a("<h4>Three-step analyses (corrected for classification error)</h4>")
      for (t in s$threeStep) a(ts_table_html(t))
    }
  }
  a("<h3>R code</h3><pre><code>", esc(res$rCode %||% ""), "</code></pre>")
  paste(S, collapse = "\n")
}

lpa_sheets <- function(res, project_name) {
  o <- res$options
  vars <- ul(o$variables)
  fit <- do.call(rbind, lapply(res$solutions, function(x) {
    d <- data.frame(Model = as.integer(x$model), Structure = x$modelLabel, Profiles = as.integer(x$classes), Estimated = isTRUE(x$ok), stringsAsFactors = FALSE)
    for (f in LPA_FIT) d[[f]] <- if (isTRUE(x$ok)) lpa_fit_val(x, f) else NA_real_
    d
  }))
  okl <- Filter(function(x) isTRUE(x$ok), res$solutions)
  est <- do.call(rbind, lapply(okl, function(x) do.call(rbind, lapply(x$estimates, function(e)
    data.frame(Model = as.integer(x$model), Profiles = as.integer(x$classes), Category = e$category, Parameter = e$parameter, Profile = as.integer(e$profile),
               Estimate = num_of(e$estimate), SE = num_of(e$se), p = num_of(e$p), stringsAsFactors = FALSE)))))
  sizes <- do.call(rbind, lapply(okl, function(x) do.call(rbind, lapply(x$sizes, function(z)
    data.frame(Model = as.integer(x$model), Profiles = as.integer(x$classes), Profile = as.integer(z$profile), N = as.integer(z$n), Proportion = num_of(z$proportion),
               EstimatedProportion = num_of(z$modelProportion), AvgPosterior = num_of(z$meanPosterior))))))
  s <- lpa_selected(res)
  out <- list(
    Info = data.frame(Item = c("Project", "Completed (UTC)", "PathLab SEM version", "tidyLPA", "mclust", "Indicators", "Standardised", "Rows used", "Rows in dataset",
                               "Bootstrap samples", "Seed", "Recommended", "Interpreted"),
                      Value = c(project_name, res$completedAt, PATHLAB_VERSION, res$packages$tidyLPA, res$packages$mclust, paste(vars, collapse = ", "),
                                isTRUE(o$standardize), res$n$used, res$n$input, o$nboot, o$seed,
                                sprintf("model %s, %s profiles (%s)", res$recommended$model, res$recommended$classes, res$recommended$method),
                                if (is.null(s)) "" else sprintf("model %s, %s profiles", s$model, s$classes)), stringsAsFactors = FALSE),
    Fit = fit, Estimates = est, Sizes = sizes)
  if (!is.null(s) && length(s$auxiliary)) {
    out$Comparisons <- do.call(rbind, lapply(s$auxiliary, function(x) do.call(rbind, lapply(x$profiles, function(p)
      data.frame(Variable = x$variable, Profile = as.integer(p$profile), N = as.integer(p$n), Mean = num_of(p$mean), SD = num_of(p$sd),
                 WelchF = num_of(x$test$F), df1 = num_of(x$test$df1), df2 = num_of(x$test$df2), p = num_of(x$test$p), Eta2 = num_of(x$test$eta2), stringsAsFactors = FALSE)))))
  }
  if (!is.null(s) && length(s$threeStep)) for (t in s$threeStep) {
    d <- ts_table_df(t)
    if (!is.null(d)) out[[substr(gsub("[^A-Za-z0-9 ]", "", sub(" [(].*", "", t$title)), 1, 31)]] <- d
  }
  out
}

lpa_pub_spec <- function(res, opts) {
  d <- opts$decimals; lz <- opts$leadingZero
  o <- res$options
  vars <- ul(o$variables)
  s <- lpa_selected(res)
  ok <- Filter(function(x) isTRUE(x$ok), res$solutions)
  has_lmr <- any(vapply(ok, function(x) !is.null(x$fit$LMR_p), TRUE))
  hdr <- c("Model", "k", "LL", "Par.", "AIC", "BIC", "SABIC", "Entropy", "Smallest (%)", "BLRT p", if (has_lmr) "LMR p")
  rows <- lapply(ok, function(x) c(as.character(x$model), as.character(x$classes), pt_num(lpa_fit_val(x, "LogLik"), 2), as.character(as.integer(lpa_fit_val(x, "parameters"))),
                                   pt_num(lpa_fit_val(x, "AIC"), 2), pt_num(lpa_fit_val(x, "BIC"), 2), pt_num(lpa_fit_val(x, "SABIC"), 2),
                                   if (as.integer(x$classes) == 1) "–" else pt_num(lpa_fit_val(x, "Entropy"), d, TRUE, lz),
                                   formatC(100 * lpa_fit_val(x, "n_min"), format = "f", digits = 1), pt_p(lpa_fit_val(x, "BLRT_p"), lz),
                                   if (has_lmr) (if (is.null(x$fit$LMR_p)) "–" else pt_p(num_of(x$fit$LMR_p), lz))))
  bold <- list()
  for (idx in c("AIC", "BIC", "SABIC")) {
    i <- which(vapply(ok, function(x) lpa_is(x, res$bestBy[[idx]]), TRUE))
    if (length(i)) bold[[length(bold) + 1]] <- c(i[1], match(idx, hdr))
  }
  note1 <- sprintf("N = %d. Model structures: %s. k = number of profiles; LL = log-likelihood; Par. = free parameters; SABIC = sample-size adjusted BIC; Smallest = percentage of cases in the smallest profile; BLRT = bootstrapped likelihood ratio test of k vs. k − 1 profiles (%d bootstrap samples); LMR = Lo-Mendell-Rubin adjusted likelihood ratio test. Lowest AIC, BIC and SABIC in bold. Recommended solution (analytic hierarchy process): model %s with %s profiles. Estimated with tidyLPA %s (mclust %s).",
                   as.integer(res$n$used), paste(vapply(ul(o$models), function(m) sprintf("%s = %s", m, tolower(LPA_MODELS[[as.character(m)]]$label)), ""), collapse = "; "),
                   as.integer(o$nboot), res$recommended$model, res$recommended$classes, res$packages$tidyLPA, res$packages$mclust)
  tabs <- list(pt_table("lpa_fit", "Fit Indices of the Latent Profile Solutions", hdr, rows, notes = list(note1), bold = bold, label_cols = 2L))
  if (!is.null(s)) {
    pm <- lpa_means(s, vars)
    sizes <- s$sizes %||% list()
    mh <- c("Indicator", vapply(pm, function(p) sprintf("Profile %d (n = %s, %.1f%%)", p$profile, sizes[[p$profile]]$n, 100 * num_of(sizes[[p$profile]]$proportion)), ""))
    mrows <- c(lapply(seq_along(vars), function(j) c(vars[j], vapply(pm, function(p) sprintf("%s (%s)", pt_num(p$mean[j], d), pt_num(p$se[j], d)), ""))),
               list(c("Average posterior probability", vapply(sizes, function(z) pt_num(z$meanPosterior, d, TRUE, lz), ""))))
    tabs[[2]] <- pt_table("lpa_means", sprintf("Profile Means and Standard Errors (Model %s, %s Profiles)", s$model, s$classes), mh, mrows,
                          notes = list(sprintf("Means with bootstrap standard errors in parentheses%s. Profile sizes are based on the most likely profile of each case. Model structure: %s.",
                                               if (isTRUE(o$standardize)) ", on standardised indicators" else "", tolower(s$modelLabel))))
    if (length(s$auxiliary)) {
      arows <- lapply(s$auxiliary, function(x) {
        tst <- x$test
        c(x$variable, vapply(x$profiles, function(p) sprintf("%s (%s)", pt_num(p$mean, 2), pt_num(p$sd, 2)), ""),
          if (is.null(tst)) rep("–", 3) else c(sprintf("%s (%s, %s)", pt_num(tst$F, 2), as.integer(tst$df1), pt_num(tst$df2, 1)), pt_p(tst$p, lz), pt_num(tst$eta2, d, TRUE, lz)))
      })
      tabs[[3]] <- pt_table("lpa_compare", "Profile Differences on Other Variables",
                            c("Variable", vapply(seq_len(as.integer(s$classes)), function(k) sprintf("Profile %d M (SD)", k), ""), "Welch F (df)", "p", "η²"), arows,
                            notes = list("Welch's one-way ANOVA on the most likely profile of each case; this ignores classification uncertainty. p values are not corrected for multiple comparisons."))
    }
  }
  tabs <- Filter(Negate(is.null), tabs)
  for (i in seq_along(tabs)) tabs[[i]]$number <- i
  tabs
}
