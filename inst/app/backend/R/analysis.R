# SEMinR analysis runner. Executed inside a background R process (see jobs.R).
#
# Every statistic returned here comes from a SEMinR object or summary:
#   estimate_pls() + summary()          -> paths, R^2, loadings, weights, reliability,
#                                          HTMT, Fornell-Larcker, cross-loadings, VIF,
#                                          f^2, AIC/BIC, total & indirect effects,
#                                          descriptives
#   bootstrap_model() + summary(alpha)  -> bootstrap mean/SD, t, percentile CI, p
#   specific_effect_significance()      -> specific indirect effects
#   predict_pls() + summary()           -> out-of-sample RMSE/MAE (PLS and LM)
# The only arithmetic PathLab adds is the RMSE/MAE difference (PLS minus LM).

progress_writer <- function(path) {
  started <- Sys.time()
  function(stage, message, fraction = NULL, estimated_seconds = NULL) {
    p <- list(stage = stage, message = message, fraction = fraction,
              estimatedSeconds = estimated_seconds,
              stageStartedAt = now_iso(),
              elapsedSeconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
    tmp <- paste0(path, ".tmp")
    jsonlite::write_json(p, tmp, auto_unbox = TRUE, null = "null", digits = NA)
    file.rename(tmp, path)
  }
}

analysis_failure <- function(stage, message, hint = NULL, detail = NULL) {
  structure(class = c("analysis_failure", "error", "condition"),
            list(message = message, call = NULL, stage = stage, hint = hint, detail = detail))
}

# Run expr capturing (and muffling) messages and warnings.
capture_conditions <- function(expr) {
  msgs <- character(0)
  warns <- character(0)
  value <- withCallingHandlers(
    expr,
    message = function(m) { msgs <<- c(msgs, sanitize_message(conditionMessage(m))); invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, sanitize_message(conditionMessage(w))); invokeRestart("muffleWarning") }
  )
  list(value = value, messages = msgs[nzchar(msgs)], warnings = warns[nzchar(warns)])
}

friendly_estimation_error <- function(msg) {
  m <- tolower(msg)
  if (grepl("zero variance", m)) return(list(message = "An indicator has zero variance after the missing-data treatment, so it cannot be standardised.",
                                             hint = "Check the Data tab for constant indicators and remove them from the model."))
  if (grepl("singular|not invertible|system is computationally", m)) return(list(message = "Estimation failed because some indicators or constructs are perfectly collinear (the regression system is singular).",
                                                                                hint = "Check for duplicated or perfectly correlated indicators, especially in Mode B constructs, and review the antecedent VIF values."))
  if (grepl("subscript out of bounds|undefined columns", m)) return(list(message = "SEMinR could not find one of the indicator columns in the data.",
                                                                       hint = "Make sure every indicator name matches a dataset column exactly."))
  list(message = "SEMinR could not estimate this model.", hint = "Review the model warnings in the Diagnostics tab, simplify the model, and try again.")
}

prepare_data <- function(df, model, settings) {
  items <- model_indicators(model)
  d <- df[, items, drop = FALSE]
  n_input <- nrow(d)
  recoded <- 0L
  if (!is.null(settings$missing$code)) {
    for (v in items) {
      hit <- !is.na(d[[v]]) & d[[v]] == settings$missing$code
      recoded <- recoded + sum(hit)
      d[[v]][hit] <- NA
    }
  }
  n_missing_cells <- sum(is.na(d))
  if (settings$missing$treatment == "none" && n_missing_cells > 0) {
    stop(analysis_failure("data", "The selected indicators contain missing values, but no missing-data treatment was chosen.",
                          "Choose mean replacement or listwise deletion in Analysis settings."))
  }
  rows <- seq_len(nrow(d))
  if (settings$missing$treatment == "listwise") {
    rows <- which(stats::complete.cases(d))
    d <- d[rows, , drop = FALSE]
  }
  list(data = d, rows = rows, nInput = n_input, nUsed = nrow(d), rowsRemoved = n_input - nrow(d),
       cellsRecoded = recoded, cellsMissing = n_missing_cells,
       cellsImputed = if (settings$missing$treatment == "mean_replacement") n_missing_cells else 0L)
}

boot_table_to_long <- function(tab, kind = "path") {
  if (is.null(tab) || length(tab) == 0) return(list())
  # SEMinR returns the text "No indirect effects" instead of a table when a model has no indirect paths.
  if (is.character(tab) && is.null(dim(tab))) return(list())
  tab <- unclass(tab)
  rn <- rownames(tab)
  lapply(seq_len(nrow(tab)), function(i) {
    parts <- strsplit(rn[i], "  ->  ", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    r <- list(from = parts[1], to = parts[length(parts)])
    if (length(parts) > 2) r$through <- arr(parts[2:(length(parts) - 1)])
    vals <- as.numeric(tab[i, ])
    keys <- c("original", "bootMean", "bootSD", "t", "lower", "upper", "p")
    for (k in seq_along(keys)) r[[keys[k]]] <- if (is.na(vals[k]) || !is.finite(vals[k])) NULL else vals[k]
    r
  })
}

list_output_long <- function(lst, inner_key, outer_key) {
  out <- list()
  for (o in names(lst)) {
    v <- unclass(lst[[o]])
    if (is.null(v) || length(v) == 0) next
    for (nm in names(v)) {
      val <- suppressWarnings(as.numeric(v[[nm]]))
      r <- list()
      r[[outer_key]] <- o
      r[[inner_key]] <- nm
      r$vif <- if (is.na(val) || !is.finite(val)) NULL else val
      out[[length(out) + 1]] <- r
    }
  }
  out
}

extract_estimation <- function(m, s, model) {
  byid <- construct_by_id(model)
  modes <- setNames(vapply(model$constructs, `[[`, "", "mode"), vapply(model$constructs, `[[`, "", "name"))
  load <- unclass(s$loadings)
  wts <- unclass(s$weights)
  vif_items <- list_output_long(s$validity$vif_items, "indicator", "construct")
  vif_lookup <- setNames(vapply(vif_items, function(r) r$vif %||% NA_real_, 0),
                         vapply(vif_items, function(r) paste(r$construct, r$indicator, sep = "\r"), ""))
  outer <- list()
  for (c in model$constructs) for (it in (if (is_hoc(c)) vapply(c$dimensions, function(d) byid[[d]]$name, "") else c$indicators)) {
    lv <- suppressWarnings(as.numeric(load[it, c$name]))
    wv <- suppressWarnings(as.numeric(wts[it, c$name]))
    vv <- vif_lookup[paste(c$name, it, sep = "\r")]
    outer[[length(outer) + 1]] <- list(construct = c$name, indicator = it, mode = c$mode, higherOrder = if (is_hoc(c)) TRUE else NULL,
                                       loading = num_or_null(lv), weight = num_or_null(wv),
                                       vif = num_or_null(vv))
  }
  rel <- unclass(s$reliability)
  reliability <- lapply(rownames(rel), function(cn) list(
    construct = cn, mode = if (cn %in% names(modes)) unname(modes[cn]) else "interaction",
    alpha = num_or_null(rel[cn, "alpha"]), rhoA = num_or_null(rel[cn, "rhoA"]),
    rhoC = num_or_null(rel[cn, "rhoC"]), AVE = num_or_null(rel[cn, "AVE"])))

  pth <- unclass(s$paths)
  fsq <- unclass(s$fSquare)
  ante <- list_output_long(s$vif_antecedents, "from", "to")
  ante_lookup <- setNames(vapply(ante, function(r) r$vif %||% NA_real_, 0),
                          vapply(ante, function(r) paste(r$from, r$to, sep = "\r"), ""))
  paths <- lapply(model$paths, function(p) {
    f <- byid[[p$from]]$name; t <- byid[[p$to]]$name
    list(id = p$id, from = f, to = t, implicit = if (isTRUE(p$implicit)) TRUE else NULL,
         coefficient = num_or_null(m$path_coef[f, t]),
         fSquare = num_or_null(fsq[f, t]),
         vif = num_or_null(ante_lookup[paste(f, t, sep = "\r")]))
  })
  for (mo in model$moderations %||% list()) {
    f <- interaction_name(model, mo); t <- byid[[mo$dv]]$name
    paths[[length(paths) + 1]] <- list(id = mo$id, from = f, to = t, moderation = TRUE,
                                       coefficient = num_or_null(m$path_coef[f, t]), fSquare = num_or_null(fsq[f, t]),
                                       vif = num_or_null(ante_lookup[paste(f, t, sep = "\r")]))
  }
  rsq <- lapply(colnames(pth), function(cn) list(construct = cn,
                                                 rSquared = num_or_null(pth["R^2", cn]),
                                                 adjRSquared = num_or_null(pth["AdjR^2", cn])))
  effects_long <- function(mat) {
    mat <- unclass(mat)
    out <- list()
    for (i in rownames(mat)) for (j in colnames(mat)) {
      v <- mat[i, j]
      if (!is.na(v) && v != 0) out[[length(out) + 1]] <- list(from = i, to = j, effect = v)
    }
    out
  }
  itc <- unclass(s$it_criteria)
  it_criteria <- lapply(colnames(itc), function(cn) list(construct = cn, AIC = num_or_null(itc["AIC", cn]), BIC = num_or_null(itc["BIC", cn])))
  desc <- unclass(s$descriptives$statistics$items)
  desc_rows <- lapply(rownames(desc), function(it) list(
    indicator = it, missing = num_or_null(desc[it, "Missing"]), mean = num_or_null(desc[it, "Mean"]),
    median = num_or_null(desc[it, "Median"]), min = num_or_null(desc[it, "Min"]), max = num_or_null(desc[it, "Max"]),
    sd = num_or_null(desc[it, "Std.Dev."]), kurtosis = num_or_null(desc[it, "Kurtosis"]), skewness = num_or_null(desc[it, "Skewness"])))

  list(
    measurement = list(
      outer = outer,
      reliability = reliability,
      htmt = matrix_to_table(blank_hoc_pairs(s$validity$htmt, model), "construct", "Construct"),
      fornellLarcker = matrix_to_table(blank_hoc_pairs(s$validity$fl_criteria, model), "construct", "Construct"),
      crossLoadings = matrix_to_table(s$validity$cross_loadings, "indicator", "Indicator"),
      congruence = matrix_to_table(blank_hoc_pairs(congruence_index(s$validity$cross_loadings), model), "construct", "Construct")
    ),
    structural = list(
      paths = paths,
      rSquared = rsq,
      totalEffects = effects_long(s$total_effects),
      totalIndirectEffects = effects_long(s$total_indirect_effects),
      itCriteria = it_criteria
    ),
    descriptives = list(
      items = desc_rows,
      constructCorrelations = matrix_to_table(s$descriptives$correlations$constructs, "construct", "Construct")
    )
  )
}

run_analysis <- function(df, model, settings, dataset_meta, progress = function(...) NULL) {
  t0 <- Sys.time()
  diagnostics <- list(messages = character(0), warnings = character(0))
  progress("preparing", "Preparing data", 0.02)
  prep <- prepare_data(df, model, settings)

  progress("estimating", "Estimating the PLS path model with SEMinR", 0.05)
  mm <- build_measurement_model(model)
  sm <- build_structural_model(model)
  miss_fun <- if (settings$missing$treatment == "mean_replacement") seminr::mean_replacement else stats::na.omit
  iw <- if (settings$innerWeights == "path_factorial") seminr::path_factorial else seminr::path_weighting
  t_est0 <- Sys.time()
  est <- tryCatch(
    capture_conditions(seminr::estimate_pls(
      data = prep$data, measurement_model = mm, structural_model = sm,
      inner_weights = iw, missing = miss_fun, missing_value = NA,
      maxIt = settings$maxIt, stopCriterion = settings$stopCriterion)),
    error = function(e) {
      fe <- friendly_estimation_error(conditionMessage(e))
      stop(analysis_failure("estimation", fe$message, fe$hint, sanitize_message(conditionMessage(e))))
    })
  est_seconds <- as.numeric(difftime(Sys.time(), t_est0, units = "secs"))
  m <- est$value
  diagnostics$messages <- c(diagnostics$messages, est$messages)
  diagnostics$warnings <- c(diagnostics$warnings, est$warnings)
  if (is.null(m) || !inherits(m, "seminr_model")) stop(analysis_failure("estimation", "SEMinR did not return an estimated model."))
  converged <- is.finite(m$weightDiff) && m$weightDiff < 10^(-settings$stopCriterion)
  if (!converged) {
    stop(analysis_failure("estimation",
                          sprintf("The PLS algorithm did not converge within %d iterations, so no results are reported.", settings$maxIt),
                          "Increase 'Maximum iterations' in Analysis settings, or check the model for problematic indicators."))
  }
  if (any(!is.finite(m$path_coef[m$path_coef != 0]))) {
    stop(analysis_failure("estimation", "Estimation produced non-finite path coefficients.", "Check for collinear or constant indicators."))
  }
  s <- capture_conditions(summary(m))
  diagnostics$warnings <- c(diagnostics$warnings, s$warnings)
  out <- extract_estimation(m, s$value, model)
  fit <- if (has_hoc(model)) list(available = FALSE, reason = "Model fit (SRMR, d_ULS, d_G) is not computed for models with higher-order constructs in PathLab yet.") else tryCatch(model_fit(m, model, unclass(s$value$reliability)),
                  error = function(e) list(available = FALSE, reason = paste("Model fit could not be computed:", sanitize_message(conditionMessage(e)))))
  fit_full <- fit
  attr(fit, "sigma") <- NULL; attr(fit, "items") <- NULL
  out$measurement$fit <- fit
  ex <- settings$extras %||% list()
  if (isTRUE(ex$fitTest) && isTRUE(fit$available)) {
    progress("fit_test", sprintf("Bootstrap test of model fit (%d samples)", ex$fitBoot), 0.16, 2 * ex$fitBoot * est_seconds * 1.5)
    out$measurement$fitTest <- tryCatch(fit_boot_test(m, model, unclass(s$value$reliability), ex$fitBoot, ex$seed),
                                        error = function(e) list(available = FALSE, reason = sanitize_message(conditionMessage(e))))
  }
  if (isTRUE(ex$cta)) out$measurement$cta <- tryCatch(cta_pls(m, ex$extrasBoot, ex$seed), error = function(e) list(error = sanitize_message(conditionMessage(e))))
  if (isTRUE(ex$endogeneity)) out$structural$endogeneity <- tryCatch(copula_test(m, ex$extrasBoot, ex$seed), error = function(e) list(error = sanitize_message(conditionMessage(e))))
  if (isTRUE(fit$seminrPathsDiffer)) diagnostics$warnings <- c(diagnostics$warnings, sprintf(
    "SEMinR %s applies the PLSc correction (rho_A) to Mode A composite construct(s) too (%s) when the model also has reflective constructs, so its path coefficients and R² differ from standard PLSc (Dijkstra & Henseler, 2015) as used by SmartPLS and cSEM. Model fit is computed with the standard definition.",
    seminr_version(), paste(unlist(fit$affectedComposites), collapse = ", ")))
  progress("estimated", "Model estimated", 0.15)

  boot <- NULL
  if (isTRUE(settings$bootstrap$enabled)) {
    b <- settings$bootstrap
    alpha <- 1 - b$confidenceLevel
    est_boot <- 3 * b$cores + b$nboot * est_seconds * 1.15 / b$cores
    progress("bootstrapping", sprintf("Bootstrapping %d samples on %d core(s)", b$nboot, b$cores), 0.15, est_boot)
    bres <- capture_conditions(seminr::bootstrap_model(m, nboot = b$nboot, cores = b$cores, seed = b$seed))
    bm <- bres$value
    boot_msgs <- unique(bres$messages[!grepl("^Bootstrapping model using seminr|successfully bootstrapped", bres$messages)])
    if (is.null(bm) || !inherits(bm, "boot_seminr_model")) {
      stop(analysis_failure("bootstrap", "Bootstrapping failed, so no bootstrap results are reported.",
                            "Try a different seed or fewer constructs measured reflectively (PLSc can fail in some resamples). The estimated model itself is valid; you can run again with bootstrapping turned off.",
                            sanitize_message(paste(boot_msgs, collapse = " "))))
    }
    successful <- as.integer(bm$boots)
    if (successful < 2) stop(analysis_failure("bootstrap", "Too few bootstrap samples succeeded to compute inference statistics."))
    progress("summarising", "Summarising bootstrap results", 0.9)
    bs <- capture_conditions(summary(bm, alpha = alpha))$value
    specific <- list()
    if (isTRUE(b$specificIndirect)) {
      for (ch in indirect_chains(model)) {
        r <- tryCatch(seminr::specific_effect_significance(bm, from = ch[1], through = ch[-c(1, length(ch))],
                                                           to = ch[length(ch)], alpha = alpha),
                      error = function(e) NULL)
        if (!is.null(r)) {
          row <- boot_table_to_long(r)[[1]]
          row$from <- ch[1]; row$to <- ch[length(ch)]; row$through <- arr(ch[-c(1, length(ch))])
          specific[[length(specific) + 1]] <- row
        }
      }
    }
    boot <- list(
      requested = b$nboot, successful = successful, failed = b$nboot - successful,
      seed = bm$seed, cores = b$cores, confidenceLevel = b$confidenceLevel, alpha = alpha,
      ciMethod = "percentile", pValueMethod = "two-sided bootstrap proportion: 2 * min(P(b <= 0), P(b > 0))",
      tMethod = "original estimate / bootstrap standard deviation",
      paths = boot_table_to_long(bs$bootstrapped_paths),
      loadings = boot_table_to_long(bs$bootstrapped_loadings),
      weights = boot_table_to_long(bs$bootstrapped_weights),
      htmt = boot_table_to_long(bs$bootstrapped_HTMT),
      totalEffects = boot_table_to_long(bs$bootstrapped_total_paths),
      totalIndirectEffects = boot_table_to_long(bs$bootstrapped_total_indirect_paths),
      specificIndirectEffects = specific,
      messages = arr(boot_msgs)
    )
    if (isTRUE(b$bca)) {
      progress("bca", "Computing BCa intervals (jackknife)", 0.9, nrow(m$data) * est_seconds)
      boot <- tryCatch(pls_bca(m, bm, boot, b$confidenceLevel), error = function(e) { diagnostics$warnings <<- c(diagnostics$warnings, paste("BCa intervals could not be computed:", sanitize_message(conditionMessage(e)))); boot })
    }
    if (successful < b$nboot) {
      diagnostics$warnings <- c(diagnostics$warnings,
                                sprintf("%d of %d bootstrap samples failed (often due to PLSc in a resample) and were excluded by SEMinR. Inference is based on %d samples.",
                                        b$nboot - successful, b$nboot, successful))
    }
  }

  pred <- NULL
  if (isTRUE(settings$predict$enabled)) {
    p <- settings$predict
    progress("predicting", sprintf("Running PLSpredict (%d folds x %d repetition(s))", p$noFolds, p$reps), 0.93)
    if (!is.null(p$seed)) set.seed(p$seed)
    hoc_pred <- has_hoc(model)
    # The case order predict_pls() draws next, so its folds are known (Q²predict and CVPAT need them).
    ord <- if (hoc_pred) NULL else predict_order(nrow(m$data))
    fail <- function(e) stop(analysis_failure("predict", "PLSpredict failed, so no prediction results are reported.",
                                              "Try fewer folds, or run the analysis without PLSpredict.", sanitize_message(conditionMessage(e))))
    # Higher-order models: SEMinR's procedure with the two-stage approach inside every fold (predict_pls_two_stage);
    # it runs on copies after the estimation above, which it does not change.
    pr <- tryCatch(capture_conditions(if (hoc_pred) predict_pls_two_stage(m, p$technique, p$noFolds, p$reps)
                                      else seminr::predict_pls(m, technique = if (p$technique == "predict_EA") seminr::predict_EA else seminr::predict_DA,
                                                               noFolds = p$noFolds, reps = p$reps)), error = fail)
    ps <- if (hoc_pred) pr$value else summary(pr$value)
    po <- unclass(ps$PLS_out_of_sample); lo <- unclass(ps$LM_out_of_sample)
    items <- lapply(colnames(po), function(it) list(
      indicator = it,
      plsRMSE = num_or_null(po["RMSE", it]), plsMAE = num_or_null(po["MAE", it]),
      lmRMSE = num_or_null(lo["RMSE", it]), lmMAE = num_or_null(lo["MAE", it]),
      diffRMSE = num_or_null(po["RMSE", it] - lo["RMSE", it]), diffMAE = num_or_null(po["MAE", it] - lo["MAE", it])))
    # Q²predict and CVPAT from the same out-of-sample predictions and folds.
    ic <- setNames(m$mmMatrix[, "construct"], m$mmMatrix[, "measurement"]); if (!is.null(m$first_stage_model)) ic <- c(ic, setNames(m$first_stage_model$mmMatrix[, "construct"], m$first_stage_model$mmMatrix[, "measurement"]))
    extra <- tryCatch(if (hoc_pred) { r <- pr$value$residuals; predict_q2_cvpat(r$actual, r$pls, r$lm, r$folds, ic) } else {
      it <- pr$value$items; cols <- colnames(it$PLS_out_of_sample_residuals); n <- nrow(m$data)
      folds <- cut(seq_len(n), breaks = p$noFolds, labels = FALSE)[match(seq_len(n), ord)]
      predict_q2_cvpat(it$item_actuals[, cols, drop = FALSE], it$PLS_out_of_sample_residuals, it$lm_out_of_sample_residuals[, cols, drop = FALSE], folds, ic)
    }, error = function(e) NULL)
    pred <- list(technique = p$technique, noFolds = p$noFolds, reps = p$reps, seed = p$seed, items = items, q2 = extra$q2, cvpat = extra$cvpat,
                 method = if (hoc_pred) "two_stage_folds" else "seminr",
                 notPredicted = if (hoc_pred) arr(ps$notPredicted) else NULL)
  }

  ipma <- NULL
  if (isTRUE(settings$ipma$enabled)) {
    progress("ipma", "Computing the importance-performance map", 0.94)
    ipma <- run_ipma(m, model, settings$ipma)
    if (isTRUE(settings$ipma$bootstrap)) {
      progress("ipma_boot", sprintf("Bootstrapping the IPMA importance (%d samples)", settings$ipma$bootN), 0.945, settings$ipma$bootN * est_seconds * 1.3)
      ipma <- tryCatch(ipma_bootstrap(m, model, settings$ipma, ipma, settings$ipma$bootN, settings$bootstrap$seed %||% 2026L, settings$bootstrap$confidenceLevel),
                       error = function(e) { diagnostics$warnings <<- c(diagnostics$warnings, paste("IPMA bootstrap failed:", sanitize_message(conditionMessage(e)))); ipma })
    }
    if (length(settings$ipma$targets)) ipma$others <- ipma_more_targets(m, model, settings$ipma)
    ipma_bounds <- ipma$bounds; ipma$bounds <- NULL
    if (length(ipma$negativeWeights)) diagnostics$warnings <- c(diagnostics$warnings,
      sprintf("IPMA: %d indicator(s) have negative rescaled weights (%s). IPMA assumes all outer weights are positive; interpret with caution.",
              length(ipma$negativeWeights), paste(unlist(ipma$negativeWeights), collapse = ", ")))
  }
  nca <- NULL
  if (isTRUE(settings$nca$enabled)) {
    nc <- settings$nca
    n_tests <- length(nca_conditions(model, nc)) * length(nc$ceilings)
    progress("nca", sprintf("Running NCA (%d permutation test(s) x %d resamples)", n_tests, nc$testRep), 0.95,
             if (nc$testRep > 0) 1 + n_tests * nc$testRep * 0.0005 else NULL)
    nca <- tryCatch(run_nca(m, model, nc), analysis_failure = function(e) stop(e),
                    error = function(e) stop(analysis_failure("nca", "The Necessary Condition Analysis failed, so no NCA results are reported.",
                                                              "Check the NCA settings, or run the analysis without NCA.", sanitize_message(conditionMessage(e)))))
  }
  mga <- NULL
  if (isTRUE(settings$mga$enabled)) {
    progress("mga", sprintf("Running PLS-MGA (%d bootstrap samples per group)%s", settings$mga$nboot, if (isTRUE(settings$mga$micom)) sprintf(" and MICOM (%d permutations)", settings$mga$micomPerm) else ""), 0.96,
             2 * (3 * settings$bootstrap$cores + settings$mga$nboot * est_seconds * 1.15 / settings$bootstrap$cores) +
               if (isTRUE(settings$mga$micom)) 2 * settings$mga$micomPerm * est_seconds else 0)
    mga <- run_mga(df, prep, model, settings, settings$bootstrap$cores)
    # Group-specific IPMA (same rescaling bounds as the full sample), shown in the IPMA tab.
    if (!is.null(ipma) && isTRUE(settings$ipma$byGroup) && !is.null(mga$groupModels)) {
      ipma$groups <- lapply(mga$groupModels, function(gm) tryCatch({ r <- run_ipma(gm$model, model, modifyList(settings$ipma, list(cipma = FALSE)), bounds = ipma_bounds)
        list(group = gm$label, n = nrow(gm$model$data), targetPerformance = r$targetPerformance, constructs = r$constructs) }, error = function(e) list(group = gm$label, error = sanitize_message(conditionMessage(e)))))
    }
    mga$groupModels <- NULL
    if (mga$nbootUsed < mga$nboot) diagnostics$warnings <- c(diagnostics$warnings,
      sprintf("PLS-MGA used %d of %d bootstrap samples per group after SEMinR removed inadmissible runs.", mga$nbootUsed, mga$nboot))
  }

  cmb <- tryCatch(run_cmb(m, model, df, prep, settings$cmb %||% list()),
                  error = function(e) { diagnostics$warnings <<- c(diagnostics$warnings, paste("Common method bias checks could not be computed:", sanitize_message(conditionMessage(e)))); NULL })
  progress("finishing", "Preparing results", 0.98)
  list(
    status = "completed",
    completedAt = now_iso(),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    environment = list(rVersion = R.version.string, seminrVersion = seminr_version(),
                       platform = R.version$platform),
    data = list(filename = dataset_meta$filename, checksum = dataset_meta$checksum,
                nInput = prep$nInput, nUsed = prep$nUsed, rowsRemoved = prep$rowsRemoved,
                cellsRecoded = prep$cellsRecoded, cellsMissing = prep$cellsMissing,
                cellsImputed = prep$cellsImputed),
    estimation = list(iterations = m$iterations, converged = converged, weightDiff = m$weightDiff,
                      seconds = est_seconds,
                      reflectiveConstructs = arr(vapply(Filter(function(c) c$mode == "reflective", model$constructs), `[[`, "", "name"))),
    measurement = out$measurement,
    structural = out$structural,
    descriptives = out$descriptives,
    bootstrap = boot,
    predict = pred,
    ipma = ipma,
    nca = nca,
    mga = mga,
    cmb = cmb,
    moderation = tryCatch(moderation_effects(m, if (!is.null(boot)) bm else NULL, settings$bootstrap$confidenceLevel), error = function(e) NULL),
    diagnostics = list(messages = arr(unique(diagnostics$messages)), warnings = arr(unique(diagnostics$warnings))),
    .derived = list(nInput = prep$nInput, rows = prep$rows, tables = list(scores = as.data.frame(m$construct_scores),
                                                                   # Min-max normalised to 0-1 for fsQCA (Sukhov et al., 2023).
                                                                   scores_norm = as.data.frame(lapply(as.data.frame(m$construct_scores), function(x) { r <- range(x, na.rm = TRUE); if (diff(r) > 0) (x - r[1]) / diff(r) else x * 0 }), check.names = FALSE)))
  )
}

# Entry point used by the background worker process.
worker_main <- function(job_dir) {
  progress <- progress_writer(file.path(job_dir, "progress.json"))
  input <- readRDS(file.path(job_dir, "input.rds"))
  res <- tryCatch({
    if (identical(input$kind, "cbsem")) {
      run_cbsem(input$data, input$model, input$cb, input$missingCode, progress)
    } else if (identical(input$kind, "gsca")) {
      run_gsca(input$data, input$model, input$gsca, input$missingCode, progress)
    } else if (identical(input$kind, "ml")) {
      if (!is.null(input$semann)) run_semann(input$data, input$semann, input$missingCode, progress) else run_ml(input$data, input$ml, input$missingCode, progress, input$declared, input$test)
    } else if (identical(input$kind, "lpa")) {
      run_lpa(input$data, input$lpa, input$missingCode, progress)
    } else if (identical(input$kind, "qca")) {
      progress("qca", "Calibrating and minimising the truth table", 0.2)
      r <- run_qca(input$data, input$qca, input$missingCode)
      r$status <- "completed"
      r$completedAt <- now_iso()
      r
    } else {
      r <- run_analysis(input$data, input$model, input$settings, input$datasetMeta, progress)
      r$rCode <- generate_r_code(input$model, input$settings, input$datasetMeta,
                                 env = r$environment, generated_at = r$completedAt)
      r
    }
  }, analysis_failure = function(e) {
    list(status = "failed", error = list(stage = e$stage, message = conditionMessage(e), hint = e$hint, detail = e$detail))
  }, pathlab_error = function(e) {
    list(status = "failed", error = list(stage = "validation", message = conditionMessage(e), hint = e$hint, details = e$details))
  }, error = function(e) {
    list(status = "failed", error = list(stage = "unknown", message = "The analysis stopped unexpectedly.",
                                          hint = "Check the model and settings and try again. Your data and model are unchanged.",
                                          detail = sanitize_message(conditionMessage(e))))
  })
  # Echo the exact submitted model (raw JSON) and normalised settings for reproducibility.
  res$id <- input$resultId
  res$datasetId <- input$datasetId
  res$submittedAt <- input$submittedAt
  # simplifyVector = FALSE keeps JSON arrays as lists, so one-element arrays stay arrays.
  # Per-row values (scores, memberships) stay on the server, next to the result (derived variables).
  res <- save_result_derived(res, file.path(job_dir, "result.json"))
  if (!(input$kind %||% "plssem") %in% c("lpa", "ml", "qca")) res$model <- jsonlite::fromJSON(input$modelJson, simplifyVector = FALSE)
  if (!(input$kind %||% "plssem") %in% c("cbsem", "gsca", "lpa", "ml", "qca")) res$settings <- input$settings
  tmp <- file.path(job_dir, "result.json.tmp")
  jsonlite::write_json(res, tmp, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  file.rename(tmp, file.path(job_dir, "result.json"))
  invisible(TRUE)
}
