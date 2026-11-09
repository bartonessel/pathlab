# Regression analysis (linear, logistic, Poisson) with hierarchical blocks.
# Estimates come from stats::lm / stats::glm; robust (heteroscedasticity-consistent) standard errors from
# sandwich::vcovHC with lmtest::coeftest / coefci / waldtest; the omnibus (type 3) tests from car::Anova and the
# estimated marginal means from emmeans, as in jamovi. The data are never changed: the project missing-value code
# is treated as missing for this analysis, rows with missing values in any model variable are left out (listwise,
# so every block uses the same rows), and centring, factor coding and reference levels only affect the model copy.

REG_FAMILIES <- c(gaussian = "Linear (OLS)", binomial = "Logistic", poisson = "Poisson")
REG_ROBUST <- c("none", "HC0", "HC1", "HC2", "HC3", "HC4")

normalize_regression <- function(raw) {
  raw <- raw %||% list()
  pick <- function(v, allowed, default) { v <- as.character(v %||% default)[1]; if (v %in% allowed) v else default }
  blocks <- lapply(raw$blocks %||% list(), function(b) list(
    predictors = unique(as_chr_vec(b$predictors)),
    interactions = Filter(function(x) !is.null(x$predictor) && !is.null(x$moderator), lapply(b$interactions %||% list(), function(x) list(
      predictor = if (is.null(x$predictor)) NULL else as.character(x$predictor)[1],
      moderator = if (is.null(x$moderator)) NULL else as.character(x$moderator)[1])))
  ))
  level <- function(v, default) {
    x <- suppressWarnings(as.numeric(v %||% default))
    if (is.na(x) || x <= 0.5 || x >= 1) api_error(400, "invalid_regression", "The confidence level must be between 0.5 and 1.")
    x
  }
  ci <- level(raw$ciLevel, 0.95)
  emm <- raw$emm %||% list()
  ref <- raw$refLevels %||% list()
  list(
    outcome = as.character(raw$outcome %||% "")[1],
    blocks = blocks,
    family = pick(raw$family, names(REG_FAMILIES), "gaussian"),
    robust = pick(raw$robust, REG_ROBUST, "none"),
    center = !isFALSE(raw$center),
    ciLevel = ci,
    # Case weights (jamovi's Weights), categorical predictors (factors, dummy coded) and their reference levels.
    weights = as.character(raw$weights %||% "")[1],
    factors = unique(as_chr_vec(raw$factors)),
    refLevels = if (length(ref)) as.list(vapply(ref, function(x) as.character(x %||% "")[1], "")) else list(),
    # Estimated marginal means: terms of one or two predictors.
    emm = list(terms = Filter(length, lapply(emm$terms %||% list(), function(t) utils::head(unique(as_chr_vec(t)), 2))),
               equalWeights = !isFALSE(emm$equalWeights), ciLevel = level(emm$ciLevel, ci))
  )
}

reg_issue <- function(severity, code, message, hint = NULL) list(severity = severity, code = code, message = message, hint = hint)

reg_terms <- function(spec, upto = length(spec$blocks)) {
  bl <- spec$blocks[seq_len(upto)]
  list(predictors = unique(unlist(lapply(bl, `[[`, "predictors"))),
       interactions = do.call(c, lapply(bl, `[[`, "interactions")) %||% list())
}

#' Problems that stop the regression (errors) or deserve attention (warnings).
validate_regression <- function(df, spec) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- reg_issue(...)
  y <- spec$outcome
  if (!nzchar(y)) add("error", "reg_outcome", "Choose the outcome (dependent variable).")
  else if (!y %in% names(df)) add("error", "reg_outcome", sprintf("The outcome '%s' is not in the dataset.", y))
  # A logistic outcome may also be text, a factor or TRUE/FALSE (two categories, as in jamovi); the others need numbers.
  else if (!is.numeric(df[[y]]) && !(spec$family == "binomial" && (is.character(df[[y]]) || is.factor(df[[y]]) || is.logical(df[[y]]))))
    add("error", "reg_outcome", if (spec$family == "binomial") "The outcome must have two categories (numbers or text)." else "The outcome must be numeric.")
  if (!length(spec$blocks) || !length(spec$blocks[[1]]$predictors)) add("error", "reg_predictors", "Add at least one predictor to block 1.")
  w <- spec$weights %||% ""
  if (nzchar(w)) {
    if (!w %in% names(df)) add("error", "reg_weights", sprintf("The weights variable '%s' is not in the dataset.", w))
    else if (!is.numeric(df[[w]])) add("error", "reg_weights", "The weights variable must be numeric.")
    else if (any(df[[w]] < 0, na.rm = TRUE)) add("error", "reg_weights", "Weights cannot be negative.")
  }
  seen <- character(0)
  for (k in seq_along(spec$blocks)) {
    b <- spec$blocks[[k]]
    if (!length(b$predictors) && !length(b$interactions)) add("error", "reg_empty_block", sprintf("Block %d is empty. Add predictors or remove the block.", k))
    for (v in b$predictors) {
      if (!v %in% names(df)) add("error", "reg_missing_column", sprintf("'%s' is not in the dataset.", v))
      else if (!is.numeric(df[[v]]) && !v %in% spec$factors) add("error", "reg_not_numeric", sprintf("'%s' is not numeric. Set its measurement level to nominal to use it as a factor.", v))
      if (identical(v, y)) add("error", "reg_outcome_as_predictor", sprintf("'%s' is the outcome and cannot also be a predictor.", v))
      if (v %in% seen) add("error", "reg_duplicate", sprintf("'%s' is entered in more than one block.", v))
    }
    seen <- c(seen, b$predictors)
    for (x in b$interactions) {
      if (identical(x$predictor, x$moderator)) add("error", "reg_interaction", "An interaction needs two different variables.")
      if (any(c(x$predictor, x$moderator) %in% spec$factors)) add("warning", "reg_factor_interaction",
        sprintf("%s \u00d7 %s involves a factor: its coefficients are shown, but no simple slopes.", x$predictor, x$moderator))
      miss <- setdiff(c(x$predictor, x$moderator), seen)
      if (length(miss)) add("error", "reg_interaction", sprintf("Interaction %s × %s: enter %s as a predictor in this or an earlier block first (main effects are required).",
                                                              x$predictor, x$moderator, paste(miss, collapse = " and ")))
    }
  }
  if (nzchar(y) && y %in% names(df) && (is.numeric(df[[y]]) || spec$family == "binomial")) {
    vals <- unique(stats::na.omit(df[[y]]))
    if (!is.numeric(vals)) vals <- as.character(vals)
    if (spec$family == "binomial" && length(vals) != 2)
      add("error", "reg_binary", sprintf("Logistic regression needs an outcome with exactly two values; '%s' has %d.", y, length(vals)))
    if (spec$family == "poisson" && is.numeric(vals) && (any(vals < 0) || any(abs(vals - round(vals)) > 1e-8)))
      add("error", "reg_counts", "Poisson regression needs a count outcome (whole numbers of 0 or more).")
    if (spec$family == "gaussian" && length(vals) <= 2) add("warning", "reg_binary_ols", "The outcome has only two values; logistic regression is usually more appropriate.")
  }
  out
}

term_label <- function(t) gsub(":", " × ", gsub("`", "", t, fixed = TRUE), fixed = TRUE)

#' Label of a model-matrix column, with factor dummies as jamovi writes them ("Gender: Male – Female").
#' `fac`: list(variable = list(levels, ref)).
coef_label <- function(t, fac) {
  parts <- strsplit(gsub("`", "", t, fixed = TRUE), ":", fixed = TRUE)[[1]]
  paste(vapply(parts, function(p) {
    for (v in names(fac)) { lv <- substring(p, nchar(v) + 1); if (startsWith(p, v) && lv %in% fac[[v]]$levels) return(paste0(v, ": ", lv, " \u2013 ", fac[[v]]$ref)) }
    p
  }, ""), collapse = " \u00d7 ")
}
#' Whether a model-matrix column belongs to a factor (a dummy, or an interaction with one).
coef_factor <- function(t, fac) {
  parts <- strsplit(gsub("`", "", t, fixed = TRUE), ":", fixed = TRUE)[[1]]
  any(vapply(parts, function(p) any(vapply(names(fac), function(v) startsWith(p, v) && substring(p, nchar(v) + 1) %in% fac[[v]]$levels, TRUE)), TRUE))
}
reg_bq <- function(v) paste0("`", gsub("`", "", v), "`")

reg_formula <- function(y, preds, ints) {
  rhs <- c(vapply(preds, reg_bq, ""), vapply(ints, function(x) paste0(reg_bq(x$predictor), ":", reg_bq(x$moderator)), ""))
  stats::as.formula(paste(reg_bq(y), "~", if (length(rhs)) paste(rhs, collapse = " + ") else "1"))
}

run_regression <- function(df, spec, missing_code = NULL) {
  issues <- validate_regression(df, spec)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE)))
    api_error(422, "invalid_regression", "The regression cannot run until the listed problems are fixed.", details = issues)
  t0 <- Sys.time()
  warn <- vapply(Filter(function(i) i$severity == "warning", issues), `[[`, "", "message")
  y <- spec$outcome
  all_terms <- reg_terms(spec)
  facs <- intersect(spec$factors, all_terms$predictors)
  int_vars <- setdiff(unique(unlist(lapply(all_terms$interactions, function(x) c(x$predictor, x$moderator)))), facs)
  wv <- if (nzchar(spec$weights %||% "")) spec$weights else NULL
  vars <- unique(c(y, all_terms$predictors, wv))
  d <- df[, vars, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in vars) {
    if (!is.numeric(d[[cn]])) next
    hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code
    recoded <- recoded + sum(hit)
    d[[cn]][hit] <- NA
  }
  for (v in facs) { x <- trimws(as.character(d[[v]])); x[!nzchar(x)] <- NA; d[[v]] <- x }
  keep <- stats::complete.cases(d)
  rows <- which(keep)
  d <- d[keep, , drop = FALSE]
  n <- nrow(d)
  # Factors: dummy coded against the reference level (the first level unless one is chosen), as in jamovi.
  fac <- list()
  for (v in facs) {
    lv <- sort(unique(d[[v]]))
    if (length(lv) < 2) api_error(422, "reg_factor_levels", sprintf("The factor '%s' has fewer than two levels in the analysed rows.", v))
    ref <- spec$refLevels[[v]] %||% ""
    if (!ref %in% lv) ref <- lv[1]
    d[[v]] <- stats::relevel(factor(d[[v]], levels = lv), ref = ref)
    fac[[v]] <- list(levels = lv, ref = ref)
  }
  wts <- if (is.null(wv)) NULL else d[[wv]]
  k_max <- length(all_terms$predictors) + length(all_terms$interactions)
  if (n <= k_max + 1) api_error(422, "reg_too_few_rows", sprintf("Only %d complete rows for %d terms.", n, k_max), "Remove predictors or check the missing values.")
  if (n < 10 * k_max) warn <- c(warn, sprintf("There are %.1f cases per term; at least 10 to 20 are usually recommended.", n / k_max))

  event <- NULL
  if (spec$family == "binomial") {
    lv <- sort(unique(d[[y]]))   # numbers ascending, text alphabetically, factors in level order, FALSE before TRUE
    if (is.factor(lv)) lv <- as.character(lv)
    event <- list(level = lv[2], reference = lv[1])
    d[[y]] <- as.integer(d[[y]] == lv[2])   # model copy only: the higher value is the event (1)
  }
  centred <- list()
  if (spec$center && length(int_vars)) for (v in int_vars) {
    centred[[v]] <- mean(d[[v]])
    d[[v]] <- d[[v]] - centred[[v]]
  }

  fam <- switch(spec$family, gaussian = NULL, binomial = stats::binomial(), poisson = stats::poisson())
  # Weights go in as a vector (do.call), then the call is shortened so later re-evaluation (emmeans) finds `d` and `w` here.
  fit_one <- function(f) {
    environment(f) <- environment()
    args <- c(list(formula = f, data = d), if (!is.null(wts)) list(weights = wts), if (!is.null(fam)) list(family = fam))
    m <- do.call(if (is.null(fam)) stats::lm else stats::glm, args)
    m$call$data <- quote(d); if (!is.null(wts)) m$call$weights <- quote(wts)
    m
  }
  vc <- function(m) if (spec$robust == "none") stats::vcov(m) else sandwich::vcovHC(m, type = spec$robust)
  null_fit <- fit_one(stats::as.formula(paste(reg_bq(y), "~ 1")))
  sd_y <- if (is.null(fam)) stats::sd(d[[y]]) else NA_real_
  # SD scaling a factor column to the standardised estimate: factor dummies are not standardised (as in jamovi), the
  # numeric variables they interact with are.
  sd_of <- function(t) {
    if (!coef_factor(t, fac)) return(NA_real_)
    parts <- strsplit(gsub("`", "", t, fixed = TRUE), ":", fixed = TRUE)[[1]]
    num <- parts[parts %in% names(d) & !parts %in% facs]
    prod(vapply(num, function(v) stats::sd(d[[v]]), 0))
  }

  fits <- list()
  blocks_out <- list()
  prev <- null_fit
  for (k in seq_along(spec$blocks)) {
    tk <- reg_terms(spec, k)
    m <- fit_one(reg_formula(y, tk$predictors, tk$interactions))
    if (any(is.na(stats::coef(m)))) api_error(422, "reg_singular", sprintf("Block %d: some coefficients cannot be estimated (perfectly collinear predictors).", k),
                                              "Remove predictors that are exact combinations of others.")
    fits[[k]] <- m
    V <- vc(m)
    ct <- lmtest::coeftest(m, vcov. = V)
    ci <- lmtest::coefci(m, vcov. = V, level = spec$ciLevel)
    X <- stats::model.matrix(m)
    vif <- if (ncol(X) > 2) diag(solve(stats::cor(X[, -1, drop = FALSE]))) else NULL
    coefs <- lapply(rownames(ct), function(t) {
      is_int <- t == "(Intercept)"
      b <- unname(ct[t, 1])
      row <- list(term = if (is_int) "(Intercept)" else coef_label(t, fac), b = b, se = unname(ct[t, 2]), stat = unname(ct[t, 3]), p = unname(ct[t, 4]),
                  ciLower = unname(ci[t, 1]), ciUpper = unname(ci[t, 2]), interaction = grepl(":", t, fixed = TRUE))
      if (!is_int && length(fac)) row$factor <- coef_factor(t, fac)
      if (!is_int && is.null(fam)) {
        sc <- sd_of(t); sc <- (if (is.na(sc)) stats::sd(X[, t]) else sc) / sd_y
        row$beta <- b * sc; row$betaLower <- row$ciLower * sc; row$betaUpper <- row$ciUpper * sc
      }
      if (!is_int && !is.null(vif)) row$vif <- unname(vif[t])
      if (!is.null(fam)) { row$expB <- exp(b); row$expLower <- exp(row$ciLower); row$expUpper <- exp(row$ciUpper) }
      row
    })
    # Change test against the previous block (block 1: against the intercept-only model).
    change <- if (spec$robust == "none") {
      if (is.null(fam)) { a <- stats::anova(prev, m); list(stat = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p = a$`Pr(>F)`[2], test = "F") }
      else { a <- stats::anova(prev, m, test = "LRT"); list(stat = a$Deviance[2], df1 = a$Df[2], p = a$`Pr(>Chi)`[2], test = "LR chi-square") }
    } else {
      w <- lmtest::waldtest(prev, m, vcov = vc, test = if (is.null(fam)) "F" else "Chisq")
      if (is.null(fam)) list(stat = w$F[2], df1 = abs(w$Df[2]), df2 = w$Res.Df[2], p = w$`Pr(>F)`[2], test = paste0("Wald F (", spec$robust, ")"))
      else list(stat = w$Chisq[2], df1 = abs(w$Df[2]), p = w$`Pr(>Chisq)`[2], test = paste0("Wald chi-square (", spec$robust, ")"))
    }
    summ <- if (is.null(fam)) {
      s <- summary(m)
      fs <- s$fstatistic
      r2_prev <- if (k == 1) 0 else summary(fits[[k - 1]])$r.squared
      list(r2 = s$r.squared, adjR2 = s$adj.r.squared, deltaR2 = s$r.squared - r2_prev,
           f = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]), p = unname(stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)),
           rmse = sqrt(mean(stats::residuals(m)^2)), sigma = s$sigma, aic = stats::AIC(m), bic = stats::BIC(m))
    } else {
      ll <- as.numeric(stats::logLik(m)); ll0 <- as.numeric(stats::logLik(null_fit))
      mcf_prev <- if (k == 1) 0 else 1 - as.numeric(stats::logLik(fits[[k - 1]])) / ll0
      out <- list(deviance = stats::deviance(m), nullDeviance = stats::deviance(null_fit), dfResidual = stats::df.residual(m),
                  logLik = ll, aic = stats::AIC(m), bic = stats::BIC(m), mcfaddenR2 = 1 - ll / ll0, deltaMcfadden = (1 - ll / ll0) - mcf_prev)
      if (spec$family == "binomial") {
        cs <- 1 - exp((2 / n) * (ll0 - ll))
        out$coxSnellR2 <- cs
        out$nagelkerkeR2 <- cs / (1 - exp((2 / n) * ll0))
      }
      if (spec$family == "poisson") out$dispersion <- sum(stats::residuals(m, type = "pearson")^2) / stats::df.residual(m)
      out
    }
    if (is.null(fam)) summ$r <- sqrt(summ$r2)
    blocks_out[[k]] <- list(block = k, added = as.list(c(spec$blocks[[k]]$predictors, vapply(spec$blocks[[k]]$interactions, function(x) paste(x$predictor, "×", x$moderator), ""))),
                            summary = summ, change = change, coefficients = coefs, anova = reg_anova(m, spec, fam, V))
    prev <- m
  }

  final <- fits[[length(fits)]]
  Vf <- vc(final)
  # Simple slopes (final model): slope of the predictor at the moderator's mean − 1 SD, mean and mean + 1 SD.
  slopes <- lapply(Filter(function(x) !any(c(x$predictor, x$moderator) %in% facs), all_terms$interactions), function(x) {
    tn <- paste0(x$predictor, ":", x$moderator)
    if (!tn %in% names(stats::coef(final))) tn <- paste0(x$moderator, ":", x$predictor)
    b <- stats::coef(final)
    wm <- mean(d[[x$moderator]]); ws <- stats::sd(d[[x$moderator]])
    dfr <- if (is.null(fam)) stats::df.residual(final) else Inf
    z <- stats::qt(1 - (1 - spec$ciLevel) / 2, dfr)
    pts <- lapply(c(-1, 0, 1), function(s) {
      w <- wm + s * ws
      est <- unname(b[x$predictor] + b[tn] * w)
      se <- sqrt(Vf[x$predictor, x$predictor] + w^2 * Vf[tn, tn] + 2 * w * Vf[x$predictor, tn])
      tval <- est / se
      list(level = c("Mean − 1 SD", "Mean", "Mean + 1 SD")[s + 2], moderatorValue = w + (centred[[x$moderator]] %||% 0),
           slope = est, se = unname(se), stat = unname(tval), p = unname(2 * stats::pt(-abs(tval), dfr)),
           ciLower = unname(est - z * se), ciUpper = unname(est + z * se))
    })
    list(predictor = x$predictor, moderator = x$moderator, interaction = unname(b[tn]), points = pts)
  })

  diagnostics <- reg_diagnostics(final, rows, spec, fam, n)
  maha <- attr(diagnostics, "mahalanobis")
  emm <- reg_emm(final, d, spec, fac, Vf, centred)
  warn <- c(warn, attr(emm, "warnings"))
  attr(emm, "warnings") <- NULL
  if (!is.null(diagnostics$dispersionWarning)) warn <- c(warn, diagnostics$dispersionWarning)
  if (!is.null(fam) && spec$family == "binomial" && any(stats::fitted(final) < 1e-8 | stats::fitted(final) > 1 - 1e-8))
    warn <- c(warn, "Some fitted probabilities are 0 or 1 (separation); coefficients and standard errors may be unreliable.")
  vifs <- unlist(lapply(blocks_out[[length(blocks_out)]]$coefficients, function(r) if (!isTRUE(r$interaction)) r$vif))
  if (length(vifs) && any(vifs > 5)) warn <- c(warn, "Some predictors have VIF above 5 (collinearity).")

  list(
    spec = spec,
    familyLabel = unname(REG_FAMILIES[spec$family]),
    n = list(input = nrow(df), used = n, rowsRemoved = nrow(df) - n, cellsRecoded = recoded),
    event = event,
    centred = if (length(centred)) lapply(names(centred), function(v) list(variable = v, mean = centred[[v]])) else list(),
    blocks = blocks_out,
    simpleSlopes = slopes,
    diagnostics = diagnostics,
    factors = if (length(fac)) lapply(names(fac), function(v) list(variable = v, levels = as.list(fac[[v]]$levels), reference = fac[[v]]$ref)) else list(),
    weights = wv,
    emm = emm,
    # Per-row values jamovi's Save writes as variables (rows not analysed stay missing).
    .derived = list(nInput = nrow(df), rows = rows, tables = list(values = data.frame(
      predicted = unname(stats::fitted(final)), residual = unname(stats::residuals(final, type = "response")),
      cooks = unname(stats::cooks.distance(final)), mahalanobis = if (is.null(maha)) rep(NA_real_, n) else maha))),
    warnings = as.list(unique(warn)),
    rCode = regression_code(spec, missing_code, centred),
    packages = list(sandwich = as.character(utils::packageVersion("sandwich")), lmtest = as.character(utils::packageVersion("lmtest")),
                    car = as.character(utils::packageVersion("car")), emmeans = as.character(utils::packageVersion("emmeans"))),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

reg_diagnostics <- function(m, rows, spec, fam, n) {
  rs <- if (is.null(fam)) stats::rstandard(m) else stats::rstandard(m, type = "deviance")
  fv <- stats::fitted(m)
  cook <- stats::cooks.distance(m)
  lev <- stats::hatvalues(m)
  idx <- if (n > 1500) round(seq(1, n, length.out = 1500)) else seq_len(n)
  ord <- order(rs)
  qq_idx <- if (n > 1500) ord[round(seq(1, n, length.out = 1500))] else ord
  thr <- 4 / n
  top <- utils::head(order(cook, decreasing = TRUE), 10)
  out <- list(
    residuals = lapply(idx, function(i) list(fitted = unname(fv[i]), residual = unname(rs[i]))),
    qq = lapply(seq_along(qq_idx), function(j) list(theoretical = stats::qnorm((match(qq_idx[j], ord) - 0.5) / n), sample = unname(rs[qq_idx[j]]))),
    cooksThreshold = thr,
    nInfluential = sum(cook > thr, na.rm = TRUE),
    influential = lapply(top, function(i) list(row = rows[i], cooksDistance = unname(cook[i]), leverage = unname(lev[i]), stdResidual = unname(rs[i]), flagged = cook[i] > thr)),
    cooksSummary = reg_summary5(cook)
  )
  # Mahalanobis distance of the predictors (the main-effect columns of the model matrix), as jamovi's Data Summary.
  X <- stats::model.matrix(m)
  asg <- attr(X, "assign"); labs <- attr(stats::terms(m), "term.labels")
  X <- X[, asg > 0 & !grepl(":", labs[pmax(asg, 1)], fixed = TRUE), drop = FALSE]
  d2 <- if (ncol(X)) tryCatch(stats::mahalanobis(X, colMeans(X), stats::cov(X)), error = function(e) NULL) else NULL
  maha <- NULL
  if (!is.null(d2)) {
    p <- stats::pchisq(d2, df = ncol(X), lower.tail = FALSE)
    o <- order(p); o <- utils::head(o[p[o] < 0.05], 50)
    out$mahalanobis <- list(df = ncol(X), summary = reg_summary5(d2),
                            counts = list(p05 = sum(p < 0.05), p01 = sum(p < 0.01), p001 = sum(p < 0.001)),
                            cases = lapply(o, function(i) list(row = rows[i], d2 = unname(d2[i]), p = unname(p[i]))))
    maha <- unname(d2)
  }
  if (is.null(fam)) {
    bp <- lmtest::bptest(m)
    out$breuschPagan <- list(stat = unname(bp$statistic), df = unname(bp$parameter), p = unname(bp$p.value))
    r <- stats::residuals(m)
    if (n >= 3 && n <= 5000) { sw <- stats::shapiro.test(r); out$shapiro <- list(stat = unname(sw$statistic), p = unname(sw$p.value)) }
    # Durbin-Watson test for first-order autocorrelation of the residuals (rows in data order), two-sided.
    dw <- tryCatch(lmtest::dwtest(m, alternative = "two.sided"), error = function(e) NULL)
    if (!is.null(dw)) out$durbinWatson <- list(stat = unname(dw$statistic), p = unname(dw$p.value))
  }
  if (!is.null(fam) && spec$family == "poisson") {
    disp <- sum(stats::residuals(m, type = "pearson")^2) / stats::df.residual(m)
    if (disp > 1.5) out$dispersionWarning <- sprintf("Overdispersion: Pearson dispersion = %.2f (above 1.5). Use robust standard errors or consider a negative binomial model.", disp)
  }
  if (!is.null(fam) && spec$family == "binomial") {
    pred <- as.integer(fv >= 0.5)
    obs <- stats::model.response(stats::model.frame(m))
    tp <- sum(pred == 1 & obs == 1); tn <- sum(pred == 0 & obs == 0); fp <- sum(pred == 1 & obs == 0); fn <- sum(pred == 0 & obs == 1)
    out$classification <- list(cutoff = 0.5, truePositive = tp, trueNegative = tn, falsePositive = fp, falseNegative = fn,
                               accuracy = (tp + tn) / n, sensitivity = if (tp + fn) tp / (tp + fn) else NA, specificity = if (tn + fp) tn / (tn + fp) else NA)
    out$roc <- reg_roc(obs, fv)
  }
  attr(out, "mahalanobis") <- maha
  out
}

reg_summary5 <- function(x) list(mean = mean(x), median = stats::median(x), sd = stats::sd(x), min = min(x), max = max(x))

#' Omnibus tests of the model terms (car::Anova, type 3, as jamovi's ANOVA test): F tests with sums of squares for a
#' linear model (Wald F with robust standard errors), likelihood-ratio chi-square for a GLM (Wald with robust SEs).
reg_anova <- function(m, spec, fam, V) {
  rob <- spec$robust != "none"
  a <- tryCatch(suppressMessages(if (is.null(fam)) { if (rob) car::Anova(m, type = 3, vcov. = V) else car::Anova(m, type = 3) }
                                 else if (rob) car::Anova(m, type = 3, test.statistic = "Wald", vcov. = V) else car::Anova(m, type = 3, test.statistic = "LR")),
                error = function(e) NULL)
  if (is.null(a)) return(NULL)
  tab <- as.data.frame(a)
  nm <- rownames(tab)
  stat <- intersect(c("F value", "F", "Chisq", "LR Chisq"), names(tab))[1]
  pc <- grep("^Pr\\(", names(tab), value = TRUE)[1]
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    t <- nm[i]
    if (t == "(Intercept)") return(NULL)
    r <- list(term = if (t == "Residuals") "Residuals" else term_label(t), df = unname(tab[i, "Df"]))
    if ("Sum Sq" %in% names(tab)) { r$ss <- unname(tab[i, "Sum Sq"]); r$ms <- r$ss / r$df }
    if (t != "Residuals") { r$stat <- unname(tab[i, stat]); r$p <- unname(tab[i, pc]) }
    r
  })
  list(test = if (is.null(fam)) (if (rob) paste0("Wald F (", spec$robust, ")") else "F") else if (rob) paste0("Wald chi-square (", spec$robust, ")") else "LR chi-square",
       rows = Filter(Negate(is.null), rows))
}

#' Estimated marginal means (emmeans, as jamovi): for each term, the predicted outcome at the levels of its factors and
#' at the mean and mean ± 1 SD of its covariates, the other predictors held at their means (factors averaged with
#' equal or proportional weights); on the response scale for a GLM.
reg_emm <- function(m, d, spec, fac, V, centred) {
  warns <- character(0)
  out <- Filter(Negate(is.null), lapply(spec$emm$terms, function(tv) {
    tv <- intersect(tv, names(d))
    if (!length(tv)) return(NULL)
    at <- list(); lab <- list()
    for (v in tv) {
      if (!is.null(fac[[v]])) { lab[[v]] <- levels(d[[v]]); next }
      mu <- mean(d[[v]]); s <- stats::sd(d[[v]]); at[[v]] <- c(mu - s, mu, mu + s)
      lab[[v]] <- sprintf("%s (%s)", c("Mean \u2212 1 SD", "Mean", "Mean + 1 SD"), formatC(at[[v]] + (centred[[v]] %||% 0), format = "f", digits = 2))
    }
    em <- tryCatch(suppressMessages(emmeans::emmeans(m, specs = tv, at = at, weights = if (spec$emm$equalWeights) "equal" else "proportional",
                                                    vcov. = V, level = spec$emm$ciLevel, type = "response", data = d)),
                   error = function(e) { warns <<- c(warns, sprintf("Marginal means of %s could not be computed: %s", paste(tv, collapse = " \u00d7 "), conditionMessage(e))); NULL })
    if (is.null(em)) return(NULL)
    tab <- as.data.frame(summary(em))
    est <- setdiff(names(tab), c(tv, "SE", "df", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL"))[1]
    lo <- intersect(c("lower.CL", "asymp.LCL"), names(tab))[1]; hi <- intersect(c("upper.CL", "asymp.UCL"), names(tab))[1]
    lvl <- function(v, x) if (!is.null(fac[[v]])) as.character(x) else lab[[v]][which.min(abs(at[[v]] - as.numeric(x)))]
    list(terms = as.list(tv), levels = lapply(tv, function(v) as.list(lab[[v]])),
         rows = lapply(seq_len(nrow(tab)), function(i) list(levels = lapply(tv, function(v) lvl(v, tab[[v]][i])),
                                                              est = unname(tab[[est]][i]), se = unname(tab$SE[i]), lower = unname(tab[[lo]][i]), upper = unname(tab[[hi]][i]))))
  }))
  attr(out, "warnings") <- warns
  out
}

#' ROC curve of a logistic regression's fitted probabilities (pROC): AUC with DeLong CI, the curve (at most
#' ~200 points) and the cut-off that maximises Youden's J (sensitivity + specificity - 1).
reg_roc <- function(obs, prob) {
  if (length(unique(obs)) != 2) return(NULL)
  rc <- tryCatch(pROC::roc(obs, prob, levels = c(0, 1), direction = "<", quiet = TRUE), error = function(e) NULL)
  if (is.null(rc)) return(NULL)
  ci <- tryCatch(as.numeric(pROC::ci.auc(rc, method = "delong")), error = function(e) c(NA, NA, NA))
  fpr <- 1 - rc$specificities; tpr <- rc$sensitivities
  o <- order(fpr, tpr)
  fpr <- fpr[o]; tpr <- tpr[o]
  sel <- if (length(fpr) > 200) unique(c(1, round(seq(1, length(fpr), length.out = 200)), length(fpr))) else seq_along(fpr)
  j <- rc$sensitivities + rc$specificities - 1
  best <- which.max(j)
  list(auc = as.numeric(pROC::auc(rc)), ciLower = ci[1], ciUpper = ci[3],
       fpr = as.list(fpr[sel]), tpr = as.list(tpr[sel]),
       youden = list(cutoff = rc$thresholds[best], sensitivity = rc$sensitivities[best], specificity = rc$specificities[best]))
}

#' R code reproducing the regression (dataset object `data`).
regression_code <- function(spec, missing_code, centred) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  y <- spec$outcome
  all_terms <- reg_terms(spec)
  wv <- if (nzchar(spec$weights %||% "")) spec$weights else NULL
  facs <- intersect(spec$factors, all_terms$predictors)
  vars <- unique(c(y, all_terms$predictors, wv))
  fam <- switch(spec$family, gaussian = NULL, binomial = "binomial()", poisson = "poisson()")
  wt <- if (is.null(wv)) "" else paste0(", weights = ", reg_bq(wv))
  fit_call <- function(f) if (is.null(fam)) paste0("lm(", f, ", data = d", wt, ")") else paste0("glm(", f, ", family = ", fam, ", data = d", wt, ")")
  lines <- c("# Regression generated by PathLab SEM",
             if (spec$robust != "none") c("library(sandwich)", "library(lmtest)") else "library(lmtest)",
             "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
             paste0("d <- data[, c(", paste(vapply(vars, q, ""), collapse = ", "), ")]"))
  if (!is.null(missing_code)) lines <- c(lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  lines <- c(lines, "d <- d[complete.cases(d), ]   # listwise: the same rows in every block")
  for (v in facs) {
    ref <- spec$refLevels[[v]] %||% ""
    lines <- c(lines, paste0("d[[", q(v), "]] <- factor(d[[", q(v), "]])", if (nzchar(ref)) paste0("; d[[", q(v), "]] <- relevel(d[[", q(v), "]], ref = ", q(ref), ")") else "", "   # factor (dummy coded)"))
  }
  if (spec$family == "binomial") lines <- c(lines, paste0("d[[", q(y), "]] <- as.integer(d[[", q(y), "]] == sort(unique(d[[", q(y), "]]))[2])   # second value (higher number, or text in alphabetical order) = event (1)"))
  for (v in names(centred)) lines <- c(lines, paste0("d[[", q(v), "]] <- d[[", q(v), "]] - mean(d[[", q(v), "]])   # mean-centred for the interaction"))
  vcv <- if (spec$robust == "none") "vcov" else paste0("function(x) vcovHC(x, type = \"", spec$robust, "\")")
  lines <- c(lines, paste0("m0 <- ", fit_call(paste(reg_bq(y), "~ 1"))))
  for (k in seq_along(spec$blocks)) {
    tk <- reg_terms(spec, k)
    f <- paste(deparse(reg_formula(y, tk$predictors, tk$interactions), width.cutoff = 500L), collapse = " ")
    lines <- c(lines, paste0("m", k, " <- ", fit_call(f)),
               paste0("coeftest(m", k, ", vcov. = ", if (spec$robust == "none") "vcov(m" else "vcovHC(m", k, if (spec$robust == "none") ")" else paste0(", type = \"", spec$robust, "\")"), ")"),
               paste0("coefci(m", k, ", vcov. = ", if (spec$robust == "none") "vcov(m" else "vcovHC(m", k, if (spec$robust == "none") ")" else paste0(", type = \"", spec$robust, "\")"), ", level = ", spec$ciLevel, ")"))
    if (is.null(fam)) lines <- c(lines, paste0("summary(m", k, ")$r.squared"))
    lines <- c(lines, if (spec$robust == "none") {
      if (is.null(fam)) paste0("anova(m", k - 1, ", m", k, ")   # change test") else paste0("anova(m", k - 1, ", m", k, ", test = \"LRT\")   # change test")
    } else paste0("waldtest(m", k - 1, ", m", k, ", vcov = ", vcv, if (is.null(fam)) ")" else ", test = \"Chisq\")", "   # change test"))
  }
  mk <- paste0("m", length(spec$blocks))
  lines <- c(lines, paste0("car::Anova(", mk, ", type = 3", if (!is.null(fam)) ", test.statistic = \"LR\"" else "", ")   # omnibus tests of the terms"))
  if (is.null(fam) && is.null(wv)) lines <- c(lines, paste0("dwtest(", mk, ", alternative = \"two.sided\")   # Durbin-Watson autocorrelation test (not for weighted models)"))
  for (tv in spec$emm$terms) lines <- c(lines, paste0("emmeans::emmeans(", mk, ", specs = c(", paste(vapply(tv, q, ""), collapse = ", "), "), weights = \"",
                                                     if (spec$emm$equalWeights) "equal" else "proportional", "\", type = \"response\", level = ", spec$emm$ciLevel,
                                                     ")   # marginal means (covariates at their mean; jamovi also uses mean \u00b1 1 SD)"))
  if (spec$family == "binomial") lines <- c(lines, "library(pROC)",
    paste0("roc_m <- roc(d[[", q(y), "]], fitted(", mk, "), levels = c(0, 1), direction = \"<\")   # ROC curve"),
    "auc(roc_m); ci.auc(roc_m, method = \"delong\")",
    "coords(roc_m, \"best\", best.method = \"youden\")   # cut-off maximising Youden's J",
    "plot(roc_m, legacy.axes = TRUE)")
  paste(lines, collapse = "\n")
}
