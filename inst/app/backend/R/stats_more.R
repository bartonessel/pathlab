# More Statistics procedures (SPSS-style Analyze menu coverage), each computed by an established R package:
# generalized linear models (MASS), linear and generalized linear mixed models (lme4 / lmerTest), two-stage least squares
# (ivreg), canonical correlation and runs test (stats), life tables (actuarial, base R), Bayesian linear regression
# (BayesFactor), ARIMA / exponential smoothing forecasts (forecast) and autocorrelations (stats), missing value analysis
# with Little's MCAR test (EM estimates, base R) and multiple imputation (mice), power analysis (pwr), meta-analysis
# (metafor), correspondence analysis (ca), multidimensional scaling (stats / MASS), latent growth curves (lavaan),
# k-means / PAM and hierarchical clustering (stats, cluster) and linear discriminant analysis (MASS).
# Every procedure returns list(tables, plots, n) in the Statistics result format; images are drawn with base graphics.

opt <- function(o, k, d = NULL) { v <- o[[k]]; if (is.null(v) || (is.character(v) && length(v) == 1 && !nzchar(v))) d else v }
num_opt <- function(o, k, d, lo = -Inf, hi = Inf) { v <- suppressWarnings(as.numeric(o[[k]] %||% d))[1]; if (!is.finite(v)) v <- d; min(hi, max(lo, v)) }
pfmt <- function(p) if (is.na(p)) "–" else if (p < 0.001) "< .001" else sprintf("%.3f", p)

#' A base-graphics figure as a Statistics image plot (PNG, base64).
stats_png <- function(draw, title = "", alt = title, w = 7, h = 4.6, dpi = 110) {
  f <- tempfile(fileext = ".png"); on.exit(unlink(f))
  grDevices::png(f, width = w * dpi, height = h * dpi, res = dpi, type = if (capabilities("cairo")) "cairo" else "windows")
  op <- graphics::par(mar = c(4.2, 4.2, 2.2, 1), family = "sans", cex = 0.9)
  ok <- tryCatch({ draw(); TRUE }, error = function(e) FALSE, finally = { graphics::par(op); grDevices::dev.off() })
  if (!ok) return(NULL)
  list(type = "image", png = jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)), width = w, height = h, title = title, alt = alt)
}

coef_rows <- function(est, se, stat, p, lower, upper, terms, extra = NULL) lapply(seq_along(est), function(i) {
  r <- list(term = gsub("`", "", terms[i]), b = unname(est[i]), se = unname(se[i]), stat = unname(stat[i]), p = unname(p[i]), lower = unname(lower[i]), upper = unname(upper[i]))
  if (!is.null(extra)) r <- c(r, lapply(extra, function(x) unname(x[i])))
  r
})
coef_cols <- function(stat = "z", ci = 95, extra = list()) c(list(stats_col("term", "Predictor", "text"), stats_col("b", "Estimate"), stats_col("se", "SE"), stats_col("stat", stat), stats_col("p", "p", pvalue = TRUE),
                                                                 stats_col("lower", "Lower", superTitle = sprintf("%g%% CI", ci)), stats_col("upper", "Upper", superTitle = sprintf("%g%% CI", ci))), extra)

model_frame <- function(d, dep, covs, factors) {
  dd <- data.frame(y = d[[dep]])
  for (v in covs) dd[[v]] <- as.numeric(d[[v]])
  for (v in factors) dd[[v]] <- factor(d[[v]])
  dd[stats::complete.cases(dd), , drop = FALSE]
}
rhs_of <- function(covs, factors) paste(sprintf("`%s`", c(covs, factors)), collapse = " + ")

# ---- Generalized linear model (incl. weighted least squares) -------------------------------------------------------
GLMX_FAMILIES <- c(gaussian = "Normal, identity link (OLS / weighted least squares)", probit = "Binomial, probit link", negbin = "Negative binomial, log link",
                   gamma = "Gamma, log link", invgauss = "Inverse Gaussian, log link", quasipoisson = "Quasi-Poisson, log link (overdispersed counts)")
run_glmx <- function(d, o) {
  fam <- opt(o, "family", "gaussian"); if (!fam %in% names(GLMX_FAMILIES)) fam <- "gaussian"
  covs <- opt(o, "covs", character(0)); facs <- opt(o, "factors", character(0)); w <- opt(o, "weights")
  dd <- model_frame(d, opt(o, "dep"), covs, facs)
  if (!is.null(w)) { dd$.w <- as.numeric(d[[w]])[as.integer(rownames(dd))]; dd <- dd[!is.na(dd$.w) & dd$.w > 0, , drop = FALSE] }
  if (fam %in% c("gamma", "invgauss") && any(dd$y <= 0, na.rm = TRUE)) api_error(422, "glm_positive", sprintf("%s model needs an outcome with values above 0; '%s' has %d value(s) of 0 or below.", if (fam == "gamma") "A Gamma" else "An inverse Gaussian", o$dep, sum(dd$y <= 0, na.rm = TRUE)), "Choose another family (e.g. negative binomial or quasi-Poisson for counts) or a positive outcome.")
  # Count models need counts: whole numbers of 0 or more (glm.nb otherwise warns "non-integer x" once per case).
  if (fam %in% c("negbin", "quasipoisson")) {
    yv <- dd$y[!is.na(dd$y)]
    if (!is.numeric(yv) || any(yv < 0) || any(abs(yv - round(yv)) > 1e-8))
      api_error(422, "glm_counts", sprintf("A %s model needs counts (whole numbers of 0 or more), but '%s' has %s.", if (fam == "negbin") "negative binomial" else "quasi-Poisson", o$dep,
                                           if (!is.numeric(yv)) "text values" else if (any(yv < 0)) "negative values" else "decimal values"),
                "For a continuous outcome choose the Normal family (or Gamma / inverse Gaussian for positive, skewed values).")
  }
  if (fam == "probit") { yy <- dd$y; lv <- sort(unique(yy)); if (length(lv) != 2) api_error(422, "glm_binary", "A probit model needs an outcome with exactly two values."); dd$y <- as.integer(yy == lv[2]) }
  form <- stats::as.formula(paste("y ~", rhs_of(covs, facs)))
  wt <- if (!is.null(w)) dd$.w else NULL
  fit <- switch(fam,
    gaussian = stats::glm(form, data = dd, weights = wt, family = stats::gaussian()),
    probit = stats::glm(form, data = dd, weights = wt, family = stats::binomial(link = "probit")),
    negbin = withCallingHandlers(MASS::glm.nb(form, data = dd, weights = wt), warning = function(w) {
      # glm.nb hits its iteration limit when theta grows without bound, i.e. the counts are not overdispersed.
      if (grepl("iteration limit|alternation limit", conditionMessage(w))) { warning("The negative binomial dispersion did not converge: the counts look no more spread out than Poisson counts. A Poisson or quasi-Poisson model may fit better.", call. = FALSE); invokeRestart("muffleWarning") }
    }),
    gamma = stats::glm(form, data = dd, weights = wt, family = stats::Gamma(link = "log")),
    invgauss = stats::glm(form, data = dd, weights = wt, family = stats::inverse.gaussian(link = "log")),
    quasipoisson = stats::glm(form, data = dd, weights = wt, family = stats::quasipoisson()))
  ci <- num_opt(o, "ciWidth", 95, 50, 99.9)
  sm <- summary(fit)$coefficients
  usez <- !fam %in% c("gaussian", "gamma", "invgauss", "quasipoisson")
  crit <- if (usez) stats::qnorm(1 - (1 - ci / 100) / 2) else stats::qt(1 - (1 - ci / 100) / 2, fit$df.residual)
  lo <- sm[, 1] - crit * sm[, 2]; hi <- sm[, 1] + crit * sm[, 2]
  expo <- isTRUE(opt(o, "expB", fam != "gaussian")) && fam != "gaussian"
  tables <- list(list(id = "glm_coef", title = paste0("Generalized Linear Model: ", GLMX_FAMILIES[[fam]]),
    columns = coef_cols(if (usez) "z" else "t", ci, if (expo) list(stats_col("expb", "Exp(B)")) else list()),
    rows = coef_rows(sm[, 1], sm[, 2], sm[, 3], sm[, 4], lo, hi, rownames(sm), if (expo) list(expb = exp(sm[, 1])) else NULL),
    notes = list(paste0(if (!is.null(w)) paste0("Weighted by ", w, ". ") else "", if (expo) "Exp(B): multiplicative effect on the mean (odds-type effects are on the probit scale for probit models). " else "",
                        if (fam == "negbin") sprintf("Dispersion θ = %.3f (SE %.3f).", fit$theta, fit$SE.theta) else ""))))
  null <- stats::update(fit, . ~ 1)
  lr <- tryCatch(stats::anova(null, fit, test = if (fam %in% c("gaussian", "gamma", "invgauss", "quasipoisson")) "F" else "Chisq"), error = function(e) NULL)
  frows <- list(list(item = "N", value = format(nrow(dd))), list(item = "Deviance (residual / null)", value = sprintf("%.3f / %.3f", stats::deviance(fit), fit$null.deviance %||% stats::deviance(null))))
  if (fam != "quasipoisson") frows <- c(frows, list(list(item = "AIC / BIC", value = sprintf("%.2f / %.2f", stats::AIC(fit), stats::BIC(fit)))))
  if (!is.null(lr)) { pc <- grep("^Pr", names(lr), value = TRUE); frows[[length(frows) + 1]] <- list(item = "Model test (vs intercept only)", value = paste0("p ", pfmt(lr[[pc]][2]))) }
  if (fam == "probit") { pr <- 1 - stats::deviance(fit) / fit$null.deviance; frows[[length(frows) + 1]] <- list(item = "McFadden pseudo-R²", value = sprintf("%.3f", pr)) }
  tables[[2]] <- list(id = "glm_fit", title = "Model Fit", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = frows, notes = list())
  list(tables = tables, plots = list(), n = nrow(dd))
}

# ---- Mixed models ----------------------------------------------------------------------------------------------
run_mixed <- function(d, o) {
  covs <- opt(o, "covs", character(0)); facs <- opt(o, "factors", character(0)); cl <- opt(o, "cluster", character(0)); sl <- intersect(opt(o, "slopes", character(0)), covs)
  if (!length(cl)) api_error(422, "mixed_cluster", "Add the cluster (grouping) variable, for example the participant, school or firm.")
  fam <- opt(o, "family", "gaussian"); if (!fam %in% c("gaussian", "binomial", "poisson")) fam <- "gaussian"
  dd <- model_frame(d, opt(o, "dep"), covs, facs)
  for (g in cl) dd[[g]] <- factor(d[[g]])[as.integer(rownames(dd))]
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (fam == "poisson" && any(dd$y < 0 | dd$y != round(dd$y), na.rm = TRUE)) api_error(422, "mixed_count", sprintf("A Poisson mixed model needs counts (whole numbers of 0 or more); '%s' has other values.", o$dep), "Choose the continuous (linear) family for this outcome.")
  if (fam == "binomial") { lv <- sort(unique(dd$y)); if (length(lv) != 2) api_error(422, "mixed_binary", "A logistic mixed model needs an outcome with two values."); dd$y <- as.integer(dd$y == lv[2]) }
  re <- paste(vapply(cl, function(g) sprintf("(%s | `%s`)", if (length(sl)) paste(c("1", sprintf("`%s`", sl)), collapse = " + ") else "1", g), ""), collapse = " + ")
  form <- stats::as.formula(paste("y ~", if (length(c(covs, facs))) rhs_of(covs, facs) else "1", "+", re))
  fit <- if (fam == "gaussian") lmerTest::lmer(form, data = dd, REML = !isFALSE(opt(o, "reml", TRUE))) else lme4::glmer(form, data = dd, family = fam)
  ci <- num_opt(o, "ciWidth", 95, 50, 99.9)
  cf <- summary(fit)$coefficients
  df <- if ("df" %in% colnames(cf)) cf[, "df"] else rep(Inf, nrow(cf))
  stat <- cf[, 3]; p <- cf[, ncol(cf)]
  crit <- if (all(is.finite(df))) stats::qt(1 - (1 - ci / 100) / 2, df) else stats::qnorm(1 - (1 - ci / 100) / 2)
  tables <- list(list(id = "mixed_fixed", title = if (fam == "gaussian") "Linear Mixed Model: Fixed Effects" else paste0("Generalized Linear Mixed Model (", fam, "): Fixed Effects"),
    columns = coef_cols(if (fam == "gaussian") "t" else "z", ci, if (fam == "gaussian") list(stats_col("df", "df")) else list()),
    rows = coef_rows(cf[, 1], cf[, 2], stat, p, cf[, 1] - crit * cf[, 2], cf[, 1] + crit * cf[, 2], rownames(cf), if (fam == "gaussian") list(df = df) else NULL),
    notes = list(if (fam == "gaussian") "Degrees of freedom and p values: Satterthwaite approximation (lmerTest)." else "Wald z tests (lme4::glmer, Laplace approximation).")))
  vc <- as.data.frame(lme4::VarCorr(fit))
  tables[[2]] <- list(id = "mixed_random", title = "Random Effects", columns = list(stats_col("grp", "Group", "text"), stats_col("var1", "Effect", "text"), stats_col("var2", "With", "text"), stats_col("vcov", "Variance / covariance"), stats_col("sdcor", "SD / correlation")),
    rows = lapply(seq_len(nrow(vc)), function(i) list(grp = vc$grp[i], var1 = vc$var1[i] %||% "", var2 = if (is.na(vc$var2[i])) "" else vc$var2[i], vcov = vc$vcov[i], sdcor = vc$sdcor[i])),
    notes = list(sprintf("Number of groups: %s.", paste(sprintf("%s = %d", names(lme4::ngrps(fit)), lme4::ngrps(fit)), collapse = ", "))))
  # ICC and Nakagawa R² (random-intercept variances; residual variance pi²/3 for logit, log(1 + 1/mean) approximations not used for Poisson).
  vi <- vc$vcov[is.na(vc$var2) & vc$var1 == "(Intercept)" & vc$grp != "Residual"]
  ve <- if (fam == "gaussian") stats::sigma(fit)^2 else if (fam == "binomial") pi^2 / 3 else log(1 + 1 / mean(dd$y))
  vf <- stats::var(as.vector(lme4::getME(fit, "X") %*% lme4::fixef(fit)))
  vr <- sum(vc$vcov[is.na(vc$var2) & vc$grp != "Residual"])
  frows <- list(list(item = "Observations", value = format(nrow(dd))), list(item = "AIC / BIC", value = sprintf("%.2f / %.2f", stats::AIC(fit), stats::BIC(fit))),
                list(item = "Log-likelihood", value = sprintf("%.2f", as.numeric(stats::logLik(fit)))),
                list(item = "ICC (random intercepts)", value = sprintf("%.3f", sum(vi) / (sum(vi) + ve))),
                list(item = "Marginal R² (fixed effects)", value = sprintf("%.3f", vf / (vf + vr + ve))), list(item = "Conditional R² (fixed + random)", value = sprintf("%.3f", (vf + vr) / (vf + vr + ve))))
  tables[[3]] <- list(id = "mixed_fit", title = "Model Fit", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = frows,
                      notes = list("R² after Nakagawa & Schielzeth (2013); ICC = intercept variance / (intercept variance + residual variance)."))
  list(tables = tables, plots = list(), n = nrow(dd))
}

# ---- Two-stage least squares ----------------------------------------------------------------------------------
run_iv2sls <- function(d, o) {
  ex <- opt(o, "exog", character(0)); en <- opt(o, "endog", character(0)); ins <- opt(o, "instruments", character(0))
  if (!length(en) || !length(ins)) api_error(422, "iv_vars", "Add at least one endogenous predictor and one instrument.")
  if (length(ins) < length(en)) api_error(422, "iv_order", "The model needs at least as many instruments as endogenous predictors (order condition).")
  vars <- unique(c(ex, en, ins))
  dd <- data.frame(y = as.numeric(d[[opt(o, "dep")]])); for (v in vars) dd[[v]] <- as.numeric(d[[v]]); dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  q <- function(x) sprintf("`%s`", x)
  form <- stats::as.formula(paste("y ~", paste(q(c(ex, en)), collapse = " + "), "|", paste(q(c(ex, ins)), collapse = " + ")))
  fit <- ivreg::ivreg(form, data = dd)
  rob <- isTRUE(opt(o, "robust", FALSE))
  # summary.ivreg needs a function (not a matrix) for vcov. when diagnostics are on; with a matrix it silently fell back
  # to classic standard errors, so robust SEs were ignored.
  sm <- summary(fit, vcov. = if (rob) function(x, ...) sandwich::vcovHC(x, type = "HC1") else NULL, diagnostics = TRUE)
  cf <- sm$coefficients; crit <- stats::qt(0.975, fit$df.residual)
  tables <- list(list(id = "iv_coef", title = "Two-Stage Least Squares: Coefficients", columns = coef_cols("t", 95),
    rows = coef_rows(cf[, 1], cf[, 2], cf[, 3], cf[, 4], cf[, 1] - crit * cf[, 2], cf[, 1] + crit * cf[, 2], rownames(cf)),
    notes = list(paste0("Endogenous: ", paste(en, collapse = ", "), ". Instruments: ", paste(ins, collapse = ", "), if (length(ex)) paste0(". Exogenous controls: ", paste(ex, collapse = ", ")) else "", ". SE: ", if (rob) "robust (HC1)" else "conventional", "."))))
  dg <- sm$diagnostics
  if (!is.null(dg)) tables[[2]] <- list(id = "iv_diag", title = "Instrument Diagnostics", columns = list(stats_col("test", "Test", "text"), stats_col("df1", "df1", "integer"), stats_col("df2", "df2"), stats_col("stat", "Statistic"), stats_col("p", "p", pvalue = TRUE)),
    rows = lapply(seq_len(nrow(dg)), function(i) list(test = rownames(dg)[i], df1 = dg[i, 1], df2 = dg[i, 2], stat = dg[i, 3], p = dg[i, 4])),
    notes = list("Weak instruments: a significant F (and F > 10) means the instruments predict the endogenous variable. Wu-Hausman: significant = OLS is inconsistent, so 2SLS is needed. Sargan: significant = some instruments may be invalid (needs more instruments than endogenous variables)."))
  list(tables = tables, plots = list(), n = nrow(dd))
}

# ---- Canonical correlation ------------------------------------------------------------------------------------
run_cancor <- function(d, o) {
  a <- opt(o, "set1", character(0)); b <- opt(o, "set2", character(0))
  if (!length(a) || !length(b)) api_error(422, "cancor_vars", "Add the variables of both sets.")
  dd <- d[, c(a, b), drop = FALSE]; dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  X <- scale(as.matrix(dd[, a, drop = FALSE])); Y <- scale(as.matrix(dd[, b, drop = FALSE]))
  cc <- stats::cancor(X, Y); r <- cc$cor; n <- nrow(dd); p <- ncol(X); q <- ncol(Y); k <- length(r)
  tests <- lapply(seq_len(k), function(i) { L <- prod(1 - r[i:k]^2); chi <- -(n - 1 - (p + q + 1) / 2) * log(L); df <- (p - i + 1) * (q - i + 1)
    list(fn = i, r = r[i], r2 = r[i]^2, wilks = L, chisq = chi, df = df, p = stats::pchisq(chi, df, lower.tail = FALSE)) })
  load <- function(M, coef) { s <- M %*% coef[, seq_len(k), drop = FALSE]; stats::cor(M, s) }
  la <- load(X, cc$xcoef); lb <- load(Y, cc$ycoef)
  lrow <- function(L, set) lapply(seq_len(nrow(L)), function(i) c(list(set = set, var = rownames(L)[i]), stats::setNames(as.list(L[i, ]), paste0("f", seq_len(k)))))
  list(tables = list(
    list(id = "cancor_tests", title = "Canonical Correlations", columns = list(stats_col("fn", "Function", "integer"), stats_col("r", "Canonical r"), stats_col("r2", "r²"), stats_col("wilks", "Wilks' Λ"), stats_col("chisq", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)),
         rows = tests, notes = list("Bartlett's χ² tests whether this and all later canonical correlations are zero.")),
    list(id = "cancor_loadings", title = "Canonical Loadings (Structure Correlations)", columns = c(list(stats_col("set", "Set", "text", combineBelow = TRUE), stats_col("var", "Variable", "text")), lapply(seq_len(k), function(i) stats_col(paste0("f", i), paste("Function", i)))),
         rows = c(lrow(la, "Set 1"), lrow(lb, "Set 2")), notes = list("Correlations of each variable with the canonical variates of its set; |r| ≥ .30 is commonly interpreted."))),
    plots = list(), n = n)
}

# ---- Runs test --------------------------------------------------------------------------------------------------
run_runs <- function(d, o) {
  rows <- lapply(opt(o, "vars", character(0)), function(v) {
    x <- as.numeric(d[[v]]); x <- x[!is.na(x)]; cut <- if (identical(opt(o, "cut", "median"), "mean")) mean(x) else stats::median(x)
    s <- x[x != cut] > cut; n1 <- sum(s); n2 <- sum(!s); R <- 1 + sum(s[-1] != s[-length(s)])
    mu <- 2 * n1 * n2 / (n1 + n2) + 1; vr <- 2 * n1 * n2 * (2 * n1 * n2 - n1 - n2) / ((n1 + n2)^2 * (n1 + n2 - 1)); z <- (R - mu) / sqrt(vr)
    list(var = v, cut = cut, below = n2, above = n1, runs = R, expected = mu, z = z, p = 2 * stats::pnorm(-abs(z)))
  })
  list(tables = list(list(id = "runs", title = "Runs Test (Wald-Wolfowitz)", rows = rows,
    columns = list(stats_col("var", "", "text"), stats_col("cut", "Cut point"), stats_col("below", "Below", "integer"), stats_col("above", "Above", "integer"), stats_col("runs", "Runs", "integer"), stats_col("expected", "Expected runs"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE)),
    notes = list("Tests whether the sequence (in data order) is random; values equal to the cut point are excluded. Too few runs suggest clustering or trends, too many suggest alternation."))), plots = list(), n = nrow(d))
}

# ---- Life table (actuarial) -------------------------------------------------------------------------------------
run_lifetable <- function(d, o) {
  tt <- as.numeric(d[[opt(o, "time")]]); st <- surv_status(d[[opt(o, "event")]], opt(o, "eventLevel"))
  g <- if (length(opt(o, "group", character(0)))) as.character(d[[opt(o, "group")[1]]]) else rep("All", length(tt))
  ok <- !is.na(tt) & !is.na(st) & !is.na(g); tt <- tt[ok]; st <- st[ok]; g <- g[ok]
  w <- num_opt(o, "width", 0, 0); if (w <= 0) w <- signif(diff(range(tt)) / 10, 2)
  brks <- seq(0, max(tt) + w, by = w)
  rows <- list()
  for (gr in unique(g)) {
    ti <- tt[g == gr]; si <- st[g == gr]; at <- length(ti); S <- 1
    for (j in seq_len(length(brks) - 1)) {
      inn <- ti >= brks[j] & ti < brks[j + 1]; dth <- sum(si[inn] == 1); cen <- sum(si[inn] == 0)
      if (at <= 0) break
      eff <- at - cen / 2; q <- if (eff > 0) dth / eff else 0; S <- S * (1 - q)
      rows[[length(rows) + 1]] <- list(group = gr, start = brks[j], entering = at, withdrawn = cen, exposed = eff, events = dth, q = q, surv = S, hazard = if (eff > 0) dth / (w * (eff - dth / 2)) else 0)
      at <- at - dth - cen
    }
  }
  list(tables = list(list(id = "lifetable", title = "Life Table (Actuarial)", rows = rows,
    columns = list(stats_col("group", "", "text", combineBelow = TRUE), stats_col("start", "Interval start"), stats_col("entering", "Entering", "integer"), stats_col("withdrawn", "Withdrawn", "integer"), stats_col("exposed", "Exposed to risk"),
                   stats_col("events", "Events", "integer"), stats_col("q", "Proportion terminating"), stats_col("surv", "Cumulative survival"), stats_col("hazard", "Hazard rate")),
    notes = list(sprintf("Interval width %s. Withdrawals are assumed to occur halfway through the interval (actuarial method).", format(w))))), plots = list(), n = length(tt))
}

# ---- Bayesian linear regression -------------------------------------------------------------------------------
run_bayesreg <- function(d, o) {
  covs <- opt(o, "covs", character(0))
  dd <- data.frame(y = as.numeric(d[[opt(o, "dep")]])); for (v in covs) dd[[make.names(v)]] <- as.numeric(d[[v]]); dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (length(covs) > 12) api_error(422, "breg_many", "Bayesian model comparison is limited to 12 predictors.")
  set.seed(num_opt(o, "seed", 2026)); sc <- opt(o, "rscale", "medium")
  fb <- stats::as.formula(paste("y ~", paste(make.names(covs), collapse = " + ")))
  bf <- BayesFactor::regressionBF(fb, data = dd, rscaleCont = sc, progress = FALSE)
  tab <- BayesFactor::extractBF(bf); tab <- tab[order(-tab$bf), , drop = FALSE]
  nm <- stats::setNames(covs, make.names(covs))
  best <- tab$bf[1]
  rows <- lapply(seq_len(min(15, nrow(tab))), function(i) list(model = paste(vapply(strsplit(rownames(tab)[i], " + ", fixed = TRUE)[[1]], function(p) if (!is.na(nm[p])) nm[[p]] else p, ""), collapse = " + "),
                                                              bf10 = tab$bf[i], bfBest = tab$bf[i] / best, error = tab$error[i]))
  full <- BayesFactor::lmBF(fb, data = dd, rscaleCont = sc, progress = FALSE)
  post <- BayesFactor::posterior(full, iterations = 4000, progress = FALSE)
  keep <- intersect(names(nm), colnames(post))
  prow <- lapply(keep, function(k) { x <- post[, k]; q <- stats::quantile(x, c(0.025, 0.975)); list(term = nm[[k]], mean = mean(x), sd = stats::sd(x), lower = q[[1]], upper = q[[2]]) })
  # Inclusion Bayes factors (prior and posterior inclusion with equal model priors).
  inc <- lapply(names(nm), function(k) { has <- vapply(strsplit(rownames(tab), " + ", fixed = TRUE), function(p) k %in% p, TRUE)
    pm <- tab$bf / sum(tab$bf); pinc <- sum(pm[has]); prior <- mean(has); list(term = nm[[k]], prior = prior, posterior = pinc, bfInc = (pinc / (1 - pinc)) / (prior / (1 - prior))) })
  list(tables = list(
    list(id = "breg_models", title = "Bayesian Linear Regression: Model Comparison", rows = rows,
         columns = list(stats_col("model", "Model (predictors)", "text"), stats_col("bf10", "BF₁₀ (vs intercept only)"), stats_col("bfBest", "BF vs best model"), stats_col("error", "Error %")),
         notes = list(sprintf("BayesFactor::regressionBF with JZS priors (r scale %s); the 15 best of %d models.", sc, nrow(tab)))),
    list(id = "breg_inclusion", title = "Predictor Inclusion", rows = inc, columns = list(stats_col("term", "Predictor", "text"), stats_col("prior", "P(incl)"), stats_col("posterior", "P(incl | data)"), stats_col("bfInc", "BF inclusion")),
         notes = list("Equal prior probability for every model; BF inclusion > 3 is moderate and > 10 strong evidence for including the predictor.")),
    list(id = "breg_post", title = "Posterior Summary (Full Model)", rows = prow, columns = list(stats_col("term", "Predictor", "text"), stats_col("mean", "Mean"), stats_col("sd", "SD"), stats_col("lower", "2.5%", superTitle = "95% credible interval"), stats_col("upper", "97.5%", superTitle = "95% credible interval")),
         notes = list("4,000 posterior draws (BayesFactor::posterior)."))), plots = list(), n = nrow(dd))
}

# ---- Time series ---------------------------------------------------------------------------------------------
run_arima <- function(d, o) {
  v <- opt(o, "var"); x <- as.numeric(d[[v]]); ord <- opt(o, "order")
  if (!is.null(ord)) x <- x[order(d[[ord]])]
  x <- x[!is.na(x)]
  if (length(x) < 8) api_error(422, "ts_short", "A time series needs at least 8 observations.")
  fr <- num_opt(o, "frequency", 1, 1, 366); h <- round(num_opt(o, "horizon", 12, 1, 500)); ci <- num_opt(o, "ciWidth", 95, 50, 99.9)
  y <- stats::ts(x, frequency = fr)
  mod <- opt(o, "model", "auto")
  fit <- switch(mod, ets = forecast::ets(y),
    manual = forecast::Arima(y, order = c(round(num_opt(o, "p", 1, 0, 10)), round(num_opt(o, "d", 0, 0, 3)), round(num_opt(o, "q", 0, 0, 10))), include.constant = TRUE),
    forecast::auto.arima(y))
  fc <- forecast::forecast(fit, h = h, level = ci)
  acc <- forecast::accuracy(fit)
  lb <- stats::Box.test(stats::residuals(fit), lag = min(10, length(x) %/% 5), type = "Ljung-Box", fitdf = length(stats::coef(fit)))
  cf <- stats::coef(fit); vc <- tryCatch(sqrt(diag(stats::vcov(fit))), error = function(e) rep(NA_real_, length(cf)))
  # ETS models have their own label (as.character.Arima fails on them).
  lab <- if (inherits(fit, "ets")) fit$method else forecast:::as.character.Arima(fit)
  tables <- list(list(id = "ts_model", title = paste("Model:", lab), columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")),
    rows = list(list(item = "Model", value = lab), list(item = "Observations", value = format(length(x))),
                list(item = "AIC / BIC", value = sprintf("%.2f / %.2f", fit$aic %||% stats::AIC(fit), fit$bic %||% stats::BIC(fit))),
                list(item = "RMSE / MAE / MAPE (training)", value = sprintf("%.3f / %.3f / %.2f%%", acc[1, "RMSE"], acc[1, "MAE"], acc[1, "MAPE"])),
                list(item = "Ljung-Box test of residuals", value = sprintf("Q = %.2f, df = %d, p %s", lb$statistic, lb$parameter, pfmt(lb$p.value)))),
    notes = list(if (mod == "auto") "Order chosen by forecast::auto.arima (AICc)." else if (mod == "ets") "Exponential smoothing state-space model chosen by forecast::ets (AICc)." else "ARIMA order set by the user.", "A non-significant Ljung-Box test means the residuals look like white noise.")))
  if (length(cf)) tables[[2]] <- list(id = "ts_coef", title = "Coefficients", columns = list(stats_col("term", "", "text"), stats_col("b", "Estimate"), stats_col("se", "SE")),
    rows = lapply(seq_along(cf), function(i) list(term = names(cf)[i], b = cf[[i]], se = if (i <= length(vc)) vc[[i]] else NULL)), notes = list())
  tables[[length(tables) + 1]] <- list(id = "ts_forecast", title = sprintf("Forecast (%d periods)", h), columns = list(stats_col("t", "Period", "integer"), stats_col("mean", "Forecast"), stats_col("lower", "Lower", superTitle = sprintf("%g%% interval", ci)), stats_col("upper", "Upper", superTitle = sprintf("%g%% interval", ci))),
    rows = lapply(seq_len(h), function(i) list(t = length(x) + i, mean = as.numeric(fc$mean)[i], lower = as.numeric(fc$lower[, 1])[i], upper = as.numeric(fc$upper[, 1])[i])), notes = list())
  pl <- stats_png(function() { n <- length(x); tt <- seq_len(n); tf <- n + seq_len(h)
    graphics::plot(c(tt, tf), c(x, as.numeric(fc$mean)), type = "n", xlab = if (is.null(ord)) "Period" else ord, ylab = v, main = "", ylim = range(c(x, fc$lower, fc$upper), na.rm = TRUE))
    graphics::polygon(c(tf, rev(tf)), c(fc$lower[, 1], rev(fc$upper[, 1])), col = "#cfe0f5", border = NA)
    graphics::lines(tt, x, col = "#334155", lwd = 1.6); graphics::lines(tt, stats::fitted(fit), col = "#e11d48", lwd = 1, lty = 2)
    graphics::lines(tf, fc$mean, col = "#1d4f86", lwd = 2.2); graphics::grid(col = "#e5e7eb")
    graphics::legend("topleft", c("Observed", "Fitted", "Forecast", sprintf("%g%% interval", ci)), col = c("#334155", "#e11d48", "#1d4f86", "#cfe0f5"), lwd = c(1.6, 1, 2.2, 8), lty = c(1, 2, 1, 1), bty = "n", cex = 0.85) },
    title = paste("Forecast of", v))
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = length(x))
}

run_acf <- function(d, o) {
  L <- round(num_opt(o, "lags", 16, 1, 200)); rows <- list(); plots <- list()
  for (v in opt(o, "vars", character(0))) {
    x <- as.numeric(d[[v]]); x <- x[!is.na(x)]; n <- length(x); if (n < 4) next
    a <- stats::acf(x, lag.max = L, plot = FALSE)$acf[-1]; pa <- stats::pacf(x, lag.max = L, plot = FALSE)$acf
    for (k in seq_along(a)) { lb <- stats::Box.test(x, lag = k, type = "Ljung-Box")
      rows[[length(rows) + 1]] <- list(var = v, lag = k, acf = a[k], se = sqrt((1 + 2 * sum(a[seq_len(k - 1)]^2)) / n), pacf = pa[k], q = unname(lb$statistic), p = lb$p.value) }
    plots[[length(plots) + 1]] <- stats_png(function() { graphics::par(mfrow = c(1, 2)); b <- 1.96 / sqrt(n)
      for (z in list(list(a, "ACF"), list(pa, "PACF"))) { graphics::plot(seq_along(z[[1]]), z[[1]], type = "h", lwd = 6, col = "#2f6fd6", ylim = c(-1, 1), xlab = "Lag", ylab = z[[2]], main = paste(z[[2]], "of", v)); graphics::abline(h = c(-b, 0, b), lty = c(2, 1, 2), col = c("#dc2626", "#64748b", "#dc2626")) } },
      title = paste("Autocorrelations of", v), w = 8, h = 3.6)
  }
  list(tables = list(list(id = "acf", title = "Autocorrelations", rows = rows,
    columns = list(stats_col("var", "", "text", combineBelow = TRUE), stats_col("lag", "Lag", "integer"), stats_col("acf", "ACF"), stats_col("se", "SE"), stats_col("pacf", "PACF"), stats_col("q", "Ljung-Box Q"), stats_col("p", "p", pvalue = TRUE)),
    notes = list("Values in data order. SE by Bartlett's formula; dashed lines in the plot are ±1.96/√n."))), plots = Filter(Negate(is.null), plots), n = nrow(d))
}

# ---- Missing values -----------------------------------------------------------------------------------------------
#' EM estimates of the mean vector and covariance matrix under multivariate normality (for Little's test).
mvn_em <- function(X, iter = 200, tol = 1e-6) {
  mu <- colMeans(X, na.rm = TRUE); S <- stats::cov(X, use = "pairwise.complete.obs"); S[is.na(S)] <- 0; diag(S)[diag(S) <= 0] <- 1e-6
  for (it in seq_len(iter)) {
    T1 <- numeric(ncol(X)); T2 <- matrix(0, ncol(X), ncol(X))
    for (i in seq_len(nrow(X))) { m <- is.na(X[i, ]); xi <- X[i, ]; Ci <- matrix(0, ncol(X), ncol(X))
      if (any(m)) { o <- !m; if (any(o)) { B <- S[m, o, drop = FALSE] %*% solve(S[o, o, drop = FALSE]); xi[m] <- mu[m] + B %*% (xi[o] - mu[o]); Ci[m, m] <- S[m, m] - B %*% S[o, m, drop = FALSE] } else { xi[m] <- mu[m]; Ci[m, m] <- S[m, m] } }
      T1 <- T1 + xi; T2 <- T2 + xi %o% xi + Ci }
    mu2 <- T1 / nrow(X); S2 <- T2 / nrow(X) - mu2 %o% mu2
    done <- max(abs(mu2 - mu)) < tol && max(abs(S2 - S)) < tol; mu <- mu2; S <- S2; if (done) break
  }
  list(mu = mu, S = S)
}

run_mva <- function(d, o) {
  vars <- opt(o, "vars", character(0)); X <- as.matrix(as.data.frame(lapply(d[, vars, drop = FALSE], as.numeric))); colnames(X) <- vars
  n <- nrow(X); miss <- is.na(X)
  t1 <- lapply(vars, function(v) { x <- X[, v]; list(var = v, n = sum(!is.na(x)), missing = sum(is.na(x)), pct = 100 * mean(is.na(x)), mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE)) })
  key <- apply(miss, 1, function(r) paste(ifelse(r, "M", "·"), collapse = " "))
  pt <- sort(table(key), decreasing = TRUE)
  t2 <- lapply(utils::head(names(pt), 20), function(k) list(pattern = k, n = as.integer(pt[[k]]), pct = 100 * pt[[k]] / n, nmiss = sum(strsplit(k, " ")[[1]] == "M")))
  tables <- list(list(id = "mva_vars", title = "Missing Values by Variable", rows = t1, columns = list(stats_col("var", "", "text"), stats_col("n", "N valid", "integer"), stats_col("missing", "Missing", "integer"), stats_col("pct", "Missing %"), stats_col("mean", "Mean"), stats_col("sd", "SD")),
                      notes = list(sprintf("Complete cases: %d of %d (%.1f%%). Missing cells: %.1f%%.", sum(stats::complete.cases(X)), n, 100 * mean(stats::complete.cases(X)), 100 * mean(miss)))),
                 list(id = "mva_patterns", title = "Missing Data Patterns", rows = t2, columns = list(stats_col("pattern", paste(vars, collapse = " "), "text"), stats_col("n", "Cases", "integer"), stats_col("pct", "%"), stats_col("nmiss", "Variables missing", "integer")),
                      notes = list("M = missing, · = observed (columns in the order of the variables). The 20 most frequent patterns.")))
  if (any(miss) && length(vars) >= 2 && length(vars) <= 40) {
    em <- tryCatch(mvn_em(X), error = function(e) NULL)
    if (!is.null(em)) {
      d2 <- 0; dfp <- 0
      for (k in unique(key)) { rows <- which(key == k); o_ <- !miss[rows[1], ]; if (!any(o_)) next
        yb <- colMeans(X[rows, o_, drop = FALSE]); dv <- yb - em$mu[o_]; d2 <- d2 + length(rows) * as.numeric(t(dv) %*% solve(em$S[o_, o_, drop = FALSE]) %*% dv); dfp <- dfp + sum(o_) }
      dfp <- dfp - length(vars)
      tables[[3]] <- list(id = "mva_little", title = "Little's MCAR Test", rows = list(list(test = "Little's MCAR test", chisq = d2, df = dfp, p = stats::pchisq(d2, dfp, lower.tail = FALSE))),
        columns = list(stats_col("test", "", "text"), stats_col("chisq", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)),
        notes = list("Little (1988), with EM estimates of the means and covariances. A non-significant result is consistent with data missing completely at random (MCAR); a significant result means listwise deletion may bias results (consider multiple imputation)."))
      tables[[4]] <- list(id = "mva_em", title = "EM Estimated Means", rows = lapply(seq_along(vars), function(i) list(var = vars[i], mean = em$mu[i], sd = sqrt(em$S[i, i]))),
        columns = list(stats_col("var", "", "text"), stats_col("mean", "EM mean"), stats_col("sd", "EM SD")), notes = list())
    }
  }
  list(tables = tables, plots = list(), n = n)
}

run_mi <- function(d, o) {
  dep <- opt(o, "dep"); covs <- opt(o, "covs", character(0)); aux <- setdiff(opt(o, "aux", character(0)), c(dep, covs))
  vars <- unique(c(dep, covs, aux))
  dd <- as.data.frame(lapply(d[, vars, drop = FALSE], function(x) if (is.factor(x)) x else as.numeric(x))); names(dd) <- make.names(vars)
  nm <- stats::setNames(vars, names(dd))
  m <- round(num_opt(o, "m", 20, 2, 100)); meth <- opt(o, "method", "pmm"); if (!meth %in% c("pmm", "norm", "cart")) meth <- "pmm"
  imp <- mice::mice(dd, m = m, method = meth, maxit = round(num_opt(o, "maxit", 10, 1, 50)), seed = round(num_opt(o, "seed", 2026)), printFlag = FALSE)
  form <- stats::as.formula(paste(make.names(dep), "~", paste(make.names(covs), collapse = " + ")))
  fits <- with(imp, stats::lm(stats::as.formula(paste(make.names(dep), "~", paste(make.names(covs), collapse = " + ")))))
  pl <- mice::pool(fits); s <- summary(pl, conf.int = TRUE)
  term <- as.character(s$term); lab <- ifelse(term %in% names(nm), nm[term], term)
  r2 <- tryCatch(mice::pool.r.squared(fits), error = function(e) NULL)
  tables <- list(
    list(id = "mi_summary", title = "Imputation Summary", rows = lapply(vars, function(v) list(var = v, missing = sum(is.na(d[[v]])), pct = 100 * mean(is.na(d[[v]])), method = imp$method[[make.names(v)]] %||% "")),
         columns = list(stats_col("var", "", "text"), stats_col("missing", "Missing", "integer"), stats_col("pct", "Missing %"), stats_col("method", "Method", "text")),
         notes = list(sprintf("mice: %d imputed datasets, %d iterations, seed %d. Variables without missing values are used as predictors only.", m, imp$iteration, round(num_opt(o, "seed", 2026))))),
    list(id = "mi_pooled", title = paste("Pooled Linear Regression of", dep, "(Rubin's Rules)"), columns = c(coef_cols("t", 95), list(stats_col("df", "df"), stats_col("fmi", "FMI"))),
         rows = lapply(seq_len(nrow(s)), function(i) list(term = lab[i], b = s$estimate[i], se = s$std.error[i], stat = s$statistic[i], p = s$p.value[i], lower = s[["2.5 %"]][i], upper = s[["97.5 %"]][i], df = s$df[i], fmi = pl$pooled$fmi[i])),
         notes = list(paste0("FMI = fraction of missing information. ", if (!is.null(r2)) sprintf("Pooled R² = %.3f (95%% CI %.3f to %.3f).", r2[1, 1], r2[1, 2], r2[1, 3]) else ""))))
  list(tables = tables, plots = list(), n = nrow(dd))
}

# ---- Power analysis ----------------------------------------------------------------------------------------------
POWER_TESTS <- c(t2 = "Independent samples t-test (d)", t1 = "One-sample t-test (d)", paired = "Paired samples t-test (d)", anova = "One-way ANOVA (f)", corr = "Correlation (r)",
                 regression = "Multiple regression (f²)", chisq = "Chi-square test (w)", prop2 = "Two proportions (h)")
run_power <- function(d, o) {
  test <- opt(o, "test", "t2"); if (!test %in% names(POWER_TESTS)) test <- "t2"; solve <- opt(o, "solve", "n")
  es <- num_opt(o, "es", 0.5); a <- num_opt(o, "alpha", 0.05, 1e-6, 0.5); pw <- num_opt(o, "power", 0.8, 0.05, 0.9999); n <- num_opt(o, "n", 50, 2)
  k <- round(num_opt(o, "k", 3, 2)); u <- round(num_opt(o, "u", 3, 1)); df <- round(num_opt(o, "df", 1, 1)); alt <- if (identical(opt(o, "alternative"), "greater")) "greater" else "two.sided"
  N <- if (solve == "n") NULL else n; P <- if (solve == "power") NULL else pw; E <- if (solve == "es") NULL else es
  r <- switch(test,
    t2 = pwr::pwr.t.test(n = N, d = E, sig.level = a, power = P, type = "two.sample", alternative = alt),
    t1 = pwr::pwr.t.test(n = N, d = E, sig.level = a, power = P, type = "one.sample", alternative = alt),
    paired = pwr::pwr.t.test(n = N, d = E, sig.level = a, power = P, type = "paired", alternative = alt),
    anova = pwr::pwr.anova.test(k = k, n = N, f = E, sig.level = a, power = P),
    corr = pwr::pwr.r.test(n = N, r = E, sig.level = a, power = P, alternative = alt),
    regression = { v <- if (is.null(N)) NULL else max(1, N - u - 1); x <- pwr::pwr.f2.test(u = u, v = v, f2 = E, sig.level = a, power = P); x$n <- x$v + u + 1; x },
    chisq = pwr::pwr.chisq.test(w = E, N = N, df = df, sig.level = a, power = P),
    prop2 = pwr::pwr.2p.test(h = E, n = N, sig.level = a, power = P, alternative = alt))
  nn <- r$n %||% r$N; eff <- r$d %||% r$f %||% r$r %||% r$f2 %||% r$w %||% r$h
  per <- test %in% c("t2", "anova", "prop2")
  rows <- list(list(item = "Test", value = POWER_TESTS[[test]]), list(item = "Effect size", value = sprintf("%.3f", eff)), list(item = "α", value = format(a)),
               list(item = "Power (1 − β)", value = sprintf("%.3f", r$power)),
               list(item = if (per) "Sample size per group" else "Total sample size", value = sprintf("%.1f (round up: %d)", nn, ceiling(nn))))
  if (per) rows[[length(rows) + 1]] <- list(item = "Total sample size", value = format(ceiling(nn) * if (test == "anova") k else 2))
  conv <- switch(test, t2 = , t1 = , paired = "d: 0.20 small, 0.50 medium, 0.80 large", anova = "f: 0.10 small, 0.25 medium, 0.40 large", corr = "r: 0.10 small, 0.30 medium, 0.50 large",
                 regression = "f²: 0.02 small, 0.15 medium, 0.35 large", chisq = "w: 0.10 small, 0.30 medium, 0.50 large", prop2 = "h: 0.20 small, 0.50 medium, 0.80 large")
  curve <- if (!is.null(eff) && is.finite(eff) && eff > 0) lapply(c(0.7, 0.8, 0.85, 0.9, 0.95, 0.99), function(p) {
    rr <- tryCatch(switch(test, t2 = pwr::pwr.t.test(d = eff, sig.level = a, power = p, type = "two.sample", alternative = alt), t1 = pwr::pwr.t.test(d = eff, sig.level = a, power = p, type = "one.sample", alternative = alt),
      paired = pwr::pwr.t.test(d = eff, sig.level = a, power = p, type = "paired", alternative = alt), anova = pwr::pwr.anova.test(k = k, f = eff, sig.level = a, power = p),
      corr = pwr::pwr.r.test(r = eff, sig.level = a, power = p, alternative = alt), regression = { x <- pwr::pwr.f2.test(u = u, f2 = eff, sig.level = a, power = p); x$n <- x$v + u + 1; x },
      chisq = pwr::pwr.chisq.test(w = eff, df = df, sig.level = a, power = p), prop2 = pwr::pwr.2p.test(h = eff, sig.level = a, power = p, alternative = alt)), error = function(e) NULL)
    list(power = p, n = if (is.null(rr)) NULL else ceiling(rr$n %||% rr$N)) }) else list()
  list(tables = list(list(id = "power", title = "Power Analysis", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = rows,
                          notes = list(paste0("pwr (Champely). Conventions (Cohen, 1988): ", conv, "."))),
                     list(id = "power_curve", title = "Required Sample Size by Power", columns = list(stats_col("power", "Power"), stats_col("n", if (per) "n per group" else "N", "integer")), rows = curve, notes = list())),
       plots = list(), n = 0L)
}

# ---- Meta-analysis ------------------------------------------------------------------------------------------------
run_meta <- function(d, o) {
  me <- opt(o, "measure", "SMD"); g <- function(k) { v <- opt(o, k); if (is.null(v)) NULL else as.numeric(d[[v]]) }
  study <- if (!is.null(opt(o, "study"))) as.character(d[[opt(o, "study")]]) else paste("Study", seq_len(nrow(d)))
  es <- switch(me,
    SMD = metafor::escalc("SMD", m1i = g("m1"), sd1i = g("sd1"), n1i = g("n1"), m2i = g("m2"), sd2i = g("sd2"), n2i = g("n2")),
    COR = metafor::escalc("ZCOR", ri = g("r"), ni = g("n")),
    generic = { yi <- g("yi"); vi <- g("vi"); if (is.null(vi) && !is.null(g("sei"))) vi <- g("sei")^2; if (is.null(yi) || is.null(vi)) api_error(422, "meta_vars", "Add the effect sizes and their variances (or standard errors)."); metafor::escalc(measure = "GEN", yi = yi, vi = vi) },
    api_error(400, "meta_measure", "Unknown effect-size type."))
  ok <- stats::complete.cases(es$yi, es$vi); es <- es[ok, ]; study <- study[ok]
  if (nrow(es) < 2) api_error(422, "meta_k", "A meta-analysis needs at least two studies with complete data.")
  method <- opt(o, "method", "REML"); if (!method %in% c("REML", "DL", "FE", "PM")) method <- "REML"
  fit <- metafor::rma(yi, vi, data = es, method = method, slab = study)
  back <- function(x) if (me == "COR") tanh(x) else x
  pr <- stats::predict(fit, transf = if (me == "COR") tanh else NULL)
  w <- stats::weights(fit)
  tables <- list(list(id = "meta_pooled", title = paste0("Pooled Effect (", if (method == "FE") "fixed" else "random", "-effects, ", method, ")"),
    columns = list(stats_col("item", "", "text"), stats_col("est", "Estimate"), stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE)),
    rows = list(list(item = switch(me, SMD = "Hedges' g", COR = "Correlation r", "Effect"), est = as.numeric(pr$pred), lower = as.numeric(pr$ci.lb), upper = as.numeric(pr$ci.ub), z = fit$zval, p = fit$pval)),
    notes = list(paste0("k = ", fit$k, " studies. ", if (method != "FE") sprintf("95%% prediction interval: %.3f to %.3f. ", pr$pi.lb %||% NA, pr$pi.ub %||% NA) else "", if (me == "COR") "Pooled on Fisher's z and transformed back to r." else ""))))
  tables[[2]] <- list(id = "meta_het", title = "Heterogeneity", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")),
    rows = list(list(item = "Q (df)", value = sprintf("%.2f (%d), p %s", fit$QE, fit$k - 1, pfmt(fit$QEp))), list(item = "I²", value = sprintf("%.1f%%", fit$I2 %||% NA)),
                list(item = "H²", value = sprintf("%.2f", fit$H2 %||% NA)), list(item = "τ² (SE)", value = sprintf("%.4f (%.4f)", fit$tau2, fit$se.tau2 %||% NA)), list(item = "τ", value = sprintf("%.4f", sqrt(fit$tau2)))),
    notes = list("I²: 25% low, 50% moderate, 75% high heterogeneity (Higgins et al., 2003)."))
  tables[[3]] <- list(id = "meta_studies", title = "Studies", columns = list(stats_col("study", "Study", "text"), stats_col("yi", "Effect"), stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI"), stats_col("w", "Weight %")),
    rows = lapply(seq_len(nrow(es)), function(i) { se <- sqrt(es$vi[i]); list(study = study[i], yi = back(es$yi[i]), lower = back(es$yi[i] - 1.96 * se), upper = back(es$yi[i] + 1.96 * se), w = w[i]) }), notes = list())
  if (!isFALSE(opt(o, "bias", TRUE)) && fit$k >= 3) {
    eg <- tryCatch(metafor::regtest(fit), error = function(e) NULL); tf <- tryCatch(metafor::trimfill(fit), error = function(e) NULL); fsn <- tryCatch(metafor::fsn(es$yi, es$vi), error = function(e) NULL)
    rows <- list()
    if (!is.null(eg)) rows[[length(rows) + 1]] <- list(item = "Egger's regression test", value = sprintf("z = %.2f, p %s", eg$zval, pfmt(eg$pval)))
    if (!is.null(tf)) rows[[length(rows) + 1]] <- list(item = "Trim-and-fill", value = sprintf("%d study(ies) imputed; adjusted estimate %.3f", tf$k0, back(as.numeric(tf$beta))))
    if (!is.null(fsn)) rows[[length(rows) + 1]] <- list(item = "Fail-safe N (Rosenthal)", value = format(fsn$fsnum))
    tables[[4]] <- list(id = "meta_bias", title = "Publication Bias", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = rows, notes = list("Egger's test: a significant result suggests funnel-plot asymmetry (small-study effects)."))
  }
  plots <- Filter(Negate(is.null), list(
    stats_png(function() metafor::forest(fit, transf = if (me == "COR") tanh else NULL, header = c("Study", "Effect [95% CI]"), col = "#1d4f86", border = "#1d4f86", cex = 0.85), title = "Forest plot", w = 8, h = max(3.5, 1.6 + 0.28 * fit$k)),
    stats_png(function() metafor::funnel(fit, col = "#1d4f86", back = "#f1f5f9", shade = "white"), title = "Funnel plot", w = 6, h = 4.6)))
  list(tables = tables, plots = plots, n = fit$k)
}

# ---- Correspondence analysis and MDS ----------------------------------------------------------------------------------
run_corresp <- function(d, o) {
  r <- opt(o, "rows")[1]; cc <- opt(o, "cols")[1]
  tb <- table(d[[r]], d[[cc]]); tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  if (min(dim(tb)) < 2) api_error(422, "ca_dims", "Both variables need at least two categories.")
  fit <- ca::ca(tb, nd = min(round(num_opt(o, "dims", 2, 1, 10)), min(dim(tb)) - 1))
  ev <- fit$sv^2; k <- length(fit$sv); chi <- suppressWarnings(stats::chisq.test(tb))
  inertia <- lapply(seq_along(ev), function(i) list(dim = i, sv = fit$sv[i], inertia = ev[i], pct = 100 * ev[i] / sum(ev), cum = 100 * sum(ev[1:i]) / sum(ev)))
  coords <- function(M, mass, type) lapply(seq_len(nrow(M)), function(i) c(list(type = type, cat = rownames(M)[i], mass = mass[i]), stats::setNames(as.list(M[i, 1:min(2, ncol(M))]), paste0("d", 1:min(2, ncol(M))))))
  rp <- fit$rowcoord %*% diag(fit$sv, nrow = length(fit$sv)); cp <- fit$colcoord %*% diag(fit$sv, nrow = length(fit$sv)); rownames(rp) <- fit$rownames; rownames(cp) <- fit$colnames
  tables <- list(list(id = "ca_inertia", title = "Correspondence Analysis: Inertia", rows = inertia,
                      columns = list(stats_col("dim", "Dimension", "integer"), stats_col("sv", "Singular value"), stats_col("inertia", "Inertia"), stats_col("pct", "% of inertia"), stats_col("cum", "Cumulative %")),
                      notes = list(sprintf("Total inertia %.4f; χ²(%d) = %.2f, p %s.", sum(ev), chi$parameter, chi$statistic, pfmt(chi$p.value)))),
                 list(id = "ca_coords", title = "Principal Coordinates", rows = c(coords(rp, fit$rowmass, r), coords(cp, fit$colmass, cc)),
                      columns = c(list(stats_col("type", "", "text", combineBelow = TRUE), stats_col("cat", "Category", "text"), stats_col("mass", "Mass")), lapply(1:min(2, ncol(rp)), function(i) stats_col(paste0("d", i), paste("Dim", i)))), notes = list()))
  pl <- if (ncol(rp) >= 2) stats_png(function() { rg <- range(c(rp[, 1:2], cp[, 1:2])) * 1.15
    graphics::plot(rp[, 1], rp[, 2], xlim = rg, ylim = rg, pch = 16, col = "#1d4f86", xlab = sprintf("Dimension 1 (%.1f%%)", 100 * ev[1] / sum(ev)), ylab = sprintf("Dimension 2 (%.1f%%)", 100 * ev[2] / sum(ev)), main = "")
    graphics::points(cp[, 1], cp[, 2], pch = 17, col = "#c2410c"); graphics::abline(h = 0, v = 0, col = "#cbd5e1", lty = 2)
    graphics::text(rp[, 1], rp[, 2], rownames(rp), pos = 3, col = "#1d4f86", cex = 0.85); graphics::text(cp[, 1], cp[, 2], rownames(cp), pos = 1, col = "#c2410c", cex = 0.85)
    graphics::legend("topright", c(r, cc), pch = c(16, 17), col = c("#1d4f86", "#c2410c"), bty = "n", cex = 0.85) }, title = "Symmetric map (principal coordinates)", w = 7, h = 6) else NULL
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = sum(tb))
}

run_mds <- function(d, o) {
  vars <- opt(o, "vars", character(0)); X <- as.data.frame(lapply(d[, vars, drop = FALSE], as.numeric)); X <- X[stats::complete.cases(X), , drop = FALSE]
  target <- opt(o, "target", "variables"); k <- round(num_opt(o, "dims", 2, 1, 5))
  if (target == "variables") { D <- stats::as.dist(1 - abs(stats::cor(X))); labs <- vars }
  else { if (nrow(X) > 300) api_error(422, "mds_cases", "MDS of cases is limited to 300 cases."); lab <- opt(o, "label"); labs <- if (!is.null(lab)) as.character(d[[lab]])[as.integer(rownames(X))] else rownames(X); D <- stats::dist(scale(X)) }
  if (identical(opt(o, "method", "classical"), "nonmetric")) { fit <- MASS::isoMDS(D, k = k, trace = FALSE); P <- fit$points; fitv <- sprintf("Kruskal stress = %.3f (%s)", fit$stress / 100, if (fit$stress / 100 < 0.05) "excellent" else if (fit$stress / 100 < 0.1) "good" else if (fit$stress / 100 < 0.2) "fair" else "poor") }
  else { fit <- stats::cmdscale(D, k = k, eig = TRUE); P <- fit$points; fitv <- sprintf("Variance accounted for (GOF) = %.3f", fit$GOF[1]) }
  rownames(P) <- labs
  list(tables = list(list(id = "mds_coords", title = "MDS Coordinates", rows = lapply(seq_len(nrow(P)), function(i) c(list(obj = labs[i]), stats::setNames(as.list(P[i, ]), paste0("d", seq_len(ncol(P)))))),
                          columns = c(list(stats_col("obj", if (target == "variables") "Variable" else "Case", "text")), lapply(seq_len(ncol(P)), function(i) stats_col(paste0("d", i), paste("Dim", i)))),
                          notes = list(paste0(fitv, ". Distances: ", if (target == "variables") "1 − |r| between variables" else "Euclidean between standardised cases", ".")))),
       plots = Filter(Negate(is.null), list(if (ncol(P) >= 2) stats_png(function() { graphics::plot(P[, 1], P[, 2], pch = 16, col = "#1d4f86", xlab = "Dimension 1", ylab = "Dimension 2", main = "", xlim = range(P[, 1]) * 1.2, ylim = range(P[, 2]) * 1.2)
         graphics::text(P[, 1], P[, 2], labs, pos = 3, cex = 0.8, col = "#1f2937"); graphics::abline(h = 0, v = 0, col = "#cbd5e1", lty = 2) }, title = "MDS map", w = 7, h = 6))), n = nrow(X))
}

# ---- Classification -------------------------------------------------------------------------------------------------
cluster_data <- function(d, o) {
  vars <- opt(o, "vars", character(0)); X <- as.data.frame(lapply(d[, vars, drop = FALSE], as.numeric)); X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 4) api_error(422, "cluster_n", "Too few complete cases for clustering.")
  list(X = X, Z = if (!isFALSE(opt(o, "standardize", TRUE))) scale(X) else as.matrix(X), vars = vars)
}
cluster_tables <- function(cd, cl, title) {
  k <- max(cl); sizes <- table(factor(cl, levels = seq_len(k)))
  cen <- lapply(seq_len(k), function(j) c(list(cluster = j, n = as.integer(sizes[[j]])), stats::setNames(as.list(colMeans(cd$X[cl == j, , drop = FALSE])), cd$vars)))
  an <- lapply(cd$vars, function(v) { a <- stats::anova(stats::lm(cd$X[[v]] ~ factor(cl))); list(var = v, f = a$`F value`[1], df1 = a$Df[1], df2 = a$Df[2], p = a$`Pr(>F)`[1]) })
  sil <- if (k > 1) mean(cluster::silhouette(cl, stats::dist(cd$Z))[, 3]) else NA
  list(list(id = "cl_centers", title = paste0(title, ": Cluster Means"), rows = cen, columns = c(list(stats_col("cluster", "Cluster", "integer"), stats_col("n", "N", "integer")), lapply(cd$vars, function(v) stats_col(v, v))),
            notes = list(sprintf("Average silhouette width = %.3f (> .50 reasonable, > .70 strong structure). Means on the original scale.", sil))),
       list(id = "cl_anova", title = "Variable Differences between Clusters (ANOVA)", rows = an, columns = list(stats_col("var", "", "text"), stats_col("f", "F"), stats_col("df1", "df1", "integer"), stats_col("df2", "df2", "integer"), stats_col("p", "p", pvalue = TRUE)),
            notes = list("Descriptive only: clusters were formed to differ on these variables, so the p values are not proper tests.")))
}
cluster_plot <- function(cd, cl, title) stats_png(function() { pc <- stats::prcomp(cd$Z); cols <- c("#1d4f86", "#c2410c", "#15803d", "#7c3aed", "#be185d", "#0e7490", "#a16207", "#475569")
  graphics::plot(pc$x[, 1], if (ncol(pc$x) > 1) pc$x[, 2] else 0 * pc$x[, 1], col = cols[(cl - 1) %% 8 + 1], pch = 16, xlab = "Principal component 1", ylab = "Principal component 2", main = "")
  graphics::legend("topright", paste("Cluster", seq_len(max(cl))), col = cols[(seq_len(max(cl)) - 1) %% 8 + 1], pch = 16, bty = "n", cex = 0.85) }, title = title)

run_kmeans <- function(d, o) {
  cd <- cluster_data(d, o); set.seed(round(num_opt(o, "seed", 2026))); k <- round(num_opt(o, "k", 3, 2, 20))
  pam <- identical(opt(o, "method", "kmeans"), "pam")
  fit <- if (pam) cluster::pam(cd$Z, k) else stats::kmeans(cd$Z, k, nstart = 25)
  cl <- if (pam) fit$clustering else fit$cluster
  sel <- lapply(2:min(10, nrow(cd$Z) - 1), function(kk) { f <- if (pam) cluster::pam(cd$Z, kk) else stats::kmeans(cd$Z, kk, nstart = 10); c2 <- if (pam) f$clustering else f$cluster
    list(k = kk, wss = if (pam) NULL else f$tot.withinss, sil = mean(cluster::silhouette(c2, stats::dist(cd$Z))[, 3])) })
  tabs <- cluster_tables(cd, cl, if (pam) "K-Medoids (PAM)" else "K-Means")
  tabs[[3]] <- list(id = "cl_k", title = "Choosing the Number of Clusters", rows = sel, columns = list(stats_col("k", "k", "integer"), stats_col("wss", "Within SS"), stats_col("sil", "Average silhouette")),
                    notes = list("Pick k at the elbow of the within-cluster sum of squares or at the highest average silhouette."))
  list(tables = tabs, plots = Filter(Negate(is.null), list(cluster_plot(cd, cl, "Clusters on the first two principal components"))), n = nrow(cd$X))
}

run_hclust <- function(d, o) {
  cd <- cluster_data(d, o); m <- opt(o, "method", "ward.D2"); if (!m %in% c("ward.D2", "average", "complete", "single")) m <- "ward.D2"
  h <- stats::hclust(stats::dist(cd$Z), method = m); k <- round(num_opt(o, "k", 3, 2, 20)); cl <- stats::cutree(h, k)
  lab <- opt(o, "label"); labs <- if (!is.null(lab)) as.character(d[[lab]])[as.integer(rownames(cd$X))] else rownames(cd$X)
  sched <- utils::tail(data.frame(step = seq_along(h$height), height = h$height), 15)
  tabs <- cluster_tables(cd, cl, "Hierarchical Clustering")
  tabs[[3]] <- list(id = "cl_sched", title = "Agglomeration Schedule (last 15 steps)", rows = lapply(seq_len(nrow(sched)), function(i) list(step = sched$step[i], clusters = nrow(cd$X) - sched$step[i], height = sched$height[i])),
                    columns = list(stats_col("step", "Step", "integer"), stats_col("clusters", "Clusters after step", "integer"), stats_col("height", "Merge distance")), notes = list(paste0("Linkage: ", m, "; Euclidean distances", if (!isFALSE(opt(o, "standardize", TRUE))) " on z-scores" else "", ". A large jump in the merge distance suggests where to cut.")))
  den <- stats_png(function() { graphics::plot(h, labels = if (length(labs) <= 80) labs else FALSE, main = "", xlab = "", sub = "", ylab = "Merge distance", cex = 0.7, hang = -1); stats::rect.hclust(h, k = k, border = c("#1d4f86", "#c2410c", "#15803d", "#7c3aed", "#be185d")[seq_len(k) %% 5 + 1]) }, title = "Dendrogram", w = 8, h = 5)
  list(tables = tabs, plots = Filter(Negate(is.null), list(den, cluster_plot(cd, cl, "Clusters on the first two principal components"))), n = nrow(cd$X))
}

run_lda <- function(d, o) {
  g <- opt(o, "group")[1]; vars <- opt(o, "vars", character(0))
  dd <- data.frame(g = factor(d[[g]])); for (v in vars) dd[[make.names(v)]] <- as.numeric(d[[v]]); dd <- droplevels(dd[stats::complete.cases(dd), , drop = FALSE])
  if (nlevels(dd$g) < 2) api_error(422, "lda_groups", "The grouping variable needs at least two groups.")
  nm <- stats::setNames(vars, make.names(vars))
  pri <- if (identical(opt(o, "prior", "equal"), "equal")) rep(1 / nlevels(dd$g), nlevels(dd$g)) else as.numeric(prop.table(table(dd$g)))
  fit <- MASS::lda(g ~ ., data = dd, prior = pri)
  sc <- as.matrix(dd[, -1, drop = FALSE]) %*% fit$scaling
  ev <- fit$svd^2; k <- length(ev)
  # Wilks' lambda per function set (Bartlett χ²).
  can <- vapply(seq_len(k), function(i) stats::cor(sc[, i], as.numeric(stats::lm(sc[, i] ~ dd$g)$fitted)), 0)
  wl <- lapply(seq_len(k), function(i) { L <- prod(1 - can[i:k]^2); chi <- -(nrow(dd) - 1 - (length(vars) + nlevels(dd$g)) / 2) * log(L); df <- (length(vars) - i + 1) * (nlevels(dd$g) - i)
    list(fn = i, eig = ev[i] / sum(ev) * 100, cancor = can[i], wilks = L, chisq = chi, df = df, p = stats::pchisq(chi, df, lower.tail = FALSE)) })
  struct <- stats::cor(as.matrix(dd[, -1, drop = FALSE]), sc)
  coefs <- lapply(seq_len(nrow(fit$scaling)), function(i) c(list(var = nm[[rownames(fit$scaling)[i]]]), stats::setNames(as.list(fit$scaling[i, ] * apply(dd[, -1, drop = FALSE], 2, stats::sd)[i]), paste0("f", seq_len(k))),
                                                             stats::setNames(as.list(struct[i, ]), paste0("s", seq_len(k)))))
  pr <- if (isTRUE(opt(o, "cv", TRUE))) MASS::lda(g ~ ., data = dd, prior = pri, CV = TRUE)$class else stats::predict(fit)$class
  ct <- table(Actual = dd$g, Predicted = pr)
  crow <- lapply(rownames(ct), function(a) c(list(actual = a), stats::setNames(as.list(as.integer(ct[a, ])), colnames(ct)), list(pct = 100 * ct[a, a] / sum(ct[a, ]))))
  list(tables = list(
    list(id = "lda_fn", title = "Discriminant Functions", rows = wl, columns = list(stats_col("fn", "Function", "integer"), stats_col("eig", "% of variance"), stats_col("cancor", "Canonical r"), stats_col("wilks", "Wilks' Λ"), stats_col("chisq", "χ²"), stats_col("df", "df", "integer"), stats_col("p", "p", pvalue = TRUE)),
         notes = list("Wilks' Λ tests this and all later functions (Bartlett's χ²).")),
    list(id = "lda_coef", title = "Standardized Coefficients and Structure Matrix", rows = coefs,
         columns = c(list(stats_col("var", "", "text")), lapply(seq_len(k), function(i) stats_col(paste0("f", i), paste("Function", i), superTitle = "Standardized coefficients")), lapply(seq_len(k), function(i) stats_col(paste0("s", i), paste("Function", i), superTitle = "Structure correlations"))), notes = list()),
    list(id = "lda_class", title = paste0("Classification Results", if (isTRUE(opt(o, "cv", TRUE))) " (leave-one-out cross-validated)" else ""), rows = crow,
         columns = c(list(stats_col("actual", "Actual group", "text")), lapply(colnames(ct), function(cn) stats_col(cn, cn, "integer", superTitle = "Predicted group")), list(stats_col("pct", "% correct"))),
         notes = list(sprintf("Overall %.1f%% correctly classified. Priors: %s.", 100 * sum(diag(ct)) / sum(ct), if (identical(opt(o, "prior", "equal"), "equal")) "equal" else "group sizes")))),
    plots = Filter(Negate(is.null), list(if (k >= 1) stats_png(function() { cols <- c("#1d4f86", "#c2410c", "#15803d", "#7c3aed", "#be185d", "#0e7490")
      if (k >= 2) graphics::plot(sc[, 1], sc[, 2], col = cols[as.integer(dd$g) %% 6 + 1], pch = 16, xlab = "Function 1", ylab = "Function 2", main = "") else graphics::stripchart(sc[, 1] ~ dd$g, method = "jitter", pch = 16, col = cols, xlab = "Function 1", main = "")
      if (k >= 2) graphics::legend("topright", levels(dd$g), col = cols[seq_len(nlevels(dd$g)) %% 6 + 1], pch = 16, bty = "n", cex = 0.85) }, title = "Discriminant scores"))), n = nrow(dd))
}

STATS_MORE_RUN <- list(glmx = run_glmx, mixed = run_mixed, iv2sls = run_iv2sls, cancor = run_cancor, runs = run_runs, lifetable = run_lifetable, bayesreg = run_bayesreg,
                       arima = run_arima, acf = run_acf, mva = run_mva, mi = run_mi, power = run_power, meta = run_meta, corresp = run_corresp, mds = run_mds, lgc = run_lgc, lca = run_lca, lta = run_lta, gmm = run_gmm,
                       kmeans = run_kmeans, hclust = run_hclust, lda = run_lda)

#' R code that reproduces each of these procedures (shown in the R code tab).
stats_more_code <- function(s, lines, q) {
  o <- s$options; v <- function(x) paste(sprintf("`%s`", unlist(x)), collapse = " + ")
  body <- switch(s$proc,
    glmx = c("library(MASS)", sprintf("fit <- %s(y ~ %s, data = data%s)", switch(o$family %||% "gaussian", negbin = "glm.nb", "glm"), v(c(o$covs, o$factors)),
            switch(o$family %||% "gaussian", probit = ", family = binomial(link = \"probit\")", gamma = ", family = Gamma(link = \"log\")", invgauss = ", family = inverse.gaussian(link = \"log\")", quasipoisson = ", family = quasipoisson()", negbin = "", ", family = gaussian()")),
            "summary(fit)"),
    mixed = c("library(lmerTest)", sprintf("fit <- %s(`%s` ~ %s + %s, data = data%s)", if ((o$family %||% "gaussian") == "gaussian") "lmer" else "lme4::glmer", o$dep, v(c(o$covs, o$factors)),
            paste(sprintf("(1 | `%s`)", unlist(o$cluster)), collapse = " + "), if ((o$family %||% "gaussian") == "gaussian") "" else sprintf(", family = %s", o$family)), "summary(fit)", "lme4::VarCorr(fit)"),
    iv2sls = c("library(ivreg)", sprintf("fit <- ivreg(`%s` ~ %s | %s, data = data)", o$dep, v(c(o$exog, o$endog)), v(c(o$exog, o$instruments))), "summary(fit, diagnostics = TRUE)"),
    cancor = c(sprintf("X <- scale(data[, c(%s)]); Y <- scale(data[, c(%s)])", paste(q(unlist(o$set1)), collapse = ", "), paste(q(unlist(o$set2)), collapse = ", ")), "cancor(X[complete.cases(X, Y), ], Y[complete.cases(X, Y), ])"),
    runs = c("# Wald-Wolfowitz runs test above/below the median", "x <- na.omit(data[[1]]); s <- x[x != median(x)] > median(x); R <- 1 + sum(diff(s) != 0); R"),
    lifetable = c("library(survival)  # actuarial life table computed in PathLab; Kaplan-Meier for comparison:", sprintf("summary(survfit(Surv(data[[%s]], data[[%s]]) ~ 1))", q(o$time), q(o$event))),
    bayesreg = c("library(BayesFactor)", sprintf("bf <- regressionBF(`%s` ~ %s, data = na.omit(data[, c(%s)]))", o$dep, v(o$covs), paste(q(c(o$dep, unlist(o$covs))), collapse = ", ")), "head(sort(bf, decreasing = TRUE), 15)"),
    arima = c("library(forecast)", sprintf("y <- ts(na.omit(data[[%s]]), frequency = %s)", q(o$var), o$frequency %||% 1), switch(o$model %||% "auto", ets = "fit <- ets(y)", manual = sprintf("fit <- Arima(y, order = c(%s, %s, %s))", o$p %||% 1, o$d %||% 0, o$q %||% 0), "fit <- auto.arima(y)"),
              sprintf("forecast(fit, h = %s)", o$horizon %||% 12), "checkresiduals(fit)"),
    acf = c(sprintf("acf(na.omit(data[[%s]]), lag.max = %s)", q(unlist(o$vars)[1]), o$lags %||% 16), sprintf("pacf(na.omit(data[[%s]]), lag.max = %s)", q(unlist(o$vars)[1]), o$lags %||% 16)),
    mva = c(sprintf("x <- data[, c(%s)]", paste(q(unlist(o$vars)), collapse = ", ")), "colSums(is.na(x)); mice::md.pattern(x)   # Little's MCAR test: naniar::mcar_test(x)"),
    mi = c("library(mice)", sprintf("imp <- mice(data[, c(%s)], m = %s, method = %s, seed = %s)", paste(q(unique(c(o$dep, unlist(o$covs), unlist(o$aux)))), collapse = ", "), o$m %||% 20, q(o$method %||% "pmm"), o$seed %||% 2026),
           sprintf("pool(with(imp, lm(%s ~ %s)))", make.names(o$dep), paste(make.names(unlist(o$covs)), collapse = " + "))),
    power = gpower_code(o),
    soper_sem = soper_code("soper_sem", o), soper_mr = soper_code("soper_mr", o),
    meta = c("library(metafor)", "es <- escalc(...)   # SMD, ZCOR or generic effect sizes as chosen", sprintf("fit <- rma(yi, vi, data = es, method = %s)", q(o$method %||% "REML")), "summary(fit); forest(fit); funnel(fit); regtest(fit); trimfill(fit)"),
    corresp = c("library(ca)", sprintf("fit <- ca(table(data[[%s]], data[[%s]]))", q(unlist(o$rows)[1]), q(unlist(o$cols)[1])), "summary(fit); plot(fit)"),
    mds = c(sprintf("x <- na.omit(data[, c(%s)])", paste(q(unlist(o$vars)), collapse = ", ")), if ((o$target %||% "variables") == "variables") "D <- as.dist(1 - abs(cor(x)))" else "D <- dist(scale(x))",
            if (identical(o$method, "nonmetric")) "MASS::isoMDS(D, k = 2)" else "cmdscale(D, k = 2, eig = TRUE)"),
    lgc = , lca = , lta = , gmm = latent_more_code(s, q),
    kmeans = c(sprintf("z <- scale(na.omit(data[, c(%s)]))", paste(q(unlist(o$vars)), collapse = ", ")), sprintf("set.seed(%s); %s", o$seed %||% 2026, if (identical(o$method, "pam")) sprintf("cluster::pam(z, %s)", o$k %||% 3) else sprintf("kmeans(z, %s, nstart = 25)", o$k %||% 3))),
    hclust = c(sprintf("z <- scale(na.omit(data[, c(%s)]))", paste(q(unlist(o$vars)), collapse = ", ")), sprintf("h <- hclust(dist(z), method = %s)", q(o$method %||% "ward.D2")), sprintf("cutree(h, k = %s); plot(h)", o$k %||% 3)),
    svy = c("library(survey)", sprintf("des <- svydesign(ids = %s, strata = %s, weights = ~`%s`, data = data, nest = TRUE)", if (is.null(o$cluster)) "~1" else sprintf("~`%s`", o$cluster), if (is.null(o$strata)) "NULL" else sprintf("~`%s`", o$strata), o$weights),
            sprintf("svymean(~`%s`, des, deff = TRUE, na.rm = TRUE)", o$dep),
            if (length(c(unlist(o$covs), unlist(o$factors)))) sprintf("summary(svyglm(`%s` ~ %s, design = des%s))", o$dep, v(c(o$covs, o$factors)), if (identical(o$family, "binomial")) ", family = quasibinomial()" else "")),
    did = c("library(sandwich); library(lmtest)", sprintf("fit <- lm(`%s` ~ `%s` * `%s`%s, data = data)   # treatment (0/1) x period (0/1)", o$dep, o$treat, o$post, if (length(c(unlist(o$covs), unlist(o$factors)))) paste(" +", v(c(o$covs, o$factors))) else ""),
            sprintf("coeftest(fit, vcov. = %s)   # the interaction is the DiD effect", if (is.null(o$cluster)) "vcovHC(fit, type = \"HC1\")" else sprintf("vcovCL(fit, cluster = ~`%s`, type = \"HC1\")", o$cluster))),
    psm = c("library(MatchIt); library(sandwich); library(lmtest)", sprintf("set.seed(%s)", o$seed %||% 2026),
            sprintf("m <- matchit(`%s` ~ %s, data = data, method = \"%s\", distance = \"glm\", estimand = \"ATT\"%s)", o$treat, v(c(o$covs, o$factors)), o$method %||% "nearest",
                    if ((o$method %||% "nearest") == "nearest") sprintf(", ratio = %s, replace = %s%s", o$ratio %||% 1, if (isTRUE(o$replace)) "TRUE" else "FALSE", if ((o$caliper %||% 0) > 0) sprintf(", caliper = %s", o$caliper) else "") else ""),
            "summary(m)   # balance before and after matching", "md <- match.data(m)",
            if (!is.null(o$dep)) c(sprintf("fit <- lm(`%s` ~ `%s`, data = md, weights = weights)", o$dep, o$treat), "coeftest(fit, vcov. = vcovCL, cluster = ~subclass)   # ATT")),
    ipw = c("library(sandwich); library(lmtest)", sprintf("ps <- fitted(glm(`%s` ~ %s, data = data, family = binomial()))", o$treat, v(c(o$covs, o$factors))),
            sprintf("t <- data$`%s`", o$treat), switch(o$estimand %||% "ATE", ATT = "w <- ifelse(t == 1, 1, ps / (1 - ps))",
              if (isFALSE(o$stabilized)) "w <- ifelse(t == 1, 1 / ps, 1 / (1 - ps))" else "w <- ifelse(t == 1, mean(t) / ps, (1 - mean(t)) / (1 - ps))   # stabilised"),
            sprintf("fit <- lm(`%s` ~ t, data = data, weights = w)", o$dep), "coeftest(fit, vcov. = vcovHC(fit, type = \"HC0\"))"),
    nca = c("library(NCA)", sprintf("d <- na.omit(data[, c(%s)])", paste(q(c(unlist(o$conditions), o$outcome)), collapse = ", ")),
            sprintf("set.seed(%s); model <- nca_analysis(d, x = c(%s), y = %s, ceilings = c(%s), test.rep = %s, steps = %s%s%s)", o$seed %||% 2026, paste(q(unlist(o$conditions)), collapse = ", "), q(o$outcome),
                    paste(q(unlist(o$ceilings %||% list("ce_fdh", "cr_fdh"))), collapse = ", "), o$testRep %||% 1000, o$steps %||% 10,
                    if (!is.null(o$corner) && o$corner != "1") sprintf(", corner = %s", o$corner) else "",
                    if (!is.null(o$scopeMin) && !is.null(o$scopeMax)) sprintf(", scope = list(c(%s, %s, %s, %s))", o$scopeMin, o$scopeMax, o$scopeMin, o$scopeMax) else ""),
            "nca_output(model, summaries = TRUE, bottlenecks = TRUE, plots = TRUE)",
            if (isTRUE(o$outliers)) sprintf("for (x in c(%s)) print(nca_outliers(d, x, %s))", paste(q(unlist(o$conditions)), collapse = ", "), q(o$outcome)),
            if (isTRUE(o$power)) sprintf("nca_power(n = nrow(d), effect = %s, rep = %s)", o$powerEffect %||% 0.1, o$powerRep %||% 100)),
    banovaRM = c("library(BayesFactor)", sprintf("w <- na.omit(data[, c(%s)]); n <- nrow(w)", paste(q(c(unlist(o$measures), unlist(o$bs))), collapse = ", ")),
                 sprintf("long <- data.frame(id = factor(rep(1:n, %d)), cond = factor(rep(c(%s), each = n)), value = unlist(w[, c(%s)]))", length(o$measures), paste(q(unlist(o$measures)), collapse = ", "), paste(q(unlist(o$measures)), collapse = ", ")),
                 if (length(o$bs)) sprintf("long[[%s]] <- factor(rep(w[[%s]], %d))", q(unlist(o$bs)), q(unlist(o$bs)), length(o$measures)) else NULL,
                 sprintf("set.seed(%s); anovaBF(value ~ %s + id, data = long, whichRandom = \"id\", rscaleFixed = %s)", o$seed %||% 2026,
                         paste(c("cond", sprintf("`%s`", unlist(o$bs))), collapse = " * "), q(o$rscale %||% "medium"))),
    kappa = {
      r <- paste(q(unlist(o$raters)), collapse = ", ")
      c("library(irr)", sprintf("ratings <- na.omit(data[, c(%s)])", r),
        if (length(o$raters) == 2) c("kappa2(ratings)   # Cohen's kappa", if (!identical(o$weights %||% "none", "none")) sprintf("kappa2(ratings, weight = %s)   # weighted kappa", q(if (identical(o$weights, "linear")) "equal" else "squared")))
        else c("kappam.fleiss(ratings, detail = TRUE)   # Fleiss' kappa", if (isTRUE(o$light) || isTRUE(o$pairwise)) "kappam.light(ratings)   # Light's kappa"),
        if (isTRUE(o$kripp)) sprintf("kripp.alpha(t(sapply(data[, c(%s)], function(x) as.numeric(factor(x)))), method = %s)", r, q(o$krippLevel %||% "nominal")))
    },
    icc = {
      m <- o$model %||% "random"; t <- if (identical(m, "oneway")) "agreement" else o$type %||% "agreement"
      c("library(irr)", sprintf("ratings <- na.omit(data[, c(%s)])", paste(q(unlist(o$raters)), collapse = ", ")),
        sprintf("icc(ratings, model = %s, type = %s, unit = \"single\")", q(if (identical(m, "oneway")) "oneway" else "twoway"), q(t)),
        sprintf("icc(ratings, model = %s, type = %s, unit = \"average\")", q(if (identical(m, "oneway")) "oneway" else "twoway"), q(t)),
        if (isTRUE(o$allForms)) "psych::ICC(ratings)   # all six Shrout & Fleiss forms")
    },
    lda = c("library(MASS)", sprintf("fit <- lda(factor(`%s`) ~ %s, data = data, CV = %s)", unlist(o$group)[1], v(o$vars), if (isTRUE(o$cv %||% TRUE)) "TRUE" else "FALSE"), "table(data[[1]], fit$class)"),
    character(0))
  paste(c(lines, body), collapse = "\n")
}

# ---- Agreement: Cohen's / Fleiss' kappa, Krippendorff's alpha and intraclass correlations (irr) ----------------------
# Cohen's kappa with its large-sample standard errors (Fleiss, Cohen & Everitt, 1969) is computed here so the categories
# keep their order (irr::kappa2 sorts numeric codes as text, which misorders weighted kappa for 10 or more categories);
# kappa and z equal irr::kappa2, and the standard errors and confidence intervals equal vcd::Kappa (weighted and unweighted).
kappa_label <- function(k) if (is.na(k)) "" else if (k < 0) "Poor" else if (k <= 0.20) "Slight" else if (k <= 0.40) "Fair" else if (k <= 0.60) "Moderate" else if (k <= 0.80) "Substantial" else "Almost perfect"
icc_label <- function(x) if (is.na(x)) "" else if (x < 0.50) "Poor" else if (x < 0.75) "Moderate" else if (x < 0.90) "Good" else "Excellent"
KAPPA_NOTE <- "Agreement: Landis & Koch (1977): below 0 poor, 0–.20 slight, .21–.40 fair, .41–.60 moderate, .61–.80 substantial, .81–1 almost perfect."

#' The categories used by the raters, in order: numeric codes sorted as numbers, otherwise the level order.
agree_levels <- function(cols) {
  lv <- unique(unlist(lapply(cols, function(x) if (is.factor(x)) levels(droplevels(x)) else as.character(sort(unique(x[!is.na(x)]))))))
  num <- suppressWarnings(as.numeric(lv))
  if (!anyNA(num)) lv[order(num)] else lv
}

cohen_kappa <- function(a, b, lev, weight = "none", ci = 95) {
  tab <- table(factor(as.character(a), levels = lev), factor(as.character(b), levels = lev))
  n <- sum(tab); k <- length(lev); p <- tab / n; pr <- rowSums(p); pc <- colSums(p)
  d <- abs(outer(seq_len(k), seq_len(k), "-")) / max(1, k - 1)
  W <- switch(weight, linear = 1 - d, quadratic = 1 - d^2, diag(k))
  po <- sum(W * p); pe <- sum(W * outer(pr, pc)); kap <- (po - pe) / (1 - pe)
  wi <- as.vector(W %*% pc); wj <- as.vector(t(W) %*% pr)
  v1 <- (sum(p * (W - outer(wi, wj, "+") * (1 - kap))^2) - (kap - pe * (1 - kap))^2) / (n * (1 - pe)^2)
  v0 <- (sum(outer(pr, pc) * (W - outer(wi, wj, "+"))^2) - pe^2) / (n * (1 - pe)^2)
  se <- sqrt(max(v1, 0)); se0 <- sqrt(max(v0, 0)); z <- kap / se0; crit <- stats::qnorm(1 - (1 - ci / 100) / 2)
  list(kappa = kap, se = se, lower = max(-1, kap - crit * se), upper = min(1, kap + crit * se), z = z, p = 2 * stats::pnorm(-abs(z)), po = po, pe = pe, n = n, tab = tab)
}

run_kappa <- function(d, o) {
  raters <- opt(o, "raters", character(0))
  if (length(raters) < 2) api_error(422, "kappa_raters", "Add at least two raters (one column per rater, one row per rated case).")
  ci <- num_opt(o, "ciWidth", 95, 50, 99.9); wt <- opt(o, "weights", "none"); if (!wt %in% c("none", "linear", "quadratic")) wt <- "none"
  all <- d[, raters, drop = FALSE]
  cc <- all[stats::complete.cases(all), , drop = FALSE]; n <- nrow(cc)
  if (n < 2) api_error(422, "kappa_cases", "Fewer than two cases were rated by every rater.")
  lev <- agree_levels(all)
  if (length(lev) < 2) api_error(422, "kappa_categories", "The raters used only one category, so agreement beyond chance cannot be estimated.")
  chr <- as.data.frame(lapply(cc, as.character), stringsAsFactors = FALSE)
  exact <- mean(apply(chr, 1, function(r) length(unique(r)) == 1))
  tables <- list(); ci_t <- sprintf("%g%% CI", ci)
  wlab <- c(none = "unweighted", linear = "linear weights", quadratic = "quadratic weights")
  if (length(raters) == 2) {
    k0 <- cohen_kappa(cc[[1]], cc[[2]], lev, "none", ci)
    rows <- list(list(stat = "Cohen's kappa", kappa = k0$kappa, se = k0$se, lower = k0$lower, upper = k0$upper, z = k0$z, p = k0$p, interp = kappa_label(k0$kappa)))
    if (wt != "none") { kw <- cohen_kappa(cc[[1]], cc[[2]], lev, wt, ci)
      rows[[2]] <- list(stat = paste0("Weighted kappa (", wlab[[wt]], ")"), kappa = kw$kappa, se = kw$se, lower = kw$lower, upper = kw$upper, z = kw$z, p = kw$p, interp = kappa_label(kw$kappa)) }
    tables[[1]] <- list(id = "kappa", title = sprintf("Cohen's Kappa: %s and %s", raters[1], raters[2]),
      columns = list(stats_col("stat", "", "text"), stats_col("kappa", "κ"), stats_col("se", "SE"), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t),
                     stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("interp", "Agreement", "text")),
      rows = rows,
      notes = Filter(Negate(is.null), list(
        sprintf("N = %d cases rated by both raters; %d categories. Observed agreement %.1f%%, expected by chance %.1f%%.", n, length(lev), 100 * k0$po, 100 * k0$pe),
        "SE: large-sample standard error (Fleiss, Cohen & Everitt, 1969); z tests κ = 0 with the standard error under the null hypothesis.",
        if (wt != "none") "Weighted kappa gives partial credit to near-misses, using the order of the categories (ordinal ratings)." else NULL,
        KAPPA_NOTE)))
    if (!isFALSE(opt(o, "crosstab", TRUE))) {
      tab <- k0$tab; js <- seq_along(lev)
      tables[[length(tables) + 1]] <- list(id = "kappa_table", title = "Agreement Table",
        columns = c(list(stats_col("cat", raters[1], "text")), lapply(js, function(j) stats_col(paste0("c", j), lev[j], "integer", superTitle = raters[2])), list(stats_col("total", "Total", "integer"))),
        rows = c(lapply(js, function(i) c(list(cat = lev[i]), stats::setNames(as.list(as.integer(tab[i, ])), paste0("c", js)), list(total = as.integer(sum(tab[i, ]))))),
                 list(c(list(cat = "Total"), stats::setNames(as.list(as.integer(colSums(tab))), paste0("c", js)), list(total = as.integer(sum(tab)))))),
        notes = list("Agreements are on the diagonal."))
    }
  } else {
    fl <- irr::kappam.fleiss(chr, detail = TRUE)
    rows <- list(list(stat = "Fleiss' kappa", kappa = fl$value, z = unname(fl$statistic), p = fl$p.value, interp = kappa_label(fl$value)))
    pk <- NULL; lk <- NULL
    if (isTRUE(opt(o, "pairwise", FALSE)) || isTRUE(opt(o, "light", FALSE))) {
      prs <- utils::combn(length(raters), 2, simplify = FALSE)
      pk <- lapply(prs, function(ij) c(list(r1 = raters[ij[1]], r2 = raters[ij[2]]), cohen_kappa(cc[[ij[1]]], cc[[ij[2]]], lev, wt, ci)[c("kappa", "se", "lower", "upper", "z", "p")]))
      lk <- mean(vapply(pk, function(x) x$kappa, 0))
      if (isTRUE(opt(o, "light", FALSE))) rows[[2]] <- list(stat = "Light's kappa (mean pairwise Cohen's)", kappa = lk, interp = kappa_label(lk))
    }
    tables[[1]] <- list(id = "kappa", title = sprintf("Fleiss' Kappa: %d Raters", length(raters)),
      columns = list(stats_col("stat", "", "text"), stats_col("kappa", "κ"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("interp", "Agreement", "text")),
      rows = rows,
      notes = list(sprintf("N = %d cases rated by all %d raters; %d categories. All raters agreed exactly on %.1f%% of the cases.", n, length(raters), length(lev), 100 * exact),
                   "Fleiss' kappa treats the categories as unordered; z tests κ = 0 (Fleiss, 1971).", KAPPA_NOTE))
    if (isTRUE(opt(o, "categories", FALSE))) {
      dt <- fl$detail
      tables[[length(tables) + 1]] <- list(id = "kappa_categories", title = "Fleiss' Kappa by Category",
        columns = list(stats_col("cat", "Category", "text"), stats_col("kappa", "κ"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("interp", "Agreement", "text")),
        rows = lapply(seq_len(nrow(dt)), function(i) list(cat = sub("^[0-9]+\\. ", "", rownames(dt)[i]), kappa = dt[i, "Kappa"], z = dt[i, "z"], p = dt[i, "p.value"], interp = kappa_label(dt[i, "Kappa"]))),
        notes = list("Agreement on each category against all the others combined."))
    }
    if (isTRUE(opt(o, "pairwise", FALSE))) {
      tables[[length(tables) + 1]] <- list(id = "kappa_pairs", title = paste0("Cohen's Kappa for Each Pair of Raters", if (wt != "none") paste0(" (", wlab[[wt]], ")") else ""),
        columns = list(stats_col("r1", "Rater", "text"), stats_col("r2", "Rater", "text"), stats_col("kappa", "κ"), stats_col("se", "SE"), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t),
                       stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("interp", "Agreement", "text")),
        rows = lapply(pk, function(x) c(x, list(interp = kappa_label(x$kappa)))),
        notes = list(sprintf("Light's kappa (the mean of these values) = %.3f.", lk)))
    }
  }
  if (isTRUE(opt(o, "kripp", FALSE))) {
    lvl <- opt(o, "krippLevel", "nominal"); if (!lvl %in% c("nominal", "ordinal", "interval", "ratio")) lvl <- "nominal"
    codes <- sapply(all, function(x) {
      num <- suppressWarnings(as.numeric(as.character(x)))
      if (lvl %in% c("interval", "ratio") && !anyNA(num[!is.na(x)])) num else as.numeric(match(as.character(x), lev))
    })
    codes <- matrix(codes, nrow = nrow(all))
    used <- rowSums(!is.na(codes)) >= 2
    ka <- irr::kripp.alpha(t(codes[used, , drop = FALSE]), method = lvl)
    tables[[length(tables) + 1]] <- list(id = "kripp", title = "Krippendorff's Alpha",
      columns = list(stats_col("level", "Level of measurement", "text"), stats_col("alpha", "α"), stats_col("units", "Cases", "integer"), stats_col("raters", "Raters", "integer"), stats_col("interp", "Agreement", "text")),
      rows = list(list(level = tools::toTitleCase(lvl), alpha = ka$value, units = sum(used), raters = length(raters),
                       interp = if (is.na(ka$value)) "" else if (ka$value >= 0.80) "Reliable" else if (ka$value >= 0.667) "Tentative" else "Unreliable")),
      notes = list("Uses every case rated by at least two raters, so missing ratings are allowed. Krippendorff (2004): α ≥ .80 reliable; .667 to .80 allows only tentative conclusions."))
  }
  list(tables = tables, plots = list(), n = n)
}

ICC_MODEL <- c(oneway = "One-way random effects", random = "Two-way random effects", mixed = "Two-way mixed effects")
run_icc <- function(d, o) {
  raters <- opt(o, "raters", character(0))
  if (length(raters) < 2) api_error(422, "icc_raters", "Add at least two raters or measurements (one column each, one row per rated case).")
  x <- as.data.frame(lapply(d[, raters, drop = FALSE], function(v) as.numeric(v)))
  x <- x[stats::complete.cases(x), , drop = FALSE]; n <- nrow(x)
  if (n < 3) api_error(422, "icc_cases", "At least three cases need a rating from every rater.")
  ci <- num_opt(o, "ciWidth", 95, 50, 99.9); r0 <- num_opt(o, "testValue", 0, -0.99, 0.99)
  model <- opt(o, "model", "random"); if (!model %in% names(ICC_MODEL)) model <- "random"
  type <- opt(o, "type", "agreement"); if (!type %in% c("agreement", "consistency")) type <- "agreement"
  if (model == "oneway") type <- "agreement"
  one <- function(m, t, u) irr::icc(x, model = if (m == "oneway") "oneway" else "twoway", type = t, unit = u, r0 = r0, conf.level = ci / 100)
  row <- function(r) list(icc = r$value, lower = r$lbound, upper = r$ubound, f = r$Fvalue, df1 = r$df1, df2 = r$df2, p = r$p.value, interp = icc_label(r$value))
  s1 <- one(model, type, "single"); sk <- one(model, type, "average")
  ci_t <- sprintf("%g%% CI", ci); tt <- sprintf("Test against %g", r0)
  cols <- list(stats_col("icc", "ICC"), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t),
               stats_col("f", "F", superTitle = tt), stats_col("df1", "df1", superTitle = tt), stats_col("df2", "df2", superTitle = tt),
               stats_col("p", "p", superTitle = tt, pvalue = TRUE), stats_col("interp", "Reliability", "text"))
  sf <- if (model == "oneway") c("ICC(1,1)", "ICC(1,k)") else if (type == "agreement") c("ICC(2,1)", "ICC(2,k)") else c("ICC(3,1)", "ICC(3,k)")
  tables <- list(list(id = "icc", title = sprintf("Intraclass Correlation: %s, %s", ICC_MODEL[[model]], if (type == "agreement") "absolute agreement" else "consistency"),
    columns = c(list(stats_col("unit", "", "text"), stats_col("name", "Form", "text")), cols),
    rows = list(c(list(unit = "Single rater", name = s1$icc.name), row(s1)), c(list(unit = sprintf("Average of %d raters", length(raters)), name = sk$icc.name), row(sk))),
    notes = Filter(Negate(is.null), list(
      sprintf("N = %d cases rated by all %d raters. Form in McGraw & Wong (1996) notation; Shrout & Fleiss (1979): %s and %s.", n, length(raters), sf[1], sf[2]),
      "Single rater: the reliability of one rater's scores. Average: the reliability of the mean of all raters' scores (report it when the mean is analysed).",
      if (model == "mixed") "Two-way mixed: these raters are the only raters of interest. The values equal the two-way random model; only the interpretation differs." else NULL,
      "Reliability: Koo & Li (2016): below .50 poor, .50–.75 moderate, .75–.90 good, above .90 excellent. Judge it by the confidence interval as well as the estimate."))))
  if (isTRUE(opt(o, "allForms", FALSE))) {
    forms <- list(c("ICC1", "oneway", "agreement", "single", "One-way random, single rater"), c("ICC2", "random", "agreement", "single", "Two-way random, absolute agreement, single rater"),
                  c("ICC3", "mixed", "consistency", "single", "Two-way mixed, consistency, single rater"), c("ICC1k", "oneway", "agreement", "average", "One-way random, average of raters"),
                  c("ICC2k", "random", "agreement", "average", "Two-way random, absolute agreement, average of raters"), c("ICC3k", "mixed", "consistency", "average", "Two-way mixed, consistency, average of raters"))
    tables[[length(tables) + 1]] <- list(id = "icc_forms", title = "All Six ICC Forms (Shrout & Fleiss)",
      columns = c(list(stats_col("form", "Form", "text"), stats_col("desc", "Model", "text")), cols),
      rows = lapply(forms, function(f) c(list(form = f[1], desc = f[5]), row(one(f[2], f[3], f[4])))),
      notes = list("Shrout & Fleiss (1979) naming, as in psych::ICC and SPSS."))
  }
  if (isTRUE(opt(o, "desc", FALSE))) {
    tables[[length(tables) + 1]] <- list(id = "icc_desc", title = "Rater Descriptives",
      columns = list(stats_col("rater", "Rater", "text"), stats_col("n", "N", "integer"), stats_col("mean", "Mean"), stats_col("sd", "SD"), stats_col("min", "Minimum"), stats_col("max", "Maximum")),
      rows = lapply(raters, function(r) list(rater = r, n = n, mean = mean(x[[r]]), sd = stats::sd(x[[r]]), min = min(x[[r]]), max = max(x[[r]]))),
      notes = list("Large differences between the rater means lower absolute agreement but not consistency."))
  }
  list(tables = tables, plots = list(), n = n)
}
STATS_MORE_RUN$kappa <- run_kappa
STATS_MORE_RUN$icc <- run_icc

# ---- Equivalence tests (TOST: two one-sided tests; Lakens, 2017) -------------------------------------------------
# Added to the t-tests and the correlation matrix. The bounds are raw differences or Cohen's d; d bounds become raw
# bounds with the standardiser TOSTER uses (pooled SD for Student's t, average SD for Welch's t, SD of the differences
# for paired samples, SD for one sample), so every value equals TOSTER::t_TOST. For one sample the bounds are around the
# test value. Correlations use Fisher's z (as TOSTER::z_cor_test).
TOST_PROCS <- c("ttestIS", "ttestPS", "ttestOneS", "corrMatrix")

tost_verdict <- function(p_eq, p_nhst, alpha) {
  eq <- p_eq < alpha; dif <- p_nhst < alpha
  if (eq && !dif) "Equivalent" else if (eq && dif) "Equivalent (trivial difference)" else if (dif) "Not equivalent (different)" else "Inconclusive"
}

#' One TOST on a mean difference: estimate m, standard error se, df, raw bounds lo/hi.
tost_t <- function(m, se, df, lo, hi, alpha) {
  tl <- (m - lo) / se; tu <- (m - hi) / se; t0 <- m / se
  pl <- stats::pt(tl, df, lower.tail = FALSE); pu <- stats::pt(tu, df); crit <- stats::qt(1 - alpha, df)
  list(m = m, se = se, df = df, lo = lo, hi = hi, tl = tl, pl = pl, tu = tu, pu = pu, p = max(pl, pu), t0 = t0, p0 = 2 * stats::pt(-abs(t0), df),
       cl = m - crit * se, cu = m + crit * se)
}

run_tost <- function(proc, d, o) {
  alpha <- num_opt(o, "eqAlpha", 0.05, 0.001, 0.25)
  lo_in <- num_opt(o, "eqLow", if (proc == "corrMatrix") -0.3 else -0.5); hi_in <- num_opt(o, "eqHigh", if (proc == "corrMatrix") 0.3 else 0.5)
  if (lo_in >= hi_in) api_error(422, "tost_bounds", "The lower equivalence bound must be below the upper bound.")
  ci_t <- sprintf("%g%% CI", 100 * (1 - 2 * alpha))
  if (proc == "corrMatrix") {
    if (lo_in <= -1 || hi_in >= 1) api_error(422, "tost_bounds", "Correlation bounds must lie between -1 and 1.")
    vars <- Filter(function(v) !is.factor(d[[v]]), o$vars); rows <- list(); out <- list()
    for (i in seq_along(vars)) for (j in seq_along(vars)) if (i < j) {
      x <- as.numeric(d[[vars[i]]]); y <- as.numeric(d[[vars[j]]]); ok <- !is.na(x) & !is.na(y); n <- sum(ok)
      if (n < 4) next
      r <- stats::cor(x[ok], y[ok]); z <- atanh(r); se <- 1 / sqrt(n - 3)
      zl <- (z - atanh(lo_in)) / se; zu <- (z - atanh(hi_in)) / se
      pl <- stats::pnorm(zl, lower.tail = FALSE); pu <- stats::pnorm(zu); p0 <- 2 * stats::pnorm(-abs(z / se)); crit <- stats::qnorm(1 - alpha)
      lab <- paste(vars[i], "–", vars[j])
      rows <- c(rows, list(list(var = lab, test = "Lower bound", bound = lo_in, stat = zl, p = pl), list(var = lab, test = "Upper bound", bound = hi_in, stat = zu, p = pu)))
      out[[length(out) + 1]] <- list(var = lab, n = n, est = r, cl = tanh(z - crit * se), cu = tanh(z + crit * se), p = max(pl, pu), p0 = p0, result = tost_verdict(max(pl, pu), p0, alpha))
    }
    if (!length(out)) return(NULL)
    return(list(tables = list(
      list(id = "tost", title = "Equivalence Test (TOST)", section = "Equivalence Test",
           columns = list(stats_col("var", "", "text", combineBelow = TRUE), stats_col("test", "Test", "text"), stats_col("bound", "Bound (r)"), stats_col("stat", "z"), stats_col("p", "p", pvalue = TRUE)),
           rows = rows, notes = list(sprintf("Pearson correlations; H₀: r ≤ %s or r ≥ %s. Fisher's z test against each bound, α = %g per test.", format(lo_in), format(hi_in), alpha))),
      list(id = "tost_result", title = "Equivalence Result", section = "Equivalence Test",
           columns = list(stats_col("var", "", "text"), stats_col("n", "N", "integer"), stats_col("est", "r"), stats_col("cl", "Lower", superTitle = ci_t), stats_col("cu", "Upper", superTitle = ci_t),
                          stats_col("p", "p (TOST)", pvalue = TRUE), stats_col("p0", "p (r = 0)", pvalue = TRUE), stats_col("result", "Result", "text")),
           rows = out, notes = list(tost_note(alpha, sprintf("[%s, %s]", format(lo_in), format(hi_in)))))),
      plot = tost_plot(out, lo_in, hi_in, "Correlation (r)", ci_t)))
  }
  sets <- list()
  if (proc == "ttestIS") {
    g <- o$group[[1]]; f <- droplevels(as.factor(d[[g]])); lv <- levels(f)
    welch <- isTRUE(o$welchs) && !isTRUE(o$students)
    for (v in o$vars) {
      ok <- !is.na(d[[v]]) & !is.na(f); x <- as.numeric(d[[v]][ok & f == lv[1]]); y <- as.numeric(d[[v]][ok & f == lv[2]])
      n1 <- length(x); n2 <- length(y); if (n1 < 2 || n2 < 2) next
      v1 <- stats::var(x); v2 <- stats::var(y)
      if (welch) { se <- sqrt(v1 / n1 + v2 / n2); df <- (v1 / n1 + v2 / n2)^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1)); sdd <- sqrt((v1 + v2) / 2) }
      else { sp <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)); se <- sp * sqrt(1 / n1 + 1 / n2); df <- n1 + n2 - 2; sdd <- sp }
      sets[[v]] <- list(m = mean(x) - mean(y), se = se, df = df, sd = sdd, n = n1 + n2)
    }
    what <- sprintf("Mean difference %s − %s (%s's t).", lv[1], lv[2], if (welch) "Welch" else "Student")
    sdname <- if (welch) "the average of the two group SDs" else "the pooled SD"
  } else if (proc == "ttestPS") {
    for (p in o$pairs) {
      ok <- !is.na(d[[p$i1]]) & !is.na(d[[p$i2]]); dd <- as.numeric(d[[p$i1]][ok]) - as.numeric(d[[p$i2]][ok]); n <- length(dd); if (n < 2) next
      sets[[paste(p$i1, "–", p$i2)]] <- list(m = mean(dd), se = stats::sd(dd) / sqrt(n), df = n - 1, sd = stats::sd(dd), n = n)
    }
    what <- "Mean of the paired differences."
    sdname <- "the SD of the differences"
  } else {
    mu <- num_opt(o, "testValue", 0)
    for (v in o$vars) { x <- as.numeric(stats::na.omit(d[[v]])); n <- length(x); if (n < 2) next
      sets[[v]] <- list(m = mean(x) - mu, se = stats::sd(x) / sqrt(n), df = n - 1, sd = stats::sd(x), n = n) }
    what <- sprintf("Mean minus the test value (%s); the bounds are around the test value.", format(mu)); sdname <- "the SD"
  }
  if (!length(sets)) return(NULL)
  smd <- !identical(opt(o, "eqBound", "d"), "raw")
  rows <- list(); out <- list(); bl <- numeric(0); bh <- numeric(0)
  for (nm in names(sets)) {
    s <- sets[[nm]]; lo <- if (smd) lo_in * s$sd else lo_in; hi <- if (smd) hi_in * s$sd else hi_in
    r <- tost_t(s$m, s$se, s$df, lo, hi, alpha); bl <- c(bl, lo); bh <- c(bh, hi)
    rows <- c(rows, list(list(var = nm, test = "Lower bound", bound = lo, bd = if (smd) lo_in else lo / s$sd, stat = r$tl, df = r$df, p = r$pl),
                         list(var = nm, test = "Upper bound", bound = hi, bd = if (smd) hi_in else hi / s$sd, stat = r$tu, df = r$df, p = r$pu)))
    out[[length(out) + 1]] <- list(var = nm, n = s$n, est = r$m, se = r$se, cl = r$cl, cu = r$cu, p = r$p, p0 = r$p0, result = tost_verdict(r$p, r$p0, alpha), lo = lo, hi = hi)
  }
  bnote <- if (smd) sprintf("Bounds set as Cohen's d = %s to %s and converted to raw units with %s.", format(lo_in), format(hi_in), sdname)
           else sprintf("Bounds set in raw units (%s to %s); d shows them divided by %s.", format(lo_in), format(hi_in), sdname)
  list(tables = list(
    list(id = "tost", title = "Equivalence Test (TOST)", section = "Equivalence Test",
         columns = list(stats_col("var", "", "text", combineBelow = TRUE), stats_col("test", "Test", "text"), stats_col("bound", "Bound"), stats_col("bd", "Bound (d)"), stats_col("stat", "t"), stats_col("df", "df"), stats_col("p", "p", pvalue = TRUE)),
         rows = rows, notes = list(paste(what, bnote, sprintf("One-sided t test against each bound, α = %g per test.", alpha)))),
    list(id = "tost_result", title = "Equivalence Result", section = "Equivalence Test",
         columns = list(stats_col("var", "", "text"), stats_col("est", "Difference"), stats_col("se", "SE"), stats_col("cl", "Lower", superTitle = ci_t), stats_col("cu", "Upper", superTitle = ci_t),
                        stats_col("p", "p (TOST)", pvalue = TRUE), stats_col("p0", "p (difference = 0)", pvalue = TRUE), stats_col("result", "Result", "text")),
         rows = lapply(out, function(x) x[c("var", "est", "se", "cl", "cu", "p", "p0", "result")]),
         notes = list(tost_note(alpha, "the bounds")))),
    plot = tost_plot(out, bl, bh, if (proc == "ttestOneS") "Difference from the test value" else "Mean difference", ci_t))
}

tost_note <- function(alpha, bounds) sprintf(paste0("Equivalent: the %g%% CI lies inside %s, so both one-sided tests are significant (p (TOST) < %g). ",
  "Trivial difference: equivalent, yet different from 0. Inconclusive: neither equivalent nor different; more data are needed (Lakens, Scheel & Isager, 2018)."), 100 * (1 - 2 * alpha), bounds, alpha)

#' Each estimate with its 1 - 2 alpha CI (thick) against the equivalence bounds (shaded).
tost_plot <- function(out, lo, hi, xlab, ci_t) {
  k <- length(out); if (!k) return(NULL)
  lo <- rep_len(lo, k); hi <- rep_len(hi, k)
  stats_png(function() {
    labs <- vapply(out, `[[`, "", "var"); y <- rev(seq_len(k))
    xr <- range(c(lo, hi, vapply(out, `[[`, 0, "cl"), vapply(out, `[[`, 0, "cu"), 0)); xr <- xr + c(-1, 1) * diff(xr) * 0.08
    graphics::par(mar = c(4.2, max(6, max(nchar(labs)) * 0.55), 2.4, 1))
    graphics::plot(NA, xlim = xr, ylim = c(0.4, k + 0.6), yaxt = "n", xlab = xlab, ylab = "", bty = "n")
    graphics::axis(2, at = y, labels = labs, las = 1, tick = FALSE, cex.axis = 0.9)
    for (i in seq_len(k)) graphics::rect(lo[i], y[i] - 0.35, hi[i], y[i] + 0.35, col = "#E0F2FE", border = NA)
    graphics::abline(v = 0, col = "#94A3B8", lty = 3)
    for (i in seq_len(k)) {
      graphics::segments(lo[i], y[i] - 0.35, lo[i], y[i] + 0.35, col = "#0369A1", lty = 2); graphics::segments(hi[i], y[i] - 0.35, hi[i], y[i] + 0.35, col = "#0369A1", lty = 2)
      eq <- startsWith(out[[i]]$result, "Equivalent"); col <- if (eq) "#15803D" else "#B45309"
      graphics::segments(out[[i]]$cl, y[i], out[[i]]$cu, y[i], lwd = 3, col = col); graphics::points(out[[i]]$est, y[i], pch = 19, cex = 1.2, col = col)
    }
    graphics::legend(x = mean(xr), y = graphics::par("usr")[4], xjust = 0.5, yjust = 0, horiz = TRUE, xpd = TRUE, bty = "n", cex = 0.8,
                     legend = c("Bounds", paste(ci_t, "equivalent"), paste(ci_t, "not shown equivalent")), col = c("#0369A1", "#15803D", "#B45309"), lty = c(2, 1, 1), lwd = c(1, 3, 3))
  }, title = "Equivalence plot", alt = "Estimates with their confidence intervals against the equivalence bounds", h = max(2.6, 1.3 + 0.55 * k))
}

#' R code for the equivalence test with TOSTER (Lakens' package; the same values).
tost_code <- function(s, q) {
  o <- s$options
  if (!s$proc %in% TOST_PROCS || !isTRUE(o$eqTest)) return(character(0))
  a <- o$eqAlpha %||% 0.05; lo <- o$eqLow %||% (if (s$proc == "corrMatrix") -0.3 else -0.5); hi <- o$eqHigh %||% (if (s$proc == "corrMatrix") 0.3 else 0.5)
  ty <- if (identical(o$eqBound, "raw")) "raw" else "SMD"
  b <- sprintf("low_eqbound = %s, high_eqbound = %s, eqbound_type = %s, alpha = %s", lo, hi, q(ty), a)
  body <- switch(s$proc,
    ttestIS = vapply(unlist(o$vars), function(v) sprintf("t_TOST(formula = `%s` ~ `%s`, data = data, %s, var.equal = %s)", v, unlist(o$group)[1], b,
                                                         if (isTRUE(o$welchs) && !isTRUE(o$students)) "FALSE" else "TRUE"), ""),
    ttestPS = vapply(o$pairs, function(p) sprintf("t_TOST(x = data[[%s]], y = data[[%s]], paired = TRUE, %s)", q(p$i1), q(p$i2), b), ""),
    ttestOneS = vapply(unlist(o$vars), function(v) sprintf("t_TOST(x = data[[%s]] - %s, %s)   # bounds around the test value", q(v), o$testValue %||% 0, b), ""),
    corrMatrix = { v <- unlist(o$vars); pr <- utils::combn(length(v), 2, simplify = FALSE)
      vapply(pr, function(ij) sprintf("z_cor_test(data[[%s]], data[[%s]], alternative = \"equivalence\", null = c(%s, %s), alpha = %s)", q(v[ij[1]]), q(v[ij[2]]), lo, hi, a), "") })
  c("", "# Equivalence test (TOST) added by PathLab SEM; the same values with TOSTER:", "library(TOSTER)", body)
}

# ---- Robust methods (WRS2; Mair & Wilcox, 2020) and bootstrap confidence intervals (boot) ----------------------------
# Options of the t-tests, the one-way and factorial ANOVA, the descriptives and the correlation matrix (off by default):
# Yuen's trimmed-mean t-test (independent: WRS2::yuen, paired: WRS2::yuend), the trimmed-mean ANOVA (WRS2::t1way with
# WRS2::lincon post hoc comparisons; t2way / t3way for two or three factors), and percentile and BCa bootstrap intervals
# for means and Pearson correlations (boot::boot, boot::boot.ci; seed 2026, so the intervals are reproducible).
ROBUST_PROCS <- c("ttestIS", "ttestPS", "anovaOneW", "ANOVA")
BOOT_PROCS <- c("descriptives", "corrMatrix")

run_robust <- function(proc, d, o) {
  if (!requireNamespace("WRS2", quietly = TRUE)) api_error(501, "stats_unavailable", "The WRS2 R package is not installed on the analysis server.", "Run Rscript backend/install_packages.R.")
  tr <- num_opt(o, "trim", 0.2, 0, 0.45)
  trn <- sprintf("%g%% trimmed means (%g%% cut from each end).", 100 * tr, 100 * tr)
  if (proc == "ttestIS") {
    g <- o$group[[1]]
    rows <- lapply(o$vars, function(v) {
      dd <- data.frame(y = as.numeric(d[[v]]), g = droplevels(as.factor(d[[g]]))); dd <- dd[stats::complete.cases(dd), ]
      r <- WRS2::yuen(y ~ g, data = dd, tr = tr)
      list(var = v, diff = r$diff, lower = r$conf.int[1], upper = r$conf.int[2], t = r$test, df = r$df, p = r$p.value, es = r$effsize)
    })
    return(list(robust_table("robust_ttest", "Robust Test: Yuen's Trimmed-Mean t-Test", rows, TRUE,
      c(trn, sprintf("Difference = trimmed mean of %s minus %s; 95%% CI of the difference. ξ: explanatory measure of effect size (.10 small, .30 medium, .50 large; Wilcox & Tian, 2011).",
                     levels(droplevels(as.factor(d[[g]])))[1], levels(droplevels(as.factor(d[[g]])))[2])))))
  }
  if (proc == "ttestPS") {
    rows <- lapply(o$pairs, function(p) {
      ok <- !is.na(d[[p$i1]]) & !is.na(d[[p$i2]])
      r <- WRS2::yuend(as.numeric(d[[p$i1]][ok]), as.numeric(d[[p$i2]][ok]), tr = tr)
      list(var = paste(p$i1, "–", p$i2), diff = r$diff, lower = r$conf.int[1], upper = r$conf.int[2], t = r$test, df = r$df, p = r$p.value, es = r$effsize)
    })
    return(list(robust_table("robust_ttest", "Robust Test: Yuen's Trimmed-Mean Paired t-Test", rows, TRUE, c(trn, "Difference of the trimmed means; 95% CI. ξ: explanatory measure of effect size."))))
  }
  if (proc == "anovaOneW") {
    g <- o$group[[1]]; out <- list(); ph <- list()
    rows <- lapply(o$deps, function(v) {
      dd <- data.frame(y = as.numeric(d[[v]]), g = droplevels(as.factor(d[[g]]))); dd <- dd[stats::complete.cases(dd), ]
      set.seed(2026); r <- WRS2::t1way(y ~ g, data = dd, tr = tr, nboot = 200)
      lc <- WRS2::lincon(y ~ g, data = dd, tr = tr); cm <- lc$comp
      ph[[length(ph) + 1]] <<- lapply(seq_len(nrow(cm)), function(i) list(var = v, g1 = lc$fnames[cm[i, 1]], g2 = lc$fnames[cm[i, 2]], psi = cm[i, "psihat"], lower = cm[i, "ci.lower"], upper = cm[i, "ci.upper"], p = cm[i, "p.value"]))
      list(var = v, f = r$test, df1 = r$df1, df2 = r$df2, p = r$p.value, es = r$effsize, eslo = r$effsize_ci[1], eshi = r$effsize_ci[2])
    })
    return(list(
      list(id = "robust_anova", title = "Robust ANOVA: Trimmed Means (Welch-type)", section = "Robust Tests", rows = rows,
           columns = list(stats_col("var", "", "text"), stats_col("f", "F"), stats_col("df1", "df1"), stats_col("df2", "df2"), stats_col("p", "p", pvalue = TRUE), stats_col("es", "ξ"),
                          stats_col("eslo", "Lower", superTitle = "ξ 95% CI"), stats_col("eshi", "Upper", superTitle = "ξ 95% CI")),
           notes = list(paste(trn, "Heteroscedastic one-way ANOVA for trimmed means (WRS2::t1way); ξ: explanatory measure of effect size with a bootstrap CI."))),
      list(id = "robust_posthoc", title = "Robust Post Hoc Comparisons (Trimmed Means)", section = "Robust Tests", rows = unlist(ph, recursive = FALSE),
           columns = list(stats_col("var", "", "text", combineBelow = TRUE), stats_col("g1", "Group", "text"), stats_col("g2", "Group", "text"), stats_col("psi", "Difference"),
                          stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI"), stats_col("p", "p", pvalue = TRUE)),
           notes = list("Linear contrasts of trimmed means (WRS2::lincon); the CIs and p values are adjusted for all pairwise comparisons."))))
  }
  if (proc == "ANOVA") {
    f <- unlist(o$factors)
    if (length(f) > 3) api_error(422, "robust_factors", "The robust (trimmed-mean) ANOVA takes up to three factors.")
    dd <- data.frame(y = as.numeric(d[[o$dep]])); nm <- c("A", "B", "C")[seq_along(f)]
    for (i in seq_along(f)) dd[[nm[i]]] <- droplevels(as.factor(d[[f[i]]]))
    dd <- dd[stats::complete.cases(dd), , drop = FALSE]
    form <- stats::as.formula(paste("y ~", paste(nm, collapse = " * ")))
    lab <- function(x) paste(f[match(strsplit(x, "")[[1]], nm)], collapse = " ✱ ")
    if (length(f) == 1) { set.seed(2026); r <- WRS2::t1way(form, data = dd, tr = tr, nboot = 200); rows <- list(list(term = f, stat = r$test, p = r$p.value)) }
    else {
      r <- if (length(f) == 2) WRS2::t2way(form, data = dd, tr = tr) else WRS2::t3way(form, data = dd, tr = tr)
      terms <- if (length(f) == 2) c("A", "B", "AB") else c("A", "B", "C", "AB", "AC", "BC", "ABC")
      rows <- lapply(terms, function(tm) list(term = gsub(" ✱ ", " × ", lab(tm)), stat = r[[paste0("Q", tolower(tm))]], p = r[[paste0(tm, ".p.value")]]))
    }
    return(list(list(id = "robust_anova", title = "Robust Factorial ANOVA: Trimmed Means", section = "Robust Tests", rows = rows,
      columns = list(stats_col("term", "", "text"), stats_col("stat", if (length(f) == 1) "F" else "Q"), stats_col("p", "p", pvalue = TRUE)),
      notes = list(paste(trn, if (length(f) == 1) "WRS2::t1way." else sprintf("Q: test statistic for trimmed means (WRS2::t%dway; Wilcox, 2017); robust to unequal variances and outliers.", length(f)))))))
  }
  NULL
}

robust_table <- function(id, title, rows, es, notes) list(id = id, title = title, section = "Robust Tests", rows = rows,
  columns = c(list(stats_col("var", "", "text"), stats_col("diff", "Difference"), stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI"),
                   stats_col("t", "t"), stats_col("df", "df"), stats_col("p", "p", pvalue = TRUE)), if (es) list(stats_col("es", "ξ"))), notes = as.list(notes))

run_boot <- function(proc, d, o) {
  R <- as.integer(num_opt(o, "bootN", 2000, 200, 20000)); lvl <- num_opt(o, "ciWidth", 95, 50, 99.9) / 100
  ci <- function(b) {
    bc <- tryCatch(boot::boot.ci(b, conf = lvl, type = c("perc", "bca")), error = function(e) NULL)
    list(pl = if (!is.null(bc$percent)) bc$percent[4] else NA, pu = if (!is.null(bc$percent)) bc$percent[5] else NA, bl = if (!is.null(bc$bca)) bc$bca[4] else NA, bu = if (!is.null(bc$bca)) bc$bca[5] else NA)
  }
  ct <- sprintf("%g%% CI", 100 * lvl)
  cols <- function(first) c(first, list(stats_col("pl", "Lower", superTitle = paste("Percentile", ct)), stats_col("pu", "Upper", superTitle = paste("Percentile", ct)),
                                        stats_col("bl", "Lower", superTitle = paste("BCa", ct)), stats_col("bu", "Upper", superTitle = paste("BCa", ct))))
  note <- sprintf("%d bootstrap resamples (seed 2026). BCa: bias-corrected and accelerated interval (Efron & Tibshirani, 1993); it is preferred when the two differ.", R)
  if (proc == "descriptives") {
    vars <- Filter(function(v) is.numeric(d[[v]]), o$vars); if (!length(vars)) return(NULL)
    rows <- lapply(vars, function(v) { x <- as.numeric(stats::na.omit(d[[v]])); set.seed(2026)
      b <- boot::boot(x, function(x, i) mean(x[i]), R = R); c(list(var = v, n = length(x), est = mean(x)), ci(b)) })
    return(list(list(id = "boot_mean", title = "Bootstrap Confidence Intervals for the Mean", section = "Bootstrap", rows = rows,
      columns = cols(list(stats_col("var", "", "text"), stats_col("n", "N", "integer"), stats_col("est", "Mean"))), notes = list(note))))
  }
  vars <- Filter(function(v) is.numeric(d[[v]]), o$vars); rows <- list()
  for (i in seq_along(vars)) for (j in seq_along(vars)) if (i < j) {
    xy <- stats::na.omit(data.frame(x = as.numeric(d[[vars[i]]]), y = as.numeric(d[[vars[j]]]))); if (nrow(xy) < 4) next
    set.seed(2026); b <- boot::boot(xy, function(z, k) stats::cor(z$x[k], z$y[k]), R = R)
    rows[[length(rows) + 1]] <- c(list(var = paste(vars[i], "–", vars[j]), n = nrow(xy), est = stats::cor(xy$x, xy$y)), ci(b))
  }
  if (!length(rows)) return(NULL)
  list(list(id = "boot_cor", title = "Bootstrap Confidence Intervals for Pearson's r", section = "Bootstrap", rows = rows,
    columns = cols(list(stats_col("var", "", "text"), stats_col("n", "N", "integer"), stats_col("est", "r"))), notes = list(paste(note, "Cases resampled in pairs."))))
}

#' R code for the robust tests and bootstrap intervals (WRS2, boot).
robust_code <- function(s, q) {
  o <- s$options; out <- character(0); tr <- o$trim %||% 0.2
  if (s$proc %in% ROBUST_PROCS && isTRUE(o$robust)) {
    out <- c("", "# Robust tests added by PathLab SEM (WRS2):", "library(WRS2)", switch(s$proc,
      ttestIS = vapply(unlist(o$vars), function(v) sprintf("yuen(`%s` ~ factor(`%s`), data = data, tr = %s)", v, unlist(o$group)[1], tr), ""),
      ttestPS = vapply(o$pairs, function(p) sprintf("ok <- complete.cases(data[[%s]], data[[%s]]); yuend(data[[%s]][ok], data[[%s]][ok], tr = %s)", q(p$i1), q(p$i2), q(p$i1), q(p$i2), tr), ""),
      anovaOneW = unlist(lapply(unlist(o$deps), function(v) c(sprintf("set.seed(2026); t1way(`%s` ~ factor(`%s`), data = data, tr = %s, nboot = 200)", v, unlist(o$group)[1], tr),
                                                               sprintf("lincon(`%s` ~ factor(`%s`), data = data, tr = %s)", v, unlist(o$group)[1], tr)))),
      ANOVA = { f <- unlist(o$factors); sprintf("%s(`%s` ~ %s, data = data, tr = %s)", c("t1way", "t2way", "t3way")[length(f)], o$dep, paste(sprintf("factor(`%s`)", f), collapse = " * "), tr) }))
  }
  if (s$proc %in% BOOT_PROCS && isTRUE(o$bootCI)) {
    R <- o$bootN %||% 2000; lvl <- (o$ciWidth %||% 95) / 100
    out <- c(out, "", "# Bootstrap confidence intervals added by PathLab SEM (boot):", "library(boot)",
      if (s$proc == "descriptives") sprintf("for (v in c(%s)) { x <- na.omit(data[[v]]); set.seed(2026); print(boot.ci(boot(x, function(x, i) mean(x[i]), R = %s), conf = %s, type = c(\"perc\", \"bca\"))) }",
                                            paste(vapply(unlist(o$vars), q, ""), collapse = ", "), R, lvl)
      else sprintf("xy <- na.omit(data[, c(%s)])   # each pair: set.seed(2026); boot.ci(boot(xy, function(z, k) cor(z[k, 1], z[k, 2]), R = %s), conf = %s, type = c(\"perc\", \"bca\"))",
                   paste(vapply(unlist(o$vars)[1:2], q, ""), collapse = ", "), R, lvl))
  }
  if (s$proc == "contTables" && isTRUE(o$bfCont)) {
    st <- o$bfSample %||% "indepMulti"
    out <- c(out, "", "# Bayesian contingency table test added by PathLab SEM (BayesFactor):", "library(BayesFactor)",
             sprintf("contingencyTableBF(table(data[[%s]], data[[%s]]), sampleType = %s%s)", q(unlist(o$rows)[1]), q(unlist(o$cols)[1]), q(st), if (st == "indepMulti") ", fixedMargin = \"rows\"" else ""))
  }
  out
}

# ---- More Bayesian analyses (BayesFactor, as in JASP) ------------------------------------------------------------
#' Bayesian contingency table (BayesFactor::contingencyTableBF), added to the Contingency Tables analysis.
run_bf_contingency <- function(d, o) {
  r <- o$rows[[1]]; cc <- o$cols[[1]]
  tab <- table(droplevels(as.factor(d[[r]])), droplevels(as.factor(d[[cc]])))
  st <- opt(o, "bfSample", "indepMulti"); if (!st %in% c("indepMulti", "jointMulti", "poisson", "hypergeom")) st <- "indepMulti"
  if (st == "hypergeom" && (nrow(tab) != 2 || ncol(tab) != 2)) api_error(422, "bf_hypergeom", "The hypergeometric sampling plan needs a 2 × 2 table.")
  bf <- if (st == "indepMulti") BayesFactor::contingencyTableBF(tab, sampleType = st, fixedMargin = "rows") else BayesFactor::contingencyTableBF(tab, sampleType = st)
  e <- BayesFactor::extractBF(bf)
  lab <- c(indepMulti = "Independent multinomial (row totals fixed)", jointMulti = "Joint multinomial (total fixed)", poisson = "Poisson (nothing fixed)", hypergeom = "Hypergeometric (both margins fixed)")
  list(id = "bf_cont", title = "Bayesian Contingency Table Test", section = "Bayesian Analysis",
       rows = list(list(plan = lab[[st]], bf10 = e$bf[1], bf01 = 1 / e$bf[1], error = 100 * e$error[1])),
       columns = list(stats_col("plan", "Sampling plan", "text"), stats_col("bf10", "BF₁₀"), stats_col("bf01", "BF₀₁"), stats_col("error", "error %")),
       notes = list(sprintf("BF₁₀: evidence for an association between %s and %s against independence (Gunel & Dickey, 1974; BayesFactor::contingencyTableBF, prior concentration a = 1). BF > 3: moderate, > 10: strong evidence.", r, cc)))
}

#' Bayesian repeated measures ANOVA (BayesFactor::anovaBF with participants as a random factor), as in JASP.
run_banova_rm <- function(d, o) {
  ms <- unlist(o$measures); bs <- unlist(o$bs)
  if (length(ms) < 2) api_error(422, "banovarm_measures", "Add at least two repeated measures.")
  w <- d[, c(ms, bs), drop = FALSE]; w <- w[stats::complete.cases(w), , drop = FALSE]; n <- nrow(w)
  if (n < 3) api_error(422, "banovarm_cases", "At least three cases need every measure.")
  long <- data.frame(id = factor(rep(seq_len(n), length(ms))), cond = factor(rep(ms, each = n), levels = ms), value = unlist(lapply(ms, function(m) as.numeric(w[[m]]))))
  nb <- if (length(bs)) paste0("B", seq_along(bs)) else character(0); for (i in seq_along(bs)) long[[nb[i]]] <- factor(rep(as.character(w[[bs[i]]]), length(ms)))
  form <- stats::as.formula(paste("value ~", paste(c("cond", nb), collapse = " * "), "+ id"))
  set.seed(as.integer(num_opt(o, "seed", 2026)))
  bf <- BayesFactor::anovaBF(form, data = long, whichRandom = "id", rscaleFixed = opt(o, "rscale", "medium"), progress = FALSE)
  e <- BayesFactor::extractBF(bf)
  rn <- function(x) { x <- gsub(" \\+ id$", "", x); x <- gsub("cond", o$rmLabel %||% "RM factor", x); for (i in seq_along(bs)) x <- gsub(nb[i], bs[i], x, fixed = TRUE); gsub(":", " × ", x) }
  rows <- lapply(seq_len(nrow(e)), function(i) list(model = rn(rownames(e)[i]), bf10 = e$bf[i], bf01 = 1 / e$bf[i], error = 100 * e$error[i]))
  rows <- rows[order(-vapply(rows, `[[`, 0, "bf10"))]
  list(tables = list(list(id = "banova", title = "Bayesian Repeated Measures ANOVA: Model Comparison",
    rows = c(list(list(model = "Null model (participants)", bf10 = 1, bf01 = 1, error = NULL)), rows),
    columns = list(stats_col("model", "Model", "text"), stats_col("bf10", "BF₁₀"), stats_col("bf01", "BF₀₁"), stats_col("error", "error %")),
    notes = list(sprintf("Bayes factors against the null model with participants as a random factor (Rouder et al., 2012; BayesFactor::anovaBF, rscaleFixed = %s); n = %d participants with every measure.", opt(o, "rscale", "medium"), n)))),
    plots = list(), n = n)
}
STATS_MORE_RUN$banovaRM <- run_banova_rm

# ---- Necessary Condition Analysis on any variables (NCA package; Dul, 2016, 2020) ------------------------------------
# The same engine as the PLS-SEM NCA tab (nca_block), on observed variables: every ceiling of the NCA package, the four
# corners, an optional theoretical scope, the permutation test (Dul et al., 2020), bottleneck tables, outlier analysis
# (Dul, 2021), scatter plots with the ceilings and an optional power analysis (nca_power). Usable next to QCA: the same
# conditions and outcome, uncalibrated (Dul, 2016; Vis & Dul, 2018).
nca_size <- function(d) if (is.null(d) || is.na(d)) "" else if (d < 0.1) "Small" else if (d < 0.3) "Medium" else if (d < 0.5) "Large" else "Very large"
NCA_CEILING_LABEL <- c(ce_fdh = "CE-FDH", cr_fdh = "CR-FDH", c_lp = "C-LP", ce_vrs = "CE-VRS", cr_vrs = "CR-VRS", cols = "COLS", qr = "QR", ols = "OLS (reference)")

run_nca_stats <- function(d, o) {
  conds <- unlist(opt(o, "conditions", character(0))); y <- opt(o, "outcome")
  if (!length(conds) || is.null(y)) api_error(422, "nca_vars", "Add at least one condition and the outcome.")
  dat <- as.data.frame(lapply(d[, c(conds, y), drop = FALSE], as.numeric), row.names = rownames(d)); dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < 3) api_error(422, "nca_cases", "NCA needs at least three complete cases.")
  ces <- intersect(unlist(opt(o, "ceilings", list("ce_fdh", "cr_fdh"))), NCA_CEILINGS); if (!length(ces)) ces <- c("ce_fdh", "cr_fdh")
  nc <- list(ceilings = ces, testRep = as.integer(num_opt(o, "testRep", 1000, 0, 20000)), steps = as.integer(num_opt(o, "steps", 10, 2, 20)),
             corner = as.integer(num_opt(o, "corner", 1, 1, 4)), outliers = isTRUE(opt(o, "outliers", FALSE)))
  if (!is.null(o$scopeMin) && !is.null(o$scopeMax) && o$scopeMax > o$scopeMin) { nc$scopeMin <- o$scopeMin; nc$scopeMax <- o$scopeMax }
  set.seed(as.integer(num_opt(o, "seed", 2026)))
  b <- nca_block(dat, conds, y, nc)
  rows <- lapply(b$rows, function(r) list(condition = r$condition, ceiling = NCA_CEILING_LABEL[[r$ceiling]], d = r$effectSize, size = nca_size(r$effectSize),
                                         acc = r$cAccuracy, fit = r$fit, p = r$p, pacc = r$pAccuracy))
  corner_txt <- c("upper left: a high level of the condition is necessary for a high outcome", "upper right: a low level of the condition is necessary for a high outcome",
                  "lower left: a high level of the condition is necessary for a low outcome", "lower right: a low level of the condition is necessary for a low outcome")[nc$corner]
  tables <- list(list(id = "nca_effects", title = sprintf("Necessary Condition Analysis: %s", y),
    columns = list(stats_col("condition", "Condition", "text", combineBelow = TRUE), stats_col("ceiling", "Ceiling", "text"), stats_col("d", "Effect size d"), stats_col("size", "Size", "text"),
                   stats_col("acc", "c-accuracy %"), stats_col("fit", "Fit %"), stats_col("p", "p", pvalue = TRUE), stats_col("pacc", "p accuracy")),
    rows = rows,
    notes = list(sprintf("N = %d complete cases. Corner: %s. Scope: %s.", nrow(dat), corner_txt, if (!is.null(nc$scopeMin)) sprintf("theoretical, %g to %g", nc$scopeMin, nc$scopeMax) else "observed minimum and maximum"),
                 sprintf("Permutation test with %d resamples (Dul et al., 2020); a condition is necessary 'in degree' when d is at least 0.10 and p < .05. Size (Dul, 2016): below 0.1 small, 0.1 to 0.3 medium, 0.3 to 0.5 large, 0.5 or more very large.", nc$testRep),
                 "Report (Dul et al., 2023): the theoretical necessity hypothesis, the ceiling technique and why, the scope, the effect size with p value, the bottleneck table, and the outlier check.")))
  for (bt in b$bottlenecks[1]) if (length(bt$rows)) tables[[length(tables) + 1]] <- list(id = "nca_bottleneck", title = sprintf("Bottleneck Table (%s; outcome and condition levels as observed values)", NCA_CEILING_LABEL[[bt$ceiling]]),
    columns = lapply(seq_along(bt$columns), function(j) stats_col(paste0("c", j), bt$columns[[j]], "text")),
    rows = lapply(bt$rows, function(r) stats::setNames(lapply(r, function(v) v %||% "NN"), paste0("c", seq_along(r)))),
    notes = list("For each outcome level (first column), the minimum level of each condition that is necessary; NN = not necessary."))
  if (length(b$outliers)) tables[[length(tables) + 1]] <- list(id = "nca_outliers", title = "Potential Outliers (Effect of Removing Each Case)",
    columns = list(stats_col("condition", "Condition", "text", combineBelow = TRUE), stats_col("case", "Case (row)", "text"), stats_col("effectOriginal", "d (all cases)"), stats_col("effectNew", "d without the case"),
                   stats_col("difAbs", "Change"), stats_col("difRel", "Change %"), stats_col("ceil", "Ceiling outlier", "text"), stats_col("scope", "Scope outlier", "text")),
    rows = unlist(lapply(b$outliers, function(x) lapply(x$rows, function(r) list(condition = x$condition, case = r$case, effectOriginal = r$effectOriginal, effectNew = r$effectNew, difAbs = r$difAbs, difRel = r$difRel,
                                                                              ceil = if (isTRUE(r$ceiling)) "Yes" else "", scope = if (isTRUE(r$scope)) "Yes" else ""))), recursive = FALSE),
    notes = list("Cases whose removal changes the effect size the most (NCA::nca_outliers; Dul, 2021). Remove a case only for a substantive reason (e.g. a data error), and report it."))
  if (isTRUE(opt(o, "power", FALSE))) {
    pe <- num_opt(o, "powerEffect", 0.1, 0.01, 0.9); reps <- as.integer(num_opt(o, "powerRep", 100, 10, 1000))
    pw <- NULL; utils::capture.output(pw <- NCA::nca_power(n = nrow(dat), effect = pe, ceiling = if (ces[1] %in% c("ce_fdh", "cr_fdh")) ces[1] else "ce_fdh", rep = reps, test.rep = 200))
    tables[[length(tables) + 1]] <- list(id = "nca_power", title = "Power of the NCA Permutation Test",
      columns = list(stats_col("n", "N", "integer"), stats_col("es", "Effect size d"), stats_col("ceiling", "Ceiling", "text"), stats_col("power", "Power")),
      rows = lapply(seq_len(nrow(pw)), function(i) list(n = pw$n[i], es = pw$ES[i], ceiling = NCA_CEILING_LABEL[[as.character(pw$ceiling[i])]], power = pw$power[i])),
      notes = list(sprintf("Share of %d simulated samples of this size (uniform data, true effect size d = %g) in which the permutation test (200 resamples) is significant at .05 (NCA::nca_power).", reps, pe)))
  }
  plots <- lapply(b$plots, function(pl) {
    cn <- pl$condition; x <- unlist(pl$x); yy <- unlist(pl$y); pr <- NULL
    stats_png(function() {
      graphics::plot(x, yy, pch = 16, col = grDevices::adjustcolor("#1d4f86", 0.6), xlab = cn, ylab = y, main = "")
      cols <- c(ce_fdh = "#b91c1c", cr_fdh = "#c2410c", c_lp = "#7c3aed", ce_vrs = "#0e7490", cr_vrs = "#15803d", cols = "#be185d", qr = "#a16207", ols = "#64748b")
      pk <- pl$ceFdhPeers
      if ("ce_fdh" %in% ces && length(pk)) { m <- do.call(rbind, lapply(pk, unlist)); m <- m[order(m[, 1]), , drop = FALSE]; graphics::lines(m[, 1], m[, 2], type = if (nc$corner %in% c(1, 3)) "s" else "S", col = cols[["ce_fdh"]], lwd = 2) }
      leg <- character(0); lc <- character(0)
      for (ce in setdiff(ces, "ce_fdh")) {
        sl <- tryCatch({ o2 <- NULL; utils::capture.output(o2 <- suppressWarnings(NCA::nca_analysis(dat, cn, y, ceilings = ce, corner = if (nc$corner == 1L) NULL else nc$corner))); o2$summaries[[cn]]$params[c("Slope", "Intercept"), ce] }, error = function(e) c(NA, NA))
        if (all(is.finite(sl))) { graphics::abline(sl[2], sl[1], col = cols[[ce]], lwd = 2, lty = if (ce == "ols") 3 else 1); leg <- c(leg, NCA_CEILING_LABEL[[ce]]); lc <- c(lc, cols[[ce]]) }
      }
      if ("ce_fdh" %in% ces) { leg <- c("CE-FDH", leg); lc <- c(cols[["ce_fdh"]], lc) }
      if (length(leg)) graphics::legend("bottomright", leg, col = lc, lwd = 2, bty = "n", cex = 0.85)
    }, title = sprintf("%s and %s", cn, y), alt = sprintf("Scatter plot of %s against %s with the NCA ceiling lines", y, cn), h = 4.2)
  })
  list(tables = tables, plots = Filter(Negate(is.null), plots), n = nrow(dat))
}
STATS_MORE_RUN$nca <- run_nca_stats

# ==== Survey designs and causal effects (Stata-style): svy, DiD, propensity-score matching, IPW ==========================

#' 0/1 coding of a treatment variable: numeric 0/1, logical, or a two-level variable (second level = treated).
treat01 <- function(x, name) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) && all(stats::na.omit(x) %in% c(0, 1))) return(as.integer(x))
  lv <- sort(unique(stats::na.omit(as.character(x))))
  if (length(lv) != 2) api_error(422, "treat_levels", sprintf("'%s' must have exactly two values (0 = control, 1 = treated).", name))
  as.integer(as.character(x) == lv[2])
}

causal_frame <- function(d, cols, factors = character(0)) {
  dd <- data.frame(row.names = seq_len(nrow(d)))
  for (v in cols) dd[[v]] <- if (v %in% factors) factor(d[[v]]) else d[[v]]
  dd[stats::complete.cases(dd), , drop = FALSE]
}
cq <- function(x) sprintf("`%s`", x)

#' Standardised mean differences (treated - control) / pooled SD, optionally weighted (Austin, 2011).
smd_rows <- function(X, tr, w = NULL) {
  w <- w %||% rep(1, length(tr))
  lapply(colnames(X), function(v) {
    x <- X[, v]
    m1 <- stats::weighted.mean(x[tr == 1], w[tr == 1]); m0 <- stats::weighted.mean(x[tr == 0], w[tr == 0])
    s <- sqrt((stats::var(x[tr == 1]) + stats::var(x[tr == 0])) / 2)   # unweighted pooled SD, as cobalt's default
    list(m1 = m1, m0 = m0, smd = if (s > 0) (m1 - m0) / s else NA_real_)
  })
}
smd_matrix <- function(dd, covs, factors) {
  X <- stats::model.matrix(stats::as.formula(paste("~", paste(cq(c(covs, factors)), collapse = " + "))), dd)[, -1, drop = FALSE]
  colnames(X) <- gsub("`", "", colnames(X)); X
}
love_plot <- function(names, before, after, title) stats_png(function() {
  k <- length(names); o <- order(abs(before))
  graphics::plot(NA, xlim = range(c(0, abs(before), abs(after), 0.25), na.rm = TRUE), ylim = c(0.5, k + 0.5), yaxt = "n", xlab = "Absolute standardised mean difference", ylab = "")
  graphics::axis(2, at = seq_len(k), labels = names[o], las = 1, cex.axis = 0.75)
  graphics::abline(v = 0.1, lty = 2, col = "#94a3b8")
  graphics::points(abs(before[o]), seq_len(k), pch = 1, col = "#b91c1c", cex = 1.2)
  graphics::points(abs(after[o]), seq_len(k), pch = 16, col = "#1d4f86", cex = 1.2)
  graphics::legend("bottomright", c("Before", "After"), pch = c(1, 16), col = c("#b91c1c", "#1d4f86"), bty = "n")
}, title = title, alt = "Love plot of the covariate balance before and after adjustment", w = 7, h = max(3.2, 0.32 * length(names) + 1.6))
balance_table <- function(names, b, a, label_after) list(id = "balance", title = "Covariate Balance",
  columns = list(stats_col("var", "Covariate", "text"), stats_col("m1b", "Treated", superTitle = "Before"), stats_col("m0b", "Control", superTitle = "Before"), stats_col("smdb", "SMD", superTitle = "Before"),
                 stats_col("m1a", "Treated", superTitle = label_after), stats_col("m0a", "Control", superTitle = label_after), stats_col("smda", "SMD", superTitle = label_after)),
  rows = lapply(seq_along(names), function(i) list(var = names[i], m1b = b[[i]]$m1, m0b = b[[i]]$m0, smdb = b[[i]]$smd, m1a = a[[i]]$m1, m0a = a[[i]]$m0, smda = a[[i]]$smd)),
  notes = list(sprintf("SMD: standardised mean difference (treated minus control, divided by the pooled SD before adjustment). |SMD| < 0.1 is usually taken as balance (Austin, 2011); %d of %d covariates are balanced after adjustment.",
                       sum(vapply(a, function(x) is.finite(x$smd) && abs(x$smd) < 0.1, TRUE)), length(a))))

# ---- Survey-weighted analysis (survey package; Lumley, 2010) ----------------------------------------------------------
run_svy <- function(d, o) {
  y <- opt(o, "dep"); covs <- unlist(opt(o, "covs", list())); facs <- unlist(opt(o, "factors", list()))
  w <- opt(o, "weights"); st <- opt(o, "strata"); cl <- opt(o, "cluster"); fam <- opt(o, "family", "gaussian")
  if (is.null(y) || is.null(w)) api_error(422, "svy_vars", "Add the dependent variable and the sampling weight.")
  dd <- causal_frame(d, unique(c(y, covs, facs, w, st, cl)), facs)
  if (any(dd[[w]] <= 0)) api_error(422, "svy_weights", "Sampling weights must be positive.")
  if (fam == "binomial") dd[[y]] <- treat01(dd[[y]], y)
  des <- survey::svydesign(ids = if (is.null(cl)) ~1 else stats::as.formula(paste("~", cq(cl))), strata = if (is.null(st)) NULL else stats::as.formula(paste("~", cq(st))),
                           weights = stats::as.formula(paste("~", cq(w))), data = dd, nest = TRUE)
  mn <- survey::svymean(stats::as.formula(paste("~", cq(y))), des, deff = TRUE)
  ci <- stats::confint(mn)
  tables <- list(list(id = "svy_mean", title = "Survey-Weighted Mean", columns = list(stats_col("var", "Variable", "text"), stats_col("mean", "Mean"), stats_col("se", "SE (design)"),
      stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI"), stats_col("deff", "Design effect")),
    rows = list(list(var = y, mean = unname(stats::coef(mn)), se = unname(survey::SE(mn)), lower = ci[1, 1], upper = ci[1, 2], deff = unname(survey::deff(mn)))),
    notes = list(sprintf("Design: weights %s%s%s (survey::svydesign). The design effect compares the design variance with simple random sampling of the same size; above 1 the design loses precision.",
                         w, if (is.null(st)) "" else paste0(", strata ", st), if (is.null(cl)) ", no clustering" else paste0(", clusters (PSU) ", cl)))))
  if (length(c(covs, facs))) {
    fit <- survey::svyglm(stats::as.formula(paste(cq(y), "~", paste(cq(c(covs, facs)), collapse = " + "))), design = des, family = if (fam == "binomial") stats::quasibinomial() else stats::gaussian())
    cf <- summary(fit)$coefficients; crit <- stats::qt(0.975, fit$df.residual)
    tables[[2]] <- list(id = "svy_coef", title = sprintf("Survey-Weighted %s Regression", if (fam == "binomial") "Logistic" else "Linear"), columns = coef_cols("t", 95),
      rows = coef_rows(cf[, 1], cf[, 2], cf[, 3], cf[, 4], cf[, 1] - crit * cf[, 2], cf[, 1] + crit * cf[, 2], rownames(cf)),
      notes = list(sprintf("survey::svyglm with design-based (linearisation) standard errors; %s. Degrees of freedom: %d (number of PSUs minus strata).",
                           if (fam == "binomial") "quasi-binomial family, coefficients are log-odds" else "Gaussian family", as.integer(fit$df.residual))))
  }
  list(tables = tables, plots = list(), n = nrow(dd))
}

# ---- Difference-in-differences ----------------------------------------------------------------------------------------
run_did <- function(d, o) {
  y <- opt(o, "dep"); tr <- opt(o, "treat"); po <- opt(o, "post"); covs <- unlist(opt(o, "covs", list())); facs <- unlist(opt(o, "factors", list())); cl <- opt(o, "cluster")
  if (is.null(y) || is.null(tr) || is.null(po)) api_error(422, "did_vars", "Add the outcome, the treatment group (0/1) and the period (0 = before, 1 = after).")
  dd <- causal_frame(d, unique(c(y, tr, po, covs, facs, cl)), facs)
  dd$.treat <- treat01(dd[[tr]], tr); dd$.post <- treat01(dd[[po]], po)
  if (length(unique(paste(dd$.treat, dd$.post))) < 4) api_error(422, "did_cells", "Every combination of group and period needs cases (treated/control × before/after).")
  form <- stats::as.formula(paste(cq(y), "~ .treat * .post", if (length(c(covs, facs))) paste("+", paste(cq(c(covs, facs)), collapse = " + ")) else ""))
  fit <- stats::lm(form, data = dd)
  V <- if (!is.null(cl)) sandwich::vcovCL(fit, cluster = dd[[cl]], type = "HC1") else sandwich::vcovHC(fit, type = "HC1")
  ct <- lmtest::coeftest(fit, vcov. = V); df <- if (!is.null(cl)) length(unique(dd[[cl]])) - 1 else fit$df.residual
  crit <- stats::qt(0.975, df); pv <- 2 * stats::pt(abs(ct[, 3]), df, lower.tail = FALSE)
  terms <- rownames(ct); terms[terms == ".treat"] <- sprintf("%s (treated group)", tr); terms[terms == ".post"] <- sprintf("%s (after)", po); terms[terms == ".treat:.post"] <- "Treated × after (DiD effect)"
  k <- which(rownames(ct) == ".treat:.post")
  cell <- function(t, p) { x <- dd[[y]][dd$.treat == t & dd$.post == p]; c(mean(x), length(x)) }
  m <- sapply(list(c(0, 0), c(0, 1), c(1, 0), c(1, 1)), function(z) cell(z[1], z[2]))
  raw_did <- (m[1, 4] - m[1, 3]) - (m[1, 2] - m[1, 1])
  tables <- list(
    list(id = "did_effect", title = "Difference-in-Differences Effect", columns = list(stats_col("term", "Effect", "text"), stats_col("b", "Estimate"), stats_col("se", "SE"), stats_col("stat", "t"), stats_col("p", "p", pvalue = TRUE),
      stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI")),
      rows = list(list(term = "Average treatment effect on the treated (DiD)", b = ct[k, 1], se = ct[k, 2], stat = ct[k, 3], p = pv[k], lower = ct[k, 1] - crit * ct[k, 2], upper = ct[k, 1] + crit * ct[k, 2])),
      notes = list(sprintf("The change in the treated group minus the change in the control group (interaction of group and period in a linear model). Standard errors: %s. The estimate is causal only under parallel trends: without treatment, both groups would have changed alike (Angrist & Pischke, 2009).",
                           if (is.null(cl)) "heteroskedasticity-robust (HC1)" else sprintf("cluster-robust by %s (%d clusters, HC1)", cl, length(unique(dd[[cl]])))))),
    list(id = "did_means", title = "Group Means by Period", columns = list(stats_col("group", "Group", "text"), stats_col("before", "Before"), stats_col("after", "After"), stats_col("change", "Change"), stats_col("nb", "n before", "integer"), stats_col("na", "n after", "integer")),
      rows = list(list(group = "Control", before = m[1, 1], after = m[1, 2], change = m[1, 2] - m[1, 1], nb = m[2, 1], na = m[2, 2]),
                  list(group = "Treated", before = m[1, 3], after = m[1, 4], change = m[1, 4] - m[1, 3], nb = m[2, 3], na = m[2, 4]),
                  list(group = "Difference (treated − control)", before = m[1, 3] - m[1, 1], after = m[1, 4] - m[1, 2], change = raw_did, nb = NA, na = NA)),
      notes = list("The bottom-right cell is the unadjusted difference-in-differences; with covariates the model estimate above can differ.")),
    list(id = "did_coef", title = "Regression Coefficients", columns = coef_cols("t", 95), rows = coef_rows(ct[, 1], ct[, 2], ct[, 3], pv, ct[, 1] - crit * ct[, 2], ct[, 1] + crit * ct[, 2], terms)))
  plot <- stats_png(function() {
    graphics::plot(c(0, 1), range(m[1, ]), type = "n", xaxt = "n", xlab = "Period", ylab = paste("Mean", y), xlim = c(-0.15, 1.15))
    graphics::axis(1, at = 0:1, labels = c("Before", "After"))
    graphics::lines(0:1, m[1, 1:2], type = "b", pch = 16, col = "#64748b", lwd = 2); graphics::lines(0:1, m[1, 3:4], type = "b", pch = 16, col = "#1d4f86", lwd = 2)
    graphics::lines(0:1, c(m[1, 3], m[1, 3] + m[1, 2] - m[1, 1]), lty = 2, col = "#1d4f86")
    graphics::legend("topleft", c("Control", "Treated", "Treated without treatment (parallel trend)"), col = c("#64748b", "#1d4f86", "#1d4f86"), lty = c(1, 1, 2), lwd = c(2, 2, 1), bty = "n", cex = 0.85)
  }, title = "Group means before and after", alt = "Means of the treated and control groups before and after, with the counterfactual parallel trend")
  list(tables = tables, plots = Filter(Negate(is.null), list(plot)), n = nrow(dd))
}

# ---- Propensity-score matching (MatchIt; Ho, Imai, King & Stuart, 2011) ---------------------------------------------
run_psm <- function(d, o) {
  y <- opt(o, "dep"); tr <- opt(o, "treat"); covs <- unlist(opt(o, "covs", list())); facs <- unlist(opt(o, "factors", list()))
  if (is.null(tr) || !length(c(covs, facs))) api_error(422, "psm_vars", "Add the treatment (0/1) and at least one covariate.")
  method <- opt(o, "method", "nearest"); ratio <- as.integer(num_opt(o, "ratio", 1, 1, 10)); cal <- num_opt(o, "caliper", 0, 0, 2); repl <- isTRUE(opt(o, "replace", FALSE))
  dd <- causal_frame(d, unique(c(y, tr, covs, facs)), facs)
  dd$.treat <- treat01(dd[[tr]], tr)
  if (min(table(dd$.treat)) < 5) api_error(422, "psm_small", "Both groups need at least five cases.")
  form <- stats::as.formula(paste(".treat ~", paste(cq(c(covs, facs)), collapse = " + ")))
  set.seed(as.integer(num_opt(o, "seed", 2026, 1)))
  args <- list(form, data = dd, method = method, distance = "glm", estimand = "ATT")
  if (method == "nearest") { args$ratio <- ratio; args$replace <- repl; if (cal > 0) args$caliper <- cal }
  m <- do.call(MatchIt::matchit, args)
  md <- MatchIt::match.data(m, drop.unmatched = TRUE)
  X <- smd_matrix(dd, covs, facs); Xm <- smd_matrix(md, covs, facs)
  b <- smd_rows(X, dd$.treat)
  a <- lapply(seq_len(ncol(X)), function(j) { x <- Xm[, j]; t <- md$.treat; w <- md$weights
    s <- sqrt((stats::var(X[dd$.treat == 1, j]) + stats::var(X[dd$.treat == 0, j])) / 2)
    m1 <- stats::weighted.mean(x[t == 1], w[t == 1]); m0 <- stats::weighted.mean(x[t == 0], w[t == 0]); list(m1 = m1, m0 = m0, smd = if (s > 0) (m1 - m0) / s else NA_real_) })
  ns <- m$nn
  tables <- list(
    list(id = "psm_n", title = "Sample Sizes", columns = list(stats_col("row", "", "text"), stats_col("c", "Control"), stats_col("t", "Treated")),
      rows = lapply(rownames(ns), function(r) list(row = r, c = ns[r, "Control"], t = ns[r, "Treated"])),
      notes = list(sprintf("MatchIt::matchit(method = \"%s\", distance = \"glm\"%s, estimand = \"ATT\"). Propensity scores from a logistic regression of %s on the covariates.",
                           method, if (method == "nearest") sprintf(", ratio = %d, replace = %s%s", ratio, if (repl) "TRUE" else "FALSE", if (cal > 0) sprintf(", caliper = %g SD", cal) else "") else "", tr))),
    balance_table(colnames(X), b, a, "Matched"))
  if (!is.null(y)) {
    fit <- stats::lm(stats::as.formula(paste(cq(y), "~ .treat")), data = md, weights = md$weights)
    V <- if (method == "nearest" && !repl) sandwich::vcovCL(fit, cluster = md$subclass) else sandwich::vcovHC(fit, type = "HC3")
    ct <- lmtest::coeftest(fit, vcov. = V); crit <- stats::qnorm(0.975)
    tables[[length(tables) + 1]] <- list(id = "psm_att", title = sprintf("Average Treatment Effect on the Treated (ATT): %s", y),
      columns = list(stats_col("term", "Effect", "text"), stats_col("b", "Estimate"), stats_col("se", "SE"), stats_col("stat", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI")),
      rows = list(list(term = "ATT (treated − matched controls)", b = ct[2, 1], se = ct[2, 2], stat = ct[2, 3], p = 2 * stats::pnorm(-abs(ct[2, 3])), lower = ct[2, 1] - crit * ct[2, 2], upper = ct[2, 1] + crit * ct[2, 2])),
      notes = list(sprintf("Weighted regression of %s on the treatment in the matched sample; %s standard errors, as recommended in the MatchIt documentation (Greifer, 2023). Causal only if there are no unmeasured confounders.",
                           y, if (method == "nearest" && !repl) "cluster-robust (by matched pair)" else "robust (HC3)")))
  }
  ps <- m$distance
  plots <- list(love_plot(colnames(X), vapply(b, `[[`, 0, "smd"), vapply(a, `[[`, 0, "smd"), "Covariate balance (Love plot)"),
    stats_png(function() {
      h0 <- graphics::hist(ps[dd$.treat == 0], breaks = seq(0, 1, 0.05), plot = FALSE); h1 <- graphics::hist(ps[dd$.treat == 1], breaks = seq(0, 1, 0.05), plot = FALSE)
      graphics::plot(h0, col = grDevices::adjustcolor("#64748b", 0.5), border = "white", main = "", xlab = "Propensity score", ylim = c(0, max(h0$counts, h1$counts)))
      graphics::plot(h1, col = grDevices::adjustcolor("#1d4f86", 0.5), border = "white", add = TRUE)
      graphics::legend("topright", c("Control", "Treated"), fill = grDevices::adjustcolor(c("#64748b", "#1d4f86"), 0.5), bty = "n")
    }, title = "Propensity scores (common support)", alt = "Histograms of the propensity scores of the treated and control groups"))
  list(tables = tables, plots = Filter(Negate(is.null), plots), n = nrow(dd))
}

# ---- Inverse probability weighting -----------------------------------------------------------------------------------
run_ipw <- function(d, o) {
  y <- opt(o, "dep"); tr <- opt(o, "treat"); covs <- unlist(opt(o, "covs", list())); facs <- unlist(opt(o, "factors", list()))
  if (is.null(y) || is.null(tr) || !length(c(covs, facs))) api_error(422, "ipw_vars", "Add the outcome, the treatment (0/1) and at least one covariate.")
  est <- opt(o, "estimand", "ATE"); stab <- isTRUE(opt(o, "stabilized", TRUE)); trim <- num_opt(o, "trim", 0, 0, 0.2)
  dd <- causal_frame(d, unique(c(y, tr, covs, facs)), facs)
  dd$.treat <- treat01(dd[[tr]], tr)
  ps_fit <- stats::glm(stats::as.formula(paste(".treat ~", paste(cq(c(covs, facs)), collapse = " + "))), data = dd, family = stats::binomial())
  ps <- stats::fitted(ps_fit)
  if (trim > 0) ps <- pmin(pmax(ps, trim), 1 - trim)
  t <- dd$.treat; pt <- mean(t)
  w <- if (est == "ATT") ifelse(t == 1, 1, ps / (1 - ps)) else if (stab) ifelse(t == 1, pt / ps, (1 - pt) / (1 - ps)) else ifelse(t == 1, 1 / ps, 1 / (1 - ps))
  X <- smd_matrix(dd, covs, facs)
  b <- smd_rows(X, t); a <- smd_rows(X, t, w)
  fit <- stats::lm(stats::as.formula(paste(cq(y), "~ .treat")), data = dd, weights = w)
  ct <- lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = "HC0")); crit <- stats::qnorm(0.975)
  ess <- function(x) sum(x)^2 / sum(x^2)
  tables <- list(
    list(id = "ipw_effect", title = sprintf("Inverse Probability Weighting: %s of %s on %s", est, tr, y),
      columns = list(stats_col("term", "Effect", "text"), stats_col("b", "Estimate"), stats_col("se", "SE (robust)"), stats_col("stat", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("lower", "Lower", superTitle = "95% CI"), stats_col("upper", "Upper", superTitle = "95% CI")),
      rows = list(list(term = if (est == "ATT") "Average treatment effect on the treated (ATT)" else "Average treatment effect (ATE)", b = ct[2, 1], se = ct[2, 2], stat = ct[2, 3], p = 2 * stats::pnorm(-abs(ct[2, 3])), lower = ct[2, 1] - crit * ct[2, 2], upper = ct[2, 1] + crit * ct[2, 2])),
      notes = list(sprintf("Propensity scores from a logistic regression; %s weights%s; weighted regression with robust (sandwich) standard errors, which are conservative because the propensity scores are estimated (Austin & Stuart, 2015). Causal only if there are no unmeasured confounders and both groups overlap.",
                           if (est == "ATT") "ATT (treated 1, controls ps/(1−ps))" else if (stab) "stabilised ATE" else "ATE", if (trim > 0) sprintf("; propensity scores trimmed to [%g, %g]", trim, 1 - trim) else ""))),
    list(id = "ipw_weights", title = "Weights", columns = list(stats_col("group", "Group", "text"), stats_col("n", "n", "integer"), stats_col("min", "Min"), stats_col("max", "Max"), stats_col("mean", "Mean"), stats_col("ess", "Effective n")),
      rows = lapply(c(0, 1), function(g) { x <- w[t == g]; list(group = if (g == 1) "Treated" else "Control", n = length(x), min = min(x), max = max(x), mean = mean(x), ess = ess(x)) }),
      notes = list("Effective sample size (Kish): (Σw)² / Σw². Very large weights (or a small effective n) signal poor overlap; consider trimming.")),
    balance_table(colnames(X), b, a, "Weighted"))
  plots <- list(love_plot(colnames(X), vapply(b, `[[`, 0, "smd"), vapply(a, `[[`, 0, "smd"), "Covariate balance (Love plot)"))
  list(tables = tables, plots = Filter(Negate(is.null), plots), n = nrow(dd))
}

STATS_MORE_RUN$svy <- run_svy
STATS_MORE_RUN$did <- run_did
STATS_MORE_RUN$psm <- run_psm
STATS_MORE_RUN$ipw <- run_ipw
