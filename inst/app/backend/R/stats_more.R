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
    lda = c("library(MASS)", sprintf("fit <- lda(factor(`%s`) ~ %s, data = data, CV = %s)", unlist(o$group)[1], v(o$vars), if (isTRUE(o$cv %||% TRUE)) "TRUE" else "FALSE"), "table(data[[1]], fit$class)"),
    character(0))
  paste(c(lines, body), collapse = "\n")
}
