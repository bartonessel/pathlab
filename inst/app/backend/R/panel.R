# Panel data regression (Statistics card), in the style of Stata's xtset / xtdescribe / xtsum / xtreg / xttest:
# pooled OLS, fixed effects (within), random effects (GLS), first differences and between estimators with individual,
# time or two-way effects (plm), conventional, cluster-robust (by panel), Driscoll-Kraay or panel-corrected standard
# errors, Stata-style fit statistics (R-squared within / between / overall, sigma_u, sigma_e, rho) and the usual
# specification tests (Hausman, Breusch-Pagan LM, F test for individual effects, Wooldridge serial correlation,
# Pesaran CD, modified Wald for groupwise heteroskedasticity).

PANEL_MODELS <- c(pooling = "Pooled OLS", within = "Fixed effects (within)", random = "Random effects (GLS)", fd = "First differences", between = "Between effects")
PANEL_EFFECTS <- c(individual = "panel (individual)", time = "time", twoways = "two-way (panel and time)")
PANEL_SE <- c(classic = "Conventional", cluster = "Cluster-robust by panel (Arellano, HC1)", dk = "Driscoll-Kraay (robust to cross-sectional dependence)", pcse = "Panel-corrected (Beck-Katz)")

#' The model data: safe column names (v1, v2, ...), the panel index and the original names of every term.
#' `need_preds = FALSE`: a model of the dependent variable's own lags alone is allowed (dynamic panel GMM).
panel_frame <- function(d, o, need_preds = TRUE) {
  preds <- c(o[["covs"]] %||% character(0), o[["factors"]] %||% character(0))
  if (is.null(o[["dep"]]) || (need_preds && !length(preds)) || is.null(o[["id"]]) || is.null(o[["time"]]))
    api_error(422, "panel_vars", if (need_preds) "Add the dependent variable, at least one predictor, the panel ID and the time variable." else "Add the dependent variable, the panel ID and the time variable.")
  if (o[["dep"]] %in% preds) api_error(422, "panel_vars", "The dependent variable cannot also be a predictor.")
  if (any(c(o[["id"]], o[["time"]]) %in% c(o[["dep"]], preds))) api_error(422, "panel_vars", "The panel ID and the time variable cannot also be in the model.")
  vars <- c(o[["dep"]], preds)
  safe <- paste0("v", seq_along(vars))
  dd <- data.frame(.id = d[[o[["id"]]]], .time = d[[o[["time"]]]])
  for (k in seq_along(vars)) dd[[safe[k]]] <- if (vars[k] %in% (o[["factors"]] %||% character(0))) factor(d[[vars[k]]]) else d[[vars[k]]]
  n_in <- nrow(dd)
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (nrow(dd) < 4) api_error(422, "panel_n", "Too few complete observations for a panel regression.")
  tm <- dd$.time
  dd$.time <- if (is.numeric(tm)) tm else { num <- suppressWarnings(as.numeric(as.character(tm))); if (!anyNA(num)) num else as.character(tm) }
  dup <- duplicated(dd[, c(".id", ".time")])
  if (any(dup)) api_error(422, "panel_duplicates", sprintf("%d row(s) repeat a panel ID and time pair (for example %s at %s).", sum(dup), dd$.id[dup][1], dd$.time[dup][1]),
                          "Each panel (ID) may have one row per time period. Check the ID and time variables, or aggregate the duplicates first.")
  if (length(unique(dd$.id)) < 2) api_error(422, "panel_n", "The panel ID has only one value: a panel needs several units.")
  dd <- dd[order(dd$.id, dd$.time), , drop = FALSE]
  labels <- stats::setNames(vars, safe)
  list(data = dd, dep = safe[1], preds = safe[-1], labels = labels, factors = safe[-1][preds %in% (o[["factors"]] %||% character(0))], n_dropped = n_in - nrow(dd))
}

#' Term labels of the model matrix in the original variable names (factor levels as "name: level").
panel_term_label <- function(term, pf) {
  if (term == "(Intercept)") return("(Intercept)")
  for (s in names(pf$labels)) {
    if (identical(term, s)) return(pf$labels[[s]])
    if (startsWith(term, s) && s %in% pf$factors) return(paste0(pf$labels[[s]], ": ", substring(term, nchar(s) + 1)))
  }
  term
}

panel_vcov <- function(fit, se) {
  switch(se,
    cluster = plm::vcovHC(fit, method = "arellano", type = "HC1", cluster = "group"),
    dk = plm::vcovSCC(fit, type = "HC0"),
    pcse = plm::vcovBK(fit, type = "HC0", cluster = "time"),
    stats::vcov(fit))
}

#' Stata's R-squared within / between / overall: squared correlations of y with the linear prediction X b (slopes only).
panel_r2 <- function(fit, pdat, pf) {
  b <- stats::coef(fit); b <- b[names(b) != "(Intercept)"]
  X <- stats::model.matrix(stats::as.formula(paste("~", paste(pf$preds, collapse = " + "))), data = as.data.frame(pdat))
  X <- X[, intersect(colnames(X), names(b)), drop = FALSE]
  if (!ncol(X)) return(list(within = NA_real_, between = NA_real_, overall = NA_real_))
  xb <- as.vector(X %*% b[colnames(X)])
  y <- as.numeric(pdat[[pf$dep]]); id <- as.character(pdat$.id)
  ym <- stats::ave(y, id); xm <- stats::ave(xb, id)
  sq <- function(a, c) { r <- suppressWarnings(stats::cor(a, c)); if (is.na(r)) NA_real_ else r^2 }
  first <- !duplicated(id)
  list(within = sq(y - ym, xb - xm), between = sq(ym[first], xm[first]), overall = sq(y, xb), xb = xb)
}

#' Modified Wald test for groupwise heteroskedasticity in the fixed-effects model (Greene; Stata's xttest3).
panel_mod_wald <- function(fe) {
  e <- as.numeric(stats::residuals(fe)); id <- as.character(plm::index(fe)[[1]])
  s2 <- mean(e^2)
  parts <- split(e, id)
  stat <- sum(vapply(parts, function(ei) { Ti <- length(ei); if (Ti < 2) return(0); si <- mean(ei^2); vi <- sum((ei^2 - si)^2) / (Ti * (Ti - 1)); if (vi <= 0) 0 else (si - s2)^2 / vi }, 0))
  list(stat = stat, df = length(parts), p = stats::pchisq(stat, length(parts), lower.tail = FALSE))
}

run_panel <- function(d, o) {
  pf <- panel_frame(d, o)
  model <- if ((o[["model"]] %||% "within") %in% names(PANEL_MODELS)) o[["model"]] %||% "within" else "within"
  effect <- if ((o[["effect"]] %||% "individual") %in% names(PANEL_EFFECTS)) o[["effect"]] %||% "individual" else "individual"
  if (model %in% c("fd", "between") && effect != "individual") effect <- "individual"
  se <- if ((o[["se"]] %||% "classic") %in% names(PANEL_SE)) o[["se"]] %||% "classic" else "classic"
  rmethod <- if ((o[["reMethod"]] %||% "swar") %in% c("swar", "walhus", "amemiya", "nerlove")) o[["reMethod"]] %||% "swar" else "swar"
  conf <- (o[["ciWidth"]] %||% 95) / 100
  pdat <- plm::pdata.frame(pf$data, index = c(".id", ".time"), drop.index = FALSE, row.names = TRUE)
  form <- stats::as.formula(paste(pf$dep, "~", paste(pf$preds, collapse = " + ")))
  est <- function(m, eff = effect) plm::plm(form, data = pdat, model = m, effect = if (m %in% c("fd", "between", "pooling")) "individual" else eff,
                                            random.method = if (m == "random") rmethod else NULL)
  fit <- est(model)
  pd <- plm::pdim(pdat)
  lab <- function(t) unname(vapply(t, panel_term_label, "", pf = pf))
  tables <- list()

  # Panel structure (xtset / xtdescribe).
  if (!isFALSE(o[["describe"]])) {
    Ti <- pd$Tint$Ti
    tvals <- sort(unique(pf$data$.time))
    rows <- list(list(item = "Panel variable (ID)", value = o[["id"]]), list(item = "Time variable", value = o[["time"]]),
                 list(item = "Panels (n)", value = format(pd$nT$n)), list(item = "Time periods (T)", value = format(pd$nT$T)),
                 list(item = "Observations (N)", value = format(pd$nT$N)),
                 list(item = "Observations per panel: min / mean / max", value = sprintf("%d / %.1f / %d", min(Ti), mean(Ti), max(Ti))),
                 list(item = "Time span", value = paste(utils::head(tvals, 1), "to", utils::tail(tvals, 1))),
                 list(item = "Balanced", value = if (isTRUE(pd$balanced)) "Yes (strongly balanced)" else "No (unbalanced)"))
    if (pf$n_dropped > 0) rows[[length(rows) + 1]] <- list(item = "Rows with missing values (excluded)", value = format(pf$n_dropped))
    tables[[length(tables) + 1]] <- list(id = "panel_describe", title = "Panel Structure", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = rows,
      notes = list("As Stata's xtset and xtdescribe. Each panel ID has at most one row per time period."))
  }

  # Within / between / overall summaries (xtsum) of the numeric variables.
  if (isTRUE(o[["xtsum"]] %||% TRUE)) {
    nums <- c(pf$dep, setdiff(pf$preds, pf$factors))
    id <- as.character(pf$data$.id)
    rows <- list()
    for (v in nums) {
      x <- as.numeric(pf$data[[v]]); m <- mean(x); xi <- stats::ave(x, id); first <- !duplicated(id); w <- x - xi + m
      nm <- pf$labels[[v]]
      rows <- c(rows, list(list(var = nm, part = "Overall", mean = m, sd = stats::sd(x), min = min(x), max = max(x), obs = paste0("N = ", length(x))),
                           list(var = nm, part = "Between", mean = NULL, sd = stats::sd(xi[first]), min = min(xi[first]), max = max(xi[first]), obs = paste0("n = ", sum(first))),
                           list(var = nm, part = "Within", mean = NULL, sd = stats::sd(w), min = min(w), max = max(w), obs = sprintf("T-bar = %.2f", length(x) / sum(first)))))
    }
    tables[[length(tables) + 1]] <- list(id = "panel_xtsum", title = "Within and Between Variation (xtsum)",
      columns = list(stats_col("var", "Variable", "text", combineBelow = TRUE), stats_col("part", "", "text"), stats_col("mean", "Mean"), stats_col("sd", "SD"), stats_col("min", "Min"), stats_col("max", "Max"), stats_col("obs", "Observations", "text")),
      rows = rows, notes = list("Between: variation of the panel means. Within: deviations from the panel means plus the overall mean (Stata's xtsum). A variable with no within variation is dropped by fixed effects."))
  }

  # Coefficients.
  V <- tryCatch(panel_vcov(fit, se), error = function(e) api_error(422, "panel_se", paste("The standard errors could not be computed:", sanitize_message(conditionMessage(e), 200))))
  b <- stats::coef(fit); sev <- sqrt(pmax(diag(V)[names(b)], 0))
  zdist <- model == "random"
  dfres <- if (zdist) Inf else fit$df.residual
  stat <- b / sev; p <- 2 * (if (zdist) stats::pnorm(-abs(stat)) else stats::pt(-abs(stat), dfres))
  crit <- if (zdist) stats::qnorm(1 - (1 - conf) / 2) else stats::qt(1 - (1 - conf) / 2, dfres)
  ci_t <- sprintf("%g%% CI", 100 * conf)
  tables[[length(tables) + 1]] <- list(id = "panel_coef", title = paste0(PANEL_MODELS[[model]], ": ", pf$labels[[pf$dep]]),
    columns = list(stats_col("term", "Predictor", "text"), stats_col("b", "Coefficient"), stats_col("se", if (se == "classic") "SE" else "Robust SE"),
                   stats_col("stat", if (zdist) "z" else "t"), stats_col("p", "p", pvalue = TRUE), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t)),
    rows = lapply(seq_along(b), function(i) list(term = lab(names(b)[i]), b = unname(b[i]), se = unname(sev[i]), stat = unname(stat[i]), p = unname(p[i]),
                                                 lower = unname(b[i] - crit * sev[i]), upper = unname(b[i] + crit * sev[i]))),
    notes = list(paste0("Effects: ", if (model %in% c("within", "random")) PANEL_EFFECTS[[effect]] else "none", ". Standard errors: ", PANEL_SE[[se]], ".",
                        if (model == "random") paste0(" Variance components: ", switch(rmethod, swar = "Swamy-Arora (as Stata's xtreg, re)", walhus = "Wallace-Hussain", amemiya = "Amemiya", nerlove = "Nerlove"), ".") else "",
                        if (model == "within") " The panel-specific intercepts (fixed effects) are absorbed; the reported intercept, if any, is not estimated." else "")))

  # Model fit (the header of Stata's xtreg output).
  r2 <- panel_r2(fit, pf$data, pf)
  wt <- tryCatch(plm::pwaldtest(fit, test = if (zdist) "Chisq" else "F", vcov = if (se == "classic") NULL else function(x) V), error = function(e) NULL)
  frows <- list(list(item = "Observations (N)", value = format(nrow(pf$data))), list(item = "Panels (n)", value = format(pd$nT$n)),
                list(item = "R-squared within", value = sprintf("%.4f", r2$within)), list(item = "R-squared between", value = sprintf("%.4f", r2$between)),
                list(item = "R-squared overall", value = sprintf("%.4f", r2$overall)))
  if (!is.null(wt)) frows[[length(frows) + 1]] <- list(item = if (zdist) sprintf("Wald chi-squared(%s)", format(wt$parameter[1])) else sprintf("F(%s, %s)", format(wt$parameter[1]), format(round(wt$parameter[2]))),
                                                      value = sprintf("%.2f, p %s", wt$statistic, if (wt$p.value < 0.001) "< .001" else sprintf("= %.3f", wt$p.value)))
  if (model == "within") {
    fe <- plm::fixef(fit, effect = if (effect == "time") "time" else "individual")
    e <- as.numeric(stats::residuals(fit))
    k <- length(b)
    s_e <- sqrt(sum(e^2) / fit$df.residual); s_u <- stats::sd(as.numeric(fe))
    idx <- as.character(pf$data[[if (effect == "time") ".time" else ".id"]])
    u <- as.numeric(fe[idx])
    frows <- c(frows, list(list(item = "sigma_u (SD of the fixed effects)", value = sprintf("%.4f", s_u)), list(item = "sigma_e (SD of the residuals)", value = sprintf("%.4f", s_e)),
                           list(item = "rho (share of variance due to u_i)", value = sprintf("%.4f", s_u^2 / (s_u^2 + s_e^2))),
                           list(item = "corr(u_i, Xb)", value = sprintf("%.4f", suppressWarnings(stats::cor(u, r2$xb))))))
  }
  if (model == "random") {
    ec <- plm::ercomp(fit)
    s_u <- sqrt(ec$sigma2[["id"]] %||% ec$sigma2[[2]]); s_e <- sqrt(ec$sigma2[["idios"]])
    th <- ec$theta
    frows <- c(frows, list(list(item = "sigma_u (SD of the panel effects)", value = sprintf("%.4f", s_u)), list(item = "sigma_e (SD of the residuals)", value = sprintf("%.4f", s_e)),
                           list(item = "rho (share of variance due to u_i)", value = sprintf("%.4f", s_u^2 / (s_u^2 + s_e^2))),
                           list(item = "theta", value = if (is.list(th)) sprintf("%.4f", mean(unlist(th[[1]]))) else if (length(th) > 1) sprintf("%.4f (mean; unbalanced)", mean(th)) else sprintf("%.4f", th)),
                           list(item = "corr(u_i, X)", value = "0 (assumed)")))
  }
  tables[[length(tables) + 1]] <- list(id = "panel_fit", title = "Model Fit", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = frows,
    notes = list("R-squared within, between and overall are the squared correlations of the outcome with the linear prediction, computed within panels, across panel means and over all observations (as in Stata)."))

  # Specification tests.
  tests <- list()
  addt <- function(name, ht, verdict, note = "") tests[[length(tests) + 1]] <<- list(test = name, stat = unname(ht$statistic[1]), df = if (is.numeric(ht$parameter) && length(ht$parameter)) paste(format(round(unname(ht$parameter), 2)), collapse = ", ") else "",
                                                                                       p = unname(ht$p.value), decision = verdict(ht$p.value), note = note)
  sig <- function(p) !is.na(p) && p < 0.05
  safe_test <- function(expr) tryCatch(expr, error = function(e) NULL)
  want <- function(k, d) isTRUE(if (is.null(o[[k]])) d else o[[k]])
  hz <- want("hausman", TRUE); ft <- want("ftest", TRUE); bp <- want("bplm", TRUE); cmp <- want("compare", FALSE)
  pool <- if (model == "pooling") fit else if (ft || bp || cmp) safe_test(est("pooling")) else NULL
  fe <- if (model == "within") fit else if (hz || ft || want("serial", FALSE) || want("hetero", FALSE) || cmp) safe_test(est("within")) else NULL
  re <- if (model == "random") fit else if (hz || cmp) safe_test(est("random")) else NULL
  if (hz && !is.null(fe) && !is.null(re)) { h <- safe_test(plm::phtest(fe, re)); if (!is.null(h)) addt("Hausman (fixed vs random effects)", h, function(p) if (sig(p)) "Prefer fixed effects" else "Random effects consistent") }
  if (ft && !is.null(fe) && !is.null(pool)) { h <- safe_test(plm::pFtest(fe, pool)); if (!is.null(h)) addt("F test for individual effects (all u_i = 0)", h, function(p) if (sig(p)) "Panel effects present (not pooled OLS)" else "Pooled OLS adequate") }
  if (bp && !is.null(pool)) { h <- safe_test(plm::plmtest(pool, effect = if (effect == "twoways") "twoways" else effect, type = "bp")); if (!is.null(h)) addt("Breusch-Pagan LM (random effects vs pooled OLS)", h, function(p) if (sig(p)) "Random effects preferred to pooled OLS" else "Pooled OLS adequate") }
  if (isTRUE(o[["serial"]]) && !is.null(fe)) { h <- safe_test(plm::pwartest(fe)); if (!is.null(h)) addt("Wooldridge serial correlation (AR(1), FE residuals)", h, function(p) if (sig(p)) "Serial correlation: use robust SE" else "No serial correlation detected") }
  if (isTRUE(o[["csd"]])) { h <- safe_test(plm::pcdtest(fit, test = "cd")); if (!is.null(h)) addt("Pesaran CD (cross-sectional dependence)", h, function(p) if (sig(p)) "Cross-sectional dependence: consider Driscoll-Kraay SE" else "No cross-sectional dependence detected") }
  if (isTRUE(o[["hetero"]]) && !is.null(fe)) { w <- safe_test(panel_mod_wald(fe)); if (!is.null(w)) addt("Modified Wald (groupwise heteroskedasticity, FE)", list(statistic = w$stat, parameter = w$df, p.value = w$p), function(p) if (sig(p)) "Heteroskedasticity: use robust SE" else "Homoskedastic across panels") }
  if (length(tests)) tables[[length(tables) + 1]] <- list(id = "panel_tests", title = "Specification Tests",
    columns = list(stats_col("test", "Test", "text"), stats_col("stat", "Statistic"), stats_col("df", "df", "text"), stats_col("p", "p", pvalue = TRUE), stats_col("decision", "At α = .05", "text")),
    rows = lapply(tests, function(t) t[c("test", "stat", "df", "p", "decision")]),
    notes = list("plm: phtest (Hausman, 1978), pFtest, plmtest (Breusch-Pagan, 1980), pwartest (Wooldridge, 2010), pcdtest (Pesaran); the modified Wald test follows Greene (Stata's xttest3)."))

  # Side-by-side comparison (esttab style).
  if (cmp) {
    fits <- list(pooling = pool, within = fe, random = re)
    fits <- fits[!vapply(fits, is.null, TRUE)]
    terms <- unique(unlist(lapply(fits, function(f) names(stats::coef(f)))))
    star <- function(pv) if (is.na(pv)) "" else if (pv < 0.001) "***" else if (pv < 0.01) "**" else if (pv < 0.05) "*" else ""
    cell <- function(f, t, m) {
      bb <- stats::coef(f); if (!t %in% names(bb)) return("")
      VV <- tryCatch(panel_vcov(f, se), error = function(e) stats::vcov(f)); ss <- sqrt(VV[t, t]); zz <- bb[[t]] / ss
      pv <- 2 * (if (m == "random") stats::pnorm(-abs(zz)) else stats::pt(-abs(zz), f$df.residual))
      sprintf("%.4f%s (%.4f)", bb[[t]], star(pv), ss)
    }
    rows <- lapply(terms, function(t) c(list(term = lab(t)), stats::setNames(lapply(names(fits), function(m) cell(fits[[m]], t, m)), names(fits))))
    rows[[length(rows) + 1]] <- c(list(term = "R-squared within / overall"), stats::setNames(lapply(names(fits), function(m) { r <- panel_r2(fits[[m]], pf$data, pf); sprintf("%.3f / %.3f", r$within, r$overall) }), names(fits)))
    rows[[length(rows) + 1]] <- c(list(term = "Observations"), stats::setNames(lapply(names(fits), function(m) format(nrow(pf$data))), names(fits)))
    tables[[length(tables) + 1]] <- list(id = "panel_compare", title = "Model Comparison",
      columns = c(list(stats_col("term", "", "text")), lapply(names(fits), function(m) stats_col(m, PANEL_MODELS[[m]], "text"))), rows = rows,
      notes = list(paste0("Coefficient (", if (se == "classic") "SE" else "robust SE", "); * p < .05, ** p < .01, *** p < .001. Pooled OLS ignores the panel structure; fixed effects use only within-panel variation; random effects assume the panel effects are unrelated to the predictors (check with the Hausman test).")))
  }
  list(tables = tables, plots = list(), n = nrow(pf$data))
}

panel_code <- function(o, lines, q) {
  vars <- c(o[["covs"]] %||% character(0), o[["factors"]] %||% character(0))
  rhs <- paste(c(sprintf("`%s`", o[["covs"]] %||% character(0)), sprintf("factor(`%s`)", o[["factors"]] %||% character(0))), collapse = " + ")
  model <- o[["model"]] %||% "within"; effect <- if (model %in% c("within", "random")) o[["effect"]] %||% "individual" else "individual"
  vc <- switch(o[["se"]] %||% "classic", cluster = "vcovHC(fit, method = \"arellano\", type = \"HC1\", cluster = \"group\")", dk = "vcovSCC(fit, type = \"HC0\")",
               pcse = "vcovBK(fit, type = \"HC0\", cluster = \"time\")", "vcov(fit)")
  c(lines, "library(plm); library(lmtest)",
    sprintf("pdat <- pdata.frame(data, index = c(%s, %s))   # xtset", q(o[["id"]]), q(o[["time"]])),
    "pdim(pdat)   # xtdescribe",
    sprintf("f <- `%s` ~ %s", o[["dep"]], rhs),
    sprintf("fit <- plm(f, data = pdat, model = %s, effect = %s%s)", q(model), q(effect), if (model == "random") sprintf(", random.method = %s", q(o[["reMethod"]] %||% "swar")) else ""),
    sprintf("coeftest(fit, vcov = %s)", vc),
    "fe <- plm(f, data = pdat, model = \"within\"); re <- plm(f, data = pdat, model = \"random\"); pool <- plm(f, data = pdat, model = \"pooling\")",
    "phtest(fe, re)                  # Hausman test", "pFtest(fe, pool)                # F test for individual effects", "plmtest(pool, type = \"bp\")      # Breusch-Pagan LM",
    if (isTRUE(o[["serial"]])) "pwartest(fe)                    # Wooldridge serial correlation",
    if (isTRUE(o[["csd"]])) "pcdtest(fit, test = \"cd\")       # Pesaran CD") |> paste(collapse = "\n")
}

# ---- Dynamic panel GMM (Stata's xtabond / xtdpdsys / xtabond2) ---------------------------------------------------
# Arellano-Bond difference GMM and Blundell-Bond system GMM (plm::pgmm): the dependent variable's own lags as
# regressors, lags of the dependent variable (and of the endogenous regressors) from `lagMin` on as GMM-style
# instruments, exogenous regressors as their own (IV-style) instruments; one- or two-step, Windmeijer-corrected robust
# standard errors for two-step; Sargan / Hansen test of the overidentifying restrictions, Arellano-Bond AR(1) / AR(2)
# tests and Wald tests of the coefficients and the time dummies.

PGMM_EST <- c(d = "Difference GMM (Arellano-Bond)", sys = "System GMM (Blundell-Bond)")

#' Coefficient label of a pgmm term: lags of a variable as Stata writes them (L1.y), time dummies as "time = value".
pgmm_label <- function(t, pf, time_name) {
  m <- regmatches(t, regexec("^lag\\((v[0-9]+), ([0-9]+)(:[0-9]+)?\\)([0-9]+)?$", t))[[1]]
  if (length(m)) { k <- if (nzchar(m[5])) as.integer(m[5]) else as.integer(m[3]); return(sprintf("L%d.%s", k, pf$labels[[m[2]]])) }
  if (t %in% names(pf$labels)) return(pf$labels[[t]])
  if (t == "(Intercept)") return(t)
  paste0(time_name, " = ", t)
}

run_pgmm <- function(d, o) {
  exog <- as.character(unlist(o[["covs"]] %||% character(0))); endog <- as.character(unlist(o[["endog"]] %||% character(0)))
  if (length(intersect(exog, endog))) api_error(422, "pgmm_vars", "A regressor cannot be both exogenous and endogenous.")
  pf <- panel_frame(d, list(dep = o[["dep"]], covs = c(exog, endog), id = o[["id"]], time = o[["time"]]), need_preds = FALSE)
  if (!is.numeric(pf$data$.time)) api_error(422, "pgmm_time", "The time variable must be numeric (a year or period number) for the lags of a dynamic panel model.")
  y <- pf$dep
  sx <- names(pf$labels)[match(exog, pf$labels)]; sn <- names(pf$labels)[match(endog, pf$labels)]
  ky <- as.integer(num_opt(o, "lagsY", 1, 1, 3))
  lmin <- as.integer(num_opt(o, "lagMin", 2, 2, 20)); lmax <- as.integer(num_opt(o, "lagMax", 99, lmin, 99))
  sys <- identical(o[["estimator"]], "sys")
  two <- !identical(o[["steps"]], "onestep")
  eff <- if (identical(o[["effect"]], "individual")) "individual" else "twoways"
  rob <- !isFALSE(o[["robust"]])
  coll <- isTRUE(o[["collapse"]])
  lagterm <- function(v, a, b) if (a == b) sprintf("lag(%s, %d)", v, a) else sprintf("lag(%s, %d:%d)", v, a, b)
  rhs <- c(lagterm(y, 1, ky), sx, sn)
  gmm <- c(lagterm(y, lmin, lmax), vapply(sn, function(v) lagterm(v, lmin, lmax), ""))
  form <- stats::as.formula(paste(y, "~", paste(rhs, collapse = " + "), "|", paste(gmm, collapse = " + ")))
  pdat <- plm::pdata.frame(pf$data, index = c(".id", ".time"), drop.index = FALSE, row.names = TRUE)
  # pgmm builds an internal call to plm() by name, so it is evaluated where plm's functions are visible (plm is not attached).
  env <- new.env(parent = asNamespace("plm")); env$form <- form; env$pdat <- pdat
  n_pan <- length(unique(pf$data$.id))
  # With few panels and many instruments (or time dummies) the GMM weighting matrix is singular: say what to change.
  singular <- function(e) if (grepl("singular", conditionMessage(e)))
    api_error(422, "pgmm_singular", sprintf("The GMM weighting matrix cannot be inverted: there are too many instruments for %d panels.", n_pan),
              "Tick 'Collapse the instrument matrix', set a shorter lag range (for example lags 2 to 4), or choose 'No time dummies' (Roodman, 2009).") else stop(e)
  ginv <- FALSE   # plm falls back on a generalized inverse of a near-singular weighting matrix (as xtabond2 does): say so plainly
  fit <- withCallingHandlers(
    tryCatch(eval(bquote(pgmm(form, data = pdat, effect = .(eff), model = .(if (two) "twosteps" else "onestep"), transformation = .(if (sys) "ld" else "d"), collapse = .(coll))), env), error = singular),
    warning = function(w) if (grepl("general inverse", conditionMessage(w))) { ginv <<- TRUE; invokeRestart("muffleWarning") })
  if (ginv) warning("The GMM weighting matrix was nearly singular, so a generalized inverse was used (as in Stata's xtabond2). The estimates are valid but suggest many instruments relative to the panels.", call. = FALSE)
  s <- tryCatch(summary(fit, robust = rob), error = singular)
  # Coefficients from coef() and the (Windmeijer-corrected) covariance: summary() leaves out the time dummies.
  bb <- stats::coef(fit); V <- tryCatch(if (rob) plm::vcovHC(fit) else stats::vcov(fit), error = singular); sev <- sqrt(pmax(diag(V)[names(bb)], 0))
  zz <- bb / sev
  cf <- cbind(bb, sev, zz, 2 * stats::pnorm(-abs(zz)))
  lab <- function(t) unname(vapply(t, pgmm_label, "", pf = pf, time_name = o[["time"]]))
  conf <- (o[["ciWidth"]] %||% 95) / 100; crit <- stats::qnorm(1 - (1 - conf) / 2); ci_t <- sprintf("%g%% CI", 100 * conf)
  se_txt <- if (!rob) "conventional" else if (two) "robust, Windmeijer (2005) finite-sample corrected" else "robust (clustered by panel)"
  is_td <- !grepl("^lag\\(|^v[0-9]+$", rownames(cf)) & rownames(cf) != "(Intercept)"
  tables <- list(list(id = "pgmm_coef", title = paste0(PGMM_EST[[if (sys) "sys" else "d"]], ": ", pf$labels[[y]]),
    columns = list(stats_col("term", "Predictor", "text"), stats_col("b", "Coefficient"), stats_col("se", if (rob) "Robust SE" else "SE"), stats_col("stat", "z"),
                   stats_col("p", "p", pvalue = TRUE), stats_col("lower", "Lower", superTitle = ci_t), stats_col("upper", "Upper", superTitle = ci_t)),
    rows = lapply(which(!is_td | isTRUE(o[["showTime"]])), function(i) list(term = lab(rownames(cf)[i]), b = cf[i, 1], se = cf[i, 2], stat = cf[i, 3], p = cf[i, 4],
                                                                              lower = cf[i, 1] - crit * cf[i, 2], upper = cf[i, 1] + crit * cf[i, 2])),
    notes = list(paste0(if (two) "Two-step" else "One-step", " estimator; standard errors ", se_txt, ".",
                        if (eff == "twoways") paste0(" Time dummies included", if (!isTRUE(o[["showTime"]])) " (not shown; see the Wald test)" else "", ".") else "",
                        " GMM instruments: lags ", lmin, " to ", if (lmax >= 99) "all available" else lmax, " of ", paste(c(pf$labels[[y]], endog), collapse = ", "),
                        if (coll) " (collapsed)" else "", if (length(exog)) paste0("; IV-style instruments: ", paste(exog, collapse = ", ")) else "", "."))))

  # Specification tests (the bottom of Stata's xtabond2 output).
  n_inst <- ncol(fit$W[[1]])
  tests <- list()
  addt <- function(name, ht, h0, verdict) tests[[length(tests) + 1]] <<- list(test = name, stat = unname(ht$statistic[1]),
    df = if (is.numeric(ht$parameter) && length(ht$parameter)) format(unname(ht$parameter[1])) else "", p = unname(ht$p.value[1]), h0 = h0, decision = verdict(unname(ht$p.value[1])))
  sig <- function(p) !is.na(p) && p < 0.05
  if (!is.null(s$m1)) addt("Arellano-Bond AR(1) in first differences", s$m1, "No first-order autocorrelation", function(p) if (sig(p)) "AR(1) present (expected)" else "No AR(1)")
  if (!is.null(s$m2)) addt("Arellano-Bond AR(2) in first differences", s$m2, "No second-order autocorrelation", function(p) if (sig(p)) "AR(2): lag-2 instruments invalid" else "No AR(2): instruments valid")
  if (!is.null(s$sargan)) addt(if (two) "Hansen J test of overidentifying restrictions" else "Sargan test of overidentifying restrictions", s$sargan,
                               "Instruments valid (overidentifying restrictions hold)", function(p) if (sig(p)) "Instruments may be invalid" else "Instruments not rejected")
  if (!is.null(s$wald.coef)) addt("Wald test, coefficients", s$wald.coef, "All coefficients are zero", function(p) if (sig(p)) "Jointly significant" else "Not jointly significant")
  if (!is.null(s$wald.td)) addt("Wald test, time dummies", s$wald.td, "No time effects", function(p) if (sig(p)) "Time effects present" else "No time effects")
  tables[[2]] <- list(id = "pgmm_tests", title = "Specification Tests",
    columns = list(stats_col("test", "Test", "text"), stats_col("stat", "Statistic"), stats_col("df", "df", "text"), stats_col("p", "p", pvalue = TRUE), stats_col("h0", "H0", "text"), stats_col("decision", "At α = .05", "text")),
    rows = tests,
    notes = list(paste0("A valid model shows AR(1) but no AR(2) in the differenced residuals (Arellano & Bond, 1991), and a non-significant ", if (two) "Hansen" else "Sargan",
                        " test. A Hansen p-value very close to 1 can signal too many instruments (Roodman, 2009).")))

  n_obs <- sum(vapply(fit$residuals, function(r) sum(!is.na(r) & r != 0), 0))
  info <- list(list(item = "Estimator", value = paste0(PGMM_EST[[if (sys) "sys" else "d"]], ", ", if (two) "two-step" else "one-step")),
               list(item = "Panels (groups)", value = format(n_pan)), list(item = "Observations used", value = format(n_obs)),
               list(item = "Instruments", value = format(n_inst)), list(item = "Lags of the dependent variable", value = format(ky)))
  tables[[3]] <- list(id = "pgmm_info", title = "Model Information", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = info,
    notes = list("Observations used: rows that enter the transformed equations (a panel loses its first periods to differencing and lags)."))
  warns <- if (n_inst > n_pan) sprintf("There are more instruments (%d) than panels (%d): the %s test is weakened and the estimates can be biased towards OLS. Collapse the instruments or limit the lag range (Roodman, 2009).",
                                      n_inst, n_pan, if (two) "Hansen" else "Sargan") else character(0)
  for (w in warns) warning(w, call. = FALSE)
  list(tables = tables, plots = list(), n = n_obs)
}

pgmm_code <- function(o, lines, q) {
  y <- sprintf("`%s`", o[["dep"]]); ky <- as.integer(o[["lagsY"]] %||% 1)
  lmin <- as.integer(o[["lagMin"]] %||% 2); lmax <- as.integer(o[["lagMax"]] %||% 99)
  lagterm <- function(v, a, b) if (a == b) sprintf("lag(%s, %d)", v, a) else sprintf("lag(%s, %d:%d)", v, a, b)
  ex <- sprintf("`%s`", unlist(o[["covs"]] %||% character(0))); en <- sprintf("`%s`", unlist(o[["endog"]] %||% character(0)))
  f <- sprintf("%s ~ %s | %s", y, paste(c(lagterm(y, 1, ky), ex, en), collapse = " + "), paste(c(lagterm(y, lmin, lmax), vapply(en, function(v) lagterm(v, lmin, lmax), "")), collapse = " + "))
  c(lines, "library(plm)",
    sprintf("pdat <- pdata.frame(data, index = c(%s, %s))   # xtset", q(o[["id"]]), q(o[["time"]])),
    sprintf("fit <- pgmm(%s, data = pdat, effect = %s, model = %s, transformation = %s%s)", f, q(if (identical(o[["effect"]], "individual")) "individual" else "twoways"),
            q(if (identical(o[["steps"]], "onestep")) "onestep" else "twosteps"), q(if (identical(o[["estimator"]], "sys")) "ld" else "d"), if (isTRUE(o[["collapse"]])) ", collapse = TRUE" else ""),
    sprintf("summary(fit, robust = %s)   # coefficients, Sargan/Hansen, AR(1), AR(2), Wald tests", if (isFALSE(o[["robust"]])) "FALSE" else "TRUE")) |> paste(collapse = "\n")
}

# ---- Panel unit-root tests (Stata's xtunitroot) -------------------------------------------------------------------
# plm::purtest on each variable: Levin-Lin-Chu, Im-Pesaran-Shin, Fisher-type ADF (Maddala-Wu inverse chi-squared, Choi's
# modified inverse chi-squared, inverse normal and inverse logit) and Hadri's LM test, with panel means (intercept),
# intercept and trend, or neither, in levels or first differences; ADF lags chosen by AIC, SIC or Hall's method, or fixed.

PURTEST_TESTS <- c(levinlin = "Levin-Lin-Chu (LLC)", ips = "Im-Pesaran-Shin (IPS)", madwu = "Fisher ADF, inverse chi-squared (Maddala-Wu)",
                   Pm = "Fisher ADF, modified inverse chi-squared (Choi Pm)", invnormal = "Fisher ADF, inverse normal (Choi Z)",
                   logit = "Fisher ADF, inverse logit (Choi L*)", hadri = "Hadri LM")

run_purtest <- function(d, o) {
  vars <- as.character(unlist(o[["vars"]] %||% character(0)))
  if (!length(vars) || is.null(o[["id"]]) || is.null(o[["time"]])) api_error(422, "purtest_vars", "Add at least one variable, the panel ID and the time variable.")
  tests <- intersect(as.character(unlist(o[["tests"]] %||% c("levinlin", "ips", "madwu", "hadri"))), names(PURTEST_TESTS))
  if (!length(tests)) api_error(422, "purtest_tests", "Choose at least one test.")
  exo <- if ((o[["exo"]] %||% "intercept") %in% c("none", "intercept", "trend")) o[["exo"]] %||% "intercept" else "intercept"
  sel <- o[["lagSel"]] %||% "AIC"
  lags <- if (identical(sel, "fixed")) as.integer(num_opt(o, "lags", 1, 0, 12)) else if (sel %in% c("AIC", "SIC", "Hall")) sel else "AIC"
  pmax <- as.integer(num_opt(o, "pmax", 4, 1, 12))
  dif <- isTRUE(o[["diff"]])
  rows <- list(); n_max <- 0; shape <- NULL
  for (v in vars) {
    pf <- panel_frame(d, list(dep = v, id = o[["id"]], time = o[["time"]]), need_preds = FALSE)
    dd <- pf$data
    if (dif) {   # first differences within each panel (consecutive periods)
      x <- as.numeric(dd$v1); prev <- c(NA, utils::head(x, -1)); same <- c(FALSE, dd$.id[-1] == dd$.id[-nrow(dd)])
      dd$v1 <- ifelse(same, x - prev, NA); dd <- dd[!is.na(dd$v1), , drop = FALSE]
    }
    pdat <- plm::pdata.frame(dd, index = c(".id", ".time"), drop.index = FALSE, row.names = TRUE)
    pd <- plm::pdim(pdat)
    n_max <- max(n_max, nrow(dd))
    shape <- sprintf("%d panels, T = %d%s", pd$nT$n, pd$nT$T, if (isTRUE(pd$balanced)) " (balanced)" else " (unbalanced)")
    label <- if (dif) paste0("D.", v) else v
    for (t in tests) {
      r <- tryCatch(suppressWarnings(plm::purtest(pdat$v1, test = t, exo = exo, lags = lags, pmax = pmax)), error = function(e) conditionMessage(e))
      h0 <- if (t == "hadri") "All panels are stationary" else "All panels contain unit roots"
      if (is.character(r)) {
        rows[[length(rows) + 1]] <- list(var = label, test = PURTEST_TESTS[[t]], statName = "", stat = NULL, p = NULL, h0 = h0, decision = paste("Not computed:", sanitize_message(r, 160)))
        next
      }
      st <- r$statistic
      p <- unname(st$p.value)
      rej <- !is.na(p) && p < 0.05
      rows[[length(rows) + 1]] <- list(var = label, test = PURTEST_TESTS[[t]], statName = names(st$statistic)[1], stat = unname(st$statistic[1]), p = p, h0 = h0,
                                       decision = if (t == "hadri") (if (rej) "Reject: unit roots in some panels" else "Stationary")
                                                  else if (!rej) "Unit root" else if (t == "levinlin") "Reject: panels stationary" else "Reject: some panels stationary")
    }
  }
  exo_txt <- switch(exo, none = "no deterministic terms", trend = "panel-specific intercepts and trends", "panel-specific intercepts")
  list(tables = list(list(id = "purtest", title = paste0("Panel Unit Root Tests", if (dif) " (first differences)" else ""),
    columns = list(stats_col("var", "Variable", "text", combineBelow = TRUE), stats_col("test", "Test", "text"), stats_col("statName", "Statistic", "text"), stats_col("stat", "Value"),
                   stats_col("p", "p", pvalue = TRUE), stats_col("h0", "H0", "text"), stats_col("decision", "At α = .05", "text")),
    rows = rows,
    notes = list(paste0("Deterministic terms: ", exo_txt, ". ADF lags: ", if (is.numeric(lags)) paste(lags, "(fixed)") else paste0("chosen by ", lags, " (at most ", pmax, ")"), ". Panel: ", shape, "."),
                 "Hadri tests the null of stationarity in every panel; the other tests the null of a unit root in every panel. LLC assumes a common autoregressive parameter; IPS and the Fisher tests allow it to differ across panels. LLC, IPS and Hadri need a balanced panel; IPS and Hadri need an intercept or trend."))),
    plots = list(), n = n_max)
}

purtest_code <- function(o, lines, q) {
  tests <- as.character(unlist(o[["tests"]] %||% c("levinlin", "ips", "madwu", "hadri")))
  sel <- o[["lagSel"]] %||% "AIC"; lags <- if (identical(sel, "fixed")) as.character(o[["lags"]] %||% 1) else q(sel)
  c(lines, "library(plm)",
    sprintf("pdat <- pdata.frame(data, index = c(%s, %s))   # xtset", q(o[["id"]]), q(o[["time"]])),
    unlist(lapply(unlist(o[["vars"]]), function(v) vapply(tests, function(t) sprintf("purtest(%s, test = %s, exo = %s, lags = %s, pmax = %s)   # xtunitroot",
      if (isTRUE(o[["diff"]])) sprintf("diff(pdat[[%s]])", q(v)) else sprintf("pdat[[%s]]", q(v)), q(t), q(o[["exo"]] %||% "intercept"), lags, o[["pmax"]] %||% 4), "")))) |> paste(collapse = "\n")
}
