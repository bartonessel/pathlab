# Latent class and growth models of the Latent profiles analysis (run through the Statistics engine):
#   * lca  - latent class analysis of categorical indicators (poLCA), with three-step distal outcomes and covariates;
#   * lta  - latent transition analysis: the same continuous indicators at two or more waves, states with invariant
#            means and transitions between waves (hidden Markov model, depmixS4);
#   * gmm  - growth mixture models and latent class growth analysis (lcmm::hlme), with three-step distal outcomes and
#            covariates;
#   * lgc  - latent growth curves (lavaan::growth): linear, quadratic, latent basis and piecewise growth, custom time
#            scores, time-invariant and time-varying covariates, multigroup models and parallel processes.
# Every number comes from these packages, except the three-step analyses (R/three_step.R) and the information criteria
# poLCA / depmixS4 do not report (SABIC, CAIC, entropy), which use the standard formulas.

lm_entropy <- function(z) { k <- ncol(z); if (k < 2) return(NA_real_); 1 + sum(z * log(pmax(z, 1e-300))) / (nrow(z) * log(k)) }
lm_sabic <- function(ll, np, n) -2 * ll + np * log((n + 2) / 24)
lm_k_range <- function(o) { lo <- round(num_opt(o, "kMin", 1, 1, 8)); hi <- round(num_opt(o, "kMax", 4, 1, 8)); if (hi < lo) api_error(422, "latent_k", "The largest number of classes must be at least the smallest."); lo:hi }
lm_pick <- function(o, ks, bic) {
  k <- round(num_opt(o, "k", 0, 0, 8))
  if (k > 0 && k %in% ks && is.finite(bic[match(k, ks)])) return(list(k = k, how = "chosen"))
  list(k = ks[which.min(replace(bic, !is.finite(bic), Inf))], how = "lowest BIC")
}
lm_lmr <- function(n, ll, np, ks) vapply(seq_along(ks), function(i) {
  j <- match(ks[i] - 1, ks)
  if (is.na(j) || !is.finite(ll[i]) || !is.finite(ll[j])) return(NA_real_)
  r <- tryCatch(tidyLPA::calc_lrt(n, ll[j], np[j], ks[j], ll[i], np[i], ks[i]), error = function(e) NULL)
  if (is.null(r)) NA_real_ else unname(r[["lmr_p"]])
}, 0)
lm_fit_cols <- function(unit, extra = list()) c(list(stats_col("k", unit, "integer"), stats_col("ll", "Log-likelihood"), stats_col("np", "Parameters", "integer"),
  stats_col("aic", "AIC"), stats_col("bic", "BIC"), stats_col("sabic", "SABIC")), extra,
  list(stats_col("entropy", "Entropy"), stats_col("smallest", "Smallest (%)"), stats_col("lmr", "LMR p", pvalue = TRUE), stats_col("sel", "", "text")))
lm_plural <- function(unit) if (unit == "Class") "classes" else paste0(tolower(unit), "s")
lm_note <- function(pick, unit) sprintf("%s %d %s. LMR = Lo-Mendell-Rubin adjusted likelihood ratio test of k vs. k − 1 %s (tidyLPA::calc_lrt). Choose the number of %s from several criteria together and from whether they are meaningful.",
                                        if (pick$how == "chosen") "Interpreted solution:" else "Interpreted solution (lowest BIC):", pick$k, if (pick$k == 1) tolower(unit) else lm_plural(unit),
                                        lm_plural(unit), lm_plural(unit))
LM_COLS <- c("#1d4f86", "#c2410c", "#15803d", "#7c3aed", "#b45309", "#be185d", "#0e7490", "#475569")
lm_quiet <- function(expr) { out <- NULL; utils::capture.output(out <- suppressMessages(expr)); out }
lm_times <- function(o, n) {
  s <- trimws(opt(o, "times", ""))
  if (!nzchar(s)) return(seq_len(n) - 1)
  t <- suppressWarnings(as.numeric(strsplit(s, "[,; ]+")[[1]]))
  t <- t[!is.na(t)]
  if (length(t) != n) api_error(422, "latent_times", sprintf("Give one time score per wave (%d numbers, e.g. 0, 1, 2); %d were given.", n, length(t)))
  if (any(diff(t) <= 0)) api_error(422, "latent_times", "The time scores must increase from wave to wave.")
  t
}
lm_sub <- function(d, rows, vars) if (length(vars)) d[rows, vars, drop = FALSE] else NULL

# ---- Latent class analysis (poLCA) --------------------------------------------------------------------------------
run_lca <- function(d, o) {
  items <- opt(o, "items", character(0)); if (length(items) < 2) api_error(422, "lca_items", "Add at least two categorical indicators.")
  ks <- lm_k_range(o); nrep <- round(num_opt(o, "nrep", 10, 1, 100)); seed <- round(num_opt(o, "seed", 2026, 1))
  levs <- lapply(items, function(v) levels(factor(d[[v]])))
  Y <- as.data.frame(lapply(items, function(v) as.integer(factor(d[[v]]))))
  names(Y) <- paste0("u", seq_along(items))
  few <- items[vapply(levs, length, 0L) < 2]; if (length(few)) api_error(422, "lca_constant", sprintf("These indicators have only one category: %s.", paste(few, collapse = ", ")))
  many <- items[vapply(levs, length, 0L) > 10]; if (length(many)) api_error(422, "lca_categories", sprintf("These indicators have more than 10 categories (use latent profiles for continuous indicators): %s.", paste(many, collapse = ", ")))
  rows <- which(rowSums(!is.na(Y)) > 0); Y <- Y[rows, , drop = FALSE]
  f <- stats::as.formula(paste0("cbind(", paste(names(Y), collapse = ", "), ") ~ 1"))
  fits <- lapply(ks, function(k) { set.seed(seed); tryCatch(lm_quiet(poLCA::poLCA(f, Y, nclass = k, nrep = if (k == 1) 1 else nrep, na.rm = FALSE, verbose = FALSE, calc.se = TRUE)), error = function(e) NULL) })
  N <- nrow(Y)
  ll <- vapply(fits, function(m) if (is.null(m)) NA_real_ else m$llik, 0); np <- vapply(fits, function(m) if (is.null(m)) NA_real_ else m$npar, 0)
  bic <- vapply(fits, function(m) if (is.null(m)) NA_real_ else m$bic, 0)
  pick <- lm_pick(o, ks, bic); lmr <- lm_lmr(N, ll, np, ks)
  fit_rows <- lapply(seq_along(ks), function(i) { m <- fits[[i]]; if (is.null(m)) return(list(k = ks[i], sel = "not estimated"))
    list(k = ks[i], ll = m$llik, np = m$npar, aic = m$aic, bic = m$bic, sabic = lm_sabic(m$llik, m$npar, N), caic = -2 * m$llik + m$npar * (log(N) + 1),
         gsq = m$Gsq, df = m$resid.df, entropy = lm_entropy(m$posterior), smallest = 100 * min(m$P), lmr = lmr[i], sel = if (ks[i] == pick$k) "interpreted" else "") })
  m <- fits[[match(pick$k, ks)]]
  if (is.null(m)) api_error(422, "lca_failed", "poLCA could not estimate the latent class model.")
  k <- pick$k; z <- m$posterior; cls <- m$predclass
  sizes <- lapply(seq_len(k), function(s) list(class = s, prev = 100 * m$P[s], n = sum(cls == s), pct = 100 * mean(cls == s), post = if (any(cls == s)) mean(z[cls == s, s]) else NA_real_))
  irp <- unlist(lapply(seq_along(items), function(j) {
    pr <- m$probs[[j]]; se <- m$probs.se[[j]]
    lapply(seq_len(ncol(pr)), function(cc) c(list(item = items[j], cat = levs[[j]][cc]),
                                              stats::setNames(as.list(pr[, cc]), paste0("p", seq_len(k))), stats::setNames(as.list(se[, cc]), paste0("se", seq_len(k)))))
  }), recursive = FALSE)
  tables <- list(
    list(id = "lca_fit", title = "Latent Class Analysis: Model Fit", rows = fit_rows,
         columns = lm_fit_cols("Classes", list(stats_col("caic", "CAIC"), stats_col("gsq", "G²"), stats_col("df", "df", "integer"))),
         notes = list(paste0(sprintf("poLCA, %d random starts per solution; N = %d. ", nrep, N), lm_note(pick, "Class"), " G² compares the model with the observed response patterns (unreliable with many empty patterns)."))),
    list(id = "lca_sizes", title = "Class Sizes and Classification Quality", rows = sizes,
         columns = list(stats_col("class", "Class", "integer"), stats_col("prev", "Estimated %"), stats_col("n", "N (modal)", "integer"), stats_col("pct", "% (modal)"), stats_col("post", "Avg. posterior probability")),
         notes = list(sprintf("Entropy = %.3f. Average posterior probabilities above .70 indicate well-separated classes.", lm_entropy(z)))),
    list(id = "lca_irp", title = "Item-Response Probabilities", rows = irp,
         columns = c(list(stats_col("item", "Indicator", "text", combineBelow = TRUE), stats_col("cat", "Category", "text")),
                     unlist(lapply(seq_len(k), function(s) list(stats_col(paste0("p", s), "P", superTitle = paste("Class", s)), stats_col(paste0("se", s), "SE", superTitle = paste("Class", s)))), recursive = FALSE)),
         notes = list("Probability of each response category within each class (poLCA standard errors).")))
  tables <- c(tables, ts_tables(z, cls, lm_sub(d, rows, opt(o, "distal", character(0))), lm_sub(d, rows, opt(o, "covs", character(0))), unit = "Class", prefix = "lca"))
  pl <- stats_png(function() {
    nc <- min(k, 4); graphics::par(mfrow = c(ceiling(k / nc), nc), mar = c(3, 7, 2, 1))
    pal <- grDevices::hcl.colors(max(vapply(levs, length, 0L)), "Blues 3", rev = TRUE)
    for (s in seq_len(k)) {
      M <- sapply(m$probs, function(pr) { v <- pr[s, ]; c(v, rep(0, length(pal) - length(v))) })
      graphics::barplot(M, horiz = TRUE, names.arg = items, las = 1, col = pal, border = NA, xlim = c(0, 1), main = sprintf("Class %d (%.0f%%)", s, 100 * m$P[s]), cex.names = 0.8, cex.main = 0.9)
    }
  }, title = "Response probabilities by class (darker = higher category)", w = 8, h = 2.4 * ceiling(k / min(k, 4)) + 0.6)
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = N)
}

# ---- Latent transition analysis (depmixS4) -------------------------------------------------------------------------
run_lta <- function(d, o) {
  waves <- Filter(function(w) length(w$vars) > 0, opt(o, "waves", list()))
  if (length(waves) < 2) api_error(422, "lta_waves", "Add at least two waves, each with the same indicators.")
  p <- length(waves[[1]]$vars)
  if (any(vapply(waves, function(w) length(w$vars), 0L) != p)) api_error(422, "lta_indicators", "Every wave needs the same number of indicators, in the same order.")
  ks <- lm_k_range(o); nstart <- round(num_opt(o, "nrep", 5, 1, 50)); seed <- round(num_opt(o, "seed", 2026, 1))
  vars <- unlist(lapply(waves, `[[`, "vars"))
  X <- as.data.frame(lapply(vars, function(v) as.numeric(d[[v]])))
  rows <- which(stats::complete.cases(X)); N <- length(rows); Tn <- length(waves)
  if (N < 30) api_error(422, "lta_n", "Too few complete cases for a latent transition analysis.")
  ind <- paste0("x", seq_len(p))
  long <- do.call(rbind, lapply(seq_len(Tn), function(t) { m <- as.data.frame(lapply(waves[[t]]$vars, function(v) as.numeric(d[[v]][rows]))); names(m) <- ind; cbind(id = seq_len(N), time = t, m) }))
  long <- long[order(long$id, long$time), ]
  resp <- lapply(ind, function(v) stats::as.formula(paste(v, "~ 1")))
  fit_k <- function(k) {
    mod <- depmixS4::depmix(resp, data = long, nstates = k, family = rep(list(stats::gaussian()), p), ntimes = rep(Tn, N))
    best <- NULL
    for (r in seq_len(if (k == 1) 1 else nstart)) {
      set.seed(seed + r)
      f <- tryCatch(lm_quiet(depmixS4::fit(mod, verbose = FALSE, emcontrol = depmixS4::em.control(rand = k > 1))), error = function(e) NULL)
      if (!is.null(f) && (is.null(best) || as.numeric(depmixS4::logLik(f)) > as.numeric(depmixS4::logLik(best)))) best <- f
    }
    best
  }
  fits <- lapply(ks, fit_k)
  ll <- vapply(fits, function(f) if (is.null(f)) NA_real_ else as.numeric(depmixS4::logLik(f)), 0); np <- vapply(fits, function(f) if (is.null(f)) NA_real_ else depmixS4::freepars(f), 0)
  bic <- -2 * ll + np * log(N)
  pick <- lm_pick(o, ks, bic); lmr <- lm_lmr(N, ll, np, ks)
  post_of <- function(f, k) { z <- as.matrix(depmixS4::posterior(f, type = "smoothing")); z[, (ncol(z) - k + 1):ncol(z), drop = FALSE] }
  fit_rows <- lapply(seq_along(ks), function(i) { f <- fits[[i]]; if (is.null(f)) return(list(k = ks[i], sel = "not estimated"))
    z <- post_of(f, ks[i])
    list(k = ks[i], ll = ll[i], np = np[i], aic = -2 * ll[i] + 2 * np[i], bic = bic[i], sabic = lm_sabic(ll[i], np[i], N), entropy = lm_entropy(z),
         smallest = 100 * min(colMeans(z)), lmr = lmr[i], sel = if (ks[i] == pick$k) "interpreted" else "") })
  k <- pick$k; f <- fits[[match(k, ks)]]
  if (is.null(f)) api_error(422, "lta_failed", "depmixS4 could not estimate the latent transition model.")
  par <- depmixS4::getpars(f)
  init <- par[seq_len(k)]; A <- matrix(par[k + seq_len(k * k)], k, k, byrow = TRUE)
  if (any(abs(rowSums(A) - 1) > 1e-6)) { A <- exp(A); A <- A / rowSums(A) }
  rp <- matrix(par[k + k * k + seq_len(2 * p * k)], nrow = k, byrow = TRUE)   # per state: intercept, sd of each indicator
  mu <- rp[, seq(1, 2 * p, 2), drop = FALSE]; sdv <- rp[, seq(2, 2 * p, 2), drop = FALSE]
  z <- post_of(f, k); tix <- rep(seq_len(Tn), N)
  labs <- vapply(seq_along(waves[[1]]$vars), function(j) paste(unique(vapply(waves, function(w) w$vars[j], "")), collapse = " / "), "")
  tables <- list(
    list(id = "lta_fit", title = "Latent Transition Analysis: Model Fit", rows = fit_rows, columns = lm_fit_cols("States"),
         notes = list(paste0(sprintf("Hidden Markov model with %d waves (depmixS4), Gaussian indicators with state-specific means and SDs that are the same at every wave (measurement invariance), %d random starts; N = %d persons with complete data. BIC with N persons. ", Tn, nstart, N), lm_note(pick, "State")))),
    list(id = "lta_means", title = "State Means and Standard Deviations", rows = lapply(seq_len(p), function(j) c(list(ind = labs[j]),
           stats::setNames(as.list(mu[, j]), paste0("m", seq_len(k))), stats::setNames(as.list(sdv[, j]), paste0("sd", seq_len(k))))),
         columns = c(list(stats_col("ind", "Indicator", "text")), unlist(lapply(seq_len(k), function(s) list(stats_col(paste0("m", s), "Mean", superTitle = paste("State", s)), stats_col(paste0("sd", s), "SD", superTitle = paste("State", s)))), recursive = FALSE)),
         notes = list("Indicator rows name the variables of each wave.")),
    list(id = "lta_prev", title = "State Prevalence at Each Wave", rows = lapply(seq_len(Tn), function(t) c(list(wave = waves[[t]]$label), stats::setNames(as.list(100 * colMeans(z[tix == t, , drop = FALSE])), paste0("s", seq_len(k))))),
         columns = c(list(stats_col("wave", "Wave", "text")), lapply(seq_len(k), function(s) stats_col(paste0("s", s), sprintf("State %d (%%)", s)))), notes = list("Mean posterior probabilities of the states at each wave.")),
    list(id = "lta_trans", title = "Transition Probabilities", rows = lapply(seq_len(k), function(s) c(list(from = sprintf("State %d", s), init = init[s]), stats::setNames(as.list(A[s, ]), paste0("t", seq_len(k))))),
         columns = c(list(stats_col("from", "From", "text"), stats_col("init", "Initial probability")), lapply(seq_len(k), function(s) stats_col(paste0("t", s), sprintf("State %d", s), superTitle = "To (next wave)"))),
         notes = list(paste0("Diagonal = probability of staying in the same state (stayers: ", paste(sprintf("State %d %.1f%%", seq_len(k), 100 * diag(A)), collapse = ", "), ").",
                             if (Tn > 2) " Transitions are assumed equal between every pair of consecutive waves (time-homogeneous)." else ""))))
  pl <- stats_png(function() {
    graphics::matplot(seq_len(p), t(mu), type = "b", lty = 1, pch = 16, col = LM_COLS[seq_len(k)], xaxt = "n", xlab = "Indicator", ylab = "State mean", main = "")
    graphics::axis(1, at = seq_len(p), labels = vapply(waves[[1]]$vars, function(v) v, ""), cex.axis = 0.8)
    graphics::legend("topright", sprintf("State %d", seq_len(k)), col = LM_COLS[seq_len(k)], lwd = 2, bty = "n", cex = 0.85)
  }, title = "State profiles")
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = N)
}

# ---- Growth mixture models and latent class growth analysis (lcmm) ------------------------------------------------
gmm_formulas <- function(o) {
  quad <- identical(opt(o, "shape", "linear"), "quadratic"); lcga <- identical(opt(o, "type", "gmm"), "lcga")
  fx <- if (quad) y ~ t + t2 else y ~ t
  mx <- if (quad) ~ t + t2 else ~ t
  rx <- if (lcga) ~ -1 else if (identical(opt(o, "random", "intercept"), "slope")) ~ 1 + t else ~ 1
  list(fixed = fx, mixture = mx, random = rx, quad = quad, lcga = lcga)
}

run_gmm <- function(d, o) {
  ms <- opt(o, "measures", character(0)); if (length(ms) < 3) api_error(422, "gmm_waves", "A growth mixture model needs at least three repeated measures (waves).")
  fm <- gmm_formulas(o)
  if (fm$quad && length(ms) < 4) api_error(422, "gmm_quad", "A quadratic growth model needs at least four waves.")
  tt <- lm_times(o, length(ms)); ks <- lm_k_range(o); nrep <- round(num_opt(o, "nrep", 10, 1, 50)); seed <- round(num_opt(o, "seed", 2026, 1))
  W <- as.data.frame(lapply(ms, function(v) as.numeric(d[[v]])))
  rows <- which(rowSums(!is.na(W)) > 0); N <- length(rows)
  if (N < 30) api_error(422, "gmm_n", "Too few cases for a growth mixture model.")
  dl <- data.frame(id = rep(seq_len(N), each = length(ms)), t = rep(tt, N), y = c(t(as.matrix(W[rows, , drop = FALSE]))))
  dl$t2 <- dl$t^2; dl <- dl[!is.na(dl$y), ]
  m1 <- tryCatch(lm_quiet(eval(bquote(lcmm::hlme(.(fm$fixed), random = .(fm$random), subject = "id", data = dl, verbose = FALSE)))), error = function(e) NULL)
  if (is.null(m1)) api_error(422, "gmm_failed", "lcmm could not estimate the one-class growth model.")
  hlme <- lcmm::hlme   # gridsearch() re-evaluates the model call by the bare function name in this frame
  fits <- lapply(ks, function(k) {
    if (k == 1) return(m1)
    set.seed(seed)
    tryCatch(lm_quiet(eval(bquote(lcmm::gridsearch(rep = .(nrep), maxiter = 15, minit = m1,
      hlme(.(fm$fixed), mixture = .(fm$mixture), random = .(fm$random), subject = "id", ng = .(k), data = dl, verbose = FALSE))))), error = function(e) NULL)
  })
  okf <- function(m) !is.null(m) && identical(as.integer(m$conv), 1L)
  ll <- vapply(fits, function(m) if (okf(m)) m$loglik else NA_real_, 0); np <- vapply(fits, function(m) if (okf(m)) length(m$best) else NA_real_, 0)
  bic <- vapply(fits, function(m) if (okf(m)) m$BIC else NA_real_, 0)
  pick <- lm_pick(o, ks, bic); lmr <- lm_lmr(N, ll, np, ks)
  zof <- function(m, k) if (k == 1) matrix(1, N, 1) else as.matrix(m$pprob[, paste0("prob", seq_len(k)), drop = FALSE])
  fit_rows <- lapply(seq_along(ks), function(i) { m <- fits[[i]]; if (!okf(m)) return(list(k = ks[i], sel = "not converged"))
    z <- zof(m, ks[i])
    list(k = ks[i], ll = ll[i], np = np[i], aic = m$AIC, bic = bic[i], sabic = lm_sabic(ll[i], np[i], N), entropy = lm_entropy(z),
         smallest = 100 * min(tabulate(if (ks[i] == 1) rep(1L, N) else m$pprob$class, ks[i])) / N, lmr = lmr[i], sel = if (ks[i] == pick$k) "interpreted" else "") })
  k <- pick$k; m <- fits[[match(k, ks)]]
  if (!okf(m)) api_error(422, "gmm_failed", "The chosen growth mixture model did not converge.")
  z <- zof(m, k); cls <- if (k == 1) rep(1L, N) else as.integer(m$pprob$class)
  est <- lcmm::estimates(m); V <- tryCatch(stats::vcov(m), error = function(e) NULL)
  se <- if (is.null(V)) rep(NA_real_, length(est)) else sqrt(pmax(diag(V), 0))[seq_along(est)]
  nm <- names(est); nmb <- if (k > 1) k - 1 else 0
  nm[seq_len(nmb)] <- sprintf("Class membership: intercept class %d (vs %d)", seq_len(nmb), k)
  nm <- sub("^t2 ", "Quadratic ", sub("^t ", "Slope ", sub("^intercept ", "Intercept ", nm)))
  nm <- sub("class([0-9]+)$", "(class \\1)", nm); nm <- sub("^varcov ", "Random-effect (co)variance ", nm); nm[nm == "stderr"] <- "Residual SD"
  isvar <- grepl("^Random-effect|^Residual", nm)
  prm <- lapply(seq_along(est), function(i) list(param = nm[i], est = unname(est[i]), se = if (isvar[i]) NA_real_ else unname(se[i]),
                                                 z = if (isvar[i]) NA_real_ else unname(est[i] / se[i]), p = if (isvar[i]) NA_real_ else 2 * stats::pnorm(-abs(unname(est[i] / se[i])))))
  sizes <- lapply(seq_len(k), function(s) list(class = s, n = sum(cls == s), pct = 100 * mean(cls == s), post = if (any(cls == s)) mean(z[cls == s, s]) else NA_real_))
  grid <- data.frame(t = seq(min(tt), max(tt), length.out = 40)); grid$t2 <- grid$t^2
  pred <- tryCatch(as.matrix(lcmm::predictY(m, newdata = grid, var.time = "t")$pred), error = function(e) NULL)
  pred <- if (is.null(pred)) NULL else pred[, grepl("^Ypred", colnames(pred)), drop = FALSE]
  what <- if (fm$lcga) "Latent class growth analysis (no within-class variation)" else sprintf("Growth mixture model with a random %s", if (identical(opt(o, "random", "intercept"), "slope")) "intercept and slope" else "intercept")
  tables <- list(
    list(id = "gmm_fit", title = paste0(if (fm$lcga) "Latent Class Growth Analysis" else "Growth Mixture Model", ": Model Fit"), rows = fit_rows, columns = lm_fit_cols("Classes"),
         notes = list(paste0(sprintf("%s, %s growth, lcmm::hlme with %d random starts (gridsearch); N = %d; time scores %s. ", what, if (fm$quad) "quadratic" else "linear", nrep, N, paste(format(tt), collapse = ", ")), lm_note(pick, "Class")))),
    list(id = "gmm_sizes", title = "Class Sizes and Classification Quality", rows = sizes,
         columns = list(stats_col("class", "Class", "integer"), stats_col("n", "N", "integer"), stats_col("pct", "%"), stats_col("post", "Avg. posterior probability")),
         notes = list(sprintf("Most likely class of each case; entropy = %s.", if (k > 1) sprintf("%.3f", lm_entropy(z)) else "–"))),
    list(id = "gmm_par", title = "Growth Parameters by Class", rows = prm,
         columns = list(stats_col("param", "Parameter", "text"), stats_col("est", "Estimate"), stats_col("se", "SE"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE)),
         notes = list("Intercept: expected level at time 0; Slope: change per unit of time. Class membership intercepts are multinomial logits.")))
  tables <- c(tables, ts_tables(z, cls, lm_sub(d, rows, opt(o, "distal", character(0))), lm_sub(d, rows, opt(o, "covs", character(0))), unit = "Class", prefix = "gmm"))
  pl <- stats_png(function() {
    set.seed(1); sub <- sample(N, min(80, N)); Wm <- as.matrix(W[rows, , drop = FALSE])
    graphics::matplot(tt, t(Wm[sub, , drop = FALSE]), type = "l", lty = 1, col = grDevices::adjustcolor(LM_COLS[cls[sub]], 0.25), xlab = "Time", ylab = "Score", main = "", xaxt = "n")
    graphics::axis(1, at = tt, labels = ms, cex.axis = 0.8)
    if (!is.null(pred)) for (s in seq_len(ncol(pred))) graphics::lines(grid$t, pred[, s], col = LM_COLS[s], lwd = 3)
    graphics::legend("topleft", sprintf("Class %d (%.0f%%)", seq_len(k), 100 * tabulate(cls, k) / N), col = LM_COLS[seq_len(k)], lwd = 3, bty = "n", cex = 0.85)
  }, title = "Class trajectories (thick) and a sample of individuals")
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = N)
}

# ---- Latent growth curves (lavaan) ---------------------------------------------------------------------------------
#' The lavaan model of a growth curve, with the variables renamed w1..wT (first process), v1..vT (second process),
#' c1.. (time-invariant covariates) and x1..xT (time-varying covariates). Returns the syntax and the factor labels.
lgc_syntax <- function(o) {
  ms <- opt(o, "measures", character(0)); ms2 <- opt(o, "measures2", character(0)); covs <- opt(o, "covs", character(0)); tvc <- opt(o, "tvc", character(0))
  Tn <- length(ms); shape <- opt(o, "shape", "linear")
  tt <- if (shape == "basis") seq_len(Tn) - 1 else lm_times(o, Tn)
  knot <- round(num_opt(o, "knot", ceiling(Tn / 2), 2, max(2, Tn - 1)))
  num <- function(x) format(x, trim = TRUE, digits = 10)
  proc <- function(vars, sfx, lab) {
    f <- function(n) paste0(n, sfx)
    lines <- paste(f("i"), "=~", paste(sprintf("1*%s", vars), collapse = " + "))
    labs <- stats::setNames(paste0("Intercept", lab), f("i"))
    if (shape %in% c("linear", "quadratic")) {
      lines <- c(lines, paste(f("s"), "=~", paste(sprintf("%s*%s", num(tt - tt[1]), vars), collapse = " + "))); labs[f("s")] <- paste0("Slope", lab)
      if (shape == "quadratic") { lines <- c(lines, paste(f("q"), "=~", paste(sprintf("%s*%s", num((tt - tt[1])^2), vars), collapse = " + "))); labs[f("q")] <- paste0("Quadratic", lab) }
    } else if (shape == "basis") {
      ld <- c("0", rep("NA", Tn - 2), "1")
      lines <- c(lines, paste(f("s"), "=~", paste(sprintf("%s*%s", ld, vars), collapse = " + "))); labs[f("s")] <- paste0("Slope (total change)", lab)
    } else {
      t0 <- tt - tt[1]; a <- ifelse(seq_len(Tn) <= knot, t0, t0[knot]); b <- ifelse(seq_len(Tn) <= knot, 0, tt - tt[knot])
      lines <- c(lines, paste(f("sa"), "=~", paste(sprintf("%s*%s", num(a), vars), collapse = " + ")), paste(f("sb"), "=~", paste(sprintf("%s*%s", num(b), vars), collapse = " + ")))
      labs[f("sa")] <- paste0("Slope to the knot", lab); labs[f("sb")] <- paste0("Slope after the knot", lab)
    }
    list(lines = lines, labs = labs)
  }
  p1 <- proc(paste0("w", seq_len(Tn)), "", if (length(ms2)) " (process 1)" else "")
  p2 <- if (length(ms2)) proc(paste0("v", seq_len(Tn)), "_2", " (process 2)") else list(lines = character(0), labs = character(0))
  labs <- c(p1$labs, p2$labs)
  lines <- c(p1$lines, p2$lines)
  if (length(covs)) lines <- c(lines, paste(names(labs), "~", paste(paste0("c", seq_along(covs)), collapse = " + ")))
  if (length(tvc)) lines <- c(lines, sprintf("w%d ~ %sx%d", seq_len(Tn), if (isTRUE(o$tvcEqual)) "btvc*" else "", seq_len(Tn)))
  list(model = paste(lines, collapse = "\n"), labs = labs, times = tt, knot = knot, shape = shape)
}

run_lgc <- function(d, o) {
  ms <- opt(o, "measures", character(0)); if (length(ms) < 3) api_error(422, "lgc_waves", "A latent growth curve needs at least three repeated measures (waves).")
  ms2 <- opt(o, "measures2", character(0)); covs <- opt(o, "covs", character(0)); tvc <- opt(o, "tvc", character(0)); grp <- opt(o, "group", character(0))
  shape <- opt(o, "shape", "linear"); Tn <- length(ms)
  if (shape == "quadratic" && Tn < 4) api_error(422, "lgc_quad", "A quadratic growth model needs at least four waves.")
  if (shape == "piecewise" && Tn < 4) api_error(422, "lgc_piece", "A piecewise growth model needs at least four waves.")
  if (length(ms2) && length(ms2) != Tn) api_error(422, "lgc_process2", sprintf("The second process needs the same number of waves as the first (%d).", Tn))
  if (length(tvc) && length(tvc) != Tn) api_error(422, "lgc_tvc", sprintf("Add one time-varying covariate per wave (%d), in the same order as the waves.", Tn))
  sx <- lgc_syntax(o)
  dd <- as.data.frame(lapply(d[, c(ms, ms2, covs, tvc), drop = FALSE], as.numeric))
  names(dd) <- c(paste0("w", seq_len(Tn)), if (length(ms2)) paste0("v", seq_len(Tn)), if (length(covs)) paste0("c", seq_along(covs)), if (length(tvc)) paste0("x", seq_len(Tn)))
  if (length(grp)) dd$grp <- droplevels(factor(d[[grp[1]]]))
  if (length(grp)) dd <- dd[!is.na(dd$grp), , drop = FALSE]
  est <- if (identical(opt(o, "estimator"), "MLR")) "MLR" else "ML"
  fit <- lavaan::growth(sx$model, data = dd, estimator = est, missing = "fiml", group = if (length(grp)) "grp" else NULL)
  if (!lavaan::lavInspect(fit, "converged")) api_error(422, "lgc_converge", "The growth model did not converge.", "Try a simpler shape, fewer covariates or rescaled variables.")
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"))
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  glab <- if (length(grp)) lavaan::lavInspect(fit, "group.label") else character(0)
  if (!"group" %in% names(pe)) pe$group <- 1L
  nms <- function(x, p) if (length(x)) stats::setNames(x, paste0(p, seq_along(x))) else character(0)
  facs <- names(sx$labs); cl <- nms(covs, "c"); ol <- c(nms(ms, "w"), nms(ms2, "v"))
  xl <- nms(tvc, "x")
  nm <- function(x) if (x %in% facs) sx$labs[[x]] else if (x %in% names(cl)) cl[[x]] else if (x %in% names(ol)) ol[[x]] else if (x %in% names(xl)) xl[[x]] else x
  gname <- function(g) if (length(glab)) glab[g] else ""
  mk <- function(df, f) lapply(seq_len(nrow(df)), function(i) { r <- df[i, ]
    list(group = gname(r$group), param = f(r), est = r$est, se = r$se, z = r$z, p = r$pvalue, std = r$std.all) })
  cols <- c(if (length(glab)) list(stats_col("group", "Group", "text", combineBelow = TRUE)), list(stats_col("param", "Parameter", "text"), stats_col("est", "Estimate"), stats_col("se", "SE"), stats_col("z", "z"), stats_col("p", "p", pvalue = TRUE), stats_col("std", "Std. estimate")))
  shape_txt <- switch(shape, linear = "linear", quadratic = "quadratic", basis = "latent basis (first loading 0, last 1, the others estimated)",
                      piecewise = sprintf("piecewise linear with the knot at wave %d (%s)", sx$knot, ms[sx$knot]))
  fit_rows <- list(list(item = "χ² (df), p", value = sprintf("%.2f (%d), p %s", fm[["chisq"]], as.integer(fm[["df"]]), pfmt(fm[["pvalue"]]))), list(item = "CFI / TLI", value = sprintf("%.3f / %.3f", fm[["cfi"]], fm[["tli"]])),
                   list(item = "RMSEA [90% CI]", value = sprintf("%.3f [%.3f, %.3f]", fm[["rmsea"]], fm[["rmsea.ci.lower"]], fm[["rmsea.ci.upper"]])), list(item = "SRMR", value = sprintf("%.3f", fm[["srmr"]])),
                   list(item = "AIC / BIC", value = sprintf("%.1f / %.1f", fm[["aic"]], fm[["bic"]])), list(item = "N", value = paste(lavaan::lavInspect(fit, "nobs"), collapse = " / ")))
  if (length(glab) > 1) {
    eq <- tryCatch(lavaan::growth(sx$model, data = dd, estimator = est, missing = "fiml", group = "grp", group.equal = "means"), error = function(e) NULL)
    lrt <- if (is.null(eq)) NULL else tryCatch(lavaan::lavTestLRT(fit, eq), error = function(e) NULL)
    if (!is.null(lrt)) fit_rows[[length(fit_rows) + 1]] <- list(item = "Equal growth means across groups: Δχ² (Δdf), p",
                                                               value = sprintf("%.2f (%d), p %s", lrt[2, "Chisq diff"], as.integer(lrt[2, "Df diff"]), pfmt(lrt[2, "Pr(>Chisq)"])))
  }
  gm <- pe[pe$op == "~1" & pe$lhs %in% facs, ]; gv <- pe[pe$op == "~~" & pe$lhs %in% facs & pe$rhs %in% facs, ]
  gr <- pe[pe$op == "~" & pe$lhs %in% facs, ]; tv <- pe[pe$op == "~" & pe$lhs %in% names(ol), ]; ld <- pe[pe$op == "=~" & !is.na(pe$se) & pe$se > 0, ]   # estimated (not fixed) loadings
  tables <- list(
    list(id = "lgc_fit", title = "Latent Growth Curve: Model Fit", columns = list(stats_col("item", "", "text"), stats_col("value", "", "text")), rows = fit_rows,
         notes = list(paste0("lavaan::growth, ", if (est == "MLR") "robust ML" else "ML", " with full-information ML for missing data; ", shape_txt, " growth",
                             if (shape != "basis") paste0("; time scores ", paste(format(sx$times), collapse = ", ")) else "", " (waves: ", paste(ms, collapse = ", "), ")",
                             if (length(ms2)) paste0("; second process: ", paste(ms2, collapse = ", ")) else "", if (length(glab)) paste0("; groups by ", grp[1]) else "", "."))),
    list(id = "lgc_means", title = "Growth Factor Means (Intercepts)", columns = cols, rows = mk(gm, function(r) nm(r$lhs)),
         notes = list(paste0("Intercept: expected level at the first wave; slopes: expected change per unit of time.", if (length(covs)) " With predictors, these are intercepts (expected values when the predictors are 0)." else ""))),
    list(id = "lgc_var", title = "Growth Factor Variances and Covariances", columns = cols, rows = mk(gv, function(r) if (r$lhs == r$rhs) paste("Var", nm(r$lhs)) else paste("Cov", nm(r$lhs), "with", nm(r$rhs))),
         notes = list(paste0("Significant variances mean individuals differ in their starting level or rate of change. The standardised covariances are correlations",
                             if (length(ms2)) ", including those between the two processes" else "", "."))))
  if (nrow(ld)) tables[[length(tables) + 1]] <- list(id = "lgc_basis", title = "Estimated Time Scores (Latent Basis)", columns = cols, rows = mk(ld, function(r) paste(nm(r$lhs), "→", nm(r$rhs))),
                                                    notes = list("Proportion of the total change (first to last wave) reached at each wave."))
  if (nrow(gr)) tables[[length(tables) + 1]] <- list(id = "lgc_pred", title = "Predictors of Growth", columns = cols, rows = mk(gr, function(r) paste(nm(r$lhs), "←", nm(r$rhs))), notes = list())
  if (nrow(tv)) tables[[length(tables) + 1]] <- list(id = "lgc_tvc", title = "Time-Varying Covariates", columns = cols, rows = mk(tv, function(r) paste(nm(r$lhs), "←", nm(r$rhs))),
                                                    notes = list(if (isTRUE(o$tvcEqual)) "Effect constrained equal at every wave." else "Effect of the covariate on the score at the same wave, net of the growth factors."))
  imp <- lavaan::lavInspect(fit, "implied"); if (!length(glab)) imp <- list(imp)
  xs <- sx$times
  pl <- stats_png(function() {
    np <- if (length(ms2)) 2 else 1; graphics::par(mfrow = c(1, np))
    for (pp in seq_len(np)) {
      vs <- if (pp == 1) paste0("w", seq_len(Tn)) else paste0("v", seq_len(Tn)); lab <- if (pp == 1) ms else ms2
      set.seed(1); sub <- dd[sample(nrow(dd), min(60, nrow(dd))), vs, drop = FALSE]
      graphics::matplot(xs, t(sub), type = "l", lty = 1, col = grDevices::adjustcolor("#94a3b8", 0.45), xlab = "Time", ylab = "Score", xaxt = "n", main = if (np > 1) sprintf("Process %d", pp) else "")
      graphics::axis(1, at = xs, labels = lab, cex.axis = 0.8)
      for (g in seq_along(imp)) {
        mu <- imp[[g]]$mean[vs]; obs <- colMeans(if (length(glab)) dd[dd$grp == glab[g], vs, drop = FALSE] else dd[, vs, drop = FALSE], na.rm = TRUE)
        graphics::points(xs, obs, col = LM_COLS[g], pch = 16); graphics::lines(xs, mu, col = LM_COLS[g], lwd = 3)
      }
      graphics::legend("topleft", c(if (length(glab)) glab else "Model-implied means", "Observed means (points)"), col = c(LM_COLS[seq_along(imp)], "#475569"), lwd = c(rep(3, length(imp)), NA), pch = c(rep(NA, length(imp)), 16), bty = "n", cex = 0.8)
    }
  }, title = "Growth trajectories", w = if (length(ms2)) 10 else 7)
  list(tables = tables, plots = Filter(Negate(is.null), list(pl)), n = sum(lavaan::lavInspect(fit, "nobs")))
}

#' R code of the latent class and growth procedures (Statistics R code tab).
latent_more_code <- function(s, q) {
  o <- s$options; vq <- function(x) paste(vapply(unlist(x), q, ""), collapse = ", ")
  switch(s$proc,
    lca = c("library(poLCA)", sprintf("y <- data[, c(%s)]", vq(o$items)), "y[] <- lapply(y, function(x) as.integer(factor(x)))   # categories coded 1, 2, ...",
            sprintf("f <- cbind(%s) ~ 1", paste(sprintf("`%s`", unlist(o$items)), collapse = ", ")),
            sprintf("fits <- lapply(%d:%d, function(k) { set.seed(%d); poLCA(f, y, nclass = k, nrep = %d, na.rm = FALSE) })", o$kMin %||% 1, o$kMax %||% 4, o$seed %||% 2026, o$nrep %||% 10),
            "sapply(fits, function(m) c(LL = m$llik, npar = m$npar, AIC = m$aic, BIC = m$bic))",
            "# LMR test of k vs. k - 1 classes: tidyLPA::calc_lrt(n, ll0, par0, k - 1, ll1, par1, k)"),
    lta = c("library(depmixS4)", "# long format: one row per person and wave (id, time), the same indicators x1, x2, ... at every wave",
            sprintf("# waves: %s", paste(vapply(o$waves, function(w) sprintf("%s = %s", w$label, paste(unlist(w$vars), collapse = ", ")), ""), collapse = "; ")),
            "mod <- depmix(list(x1 ~ 1, x2 ~ 1), data = long, nstates = k, family = list(gaussian(), gaussian()), ntimes = rep(n_waves, n_persons))",
            sprintf("set.seed(%d); fm <- fit(mod, emcontrol = em.control(rand = TRUE))   # best of %d random starts in PathLab", o$seed %||% 2026, o$nrep %||% 5), "summary(fm); posterior(fm, type = \"smoothing\")"),
    gmm = { fm <- gmm_formulas(o)
      c("library(lcmm)", sprintf("w <- data[, c(%s)]", vq(o$measures)), sprintf("tt <- c(%s)   # time scores", paste(lm_times(o, length(o$measures)), collapse = ", ")),
        "long <- data.frame(id = rep(seq_len(nrow(w)), each = ncol(w)), t = rep(tt, nrow(w)), y = c(t(as.matrix(w)))); long$t2 <- long$t^2; long <- na.omit(long)",
        sprintf("m1 <- hlme(%s, random = %s, subject = \"id\", data = long)", deparse(fm$fixed), deparse(fm$random)),
        sprintf("set.seed(%d); m2 <- gridsearch(rep = %d, maxiter = 15, minit = m1, hlme(%s, mixture = %s, random = %s, subject = \"id\", ng = 2, data = long))",
                o$seed %||% 2026, o$nrep %||% 10, deparse(fm$fixed), deparse(fm$mixture), deparse(fm$random)),
        "summarytable(m1, m2, which = c(\"G\", \"loglik\", \"npm\", \"AIC\", \"BIC\", \"SABIC\", \"entropy\", \"%class\"))", "summary(m2); postprob(m2)") },
    lgc = { sx <- lgc_syntax(o); vars <- c(o$measures, o$measures2, o$covs, o$tvc)
      nn <- c(paste0("w", seq_along(o$measures)), if (length(o$measures2)) paste0("v", seq_along(o$measures2)), if (length(o$covs)) paste0("c", seq_along(o$covs)), if (length(o$tvc)) paste0("x", seq_along(o$tvc)))
      c("library(lavaan)", sprintf("d <- data[, c(%s)]", vq(vars)), sprintf("names(d) <- c(%s)", vq(nn)),
        if (length(o$group)) sprintf("d$grp <- factor(data[[%s]])", q(o$group[1])),
        "model <- '", sx$model, "'",
        sprintf("fit <- growth(model, data = d, estimator = %s, missing = \"fiml\"%s)", q(if (identical(o$estimator, "MLR")) "MLR" else "ML"), if (length(o$group)) ", group = \"grp\"" else ""),
        "summary(fit, fit.measures = TRUE, standardized = TRUE)",
        if (length(o$group)) "lavTestLRT(fit, update(fit, group.equal = \"means\"))   # equal growth means across groups") },
    character(0))
}
