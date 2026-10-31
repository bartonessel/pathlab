# Power analysis as in G*Power (Faul, Erdfelder, Lang & Buchner, 2007; 2009): test families (t, F, χ², z), the
# statistical tests most used in the social sciences, and the five types of power analysis (a priori, post hoc,
# sensitivity, criterion, compromise). Exact central and noncentral t, F and χ² distributions (base R), with the
# same noncentrality parameters, degrees of freedom and output parameters as G*Power. Returns the input / output table,
# the sample size by power table, the central and noncentral distributions (G*Power's distribution plot) and the X-Y
# plot data (power by sample size, sample size by power, or power by effect size) for the interactive charts.

GP_TESTS <- list(
  t_ind = list(family = "t", label = "Means: Difference between two independent means (two groups)", es = "d", conv = c(0.2, 0.5, 0.8)),
  t_paired = list(family = "t", label = "Means: Difference between two dependent means (matched pairs)", es = "dz", conv = c(0.2, 0.5, 0.8)),
  t_one = list(family = "t", label = "Means: Difference from constant (one sample case)", es = "d", conv = c(0.2, 0.5, 0.8)),
  t_reg1 = list(family = "t", label = "Linear multiple regression: Fixed model, single regression coefficient", es = "f²", conv = c(0.02, 0.15, 0.35)),
  f_anova1 = list(family = "F", label = "ANOVA: Fixed effects, omnibus, one-way", es = "f", conv = c(0.1, 0.25, 0.4)),
  f_anova_special = list(family = "F", label = "ANOVA: Fixed effects, special, main effects and interactions", es = "f", conv = c(0.1, 0.25, 0.4)),
  f_rm_between = list(family = "F", label = "ANOVA: Repeated measures, between factors", es = "f", conv = c(0.1, 0.25, 0.4)),
  f_rm_within = list(family = "F", label = "ANOVA: Repeated measures, within factors", es = "f", conv = c(0.1, 0.25, 0.4)),
  f_rm_inter = list(family = "F", label = "ANOVA: Repeated measures, within-between interaction", es = "f", conv = c(0.1, 0.25, 0.4)),
  f_reg_r2 = list(family = "F", label = "Linear multiple regression: Fixed model, R² deviation from zero", es = "f²", conv = c(0.02, 0.15, 0.35)),
  f_reg_inc = list(family = "F", label = "Linear multiple regression: Fixed model, R² increase", es = "f²", conv = c(0.02, 0.15, 0.35)),
  x_gof = list(family = "chisq", label = "Goodness-of-fit tests: Contingency tables", es = "w", conv = c(0.1, 0.3, 0.5)),
  z_corr = list(family = "z", label = "Correlation: Difference from constant (Fisher z, one sample)", es = "ρ", conv = c(0.1, 0.3, 0.5)),
  z_prop2 = list(family = "z", label = "Proportions: Difference between two independent proportions (arcsine h)", es = "h", conv = c(0.2, 0.5, 0.8)))
GP_TYPES <- c(apriori = "A priori: Compute required sample size", posthoc = "Post hoc: Compute achieved power", sensitivity = "Sensitivity: Compute required effect size",
              criterion = "Criterion: Compute required α", compromise = "Compromise: Compute implied α and power")
# The tests of the earlier (pwr) version map onto G*Power's.
GP_OLD <- c(t2 = "t_ind", t1 = "t_one", paired = "t_paired", anova = "f_anova1", corr = "z_corr", regression = "f_reg_r2", chisq = "x_gof", prop2 = "z_prop2")

gp_par <- function(o) {
  test <- opt(o, "gtest", NULL) %||% unname(GP_OLD[opt(o, "test", "t2")]) %||% "t_ind"
  if (!test %in% names(GP_TESTS)) test <- "t_ind"
  type <- opt(o, "gtype", NULL) %||% switch(opt(o, "solve", "n"), power = "posthoc", es = "sensitivity", "apriori")
  if (!type %in% names(GP_TYPES)) type <- "apriori"
  list(test = test, type = type, fam = GP_TESTS[[test]]$family,
       tails = if (identical(opt(o, "alternative"), "greater") || identical(as.character(opt(o, "tails", "2")), "1")) 1L else 2L,
       es = num_opt(o, "es", 0.5), alpha = num_opt(o, "alpha", 0.05, 1e-10, 0.999), power = num_opt(o, "power", 0.8, 0.01, 0.99999),
       n = round(num_opt(o, "n", 128, 2, 1e7)), ratio = num_opt(o, "ratio", 1, 0.01, 100), k = round(num_opt(o, "k", 3, 1, 1000)),
       numdf = num_opt(o, "numdf", 1, 1, 1000), m = round(num_opt(o, "m", 3, 2, 100)), rho = num_opt(o, "rho", 0.5, -0.99, 0.99), eps = num_opt(o, "eps", 1, 0.01, 1),
       p = round(num_opt(o, "p", num_opt(o, "u", 3, 1), 1, 1000)), q = round(num_opt(o, "q", 1, 1, 1000)), df = round(num_opt(o, "df", 1, 1, 10000)),
       rho0 = num_opt(o, "rho0", 0, -0.99, 0.99), qratio = num_opt(o, "qratio", 1, 0.001, 1000), xy = opt(o, "xy", "n_power"), esList = opt(o, "esList", ""))
}

#' Design of a test at total sample size N: distribution, noncentrality, df and group sizes (G*Power's formulas).
gp_design <- function(P, es, N) {
  t <- P$test; n1 <- NA_real_; n2 <- NA_real_
  if (t %in% c("t_ind", "z_prop2")) { n1 <- N / (1 + P$ratio); n2 <- N - n1 }
  d <- switch(t,
    t_ind = list(dist = "t", ncp = es * sqrt(n1 * n2 / N), df1 = N - 2),
    t_paired = , t_one = list(dist = "t", ncp = es * sqrt(N), df1 = N - 1),
    t_reg1 = list(dist = "t", ncp = sqrt(max(0, es) * N), df1 = N - P$p - 1),
    f_anova1 = list(dist = "F", ncp = es^2 * N, df1 = P$k - 1, df2 = N - P$k),
    f_anova_special = list(dist = "F", ncp = es^2 * N, df1 = P$numdf, df2 = N - P$k),
    f_rm_between = list(dist = "F", ncp = es^2 * N * P$m / (1 + (P$m - 1) * P$rho), df1 = P$k - 1, df2 = N - P$k),
    f_rm_within = list(dist = "F", ncp = es^2 * N * P$m * P$eps / (1 - P$rho), df1 = (P$m - 1) * P$eps, df2 = (N - P$k) * (P$m - 1) * P$eps),
    f_rm_inter = list(dist = "F", ncp = es^2 * N * P$m * P$eps / (1 - P$rho), df1 = (P$k - 1) * (P$m - 1) * P$eps, df2 = (N - P$k) * (P$m - 1) * P$eps),
    f_reg_r2 = list(dist = "F", ncp = es * N, df1 = P$p, df2 = N - P$p - 1),
    f_reg_inc = list(dist = "F", ncp = es * N, df1 = P$q, df2 = N - P$p - 1),
    x_gof = list(dist = "chisq", ncp = es^2 * N, df1 = P$df),
    z_corr = list(dist = "z", ncp = (atanh(min(0.99999, max(-0.99999, es))) - atanh(P$rho0)) * sqrt(max(0, N - 3))),
    z_prop2 = list(dist = "z", ncp = es * sqrt(n1 * n2 / N)))
  d$df2 <- d$df2 %||% NA_real_
  d$N <- N; d$n1 <- n1; d$n2 <- n2
  d$valid <- is.finite(d$ncp) && (d$dist %in% c("z") || (is.finite(d$df1) && d$df1 > 0)) && (d$dist != "F" || (is.finite(d$df2) && d$df2 > 0)) &&
    (!t %in% c("t_ind", "z_prop2") || (n1 >= 1 && n2 >= 1)) && (t != "z_corr" || N > 3)
  d
}

#' Critical value(s) and power of a design at significance level alpha.
gp_power <- function(d, alpha, tails) {
  if (!isTRUE(d$valid)) return(list(power = NA_real_, crit = NA_real_))
  suppressWarnings(switch(d$dist,
    t = if (tails == 2) { c <- stats::qt(1 - alpha / 2, d$df1); list(power = stats::pt(c, d$df1, d$ncp, lower.tail = FALSE) + stats::pt(-c, d$df1, d$ncp), crit = c(-c, c)) }
        else { c <- stats::qt(1 - alpha, d$df1); list(power = stats::pt(c, d$df1, d$ncp, lower.tail = FALSE), crit = c) },
    z = if (tails == 2) { c <- stats::qnorm(1 - alpha / 2); list(power = stats::pnorm(c - d$ncp, lower.tail = FALSE) + stats::pnorm(-c - d$ncp), crit = c(-c, c)) }
        else { c <- stats::qnorm(1 - alpha); list(power = stats::pnorm(c - d$ncp, lower.tail = FALSE), crit = c) },
    F = { c <- stats::qf(1 - alpha, d$df1, d$df2); list(power = stats::pf(c, d$df1, d$df2, d$ncp, lower.tail = FALSE), crit = c) },
    chisq = { c <- stats::qchisq(1 - alpha, d$df1); list(power = stats::pchisq(c, d$df1, d$ncp, lower.tail = FALSE), crit = c) }))
}

# Sample sizes move in G*Power's units: n1 with n2 = ceiling(n1 × ratio) for two groups; equal n per group for ANOVA.
gp_unit_n <- function(P, i) {
  if (P$test %in% c("t_ind", "z_prop2")) i + ceiling(i * P$ratio - 1e-9)
  else if (P$test %in% c("f_anova1", "f_anova_special", "f_rm_between", "f_rm_within", "f_rm_inter")) i * P$k
  else i
}
gp_design_n <- function(P, es, i) {
  N <- gp_unit_n(P, i); d <- gp_design(P, es, N)
  if (P$test %in% c("t_ind", "z_prop2")) { d$n1 <- i; d$n2 <- N - i
    d$ncp <- if (P$test == "t_ind") es * sqrt(d$n1 * d$n2 / N) else es * sqrt(d$n1 * d$n2 / N) }
  d
}

#' Smallest sample size reaching the target power (exponential then binary search over G*Power's units).
gp_required <- function(P, es, alpha, target, max_unit = 5e6) {
  pw <- function(i) { r <- gp_power(gp_design_n(P, es, i), alpha, P$tails)$power; if (is.na(r)) -1 else r }
  lo <- 1; while (pw(lo) < 0 && lo < 1e4) lo <- lo + 1
  if (pw(lo) >= target) return(lo)
  hi <- lo * 2; while (pw(hi) < target) { lo <- hi; hi <- hi * 2; if (hi > max_unit) return(NA_integer_) }
  while (hi - lo > 1) { mid <- floor((lo + hi) / 2); if (pw(mid) >= target) hi <- mid else lo <- mid }
  hi
}

gp_es_bounds <- function(P) switch(P$test, z_corr = c(P$rho0 + 1e-6, 0.99999), z_prop2 = c(1e-6, pi), t_reg1 = , f_reg_r2 = , f_reg_inc = c(1e-8, 100), c(1e-6, 20))
gp_root <- function(f, lo, hi) { a <- f(lo); b <- f(hi); if (!is.finite(a) || !is.finite(b) || a * b > 0) return(NA_real_); stats::uniroot(f, c(lo, hi), tol = 1e-10)$root }

#' Solve one power analysis: returns the design, power, alpha, effect size and sample size.
gp_solve <- function(P) {
  es <- P$es; alpha <- P$alpha; i <- NULL
  if (P$type == "apriori") {
    i <- gp_required(P, es, alpha, P$power)
    if (is.na(i)) api_error(422, "stats_failed", "The required sample size exceeds 10 million; check the effect size.")
    d <- gp_design_n(P, es, i)
  } else {
    d <- gp_design(P, es, P$n)
    if (P$test %in% c("t_ind", "z_prop2")) { n1 <- round(P$n / (1 + P$ratio)); d <- gp_design(P, es, P$n); d$n1 <- n1; d$n2 <- P$n - n1; d$ncp <- es * sqrt(d$n1 * d$n2 / P$n) }
    if (!isTRUE(d$valid)) api_error(422, "stats_failed", "The total sample size is too small for this design (the error degrees of freedom must be positive).")
    if (P$type == "sensitivity") {
      b <- gp_es_bounds(P)
      es <- gp_root(function(e) { dd <- gp_design(P, e, P$n); if (P$test %in% c("t_ind", "z_prop2")) dd$ncp <- e * sqrt(d$n1 * d$n2 / P$n); gp_power(dd, alpha, P$tails)$power - P$power }, b[1], b[2])
      if (is.na(es)) api_error(422, "stats_failed", "No effect size reaches this power with this sample size.")
      d <- gp_design(P, es, P$n); if (P$test %in% c("t_ind", "z_prop2")) { d$n1 <- round(P$n / (1 + P$ratio)); d$n2 <- P$n - d$n1; d$ncp <- es * sqrt(d$n1 * d$n2 / P$n) }
    } else if (P$type == "criterion") {
      alpha <- gp_root(function(a) gp_power(d, a, P$tails)$power - P$power, 1e-12, 0.9999)
      if (is.na(alpha)) api_error(422, "stats_failed", "No α between 0 and 1 gives this power with this sample size and effect size.")
    } else if (P$type == "compromise") {
      alpha <- gp_root(function(a) (1 - gp_power(d, a, P$tails)$power) - P$qratio * a, 1e-12, 0.99999)
      if (is.na(alpha)) api_error(422, "stats_failed", "The compromise α could not be found.")
    }
  }
  pr <- gp_power(d, alpha, P$tails)
  list(d = d, es = es, alpha = alpha, power = pr$power, crit = pr$crit, unit = i)
}

gp_fmt <- function(x, k = 4) if (is.null(x) || length(x) == 0 || all(is.na(x))) "–" else paste(formatC(x, digits = k, format = "fg", flag = "#"), collapse = ", ")
gp_trim <- function(x, k = 7) sub("\\.$", "", sub("0+$", "", formatC(x, digits = k, format = "f")))

#' Densities of the statistic under H0 (central) and H1 (noncentral), as in G*Power's distribution plot.
gp_dist_plot <- function(P, S) {
  d <- S$d; df1 <- d$df1; df2 <- d$df2; ncp <- d$ncp
  q0 <- function(p) switch(d$dist, t = stats::qt(p, df1), z = stats::qnorm(p), F = stats::qf(p, df1, df2), chisq = stats::qchisq(p, df1))
  q1 <- function(p) suppressWarnings(switch(d$dist, t = stats::qt(p, df1, ncp), z = stats::qnorm(p, ncp), F = stats::qf(p, df1, df2, ncp), chisq = stats::qchisq(p, df1, ncp)))
  f0 <- function(x) switch(d$dist, t = stats::dt(x, df1), z = stats::dnorm(x), F = stats::df(x, df1, df2), chisq = stats::dchisq(x, df1))
  f1 <- function(x) suppressWarnings(switch(d$dist, t = stats::dt(x, df1, ncp), z = stats::dnorm(x, ncp), F = stats::df(x, df1, df2, ncp), chisq = stats::dchisq(x, df1, ncp)))
  crit <- S$crit
  if (d$dist %in% c("t", "z")) { lo <- min(q0(0.0005), q1(0.0005), crit); hi <- max(q0(0.9995), q1(0.9995), crit) }
  else { lo <- 0; hi <- max(q0(0.999), q1(0.999), crit * 1.1) }
  pad <- (hi - lo) * 0.04; if (d$dist %in% c("t", "z")) lo <- lo - pad; hi <- hi + pad
  x <- seq(lo, hi, length.out = 241); if (!d$dist %in% c("t", "z")) x[1] <- hi * 1e-4
  y0 <- f0(x); y1 <- f1(x)
  cap <- 1.15 * max(c(y0[x > hi * 0.04], y1[x > hi * 0.04]), na.rm = TRUE)   # F and χ² with 1 or 2 df rise to infinity at 0
  if (!d$dist %in% c("t", "z")) { y0 <- pmin(y0, cap); y1 <- pmin(y1, cap) }
  y0[!is.finite(y0)] <- 0; y1[!is.finite(y1)] <- 0
  list(type = "powerDist", stat = switch(d$dist, t = "t", z = "z", F = "F", chisq = "χ²"), x = signif(x, 6), h0 = signif(y0, 6), h1 = signif(y1, 6),
       crit = as.list(signif(crit, 6)), tails = P$tails, ncp = ncp, df1 = df1, df2 = if (is.na(df2)) NULL else df2, alpha = S$alpha, power = S$power,
       title = "Central and noncentral distributions")
}

#' X-Y plot data: power by total sample size, required sample size by power, or power by effect size.
gp_xy_plot <- function(P, S) {
  esl <- suppressWarnings(as.numeric(strsplit(gsub("[;\\s]+", ",", as.character(P$esList), perl = TRUE), ",")[[1]]))
  esl <- esl[is.finite(esl) & esl > 0]
  if (!length(esl)) esl <- sort(unique(signif(S$es * c(0.75, 1, 1.25), 3)))   # curves around the effect of the design
  if (P$test == "z_corr" && !length(esl[esl > P$rho0 & esl < 1])) esl <- S$es
  if (P$test == "z_corr") esl <- esl[esl > P$rho0 & esl < 1]
  esl <- utils::head(esl, 6); if (!length(esl)) esl <- S$es
  a <- S$alpha; lab <- GP_TESTS[[P$test]]$es
  if (P$xy == "power_es") {
    Ns <- unique(pmax(gp_unit_n(P, 2), round(S$d$N * c(0.5, 1, 2))))
    b <- gp_es_bounds(P); top <- max(S$es * 2.2, GP_TESTS[[P$test]]$conv[3] * 1.4); if (P$test == "z_corr") top <- min(0.99, top)
    xs <- seq(max(b[1], if (P$test == "z_corr") P$rho0 + 0.005 else 0.001), top, length.out = 80)
    series <- lapply(Ns, function(N) list(label = paste("N =", N), points = lapply(xs, function(e) { dd <- gp_design(P, e, N); list(x = signif(e, 5), y = signif(gp_power(dd, a, P$tails)$power, 5)) })))
    return(list(type = "powerCurve", mode = P$xy, xLabel = paste0("Effect size ", lab), yLabel = "Power (1 − β)", series = series, current = list(x = S$es, y = S$power), target = if (P$type == "posthoc") NULL else P$power,
                title = "Power as a function of the effect size"))
  }
  if (P$xy == "power_n") {
    need <- vapply(esl, function(e) { i <- gp_required(P, e, a, 0.95); if (is.na(i)) NA_real_ else gp_unit_n(P, i) }, numeric(1))
    top <- min(max(c(S$d$N * 1.6, stats::median(need, na.rm = TRUE) * 1.15), na.rm = TRUE), 1e6); low <- gp_unit_n(P, 1)
    units <- unique(round(seq(1, max(2, top / max(1, gp_unit_n(P, 1))), length.out = 90)))
    series <- lapply(esl, function(e) list(label = paste(lab, "=", gp_trim(e, 3)), points = Filter(Negate(is.null), lapply(units, function(i) { dd <- gp_design_n(P, e, i); pw <- gp_power(dd, a, P$tails)$power
      if (is.na(pw)) NULL else list(x = dd$N, y = signif(pw, 5)) }))))
    return(list(type = "powerCurve", mode = P$xy, xLabel = "Total sample size N", yLabel = "Power (1 − β)", series = series, current = list(x = S$d$N, y = S$power), target = if (P$type == "posthoc") NULL else P$power,
                title = "Power as a function of the total sample size", lowX = low))
  }
  pws <- seq(0.5, 0.99, by = 0.01)
  series <- lapply(esl, function(e) list(label = paste(lab, "=", gp_trim(e, 3)), points = Filter(Negate(is.null), lapply(pws, function(p) { i <- gp_required(P, e, a, p); if (is.na(i)) NULL else list(x = p, y = gp_unit_n(P, i)) }))))
  list(type = "powerCurve", mode = "n_power", xLabel = "Power (1 − β)", yLabel = "Total sample size N", series = series, current = list(x = S$power, y = S$d$N), target = NULL,
       title = "Total sample size as a function of power")
}

run_gpower <- function(d, o) {
  P <- gp_par(o); S <- gp_solve(P); D <- S$d; T <- GP_TESTS[[P$test]]
  two <- P$test %in% c("t_ind", "z_prop2"); grp <- P$test %in% c("f_anova1", "f_anova_special", "f_rm_between", "f_rm_within", "f_rm_inter")
  stat <- switch(D$dist, t = "t", z = "z", F = "F", chisq = "χ²")
  calc <- function(key) P$type %in% key
  inp <- list(
    if (D$dist %in% c("t", "z")) c("Tail(s)", if (P$tails == 2) "Two" else "One"),
    if (P$test == "z_corr") c("Correlation ρ H0", gp_trim(P$rho0, 4)),
    if (!calc("sensitivity")) c(paste0("Effect size ", T$es, if (P$test == "z_corr") " (ρ H1)" else ""), gp_trim(P$es, 6)),
    if (!calc(c("criterion", "compromise"))) c("α err prob", gp_trim(P$alpha, 6)),
    if (calc("compromise")) c("β/α ratio", gp_trim(P$qratio, 4)),
    if (!calc(c("posthoc", "compromise"))) c("Power (1 − β err prob)", gp_trim(P$power, 6)),
    if (!calc("apriori")) c("Total sample size", format(P$n)),
    if (two) c("Allocation ratio N2/N1", gp_trim(P$ratio, 4)),
    if (P$test == "t_reg1") c("Number of predictors", format(P$p)),
    if (P$test == "f_reg_r2") c("Number of predictors", format(P$p)),
    if (P$test == "f_reg_inc") c("Number of tested predictors", format(P$q)),
    if (P$test == "f_reg_inc") c("Total number of predictors", format(P$p)),
    if (grp) c(if (P$test == "f_anova1") "Number of groups" else "Number of groups", format(P$k)),
    if (P$test == "f_anova_special") c("Numerator df", gp_trim(P$numdf, 4)),
    if (P$test %in% c("f_rm_between", "f_rm_within", "f_rm_inter")) c("Number of measurements", format(P$m)),
    if (P$test %in% c("f_rm_between", "f_rm_within", "f_rm_inter")) c("Corr among rep measures", gp_trim(P$rho, 4)),
    if (P$test %in% c("f_rm_within", "f_rm_inter")) c("Nonsphericity correction ε", gp_trim(P$eps, 4)),
    if (P$test == "x_gof") c("Df", format(P$df)))
  outp <- list(
    if (calc("sensitivity")) c(paste0("Effect size ", T$es), gp_fmt(S$es, 6)),
    if (calc(c("criterion", "compromise"))) c("α err prob", gp_fmt(S$alpha, 6)),
    if (calc("compromise")) c("β err prob", gp_fmt(1 - S$power, 6)),
    c(paste("Noncentrality parameter", switch(D$dist, t = "δ", z = "δ", "λ")), gp_fmt(D$ncp, 7)),
    c(paste("Critical", stat), gp_fmt(S$crit[length(S$crit)], 7)),
    if (D$dist == "t" || D$dist == "chisq") c("Df", gp_fmt(D$df1, 6)),
    if (D$dist == "F") c("Numerator df", gp_fmt(D$df1, 6)),
    if (D$dist == "F") c("Denominator df", gp_fmt(D$df2, 6)),
    if (two) c("Sample size group 1", format(D$n1)),
    if (two) c("Sample size group 2", format(D$n2)),
    if (grp && calc("apriori")) c("Sample size per group", format(D$N / P$k)),
    c("Total sample size", format(D$N)),
    c(if (calc("apriori")) "Actual power" else "Power (1 − β err prob)", gp_fmt(S$power, 7)))
  rows <- c(lapply(Filter(Negate(is.null), inp), function(r) list(section = "Input parameters", item = r[1], value = r[2])),
            lapply(Filter(Negate(is.null), outp), function(r) list(section = "Output parameters", item = r[1], value = r[2])))
  conv <- T$conv
  sentence <- switch(P$type,
    apriori = sprintf("An a priori power analysis (%s; %s) indicated that a total sample of N = %d is required to detect an effect of %s = %s with power (1 − β) = %s at α = %s (%s), actual power = %.3f.",
                      T$label, if (D$dist %in% c("t", "z")) paste0(if (P$tails == 2) "two" else "one", "-tailed") else paste(stat, "test"), as.integer(D$N), T$es, gp_trim(P$es, 4), gp_trim(P$power, 4), gp_trim(P$alpha, 4), "G*Power-equivalent computation", S$power),
    posthoc = sprintf("A post hoc power analysis (%s) showed that with N = %d and α = %s, the power to detect %s = %s was %.3f.", T$label, as.integer(D$N), gp_trim(P$alpha, 4), T$es, gp_trim(P$es, 4), S$power),
    sensitivity = sprintf("A sensitivity power analysis (%s) showed that with N = %d, α = %s and power = %s, the minimum detectable effect is %s = %.3f.", T$label, as.integer(D$N), gp_trim(P$alpha, 4), gp_trim(P$power, 4), T$es, S$es),
    criterion = sprintf("A criterion power analysis (%s) showed that with N = %d and %s = %s, α = %.4f is needed for power = %s.", T$label, as.integer(D$N), T$es, gp_trim(P$es, 4), S$alpha, gp_trim(P$power, 4)),
    compromise = sprintf("A compromise power analysis (%s; β/α = %s) with N = %d and %s = %s gave α = %.4f and power = %.3f.", T$label, gp_trim(P$qratio, 4), as.integer(D$N), T$es, gp_trim(P$es, 4), S$alpha, S$power))
  curve <- lapply(c(0.7, 0.8, 0.85, 0.9, 0.95, 0.99), function(p) { i <- gp_required(P, S$es, S$alpha, p); list(power = p, n = if (is.na(i)) NULL else gp_unit_n(P, i)) })
  list(tables = list(
    list(id = "power", title = paste0(T$label, " — ", sub(":.*", "", GP_TYPES[[P$type]])), columns = list(stats_col("section", "", "text", combineBelow = TRUE), stats_col("item", "Parameter", "text"), stats_col("value", "Value", "text")),
         rows = rows, notes = list(paste0("Test family: ", switch(T$family, t = "t tests", F = "F tests", chisq = "χ² tests", z = "z tests"), ". ", GP_TYPES[[P$type]], ". ",
                                          sprintf("Effect size conventions (Cohen, 1988): %s = %s small, %s medium, %s large.", T$es, conv[1], conv[2], conv[3])), sentence)),
    list(id = "power_curve", title = "Required Total Sample Size by Power", columns = list(stats_col("power", "Power (1 − β)"), stats_col("n", "Total N", "integer")),
         rows = curve, notes = list(sprintf("For %s = %s at α = %s.", T$es, gp_trim(S$es, 4), gp_trim(S$alpha, 4))))),
    plots = list(gp_dist_plot(P, S), gp_xy_plot(P, S)), n = 0L)
}
STATS_MORE_RUN$power <- run_gpower

gpower_code <- function(o) {
  P <- gp_par(o); T <- GP_TESTS[[P$test]]
  c("# Power analysis as in G*Power (exact noncentral distributions, base R)",
    sprintf("# %s; %s", T$label, GP_TYPES[[P$type]]),
    sprintf("es <- %s; alpha <- %s; N <- %s", gp_trim(P$es, 6), gp_trim(P$alpha, 6), P$n),
    switch(GP_TESTS[[P$test]]$family,
      t = c(switch(P$test, t_ind = sprintf("n1 <- N / (1 + %s); n2 <- N - n1; ncp <- es * sqrt(n1 * n2 / N); df <- N - 2", gp_trim(P$ratio, 4)),
                   t_reg1 = sprintf("ncp <- sqrt(es * N); df <- N - %d - 1", P$p), "ncp <- es * sqrt(N); df <- N - 1"),
            if (P$tails == 2) "crit <- qt(1 - alpha / 2, df); power <- pt(crit, df, ncp, lower.tail = FALSE) + pt(-crit, df, ncp)" else "crit <- qt(1 - alpha, df); power <- pt(crit, df, ncp, lower.tail = FALSE)"),
      F = c(switch(P$test, f_anova1 = sprintf("k <- %d; ncp <- es^2 * N; df1 <- k - 1; df2 <- N - k", P$k), f_anova_special = sprintf("ncp <- es^2 * N; df1 <- %s; df2 <- N - %d", gp_trim(P$numdf), P$k),
                   f_rm_between = sprintf("m <- %d; rho <- %s; ncp <- es^2 * N * m / (1 + (m - 1) * rho); df1 <- %d - 1; df2 <- N - %d", P$m, gp_trim(P$rho), P$k, P$k),
                   f_rm_within = sprintf("m <- %d; rho <- %s; eps <- %s; ncp <- es^2 * N * m * eps / (1 - rho); df1 <- (m - 1) * eps; df2 <- (N - %d) * (m - 1) * eps", P$m, gp_trim(P$rho), gp_trim(P$eps), P$k),
                   f_rm_inter = sprintf("m <- %d; rho <- %s; eps <- %s; ncp <- es^2 * N * m * eps / (1 - rho); df1 <- (%d - 1) * (m - 1) * eps; df2 <- (N - %d) * (m - 1) * eps", P$m, gp_trim(P$rho), gp_trim(P$eps), P$k, P$k),
                   f_reg_r2 = sprintf("ncp <- es * N; df1 <- %d; df2 <- N - %d - 1", P$p, P$p), f_reg_inc = sprintf("ncp <- es * N; df1 <- %d; df2 <- N - %d - 1", P$q, P$p)),
            "crit <- qf(1 - alpha, df1, df2); power <- pf(crit, df1, df2, ncp, lower.tail = FALSE)"),
      chisq = c(sprintf("ncp <- es^2 * N; df <- %d", P$df), "crit <- qchisq(1 - alpha, df); power <- pchisq(crit, df, ncp, lower.tail = FALSE)"),
      z = c(if (P$test == "z_corr") sprintf("ncp <- (atanh(es) - atanh(%s)) * sqrt(N - 3)", gp_trim(P$rho0)) else sprintf("n1 <- N / (1 + %s); n2 <- N - n1; ncp <- es * sqrt(n1 * n2 / N)", gp_trim(P$ratio, 4)),
            if (P$tails == 2) "crit <- qnorm(1 - alpha / 2); power <- pnorm(crit - ncp, lower.tail = FALSE) + pnorm(-crit - ncp)" else "crit <- qnorm(1 - alpha); power <- pnorm(crit - ncp, lower.tail = FALSE)")),
    "power",
    if (P$type == "apriori") "# A priori: increase N until power >= the target (as G*Power does)" else NULL)
}
