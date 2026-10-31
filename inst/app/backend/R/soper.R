# Daniel Soper's a-priori sample size calculators (danielsoper.com/statcalc), as used in many SEM and regression
# studies:
#   * Structural equation models (calculator 89): Westland (2010). Two lower bounds, the larger is recommended:
#       n1 = ceil(50 (j/k)^2 - 450 (j/k) + 1100)          model structure (j observed, k latent variables)
#       n2 = ceil(largest root of Westland's quadratic)     detecting the effect delta at power 1 - beta, with the
#            Sidak-corrected alpha = 1 - (1 - alpha*)^(2 / (k (k - 1))) over the k (k - 1) / 2 possible links and the
#            variance of the Gini correlation of a bivariate normal pair evaluated at rho = delta (Xu et al., 2010).
#     Checked against published outputs of the calculator (delta .2, power .8, 9 latent / 37 observed -> 460;
#     7 latent / 53 observed -> 560).
#   * Multiple regression (calculator 1): the smallest n whose power for the overall F test reaches the target, from
#     the noncentral F distribution with df1 = k predictors, df2 = n - k - 1 and lambda = f^2 n (Cohen, 1988).

soper_sem_parts <- function(delta, power, k, j, alpha) {
  a <- 1 - (1 - alpha)^(2 / (k * (k - 1)))                     # Sidak correction over the possible links
  za <- stats::qnorm(1 - a / 2); zb <- stats::qnorm(1 - power)
  H <- (delta / (za - zb))^2
  rho <- delta
  A <- 1 - rho^2; B <- rho * asin(rho / 2); C <- rho * asin(rho); D <- A / sqrt(3 - A)
  E <- A * (pi / 6 - B + D) + H
  n2 <- ceiling((E + sqrt(E^2 + 4 * A * H * (pi / 6 + sqrt(A) + 2 * B - C - 2 * D))) / (2 * H))
  r <- j / k
  n1 <- ceiling(50 * r^2 - 450 * r + 1100)
  list(alphaSidak = a, za = za, zb = zb, H = H, n1 = n1, n2 = n2, n = max(n1, n2), ratio = r)
}

soper_mr_n <- function(f2, power, k, alpha, max_n = 1e6) {
  pw <- function(n) { d2 <- n - k - 1; 1 - stats::pf(stats::qf(1 - alpha, k, d2), k, d2, ncp = f2 * n) }
  lo <- k + 2
  if (pw(lo) >= power) return(list(n = lo, power = pw(lo)))
  hi <- lo; while (pw(hi) < power) { hi <- hi * 2; if (hi > max_n) return(list(n = NA_integer_, power = NA_real_)) }
  while (hi - lo > 1) { mid <- (lo + hi) %/% 2; if (pw(mid) >= power) hi <- mid else lo <- mid }   # power rises with n
  list(n = hi, power = pw(hi))
}

soper_row <- function(sec, item, value) list(section = sec, item = item, value = value)
soper_num <- function(x, k = 4) sub("\\.$", "", sub("0+$", "", formatC(x, digits = k, format = "f")))

run_soper_sem <- function(d, o) {
  delta <- num_opt(o, "effect", 0.3, 0.01, 0.99); power <- num_opt(o, "power", 0.8, 0.5, 0.999)
  k <- round(num_opt(o, "latent", 5, 2, 500)); j <- round(num_opt(o, "observed", 20, 1, 5000)); alpha <- num_opt(o, "alpha", 0.05, 1e-6, 0.5)
  if (j < k) api_error(400, "invalid_option", "The number of observed variables must be at least the number of latent variables.")
  p <- soper_sem_parts(delta, power, k, j, alpha)
  rows <- list(
    soper_row("Input parameters", "Anticipated effect size (δ)", soper_num(delta)),
    soper_row("Input parameters", "Desired statistical power (1 − β)", soper_num(power)),
    soper_row("Input parameters", "Number of latent variables (k)", format(k)),
    soper_row("Input parameters", "Number of observed variables (j)", format(j)),
    soper_row("Input parameters", "Probability level (α)", soper_num(alpha)),
    soper_row("Output parameters", "Minimum sample size to detect effect", format(p$n2)),
    soper_row("Output parameters", "Minimum sample size for model structure", format(p$n1)),
    soper_row("Output parameters", "Recommended minimum sample size", format(p$n)),
    soper_row("Details", "Šidák-corrected α over k(k − 1)/2 links", formatC(p$alphaSidak, digits = 4, format = "g")),
    soper_row("Details", "Ratio of observed to latent variables (j/k)", soper_num(p$ratio, 3)),
    soper_row("Details", "Binding constraint", if (p$n2 >= p$n1) "Detecting the effect" else "Model structure"))
  effects <- sort(unique(c(0.1, 0.2, 0.3, 0.4, 0.5, round(delta, 3))))
  grid <- lapply(effects, function(e) { q <- soper_sem_parts(e, power, k, j, alpha); list(effect = e, effectN = q$n2, structureN = q$n1, n = q$n) })
  pws <- c(0.8, 0.9, 0.95); pws <- sort(unique(c(pws, power)))
  xs <- seq(0.1, 0.6, by = 0.01)
  series <- lapply(pws, function(pw) list(label = paste("Power", soper_num(pw, 2)),
                                          points = lapply(xs, function(e) list(x = e, y = max(soper_sem_parts(e, pw, k, j, alpha)$n2, p$n1)))))
  sentence <- sprintf("An a-priori sample size calculation for structural equation models (Soper, 2024; Westland, 2010), with an anticipated effect size of %s, a desired statistical power of %s, %d latent and %d observed variables and a probability level of %s, indicated a minimum sample size of %d to detect the effect and %d for the model structure; the recommended minimum sample size is therefore %d.",
                      soper_num(delta), soper_num(power), as.integer(k), as.integer(j), soper_num(alpha), as.integer(p$n2), as.integer(p$n1), as.integer(p$n))
  list(tables = list(
    list(id = "soper_sem", title = "A-priori Sample Size for Structural Equation Models", columns = list(stats_col("section", "", "text", combineBelow = TRUE), stats_col("item", "Parameter", "text"), stats_col("value", "Value", "text")),
         rows = rows, notes = list("Westland (2010): the larger of the sample size needed to detect the effect on every possible link between latent variables (Šidák-corrected α) and the sample size the ratio of observed to latent variables requires.", sentence)),
    list(id = "soper_sem_grid", title = "Required Sample Size by Anticipated Effect Size", columns = list(stats_col("effect", "Effect size δ"), stats_col("effectN", "To detect effect", "integer"), stats_col("structureN", "For model structure", "integer"), stats_col("n", "Recommended", "integer")),
         rows = grid, notes = list(sprintf("Power %s, α = %s, %d latent and %d observed variables.", soper_num(power), soper_num(alpha), as.integer(k), as.integer(j))))),
    plots = list(list(type = "powerCurve", mode = "n_es", xLabel = "Anticipated effect size δ", yLabel = "Recommended minimum sample size", series = series,
                      current = list(x = delta, y = p$n), target = NULL, title = "Recommended sample size as a function of the effect size")), n = 0L)
}

run_soper_mr <- function(d, o) {
  f2 <- num_opt(o, "f2", 0.15, 0.001, 10); power <- num_opt(o, "power", 0.8, 0.5, 0.999)
  k <- round(num_opt(o, "predictors", 3, 1, 1000)); alpha <- num_opt(o, "alpha", 0.05, 1e-6, 0.5)
  r <- soper_mr_n(f2, power, k, alpha)
  if (is.na(r$n)) api_error(422, "no_solution", "No sample size up to one million reaches this power; check the effect size.")
  r2 <- f2 / (1 + f2)
  rows <- list(
    soper_row("Input parameters", "Anticipated effect size (f²)", soper_num(f2)),
    soper_row("Input parameters", "Desired statistical power (1 − β)", soper_num(power)),
    soper_row("Input parameters", "Number of predictors", format(k)),
    soper_row("Input parameters", "Probability level (α)", soper_num(alpha)),
    soper_row("Output parameters", "Minimum required sample size", format(r$n)),
    soper_row("Details", "Actual power at this sample size", soper_num(r$power, 4)),
    soper_row("Details", "Equivalent R² (f² / (1 + f²))", soper_num(r2, 4)),
    soper_row("Details", "Noncentrality parameter λ = f² n", soper_num(f2 * r$n, 4)),
    soper_row("Details", "F test degrees of freedom", sprintf("%d, %d", as.integer(k), as.integer(r$n - k - 1))))
  grid <- lapply(c(0.02, 0.15, 0.35), function(e) list(label = c("0.02" = "Small", "0.15" = "Medium", "0.35" = "Large")[[format(e)]], f2 = e, n = soper_mr_n(e, power, k, alpha)$n))
  f2s <- sort(unique(c(0.02, 0.15, 0.35, round(f2, 4))))
  top <- min(max(r$n * 1.8, 60), 2000)
  ns <- unique(round(seq(k + 2, top, length.out = 90)))
  series <- lapply(f2s[f2s >= 0.05 | f2s == round(f2, 4)], function(e) list(label = paste("f² =", soper_num(e, 3)),
    points = lapply(ns, function(n) list(x = n, y = signif(1 - stats::pf(stats::qf(1 - alpha, k, n - k - 1), k, n - k - 1, ncp = e * n), 5)))))
  sentence <- sprintf("An a-priori sample size calculation for multiple regression (Soper, 2024; Cohen, 1988), with an anticipated effect size of f² = %s, a desired statistical power of %s, %d predictors and a probability level of %s, indicated a minimum required sample size of %d.",
                      soper_num(f2), soper_num(power), as.integer(k), soper_num(alpha), as.integer(r$n))
  list(tables = list(
    list(id = "soper_mr", title = "A-priori Sample Size for Multiple Regression", columns = list(stats_col("section", "", "text", combineBelow = TRUE), stats_col("item", "Parameter", "text"), stats_col("value", "Value", "text")),
         rows = rows, notes = list("Overall F test of R² from the noncentral F distribution with λ = f² n (Cohen, 1988). Effect size conventions: f² = 0.02 small, 0.15 medium, 0.35 large.", sentence)),
    list(id = "soper_mr_grid", title = "Required Sample Size for Cohen's Effect Sizes", columns = list(stats_col("label", "Effect", "text"), stats_col("f2", "f²"), stats_col("n", "Minimum N", "integer")),
         rows = grid, notes = list(sprintf("Power %s, α = %s, %d predictors.", soper_num(power), soper_num(alpha), as.integer(k))))),
    plots = list(list(type = "powerCurve", mode = "power_n", xLabel = "Sample size N", yLabel = "Power (1 − β)", series = series,
                      current = list(x = r$n, y = r$power), target = power, title = "Power as a function of the sample size")), n = 0L)
}

STATS_MORE_RUN$soper_sem <- run_soper_sem
STATS_MORE_RUN$soper_mr <- run_soper_mr

soper_code <- function(proc, o) {
  if (proc == "soper_sem") return(c(
    "# A-priori sample size for SEM (Soper; Westland, 2010)",
    sprintf("delta <- %s; power <- %s; k <- %s; j <- %s; alpha <- %s", o$effect %||% 0.3, o$power %||% 0.8, o$latent %||% 5, o$observed %||% 20, o$alpha %||% 0.05),
    "a <- 1 - (1 - alpha)^(2 / (k * (k - 1)))                       # Sidak-corrected alpha",
    "H <- (delta / (qnorm(1 - a / 2) - qnorm(1 - power)))^2",
    "A <- 1 - delta^2; B <- delta * asin(delta / 2); C <- delta * asin(delta); D <- A / sqrt(3 - A)",
    "E <- A * (pi / 6 - B + D) + H",
    "n_effect <- ceiling((E + sqrt(E^2 + 4 * A * H * (pi / 6 + sqrt(A) + 2 * B - C - 2 * D))) / (2 * H))",
    "n_structure <- ceiling(50 * (j / k)^2 - 450 * (j / k) + 1100)",
    "max(n_effect, n_structure)                                     # recommended minimum sample size"))
  c("# A-priori sample size for multiple regression (Soper; Cohen, 1988)",
    sprintf("f2 <- %s; power <- %s; k <- %s; alpha <- %s", o$f2 %||% 0.15, o$power %||% 0.8, o$predictors %||% 3, o$alpha %||% 0.05),
    "pw <- function(n) 1 - pf(qf(1 - alpha, k, n - k - 1), k, n - k - 1, ncp = f2 * n)",
    "n <- k + 2; while (pw(n) < power) n <- n + 1",
    "n   # minimum required sample size")
}
