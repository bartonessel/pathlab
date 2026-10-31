# Three-step analyses for latent profiles, classes and growth mixtures (Vermunt, 2010; Bakk, Tekle & Vermunt, 2013;
# Asparouhov & Muthén, 2014): once the classes are estimated, relate them to other variables while correcting for
# classification error, instead of treating the most likely class of each case as known.
#   * Distal outcomes: BCH weighting (Bolck, Croon & Hagenaars, 2004) - class means of an outcome, an overall Wald test
#     and pairwise comparisons.
#   * Covariates (predictors of class membership): the ML three-step method - a multinomial logistic regression of the
#     latent class on the covariates with the classification error fixed at its step-2 value.
# Both use only the posterior probabilities `z` (cases x classes) and the modal assignment `cls` of step 1.

# A column of a Statistics table (the same form as stats_col in R/stats.R, which the analysis worker does not load).
ts_col <- function(name, title, type = "number", superTitle = "", pvalue = FALSE, combineBelow = FALSE)
  list(name = name, title = title, superTitle = superTitle, type = type, pvalue = pvalue, combineBelow = combineBelow)

#' Classification error of the modal assignment: D[s, t] = P(assigned to t | true class s).
ts_dmatrix <- function(z, cls) {
  k <- ncol(z)
  A <- vapply(seq_len(k), function(t) as.numeric(cls == t), numeric(length(cls)))
  if (!is.matrix(A)) A <- matrix(A, ncol = k)
  sweep(t(z) %*% A, 1, pmax(colSums(z), 1e-12), "/")
}

ts_inverse <- function(D) tryCatch(solve(D), error = function(e) MASS::ginv(D))

#' BCH class means of one numeric distal outcome. Returns NULL when fewer than two classes have cases.
ts_bch <- function(z, cls, y) {
  k <- ncol(z)
  ok <- !is.na(y)
  if (sum(ok) < k + 2) return(NULL)
  H <- ts_inverse(ts_dmatrix(z, cls))
  W <- H[cls[ok], , drop = FALSE]
  y <- y[ok]
  sw <- colSums(W)
  mu <- colSums(W * y) / sw
  E <- sweep(W * outer(y, mu, "-"), 2, sw, "/")   # influence of each case on each class mean
  V <- crossprod(E)
  sdv <- sqrt(pmax(colSums(W * outer(y, mu, "-")^2) / sw, 0))
  C <- cbind(-1, diag(k - 1))                       # differences from class 1
  d <- C %*% mu
  wald <- tryCatch(as.numeric(t(d) %*% solve(C %*% V %*% t(C)) %*% d), error = function(e) NA_real_)
  pairs <- utils::combn(k, 2)
  pw <- lapply(seq_len(ncol(pairs)), function(j) {
    a <- pairs[1, j]; b <- pairs[2, j]
    se <- sqrt(max(V[a, a] + V[b, b] - 2 * V[a, b], 0))
    zv <- (mu[b] - mu[a]) / se
    list(a = a, b = b, diff = unname(mu[b] - mu[a]), se = se, z = unname(zv), p = 2 * stats::pnorm(-abs(unname(zv))))
  })
  padj <- stats::p.adjust(vapply(pw, `[[`, 0, "p"), "holm")
  for (j in seq_along(pw)) pw[[j]]$pHolm <- padj[j]
  list(n = sum(ok), mean = unname(mu), se = sqrt(pmax(diag(V), 0)), sd = unname(sdv),
       wald = wald, df = k - 1L, p = if (is.na(wald)) NA_real_ else stats::pchisq(wald, k - 1, lower.tail = FALSE), pairs = pw)
}

#' ML three-step multinomial logistic regression of class membership on covariates X (cases x q, complete).
#' The last class is the reference (as in Mplus and lcmm).
ts_ml3 <- function(z, cls, X) {
  k <- ncol(z); q <- ncol(X)
  D <- ts_dmatrix(z, cls)
  Xd <- cbind(1, X)
  Dc <- D[, cls, drop = FALSE]   # k x n: P(assigned c_i | class s)
  negll <- function(b) {
    B <- matrix(b, q + 1, k - 1)
    eta <- cbind(Xd %*% B, 0)
    eta <- eta - apply(eta, 1, max)
    P <- exp(eta); P <- P / rowSums(P)
    -sum(log(pmax(rowSums(P * t(Dc)), 1e-300)))
  }
  # Start from the class proportions (intercepts) and no covariate effects.
  pr <- pmax(colMeans(z), 1e-4)
  b0 <- matrix(0, q + 1, k - 1); b0[1, ] <- log(pr[-k] / pr[k])
  opt <- stats::optim(c(b0), negll, method = "BFGS", control = list(maxit = 1000, reltol = 1e-10))
  Hs <- tryCatch(stats::optimHess(opt$par, negll), error = function(e) NULL)
  V <- if (is.null(Hs)) NULL else tryCatch(solve(Hs), error = function(e) NULL)
  se <- if (is.null(V)) rep(NA_real_, length(opt$par)) else sqrt(pmax(diag(V), 0))
  terms <- c("(Intercept)", colnames(X))
  rows <- list()
  for (s in seq_len(k - 1)) for (j in seq_len(q + 1)) {
    i <- (s - 1) * (q + 1) + j
    b <- opt$par[i]; zv <- b / se[i]
    rows[[length(rows) + 1]] <- list(class = s, term = terms[j], b = b, se = se[i], z = zv, p = 2 * stats::pnorm(-abs(zv)),
                                     or = if (j > 1) exp(b) else NA_real_, lo = if (j > 1) exp(b - 1.959964 * se[i]) else NA_real_, hi = if (j > 1) exp(b + 1.959964 * se[i]) else NA_real_)
  }
  # Overall Wald test of each covariate (all its k - 1 coefficients zero).
  overall <- lapply(seq_len(q), function(j) {
    idx <- (seq_len(k - 1) - 1) * (q + 1) + j + 1
    w <- if (is.null(V)) NA_real_ else tryCatch(as.numeric(t(opt$par[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% opt$par[idx]), error = function(e) NA_real_)
    list(term = colnames(X)[j], wald = w, df = k - 1L, p = if (is.na(w)) NA_real_ else stats::pchisq(w, k - 1, lower.tail = FALSE))
  })
  list(n = nrow(X), converged = opt$convergence == 0, loglik = -opt$value, rows = rows, overall = overall, reference = k)
}

#' Stats tables (for the Statistics result view) of the three-step analyses. `unit` names the classes ("Profile",
#' "Class"); `distal` and `covs` are data frames over the same cases as z (they may contain missing values).
ts_tables <- function(z, cls, distal = NULL, covs = NULL, unit = "Class", prefix = "ts") {
  k <- ncol(z)
  if (k < 2) return(list())
  out <- list()
  if (!is.null(distal) && ncol(distal)) {
    res <- lapply(names(distal), function(v) c(list(variable = v), ts_bch(z, cls, as.numeric(distal[[v]]))))
    res <- Filter(function(r) !is.null(r$mean), res)
    if (length(res)) {
      rows <- lapply(res, function(r) c(list(var = r$variable, n = r$n),
                                        stats::setNames(as.list(r$mean), paste0("m", seq_len(k))), stats::setNames(as.list(r$se), paste0("se", seq_len(k))),
                                        list(wald = r$wald, df = r$df, p = r$p)))
      cols <- c(list(ts_col("var", "Outcome", "text"), ts_col("n", "N", "integer")),
                unlist(lapply(seq_len(k), function(s) list(ts_col(paste0("m", s), "Mean", superTitle = paste(unit, s)), ts_col(paste0("se", s), "SE", superTitle = paste(unit, s)))), recursive = FALSE),
                list(ts_col("wald", "Wald χ²"), ts_col("df", "df", "integer"), ts_col("p", "p", pvalue = TRUE)))
      out[[length(out) + 1]] <- list(id = paste0(prefix, "_bch"), title = sprintf("Distal Outcomes by %s (BCH three-step)", unit), columns = cols, rows = rows,
                                     notes = list(sprintf("Class means weighted with BCH weights (Bolck, Croon & Hagenaars, 2004; Bakk & Vermunt, 2016), which correct for classification error; robust standard errors. The Wald test compares all %ss (df = %d).", tolower(unit), k - 1)))
      prow <- unlist(lapply(res, function(r) lapply(r$pairs, function(p) list(var = r$variable, pair = sprintf("%s %d vs %d", unit, p$b, p$a), diff = p$diff, se = p$se, z = p$z, p = p$p, pHolm = p$pHolm))), recursive = FALSE)
      out[[length(out) + 1]] <- list(id = paste0(prefix, "_bch_pairs"), title = sprintf("Pairwise %s Differences on the Distal Outcomes", unit), rows = prow,
                                     columns = list(ts_col("var", "Outcome", "text", combineBelow = TRUE), ts_col("pair", "Comparison", "text"), ts_col("diff", "Difference"), ts_col("se", "SE"),
                                                    ts_col("z", "z"), ts_col("p", "p", pvalue = TRUE), ts_col("pHolm", "p (Holm)", pvalue = TRUE)),
                                     notes = list(sprintf("Difference = mean of the first-named minus the second-named %s; Holm correction across all comparisons.", tolower(unit))))
    }
  }
  if (!is.null(covs) && ncol(covs)) {
    X <- as.matrix(as.data.frame(lapply(covs, as.numeric)))
    ok <- stats::complete.cases(X)
    if (sum(ok) > (ncol(X) + 1) * (k - 1) + 5) {
      m <- ts_ml3(z[ok, , drop = FALSE], cls[ok], X[ok, , drop = FALSE])
      rows <- lapply(m$rows, function(r) list(class = sprintf("%s %d vs %d", unit, r$class, m$reference), term = r$term, b = r$b, se = r$se, z = r$z, p = r$p, or = r$or, lo = r$lo, hi = r$hi))
      out[[length(out) + 1]] <- list(id = paste0(prefix, "_ml3"), title = sprintf("Predictors of %s Membership (ML three-step)", unit), rows = rows,
                                     columns = list(ts_col("class", "Comparison", "text", combineBelow = TRUE), ts_col("term", "Predictor", "text"), ts_col("b", "B"), ts_col("se", "SE"),
                                                    ts_col("z", "z"), ts_col("p", "p", pvalue = TRUE), ts_col("or", "Odds ratio"), ts_col("lo", "Lower", superTitle = "95% CI (OR)"), ts_col("hi", "Upper", superTitle = "95% CI (OR)")),
                                     notes = list(sprintf("Multinomial logistic regression of the latent %s on the predictors with the classification error fixed (ML three-step; Vermunt, 2010). Reference: %s %d. n = %d%s.",
                                                          tolower(unit), tolower(unit), m$reference, m$n, if (m$converged) "" else "; the estimation did not fully converge")))
      out[[length(out) + 1]] <- list(id = paste0(prefix, "_ml3_wald"), title = "Overall Effect of Each Predictor", rows = lapply(m$overall, function(r) list(term = r$term, wald = r$wald, df = r$df, p = r$p)),
                                     columns = list(ts_col("term", "Predictor", "text"), ts_col("wald", "Wald χ²"), ts_col("df", "df", "integer"), ts_col("p", "p", pvalue = TRUE)),
                                     notes = list(sprintf("Tests whether the predictor is related to %s membership at all (all its coefficients zero).", tolower(unit))))
    }
  }
  out
}

#' One stats table as an HTML table for the analysis reports.
ts_table_html <- function(t) {
  cols <- t$columns
  hdr <- vapply(cols, function(c) html_escape(paste0(if (nzchar(c$superTitle %||% "")) paste0(c$superTitle, ": ") else "", c$title)), "")
  rows <- lapply(t$rows, function(r) vapply(cols, function(c) {
    v <- r[[c$name]]
    if (is.null(v) || (is.atomic(v) && length(v) == 1 && is.na(v))) return("&ndash;")
    if (identical(c$type, "text")) html_escape(as.character(v)) else if (isTRUE(c$pvalue)) fmt_p(v) else if (identical(c$type, "integer")) as.character(as.integer(v)) else fmt(v)
  }, ""))
  num <- which(!vapply(cols, function(c) identical(c$type, "text"), TRUE))
  paste0(html_table(hdr, rows, html_escape(t$title), num_cols = num),
         paste(vapply(t$notes %||% list(), function(n) paste0("<p class=\"note\">", html_escape(n), "</p>"), ""), collapse = ""))
}

#' One stats table as a data frame for the workbooks.
ts_table_df <- function(t) {
  cols <- t$columns
  if (!length(t$rows)) return(NULL)
  df <- as.data.frame(lapply(cols, function(c) { v <- lapply(t$rows, function(r) r[[c$name]] %||% NA)
    if (identical(c$type, "text")) vapply(v, function(x) as.character(x)[1], "") else vapply(v, function(x) suppressWarnings(as.numeric(x)[1]), 0) }), stringsAsFactors = FALSE)
  names(df) <- make.unique(vapply(cols, function(c) paste0(if (nzchar(c$superTitle %||% "")) paste0(c$superTitle, " ") else "", c$title), ""))
  df
}
