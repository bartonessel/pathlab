# Exploratory factor analysis and principal component analysis (psych + GPArotation).
# Every number comes from psych: KMO(), cortest.bartlett(), fa.parallel(), fa() and principal().
# The data are never changed: the missing-value code is treated as missing for this analysis only,
# and listwise deletion / the split-sample half only select rows, all of which is reported.

EFA_METHODS <- c(pca = "Principal components", minres = "Minimum residual", ml = "Maximum likelihood", pa = "Principal axis")
EFA_ROTATIONS <- c(oblimin = "Oblimin (oblique)", promax = "Promax (oblique)", varimax = "Varimax (orthogonal)", none = "None")
EFA_OBLIQUE <- c("oblimin", "promax")

normalize_efa <- function(raw) {
  raw <- raw %||% list()
  pick <- function(v, allowed, default) { v <- as.character(v %||% default)[1]; if (v %in% allowed) v else default }
  split <- raw$split %||% list()
  list(
    variables = unique(as_chr_vec(raw$variables)),
    method = pick(raw$method, names(EFA_METHODS), "minres"),
    rotation = pick(raw$rotation, names(EFA_ROTATIONS), "oblimin"),
    cor = pick(raw$cor, c("pearson", "polychoric"), "pearson"),
    nfactorsRule = pick(raw$nfactorsRule, c("parallel", "kaiser", "fixed"), "parallel"),
    nfactors = int_in(raw[["nfactors"]], 2L, 1L, 50L, "The number of factors"),   # [[ ]]: $ would partially match nfactorsRule
    cutoff = {
      v <- suppressWarnings(as.numeric(raw$cutoff %||% 0.4))
      if (is.na(v) || v < 0.1 || v > 0.9) api_error(400, "invalid_efa", "The loading cut-off must be between 0.10 and 0.90.")
      v
    },
    missing = pick(raw$missing, c("listwise", "pairwise"), "listwise"),
    parallelIter = int_in(raw$parallelIter, 20L, 5L, 200L, "The number of parallel-analysis iterations"),
    seed = int_in(raw$seed, 2026L, 1L, .Machine$integer.max, "The seed"),
    split = list(enabled = isTRUE(split$enabled), half = if (identical(as.integer(split$half %||% 1L), 2L)) 2L else 1L)
  )
}

efa_issue <- function(severity, code, message, hint = NULL) {
  list(severity = severity, code = code, message = message, hint = hint)
}

#' Check the options against the data. Returns a list of issues (errors stop the analysis).
validate_efa <- function(df, o) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- efa_issue(...)
  v <- o$variables
  if (length(v) < 3) add("error", "efa_variables", "Choose at least three variables for the factor analysis.")
  miss <- setdiff(v, names(df))
  if (length(miss)) add("error", "efa_missing_column", sprintf("Not in the dataset: %s.", paste(miss, collapse = ", ")))
  bad <- Filter(function(cn) cn %in% names(df) && !is.numeric(df[[cn]]), v)
  if (length(bad)) add("error", "efa_not_numeric", sprintf("Factor analysis needs numeric variables; not numeric: %s.", paste(bad, collapse = ", ")))
  const <- Filter(function(cn) cn %in% names(df) && is.numeric(df[[cn]]) && length(unique(stats::na.omit(df[[cn]]))) < 2, v)
  if (length(const)) add("error", "efa_constant", sprintf("These variables have no variance: %s.", paste(const, collapse = ", ")))
  if (o$nfactorsRule == "fixed" && length(v) >= 3 && o$nfactors >= length(v)) {
    add("error", "efa_nfactors", sprintf("Extract fewer factors than variables (at most %d here).", length(v) - 1))
  }
  out
}

efa_rows <- function(df, o, missing_code) {
  d <- df[, o$variables, drop = FALSE]
  recoded <- 0L
  if (!is.null(missing_code)) for (cn in names(d)) {
    hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code
    recoded <- recoded + sum(hit)
    d[[cn]][hit] <- NA
  }
  n_input <- nrow(d)
  rows <- seq_len(n_input)
  # Split-sample: a reproducible random half (seeded), so EFA and a later model can use different halves.
  if (o$split$enabled) {
    set.seed(o$seed)
    perm <- sample(n_input)
    h <- floor(n_input / 2)
    rows <- sort(if (o$split$half == 1L) perm[seq_len(h)] else perm[(h + 1):n_input])
  }
  d <- d[rows, , drop = FALSE]
  n_split <- nrow(d)
  if (o$missing == "listwise") {
    keep <- stats::complete.cases(d)
    rows <- rows[keep]
    d <- d[keep, , drop = FALSE]
  }
  list(data = d, rows = rows, nInput = n_input, nSplit = n_split, nUsed = nrow(d), cellsRecoded = recoded,
       cellsMissing = sum(is.na(d)), rowsRemoved = n_split - nrow(d))
}

efa_cor <- function(d, o) {
  if (o$cor == "polychoric") {
    pc <- tryCatch(suppressWarnings(psych::polychoric(d)), error = function(e) {
      api_error(422, "efa_polychoric", "Polychoric correlations could not be estimated.",
                "Polychoric correlations need ordinal items with few categories (up to about 8). Use Pearson correlations instead.")
    })
    return(pc$rho)
  }
  stats::cor(d, use = if (o$missing == "pairwise") "pairwise.complete.obs" else "everything")
}

mat_rows <- function(m, row_key = "variable") {
  lapply(seq_len(nrow(m)), function(i) {
    r <- as.list(unname(m[i, ]))
    names(r) <- colnames(m)
    c(setNames(list(rownames(m)[i]), row_key), r)
  })
}

#' Run the EFA / PCA. `missing_code`: the project's missing-value code (or NULL).
run_efa <- function(df, o, missing_code = NULL) {
  issues <- validate_efa(df, o)
  if (any(vapply(issues, function(i) i$severity == "error", TRUE))) {
    api_error(422, "invalid_efa", "The factor analysis cannot run until the listed problems are fixed.", details = issues)
  }
  warn <- character(0)
  t0 <- Sys.time()
  dat <- efa_rows(df, o, missing_code)
  d <- dat$data
  p <- ncol(d)
  n <- dat$nUsed
  if (n < p + 2) api_error(422, "efa_too_few_rows", sprintf("Only %d complete rows remain for %d variables.", n, p),
                           "Use pairwise deletion, fewer variables, or turn off the split-sample option.")
  if (n < 100) warn <- c(warn, sprintf("The sample is small for factor analysis (n = %d; at least 100, preferably 200 or more, is usually recommended).", n))
  if (n / p < 5) warn <- c(warn, sprintf("There are %.1f cases per variable (5 to 10 or more is usually recommended).", n / p))

  R <- efa_cor(d, o)
  if (any(!is.finite(R))) api_error(422, "efa_cor", "Some correlations could not be computed (too few overlapping cases).",
                                    "Use listwise deletion or remove variables with many missing values.")
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < 1e-8) warn <- c(warn, "The correlation matrix is not positive definite (some variables are (nearly) linear combinations of others); psych smooths it, so treat the results with caution.")

  kmo <- suppressWarnings(psych::KMO(R))
  bart <- suppressWarnings(psych::cortest.bartlett(R, n = n))

  # Parallel analysis (Horn, 1965): observed eigenvalues against eigenvalues of random data of the same size.
  is_pca <- o$method == "pca"
  set.seed(o$seed)
  pa <- tryCatch({
    invisible(utils::capture.output(res <- suppressWarnings(suppressMessages(
      psych::fa.parallel(R, n.obs = n, fm = if (is_pca) "minres" else o$method, fa = if (is_pca) "pc" else "fa",
                         n.iter = o$parallelIter, plot = FALSE, cor = "cor")))))
    res
  }, error = function(e) NULL)
  observed <- if (!is.null(pa)) (if (is_pca) pa$pc.values else pa$fa.values) else ev
  simulated <- if (!is.null(pa)) (if (is_pca) pa$pc.sim else pa$fa.sim) else rep(NA_real_, p)
  suggested_parallel <- if (!is.null(pa)) as.integer(if (is_pca) pa$ncomp else pa$nfact) else NA_integer_
  suggested_kaiser <- as.integer(sum(ev > 1))

  k <- switch(o$nfactorsRule,
              parallel = if (is.na(suggested_parallel)) suggested_kaiser else suggested_parallel,
              kaiser = suggested_kaiser,
              fixed = o$nfactors)
  k <- max(1L, min(as.integer(k), p - 1L))
  if (o$nfactorsRule != "fixed" && is.na(suggested_parallel) && o$nfactorsRule == "parallel") {
    warn <- c(warn, "Parallel analysis failed; the number of factors follows the eigenvalue > 1 rule instead.")
  }
  rotate <- if (k == 1L) "none" else o$rotation

  fit_warn <- character(0)
  f <- withCallingHandlers(
    tryCatch(
      if (is_pca) psych::principal(R, nfactors = k, rotate = rotate, n.obs = n)
      else psych::fa(R, nfactors = k, fm = o$method, rotate = rotate, n.obs = n),
      error = function(e) api_error(422, "efa_failed", paste("The factor analysis failed:", conditionMessage(e)),
                                    "Try fewer factors, another extraction method, or check for variables with (near) perfect correlations.")),
    warning = function(w) { fit_warn <<- c(fit_warn, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (length(fit_warn)) warn <- c(warn, unique(fit_warn))

  L <- unclass(f$loadings)
  L <- L[, seq_len(ncol(L)), drop = FALSE]
  factors <- colnames(L)
  h2 <- if (!is.null(f$communality)) f$communality else rowSums(L^2)
  if (any(h2 > 1 + 1e-6)) warn <- c(warn, sprintf("Heywood case: communality above 1 for %s. The solution may be improper; try fewer factors or another method.",
                                                  paste(names(h2)[h2 > 1 + 1e-6], collapse = ", ")))
  # Item assignment: highest absolute loading; flags for weak primary loadings and cross-loadings.
  items <- lapply(rownames(L), function(v) {
    a <- unname(abs(L[v, ]))
    ord <- order(a, decreasing = TRUE)
    prim <- ord[1]
    sec <- if (length(ord) > 1) ord[2] else NA
    list(variable = v, factor = factors[prim], loading = unname(L[v, prim]),
         secondFactor = if (is.na(sec)) NULL else factors[sec], secondLoading = if (is.na(sec)) NULL else unname(L[v, sec]),
         communality = unname(h2[v]), belowCutoff = a[prim] < o$cutoff,
         crossLoading = !is.na(sec) && a[sec] >= o$cutoff)
  })
  va <- f$Vaccounted
  variance <- lapply(factors, function(fc) list(
    factor = fc,
    ssLoadings = unname(va["SS loadings", fc]),
    proportion = unname(va["Proportion Var", fc]),
    cumulative = if ("Cumulative Var" %in% rownames(va)) unname(va["Cumulative Var", fc]) else unname(va["Proportion Var", fc])
  ))
  # Unrotated solution (same extraction, rotate = "none") so both can be reported; equal to L when not rotated.
  unrot <- if (rotate == "none") NULL else tryCatch(suppressWarnings(suppressMessages(
    if (is_pca) psych::principal(R, nfactors = k, rotate = "none", n.obs = n)
    else psych::fa(R, nfactors = k, fm = o$method, rotate = "none", n.obs = n))), error = function(e) NULL)
  unrotated <- if (is.null(unrot)) NULL else {
    L0 <- unclass(unrot$loadings)[, seq_len(k), drop = FALSE]
    colnames(L0) <- paste0(if (is_pca) "PC" else "F", seq_len(k))
    va0 <- unrot$Vaccounted
    list(factors = colnames(L0), loadings = mat_rows(L0),
         variance = lapply(seq_len(k), function(j) list(
           factor = colnames(L0)[j], ssLoadings = unname(va0["SS loadings", j]), proportion = unname(va0["Proportion Var", j]),
           cumulative = if ("Cumulative Var" %in% rownames(va0)) unname(va0["Cumulative Var", j]) else unname(va0["Proportion Var", j]))))
  }
  phi <- if (!is.null(f$Phi) && rotate %in% EFA_OBLIQUE) { m <- f$Phi; dimnames(m) <- list(factors, factors); mat_rows(m, "factor") } else NULL
  fit <- list(
    chisq = if (!is.null(f$STATISTIC)) unname(f$STATISTIC) else NULL,
    df = if (!is.null(f$dof)) unname(f$dof) else NULL,
    p = if (!is.null(f$PVAL)) unname(f$PVAL) else NULL,
    rmsr = if (!is.null(f$rms)) unname(f$rms) else NULL,
    tli = if (!is_pca && !is.null(f$TLI)) unname(f$TLI) else NULL,
    rmsea = if (!is_pca && !is.null(f$RMSEA)) unname(f$RMSEA[1]) else NULL,
    rmseaLower = if (!is_pca && !is.null(f$RMSEA)) unname(f$RMSEA[2]) else NULL,
    rmseaUpper = if (!is_pca && !is.null(f$RMSEA)) unname(f$RMSEA[3]) else NULL,
    bic = if (!is_pca && !is.null(f$BIC)) unname(f$BIC) else NULL,
    fitOffDiagonal = if (!is.null(f$fit.off)) unname(f$fit.off) else NULL
  )
  list(
    options = o,
    n = list(input = dat$nInput, split = dat$nSplit, used = n, rowsRemoved = dat$rowsRemoved,
             cellsRecoded = dat$cellsRecoded, cellsMissing = dat$cellsMissing),
    methodLabel = unname(EFA_METHODS[o$method]),
    rotationApplied = rotate,
    kmo = list(overall = unname(kmo$MSA), items = lapply(names(kmo$MSAi), function(v) list(variable = v, msa = unname(kmo$MSAi[v])))),
    bartlett = list(chisq = unname(bart$chisq), df = unname(bart$df), p = unname(bart$p.value)),
    eigen = list(observed = unname(observed), simulated = unname(simulated), correlation = unname(ev),
                 kind = if (is_pca) "components" else "factors"),
    suggested = list(parallel = suggested_parallel, kaiser = suggested_kaiser),
    nfactors = k,
    factors = factors,
    loadings = mat_rows(L),
    unrotated = unrotated,
    items = items,
    variance = variance,
    phi = phi,
    fit = fit,
    warnings = as.list(unique(warn)),
    rCode = efa_code(o, k, rotate, missing_code),
    psychVersion = as.character(utils::packageVersion("psych")),
    durationSeconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    .derived = efa_scores(d, dat$rows, dat$nInput, f, R, is_pca)
  )
}

#' Factor scores (regression / Thurstone method) or component scores from psych::factor.scores for the rows with
#' complete data on the analysed variables (kept on the server for derived variables).
efa_scores <- function(d, rows, n_input, f, R, is_pca) {
  cc <- stats::complete.cases(d)
  if (!any(cc)) return(NULL)
  X <- as.matrix(d[cc, , drop = FALSE])
  sc <- tryCatch(suppressWarnings(
    if (is_pca) psych::factor.scores(scale(X), f, method = "components")$scores
    else psych::factor.scores(X, f, rho = R, method = "Thurstone")$scores), error = function(e) NULL)
  if (is.null(sc)) return(NULL)
  list(nInput = n_input, rows = rows[cc], tables = list(scores = as.data.frame(sc)))
}

#' R code reproducing the EFA with psych (the dataset object is called `data`, as in the SEMinR script).
efa_code <- function(o, k, rotate, missing_code) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  vars <- paste0("c(", paste(vapply(o$variables, q, ""), collapse = ", "), ")")
  is_pca <- o$method == "pca"
  lines <- c(
    "# Exploratory factor analysis generated by PathLab SEM (psych package)",
    "library(psych)",
    "library(GPArotation)",
    "# data <- read.csv(\"your_data.csv\")   # the same dataset as in the app",
    paste0("items <- ", vars),
    "d <- data[, items]"
  )
  if (!is.null(missing_code)) lines <- c(lines, paste0("d[d == ", missing_code, "] <- NA   # project missing-value code"))
  if (o$split$enabled) lines <- c(lines,
    paste0("set.seed(", o$seed, ")"),
    "perm <- sample(nrow(d)); h <- floor(nrow(d) / 2)",
    if (o$split$half == 1L) "d <- d[sort(perm[seq_len(h)]), ]   # first random half" else "d <- d[sort(perm[(h + 1):nrow(d)]), ]   # second random half")
  if (o$missing == "listwise") lines <- c(lines, "d <- d[complete.cases(d), ]   # listwise deletion")
  lines <- c(lines,
    if (o$cor == "polychoric") "R <- polychoric(d)$rho" else if (o$missing == "pairwise") "R <- cor(d, use = \"pairwise.complete.obs\")" else "R <- cor(d)",
    "n <- nrow(d)",
    "KMO(R)",
    "cortest.bartlett(R, n = n)",
    paste0("set.seed(", o$seed, ")"),
    paste0("fa.parallel(R, n.obs = n, fm = \"", if (is_pca) "minres" else o$method, "\", fa = \"", if (is_pca) "pc" else "fa", "\", n.iter = ", o$parallelIter, ")"),
    if (is_pca) paste0("fit <- principal(R, nfactors = ", k, ", rotate = \"", rotate, "\", n.obs = n)")
    else paste0("fit <- fa(R, nfactors = ", k, ", fm = \"", o$method, "\", rotate = \"", rotate, "\", n.obs = n)"),
    paste0("print(fit, cut = ", o$cutoff, ", sort = TRUE)")
  )
  paste(lines, collapse = "\n")
}
