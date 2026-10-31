# Data screening before analysis: cases the researcher may want to remove, with the reason for each. Nothing is
# removed here; the app shows the flagged cases and the user decides (removals become reversible row deletions).
#   * Straight-lining: the SD of a case's answers across the Likert items is below a threshold (default 0.25).
#   * Missing answers: more than a share of the items unanswered (default 15%; Hair et al., 2022).
#   * Out-of-range answers: values outside the scale of the items (e.g. 9 on a 1-7 scale).
#   * Multivariate outliers: Mahalanobis D^2 on the items with p < .001 against chi-square(df = items)
#     (Tabachnick & Fidell, 2019).
#   * Influential cases: Cook's distance > 4/n in the regression(s) of an outcome on its predictors (Cook, 1977).
#     Before a model exists the user can name an outcome and predictors; with a model, each endogenous construct's
#     mean item score is regressed on the mean item scores of its predictors.

SCREEN_DEFAULTS <- list(sdCut = 0.25, missingCut = 0.15, mahaP = 0.001, cookRule = "4n")

#' Likert-type items: numeric columns with whole values on a short scale (at most 11 points, between -10 and 10);
#' the largest group sharing one scale (min, max) is used, so demographics on other scales are left out.
screen_detect_items <- function(df, missing_code = NULL) {
  cand <- list()
  for (cn in names(df)) {
    x <- df[[cn]]
    if (!is.numeric(x)) next
    if (!is.null(missing_code)) x[!is.na(x) & x == missing_code] <- NA
    v <- x[!is.na(x)]
    if (length(v) < 5 || any(abs(v - round(v)) > 1e-9)) next
    u <- unique(v)
    if (length(u) < 2 || length(u) > 11 || min(v) < -10 || max(v) > 10) next
    cand[[cn]] <- c(min(v), max(v))
  }
  if (!length(cand)) return(list(items = character(0), scale = NULL))
  key <- vapply(cand, function(s) paste(s, collapse = ":"), "")
  tab <- sort(table(key), decreasing = TRUE)
  best <- names(tab)[1]
  # A wider scale that contains the most common one (e.g. 1-7 items where some answers only reach 1-6) joins it.
  sc <- as.numeric(strsplit(best, ":")[[1]])
  items <- names(cand)[vapply(cand, function(s) s[1] >= sc[1] && s[2] <= sc[2], TRUE)]
  list(items = items, scale = sc)
}

screen_mahalanobis <- function(X) {
  ok <- stats::complete.cases(X)
  d2 <- rep(NA_real_, nrow(X))
  if (sum(ok) <= ncol(X) + 1) return(list(d2 = d2, note = "Too few complete cases for Mahalanobis distances."))
  Xc <- X[ok, , drop = FALSE]
  S <- stats::cov(Xc)
  inv <- tryCatch(solve(S), error = function(e) NULL)
  note <- NULL
  if (is.null(inv)) {   # singular covariance (e.g. an item that is a sum of others): Moore-Penrose inverse
    sv <- svd(S); keep <- sv$d > max(sv$d) * 1e-10
    inv <- sv$v[, keep, drop = FALSE] %*% diag(1 / sv$d[keep], sum(keep)) %*% t(sv$u[, keep, drop = FALSE])
    note <- "The items' covariance matrix is singular; a generalised inverse was used."
  }
  d2[ok] <- stats::mahalanobis(Xc, colMeans(Xc), inv, inverted = TRUE)
  list(d2 = d2, note = note)
}

screen_cook <- function(df, regs) {
  # regs: list of list(outcome = name, predictors = c(...)) on columns of df; returns the max Cook's D per case
  cd <- rep(NA_real_, nrow(df)); used <- list()
  for (r in regs) {
    vars <- c(r$outcome, r$predictors)
    if (!length(r$predictors) || !all(vars %in% names(df))) next
    d <- df[, vars, drop = FALSE]
    ok <- stats::complete.cases(d)
    if (sum(ok) <= length(r$predictors) + 2) next
    fit <- stats::lm(stats::reformulate(sprintf("`%s`", r$predictors), response = sprintf("`%s`", r$outcome)), data = d[ok, , drop = FALSE])
    v <- stats::cooks.distance(fit)
    cur <- cd[ok]; cd[ok] <- ifelse(is.na(cur), v, pmax(cur, v))
    used[[length(used) + 1]] <- list(outcome = r$outcome, predictors = as.list(r$predictors), n = sum(ok))
  }
  list(cook = cd, regressions = used)
}

#' opts: items (character, empty = detect), sdCut, missingCut, mahaP, cookRule ("4n" | "1"), regressions (list of
#' {outcome, predictors} on dataset columns), constructs ({name, indicators}) + paths ({from, to}) for model-based
#' Cook's distance, idColumn (shown in the table), missingCode.
run_screening <- function(df, opts = list()) {
  code <- suppressWarnings(as.numeric(opts$missingCode %||% NA)); if (is.na(code)) code <- NULL
  num <- function(k, lo, hi) { v <- suppressWarnings(as.numeric(opts[[k]] %||% SCREEN_DEFAULTS[[k]])); if (is.na(v) || v < lo || v > hi) SCREEN_DEFAULTS[[k]] else v }
  sd_cut <- num("sdCut", 0, 10); miss_cut <- num("missingCut", 0, 1); maha_p <- num("mahaP", 1e-8, 0.5)
  cook_rule <- if (identical(opts$cookRule, "1")) "1" else "4n"
  det <- screen_detect_items(df, code)
  items <- as.character(unlist(opts$items %||% list()))
  chosen <- length(items) > 0
  items <- if (chosen) intersect(items, names(df)) else det$items
  # No Likert-type items (e.g. WEKA / machine-learning data with continuous measurements): screen every numeric column
  # that varies and is not a running case number, for missing values and outliers only.
  fallback <- !chosen && length(items) < 2
  if (fallback) items <- Filter(function(cn) {
    x <- df[[cn]]; if (!is.numeric(x)) return(FALSE); v <- x[!is.na(x)]
    length(unique(v)) >= 3 && !(all(abs(v - round(v)) < 1e-9) && length(unique(v)) == length(v))
  }, names(df))
  items <- items[vapply(items, function(cn) is.numeric(df[[cn]]), TRUE)]
  if (length(items) < 2) api_error(422, "screen_items", "Choose at least two numeric columns to screen.", "The dataset needs at least two numeric columns; add them under Settings.")
  # Straight-lining and range checks only make sense for items answered on one response scale.
  likert <- length(opts$scale) == 2 || (!is.null(det$scale) && all(items %in% det$items))
  X <- as.matrix(df[, items, drop = FALSE])
  if (!is.null(code)) X[!is.na(X) & X == code] <- NA
  n <- nrow(X); p <- ncol(X)
  answered <- rowSums(!is.na(X))
  sds <- if (likert) apply(X, 1, function(r) { r <- r[!is.na(r)]; if (length(r) >= 3) stats::sd(r) else NA_real_ }) else rep(NA_real_, n)
  miss <- 1 - answered / p
  scale <- if (length(opts$scale) == 2) as.numeric(unlist(opts$scale)) else if (likert) det$scale else NULL
  oor <- if (!is.null(scale)) rowSums(!is.na(X) & (X < scale[1] | X > scale[2])) else rep(0L, n)
  mh <- screen_mahalanobis(X)
  pv <- stats::pchisq(mh$d2, df = p, lower.tail = FALSE)
  # Cook's distance: explicit regressions, or the model (construct means of their indicators)
  regs <- lapply(opts$regressions %||% list(), function(r) list(outcome = as.character(r$outcome %||% ""), predictors = as.character(unlist(r$predictors %||% list()))))
  regs <- Filter(function(r) nzchar(r$outcome) && length(r$predictors), regs)
  cdf <- df; model_based <- FALSE
  if (length(opts$constructs) && length(opts$paths)) {
    model_based <- TRUE
    cons <- lapply(opts$constructs, function(c) list(name = as.character(c$name), ind = intersect(as.character(unlist(c$indicators)), names(df))))
    cons <- Filter(function(c) length(c$ind) > 0, cons)
    for (c in cons) { M <- as.matrix(df[, c$ind, drop = FALSE]); if (!is.null(code)) M[!is.na(M) & M == code] <- NA; cdf[[paste0(".lv.", c$name)]] <- rowMeans(M, na.rm = TRUE) }
    tos <- unique(vapply(opts$paths, function(x) as.character(x$to), ""))
    regs <- lapply(tos, function(t) list(outcome = paste0(".lv.", t), predictors = paste0(".lv.", unique(vapply(Filter(function(x) identical(as.character(x$to), t), opts$paths), function(x) as.character(x$from), "")))))
  }
  ck <- screen_cook(cdf, regs)
  cook_cut <- if (cook_rule == "1") 1 else 4 / n
  id_col <- as.character(opts$idColumn %||% "")
  ids <- if (nzchar(id_col) && id_col %in% names(df)) as.character(df[[id_col]]) else NULL
  flags <- list(
    straight = !is.na(sds) & sds < sd_cut,
    missing = miss > miss_cut,
    range = oor > 0,
    mahalanobis = !is.na(pv) & pv < maha_p,
    cook = !is.na(ck$cook) & ck$cook > cook_cut)
  any_flag <- Reduce(`|`, flags)
  rows <- which(any_flag)
  cases <- lapply(rows, function(i) list(
    row = i, id = if (!is.null(ids)) ids[i] else NULL,
    reasons = as.list(names(flags)[vapply(flags, function(f) f[i], TRUE)]),
    sd = num_or_null(sds[i]), missingShare = miss[i], outOfRange = as.integer(oor[i]),
    d2 = num_or_null(mh$d2[i]), p = num_or_null(pv[i]), cook = num_or_null(ck$cook[i])))
  # Several flags first, then by strength
  ord <- order(-vapply(cases, function(c) length(c$reasons), 0L), vapply(cases, function(c) c$p %||% 1, 0))
  regs_out <- lapply(ck$regressions, function(r) list(outcome = sub("^\\.lv\\.", "", r$outcome), predictors = as.list(sub("^\\.lv\\.", "", unlist(r$predictors))), n = r$n))
  list(n = n, items = as.list(items), detectedItems = as.list(det$items), likert = likert, scale = if (is.null(scale)) NULL else as.list(scale),
       thresholds = list(sdCut = sd_cut, missingCut = miss_cut, mahaP = maha_p, mahaCritical = stats::qchisq(1 - maha_p, df = p), cookRule = cook_rule, cookCut = cook_cut),
       counts = lapply(flags, sum), flagged = sum(any_flag), cases = cases[ord],
       cook = list(modelBased = model_based, regressions = regs_out),
       notes = as.list(c(if (fallback) sprintf("No Likert-type items were found, so the %d numeric columns were screened for missing values and multivariate outliers. Straight-lining and out-of-range answers apply to questionnaire items only and were not checked.", length(items))
                         else if (!likert) "The screened columns are not all on one response scale, so straight-lining and out-of-range answers were not checked.",
                         mh$note, if (!length(ck$regressions)) "Cook's distance was not computed: name an outcome and its predictors, or screen again once the model is drawn.")),
       rCode = screening_code(items, sd_cut, miss_cut, maha_p, cook_rule, regs_out, model_based, code, likert))
}

screening_code <- function(items, sd_cut, miss_cut, maha_p, cook_rule, regs, model_based, code, likert = TRUE) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  v <- function(x) paste0("c(", paste(vapply(x, q, ""), collapse = ", "), ")")
  out <- c("# Data screening (PathLab SEM): flag cases, then decide which to remove",
           sprintf("items <- %s", v(items)),
           "X <- as.matrix(data[, items])",
           if (!is.null(code)) sprintf("X[X == %s] <- NA   # project missing-value code", format(code)),
           if (likert) sprintf("case_sd <- apply(X, 1, function(r) { r <- r[!is.na(r)]; if (length(r) >= 3) sd(r) else NA })   # straight-lining: SD < %s", sd_cut),
           sprintf("missing_share <- rowMeans(is.na(X))                                       # more than %s%% missing", round(100 * miss_cut)),
           "ok <- complete.cases(X)",
           "d2 <- rep(NA, nrow(X)); d2[ok] <- mahalanobis(X[ok, ], colMeans(X[ok, ]), cov(X[ok, ]))",
           sprintf("maha_p <- pchisq(d2, df = ncol(X), lower.tail = FALSE)                     # multivariate outlier: p < %s", format(maha_p)))
  if (length(regs)) {
    out <- c(out, if (model_based) "# Cook's distance: each endogenous construct (mean of its items) on its predictors" else "# Cook's distance")
    for (r in regs) {
      if (model_based) out <- c(out, sprintf("# %s ~ %s (construct means)", r$outcome, paste(unlist(r$predictors), collapse = " + ")))
      else out <- c(out, sprintf("fit <- lm(%s ~ %s, data = data); cooks <- cooks.distance(fit)   # flag > %s", r$outcome, paste(unlist(r$predictors), collapse = " + "), if (cook_rule == "1") "1" else "4 / n"))
    }
  }
  out <- c(out, if (likert) sprintf("flagged <- which(case_sd < %s | missing_share > %s | maha_p < %s)", sd_cut, miss_cut, format(maha_p))
                else sprintf("flagged <- which(missing_share > %s | maha_p < %s)", miss_cut, format(maha_p)))
  paste(out, collapse = "\n")
}

#' Save the flags as a 0/1 variable of the dataset (kept apart from the uploaded data, like other derived variables).
screen_flag_variable <- function(proj, did, rows, name) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.")
  name <- trimws(as.character(name %||% "screen_flag"))
  if (!nzchar(name) || nchar(name) > 64) api_error(400, "derived_name", "Variable names must have 1 to 64 characters.")
  if (name %in% names(ds$data) && is.null(ds$derived[[name]])) api_error(409, "derived_name", sprintf("The dataset already has a column named '%s'.", name))
  n <- nrow(ds$data)
  vals <- rep(0L, n); rr <- as.integer(unlist(rows)); vals[rr[rr >= 1 & rr <= n]] <- 1L
  ds$derived <- ds$derived %||% list()
  ds$derived[[name]] <- list(name = name, values = vals, kind = "screen_flag", kindLabel = unname(DERIVED_KINDS["screen_flag"]), analysisId = "", analysisName = "Data screening",
                             resultId = "", set = "", from = "", label = "Flagged in data screening (1 = flagged)", nValues = n, createdAt = now_iso())
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

#' Report paragraph for the cases removed in data screening (`rec` is the record the app keeps in the project).
screening_report_html <- function(rec) {
  if (is.null(rec) || is.null(rec$nBefore)) return(NULL)
  th <- rec$thresholds %||% list()
  br <- rec$byReason %||% list()
  part <- function(k, lab) { v <- as.integer(br[[k]] %||% 0L); if (v > 0) sprintf("%d %s", v, lab) else NULL }
  why <- unlist(list(part("straight", "for straight-lining"), part("missing", "for missing answers"), part("range", "for out-of-range answers"),
                     part("mahalanobis", "as multivariate outliers"), part("cook", "as influential cases")))
  cook_txt <- if (isTRUE(rec$cookModelBased)) "Cook's distance from regressions of each endogenous construct (mean item scores) on its predictors" else "Cook's distance from the regression the user specified"
  cook_cut <- if (identical(th$cookRule, "1")) "D &gt; 1" else "D &gt; 4/n"
  paste0("<h3>Data screening</h3><p>Before the analysis, the ", rec$items %||% "", " Likert-type items were screened for straight-lining ",
    "(standard deviation of a respondent's answers below ", th$sdCut %||% 0.25, "), missing answers (more than ", round(100 * (th$missingCut %||% 0.15)),
    "% of the items), answers outside the response scale, multivariate outliers (Mahalanobis distance, p &lt; ", th$mahaP %||% 0.001,
    ") and influential cases (", cook_txt, ", ", cook_cut, "). ",
    sprintf("%d of %d cases were removed", as.integer(rec$removed), as.integer(rec$nBefore)),
    if (length(why)) paste0(" (", paste(why, collapse = "; "), "; a case can have more than one reason)") else "",
    sprintf(", leaving n = %d for the analysis.</p>", as.integer(rec$nAfter)))
}
