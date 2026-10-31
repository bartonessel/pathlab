# Computed variables (jamovi style: Data > Compute). A formula such as MEAN(PE1, PE2, PE3, PE4) or IF(age >= 40, 1, 0)
# is parsed with R's parser and checked against a whitelist (jamovi's function names, the dataset's variables,
# arithmetic, comparison and logical operators) before it is evaluated in an environment that contains nothing else.
# The result is kept as a derived column (kind "computed", with its formula); the uploaded data are never changed.

#' Row-wise functions take several variables; ignore_missing = the number of missing values tolerated (jamovi).
cv_rowwise <- function(f) function(..., ignore_missing = 0) {
  m <- do.call(cbind, lapply(list(...), as.numeric))
  out <- apply(m, 1, function(r) { k <- sum(is.na(r)); if (k > ignore_missing || k == length(r)) NA_real_ else f(r[!is.na(r)]) })
  as.numeric(out)
}

COMPUTE_FUNCTIONS <- list(
  # Math
  ABS = list(fn = function(x) abs(x), group = "Math", doc = "ABS(x): absolute value."),
  EXP = list(fn = function(x) exp(x), group = "Math", doc = "EXP(x): e to the power x."),
  LN = list(fn = function(x) log(x), group = "Math", doc = "LN(x): natural logarithm."),
  LOG10 = list(fn = function(x) log10(x), group = "Math", doc = "LOG10(x): base-10 logarithm."),
  SQRT = list(fn = function(x) sqrt(x), group = "Math", doc = "SQRT(x): square root."),
  ROUND = list(fn = function(x, digits = 0) round(x, digits), group = "Math", doc = "ROUND(x, digits = 0): rounded value."),
  FLOOR = list(fn = function(x) floor(x), group = "Math", doc = "FLOOR(x): largest whole number not above x."),
  CEILING = list(fn = function(x) ceiling(x), group = "Math", doc = "CEILING(x): smallest whole number not below x."),
  INT = list(fn = function(x) as.integer(trunc(as.numeric(x))), group = "Math", doc = "INT(x): whole-number part of x."),
  # Statistical, across variables (row-wise)
  MEAN = list(fn = cv_rowwise(mean), group = "Statistical", doc = "MEAN(a, b, ..., ignore_missing = 0): mean of the variables for each row; NA when more values than ignore_missing are missing."),
  SUM = list(fn = cv_rowwise(sum), group = "Statistical", doc = "SUM(a, b, ..., ignore_missing = 0): sum of the variables for each row."),
  MIN = list(fn = cv_rowwise(min), group = "Statistical", doc = "MIN(a, b, ...): smallest of the variables for each row."),
  MAX = list(fn = cv_rowwise(max), group = "Statistical", doc = "MAX(a, b, ...): largest of the variables for each row."),
  MEDIAN = list(fn = cv_rowwise(stats::median), group = "Statistical", doc = "MEDIAN(a, b, ...): median of the variables for each row."),
  STDEV = list(fn = cv_rowwise(function(v) if (length(v) > 1) stats::sd(v) else NA_real_), group = "Statistical", doc = "STDEV(a, b, ...): standard deviation of the variables for each row."),
  VARIANCE = list(fn = cv_rowwise(function(v) if (length(v) > 1) stats::var(v) else NA_real_), group = "Statistical", doc = "VARIANCE(a, b, ...): variance of the variables for each row."),
  # Statistical, down a variable (column-wise; the same value on every row)
  VMEAN = list(fn = function(x) rep(mean(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VMEAN(x): mean of the whole variable."),
  VMED = list(fn = function(x) rep(stats::median(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VMED(x): median of the whole variable."),
  VSUM = list(fn = function(x) rep(sum(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VSUM(x): sum of the whole variable."),
  VSTDEV = list(fn = function(x) rep(stats::sd(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VSTDEV(x): standard deviation of the whole variable."),
  VVAR = list(fn = function(x) rep(stats::var(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VVAR(x): variance of the whole variable."),
  VMIN = list(fn = function(x) rep(min(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VMIN(x): smallest value of the variable."),
  VMAX = list(fn = function(x) rep(max(x, na.rm = TRUE), length(x)), group = "Variable-wise", doc = "VMAX(x): largest value of the variable."),
  VN = list(fn = function(x) rep(sum(!is.na(x)), length(x)), group = "Variable-wise", doc = "VN(x): number of non-missing values."),
  # Transform
  Z = list(fn = function(x) as.numeric(scale(as.numeric(x))), group = "Transform", doc = "Z(x): z-score (mean 0, SD 1)."),
  SCALE = list(fn = function(x) as.numeric(scale(as.numeric(x))), group = "Transform", doc = "SCALE(x): standardised value (same as Z)."),
  NORM = list(fn = function(x) { x <- as.numeric(x); lo <- min(x, na.rm = TRUE); hi <- max(x, na.rm = TRUE); if (!is.finite(lo) || hi == lo) ifelse(is.na(x), NA_real_, 0) else (x - lo) / (hi - lo) },
              group = "Transform", doc = "NORM(x): rescaled to 0-1, (x - min) / (max - min) (a constant variable becomes 0)."),
  RANK = list(fn = function(x) { r <- rank(x, na.last = "keep", ties.method = "average"); r }, group = "Transform", doc = "RANK(x): rank of each value (ties averaged)."),
  BOXCOX = list(fn = function(x, lambda = 0) if (lambda == 0) log(x) else (x^lambda - 1) / lambda, group = "Transform", doc = "BOXCOX(x, lambda = 0): Box-Cox transformation (lambda 0 = natural log)."),
  # Logical
  IF = list(fn = function(cond, yes, no = NA) ifelse(cond, yes, no), group = "Logical", doc = "IF(condition, value if true, value if false): e.g. IF(age >= 40, 'older', 'younger')."),
  IFMISS = list(fn = function(x, yes, no = x) ifelse(is.na(x), yes, no), group = "Logical", doc = "IFMISS(x, value if missing, value otherwise)."),
  ISMISS = list(fn = function(x) is.na(x), group = "Logical", doc = "ISMISS(x): TRUE when x is missing."),
  NOT = list(fn = function(x) !x, group = "Logical", doc = "NOT(x): logical negation."),
  # Text and reference
  TEXT = list(fn = function(x) as.character(x), group = "Text", doc = "TEXT(x): value as text."),
  VALUE = list(fn = function(x) suppressWarnings(as.numeric(as.character(x))), group = "Text", doc = "VALUE(x): text converted to a number."),
  ROW = list(fn = function() NULL, group = "Reference", doc = "ROW(): row number.")
)
COMPUTE_OPERATORS <- c("+", "-", "*", "/", "^", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!", "(", "%%")

#' jamovi formula text -> R expression text: "and", "or", "not" keywords, "=" comparisons are not changed
#' (jamovi uses ==), and a leading "=" is dropped.
cv_translate <- function(f) {
  f <- trimws(sub("^\\s*=", "", f))
  # Replace keywords outside quotes and backticks.
  parts <- strsplit(f, "(?=[\"'`])", perl = TRUE)[[1]]
  out <- character(0); q <- NULL
  for (p in parts) {
    if (is.null(q) && p %in% c("\"", "'", "`")) { q <- p; out <- c(out, p); next }
    if (!is.null(q)) { out <- c(out, p); if (p == q) q <- NULL; next }
    p <- gsub("\\band\\b", "&", p, ignore.case = TRUE)
    p <- gsub("\\bor\\b", "|", p, ignore.case = TRUE)
    p <- gsub("\\bnot\\b", "!", p, ignore.case = TRUE)
    out <- c(out, p)
  }
  paste(out, collapse = "")
}

#' Check every node of the parsed formula against the whitelist; returns the variables it uses.
cv_check <- function(e, vars) {
  used <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      fn <- x[[1]]
      if (!is.symbol(fn)) api_error(400, "compute_formula", "This formula form is not supported.")
      nm <- as.character(fn)
      if (!(nm %in% COMPUTE_OPERATORS || nm %in% names(COMPUTE_FUNCTIONS)))
        api_error(400, "compute_function", sprintf("Unknown function '%s'. Use the functions listed under fx (e.g. MEAN, SUM, IF, Z).", nm))
      args <- as.list(x)[-1]
      for (a in args) walk(a)
    } else if (is.symbol(x)) {
      nm <- as.character(x)
      if (!nzchar(nm)) return()
      if (nm %in% c("TRUE", "FALSE", "NA")) return()
      if (!nm %in% vars) api_error(400, "compute_variable", sprintf("There is no variable called '%s'. Use backticks for names with spaces, e.g. `my var`.", nm))
      used <<- c(used, nm)
    } else if (!(is.numeric(x) || is.character(x) || is.logical(x) || is.null(x))) {
      api_error(400, "compute_formula", "This formula form is not supported.")
    }
  }
  walk(e)
  unique(used)
}

#' Evaluate a jamovi-style formula on the dataset (uploaded + derived columns). Returns list(values, used).
compute_values <- function(df, formula) {
  txt <- cv_translate(formula)
  if (!nzchar(txt)) api_error(400, "compute_formula", "Enter a formula, for example MEAN(PE1, PE2).")
  e <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(err) api_error(400, "compute_syntax", paste("The formula could not be read:", sanitize_message(conditionMessage(err)))))
  if (length(e) != 1) api_error(400, "compute_formula", "Enter a single formula.")
  e <- e[[1]]
  used <- cv_check(e, names(df))
  env <- new.env(parent = emptyenv())
  for (op in setdiff(COMPUTE_OPERATORS, "(")) assign(op, get(op, envir = baseenv()), envir = env)
  assign("(", get("(", envir = baseenv()), envir = env)
  assign("TRUE", TRUE, envir = env); assign("FALSE", FALSE, envir = env); assign("NA", NA, envir = env)
  n <- nrow(df)
  for (fname in names(COMPUTE_FUNCTIONS)) assign(fname, COMPUTE_FUNCTIONS[[fname]]$fn, envir = env)
  assign("ROW", function() seq_len(n), envir = env)
  for (v in used) assign(v, df[[v]], envir = env)
  val <- tryCatch(eval(e, env), error = function(err) api_error(400, "compute_eval", paste("The formula could not be computed:", sanitize_message(conditionMessage(err)))))
  if (is.factor(val)) val <- as.character(val)
  if (length(val) == 1) val <- rep(val, n)
  if (length(val) != n) api_error(400, "compute_length", "The formula must give one value per row.")
  if (is.logical(val)) val <- as.integer(val)
  list(values = if (is.numeric(val)) as.numeric(val) else as.character(val), used = used)
}

#' Add or replace a computed variable (a derived column with its formula).
add_computed_variable <- function(proj, did, name, formula, description = "", kind = "computed") {
  kind <- if (identical(kind, "filter")) "filter" else "computed"
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name) || nchar(name) > 64) api_error(400, "derived_name", "Give the variable a name of 1 to 64 characters.")
  if (name %in% names(ds$data)) api_error(409, "derived_name", sprintf("The dataset already has a column named '%s'.", name), "Uploaded columns are never overwritten; choose another name.")
  full <- dataset_with_derived(ds)$data
  full[[name]] <- NULL   # a variable may be recomputed, but not from itself
  cv <- compute_values(full, as.character(formula %||% ""))
  if (kind == "filter") cv$values <- filter_values(cv$values)
  ds$derived <- ds$derived %||% list()
  ds$derived[[name]] <- computed_entry(name, cv, kind, formula, description)
  # Variables computed from this one (a mean of means) follow its new values.
  if (any(vapply(ds$derived, function(v) name %in% unlist(v$uses), logical(1)))) ds <- recompute_computed(ds)
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

computed_entry <- function(name, cv, kind, formula, description) {
  list(name = name, values = cv$values, kind = kind, kindLabel = if (kind == "filter") "Row filter" else "Computed variable", nTrue = if (kind == "filter") sum(cv$values == 1, na.rm = TRUE) else NULL, analysisId = "", analysisName = "",
       resultId = "", set = "", from = "", label = as.character(formula), formula = as.character(formula),
       description = as.character(description %||% ""), uses = as.list(cv$used), nValues = sum(!is.na(cv$values)), createdAt = now_iso())
}

#' Add or replace several computed variables and filters in one go (a project restored on a restarted server, every
#' formula again after a rename, or Bulk compute). One may use another whatever order they come in: a definition whose
#' variables are not there yet is tried again once the others are done. `defs`: list of list(name, formula, description,
#' kind). Returns list(ds = the dataset as the analyses see it, failed = list(list(name, message))).
add_computed_variables <- function(proj, did, defs) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  ds$derived <- ds$derived %||% list()
  full <- dataset_with_derived(ds)$data
  pending <- defs; errs <- list()
  repeat {
    left <- list()
    for (d in pending) {
      name <- trimws(as.character(d$name %||% "")); kind <- if (identical(d$kind, "filter")) "filter" else "computed"
      r <- tryCatch({
        if (!nzchar(name) || nchar(name) > 64) api_error(400, "derived_name", "Give the variable a name of 1 to 64 characters.")
        if (name %in% names(ds$data)) api_error(409, "derived_name", sprintf("The dataset already has a column named '%s'.", name), "Uploaded columns are never overwritten; choose another name.")
        f <- full; f[[name]] <- NULL
        cv <- compute_values(f, as.character(d$formula %||% ""))
        if (kind == "filter") cv$values <- filter_values(cv$values)
        cv
      }, error = function(e) e)
      if (inherits(r, "error")) { left[[length(left) + 1]] <- d; errs[[name]] <- conditionMessage(r); next }
      ds$derived[[name]] <- computed_entry(name, r, kind, d$formula, d$description)
      full[[name]] <- r$values
    }
    if (!length(left) || length(left) == length(pending)) break
    pending <- left
  }
  if (length(left) < length(defs)) ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  list(ds = dataset_with_derived(ds), failed = lapply(left, function(d) { n <- trimws(as.character(d$name %||% "")); list(name = n, message = errs[[n]]) }))
}

compute_function_list <- function() unname(lapply(names(COMPUTE_FUNCTIONS), function(n) list(name = n, group = COMPUTE_FUNCTIONS[[n]]$group, doc = COMPUTE_FUNCTIONS[[n]]$doc)))

#' A row filter's values: 1 = row kept, 0 = row excluded (logical or numeric results; missing = excluded).
filter_values <- function(v) {
  x <- suppressWarnings(as.numeric(v))
  if (all(is.na(x) | x %in% c(0, 1))) { x[is.na(x)] <- 0; return(x) }
  api_error(400, "filter_formula", "A filter must be a condition that is true or false for each row, for example age >= 18 or gender == 'Female'.")
}
