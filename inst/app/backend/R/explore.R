# Attribute explorer for machine learning, after WEKA Explorer's Preprocess panel: for every attribute its type,
# missing / distinct / unique counts, statistics (numeric: minimum, maximum, mean, standard deviation; nominal: the
# count of each label) and a histogram whose bars are split by the class attribute. Numeric bins are equal-width
# between the minimum and the maximum (Sturges' rule for their number), as WEKA draws them. Nothing is changed.

EXPLORE_MAX_LEVELS <- 50L   # text columns with more labels are treated as free text (WEKA's string attributes)

explore_kind <- function(x) {
  if (is.numeric(x)) return("numeric")
  if (inherits(x, c("Date", "POSIXt"))) return("date")
  v <- unique(x[!is.na(x)])
  if (length(v) > EXPLORE_MAX_LEVELS) "string" else "nominal"
}

#' Labels of a nominal attribute: those declared in the file (ARFF header order, including labels no case has, as
#' WEKA lists them), then any other value in the data; without a declaration, in order of first appearance.
explore_levels <- function(x, declared = NULL) {
  v <- x[!is.na(x)]
  if (is.logical(v)) v <- as.character(v)
  seen <- unique(as.character(v))
  if (is.null(declared)) return(seen)
  c(declared, setdiff(seen, declared))
}

#' opts: class (attribute name or ""), bins (optional fixed number of bins), missingCode.
#' declared: the label set of each nominal attribute as the file declares it (named list; ARFF), or NULL.
run_explore <- function(df, opts = list(), declared = NULL) {
  code <- suppressWarnings(as.numeric(opts$missingCode %||% NA)); if (is.na(code)) code <- NULL
  for (cn in names(df)) if (is.numeric(df[[cn]]) && !is.null(code)) df[[cn]][!is.na(df[[cn]]) & df[[cn]] == code] <- NA
  n <- nrow(df)
  cls <- as.character(opts$class %||% "")[1]
  if (!nzchar(cls) || !cls %in% names(df)) cls <- ""
  fixed_bins <- suppressWarnings(as.integer(opts$bins %||% NA))
  ckind <- if (nzchar(cls)) explore_kind(df[[cls]]) else "none"
  cvals <- if (ckind == "nominal") as.character(df[[cls]]) else NULL
  clev <- if (ckind == "nominal") explore_levels(df[[cls]], declared[[cls]]) else character(0)
  split <- function(idx) {   # counts per class label for the rows idx (missing class last, if any)
    if (!length(clev)) return(NULL)
    cv <- cvals[idx]
    c(as.list(vapply(clev, function(l) sum(cv == l, na.rm = TRUE), 0L, USE.NAMES = FALSE)))
  }
  attrs <- lapply(seq_along(df), function(j) {
    nm <- names(df)[j]; x <- df[[j]]; kind <- explore_kind(x)
    miss <- sum(is.na(x)); v <- x[!is.na(x)]
    tab <- table(as.character(v))
    out <- list(index = j, name = nm, type = kind, missing = miss, missingPct = if (n) miss / n else 0,
                distinct = length(tab), unique = sum(tab == 1), uniquePct = if (length(v)) sum(tab == 1) / length(v) else 0)
    if (kind == "numeric" || kind == "date") {
      xv <- as.numeric(v)
      out$stats <- if (length(xv)) list(min = min(xv), max = max(xv), mean = mean(xv), sd = if (length(xv) > 1) stats::sd(xv) else NA_real_) else NULL
      if (length(xv)) {
        lo <- min(xv); hi <- max(xv)
        k <- if (!is.na(fixed_bins) && fixed_bins >= 1) min(fixed_bins, 100L) else if (hi == lo) 1L else min(max(grDevices::nclass.Sturges(xv), 1L), 30L)
        if (length(unique(xv)) < k) k <- max(length(unique(xv)), 1L)
        br <- if (hi == lo) c(lo, hi) else seq(lo, hi, length.out = k + 1)
        b <- if (hi == lo) rep(1L, length(xv)) else pmin(findInterval(xv, br, rightmost.closed = TRUE), k)
        rows <- which(!is.na(x))
        out$bins <- lapply(seq_len(k), function(i) {
          idx <- rows[b == i]
          list(lo = br[i], hi = br[min(i + 1, length(br))], count = length(idx), byClass = split(idx))
        })
      }
    } else if (kind == "nominal") {
      lev <- explore_levels(x, declared[[nm]])
      cx <- as.character(x)
      out$levels <- lapply(lev, function(l) { idx <- which(!is.na(cx) & cx == l); list(label = l, count = length(idx), byClass = split(idx)) })
    }
    out
  })
  list(n = n, nAttributes = ncol(df), class = if (nzchar(cls)) cls else NULL, classType = ckind, classLevels = as.list(clev),
       classMissing = if (ckind == "nominal") sum(is.na(cvals)) else 0L, attributes = attrs,
       rCode = explore_code(cls, ckind))
}

explore_code <- function(cls, ckind) {
  q <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
  c("# Attribute summaries (as in WEKA's Preprocess panel)",
    "summary(data)",
    "sapply(data, function(x) c(missing = sum(is.na(x)), distinct = length(unique(na.omit(x))),",
    "                           unique = sum(table(x) == 1)))",
    if (ckind == "nominal") c(sprintf("# Each attribute split by the class %s", cls),
      sprintf("for (v in setdiff(names(data), %s)) {", q(cls)),
      "  x <- data[[v]]",
      "  if (is.numeric(x)) x <- cut(x, breaks = nclass.Sturges(na.omit(x)), include.lowest = TRUE)",
      sprintf("  print(table(x, data[[%s]], dnn = c(v, %s)))", q(cls), q(cls)),
      "}")) |> paste(collapse = "\n")
}

# ---- Visualize (WEKA's scatter plot matrix) -------------------------------------------------------------------
# The values of every numeric and nominal attribute for plotting (nominal values as 0-based label indices, missing as
# null), and Pearson correlations of the numeric attributes on pairwise complete cases. Large datasets are sampled
# (seeded) to at most maxRows cases; the sample is reported, the data are never changed.
VISUALIZE_MAX_ROWS <- 20000L

run_visualize_data <- function(df, opts = list(), declared = NULL) {
  code <- suppressWarnings(as.numeric(opts$missingCode %||% NA)); if (is.na(code)) code <- NULL
  for (cn in names(df)) if (is.numeric(df[[cn]]) && !is.null(code)) df[[cn]][!is.na(df[[cn]]) & df[[cn]] == code] <- NA
  n <- nrow(df)
  max_rows <- suppressWarnings(as.integer(opts$maxRows %||% VISUALIZE_MAX_ROWS)); if (is.na(max_rows) || max_rows < 100L) max_rows <- VISUALIZE_MAX_ROWS
  rows <- seq_len(n)
  if (n > max_rows) { set.seed(as.integer(opts$seed %||% 1L)); rows <- sort(sample.int(n, max_rows)) }
  keep <- names(df)[vapply(df, function(x) explore_kind(x) %in% c("numeric", "date", "nominal"), TRUE)]
  cols <- lapply(keep, function(nm) {
    x <- df[[nm]][rows]
    if (explore_kind(df[[nm]]) == "nominal") {
      lev <- explore_levels(df[[nm]], declared[[nm]])
      list(name = nm, type = "nominal", levels = as.list(lev), values = as.list(match(as.character(x), lev) - 1L))
    } else {
      v <- as.numeric(x)
      list(name = nm, type = "numeric", values = as.list(v),
           min = if (any(!is.na(v))) min(v, na.rm = TRUE) else NULL, max = if (any(!is.na(v))) max(v, na.rm = TRUE) else NULL)
    }
  })
  num <- keep[vapply(keep, function(nm) explore_kind(df[[nm]]) != "nominal", TRUE)]
  r <- if (length(num) >= 2) suppressWarnings(stats::cor(sapply(num, function(nm) as.numeric(df[[nm]])), use = "pairwise.complete.obs")) else NULL
  corr <- lapply(keep, function(a) lapply(keep, function(b) if (!is.null(r) && a %in% num && b %in% num) { v <- r[a, b]; if (is.na(v)) NULL else unname(v) } else NULL))
  list(n = n, shown = length(rows), sampled = length(rows) < n, attributes = cols, correlations = corr,
       rCode = paste(c("# Scatter plot matrix (as WEKA's Visualize panel), coloured by a class attribute",
                       "num <- data[sapply(data, is.numeric)]",
                       "pairs(num, pch = 4, cex = 0.6)",
                       "round(cor(num, use = \"pairwise.complete.obs\"), 3)   # Pearson correlations, pairwise complete cases"), collapse = "\n"))
}
