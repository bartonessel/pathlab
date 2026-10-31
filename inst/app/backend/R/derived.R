# Derived variables: per-row values that an analysis produced (factor scores, construct scores, profile or segment
# membership), added to a dataset only when the user asks for it. The uploaded data are never changed: derived
# columns are kept apart in `ds$derived`, appended when the dataset is read for an analysis, labelled with their
# source analysis and result, and removed again on request.
#
# Each analysis stores its per-row values next to its result JSON (`<result>.derived.rds`), as
#   list(nInput = rows in the dataset, rows = dataset rows the analysis used, tables = list(<set> = data.frame(...)))
# so a variable is built from exactly the numbers R computed; rows the analysis did not use are missing (NA).

DERIVED_KINDS <- c(efa_scores = "Factor scores (EFA/PCA)", pls_scores = "Construct scores (PLS-SEM)", cb_scores = "Factor scores (CB-SEM)", gsca_scores = "Component scores (GSCA)",
                   lpa_profile = "Profile membership (LPA)", fimix_segment = "Segment membership (FIMIX-PLS)",
                   qca_sets = "Calibrated set membership (QCA)", ml_predictions = "Predictions (machine learning)", reg_values = "Regression values (Statistics)",
                   computed = "Computed variable", text_codes = "Code presence (text analysis)", screen_flag = "Data screening flag")

derived_rds_path <- function(json_path) sub("\\.json$", ".derived.rds", json_path)

#' Split the per-row values off a result and save them next to its JSON file.
save_result_derived <- function(res, json_path) {
  d <- res$.derived
  res$.derived <- NULL
  if (!is.null(d)) {
    saveRDS(d, derived_rds_path(json_path))
    # Which values can be saved as variables, by solution / table (the values themselves stay on the server).
    res$derivedColumns <- lapply(d$tables, function(t) as.list(names(t)))
  }
  res
}

read_result_derived <- function(proj, rid) {
  path <- proj$results[[rid]]
  if (is.null(path) || !file.exists(path)) api_error(404, "result_not_found", "The analysis result was not found.", "Run the analysis again.")
  dp <- derived_rds_path(path)
  if (!file.exists(dp)) api_error(404, "no_derived", "This result has no per-case values to save.", "Run the analysis again with this version of PathLab SEM.")
  readRDS(dp)
}

#' The dataset as the analyses see it: the uploaded columns plus the derived columns.
dataset_with_derived <- function(ds) {
  if (length(ds$edits)) ds$data <- apply_edits(ds$data, ds$edits)
  for (v in ds$derived %||% list()) ds$data[[v$name]] <- v$values
  ds
}

derived_meta <- function(v) v[setdiff(names(v), "values")]

#' Column profiles of the uploaded and derived columns (derived ones carry their source).
refresh_derived_profile <- function(ds) {
  full <- dataset_with_derived(ds)
  prof <- profile_dataset(full$data)
  for (i in seq_along(prof)) {
    v <- ds$derived[[prof[[i]]$name]]
    if (!is.null(v)) prof[[i]]$derived <- derived_meta(v)
  }
  ds$profile <- prof
  ds
}

#' Add columns from a stored result. `body`: list(resultId, set, columns = list(list(from, name)), source = list(analysisId, analysisName, kind)).
add_derived_variables <- function(proj, did, body) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  rid <- as.character(body$resultId %||% "")
  res <- read_result(proj, rid)
  if (!identical(as.character(res$datasetId %||% ""), did))
    api_error(409, "derived_dataset", "These results were computed on another dataset.", "Run the analysis again on the current dataset.")
  d <- read_result_derived(proj, rid)
  if (!identical(as.integer(d$nInput), nrow(ds$data)))
    api_error(409, "derived_rows", "The number of rows of the dataset changed since the analysis ran.", "Run the analysis again.")
  set <- as.character(body$set %||% "")
  tab <- d$tables[[set]]
  if (is.null(tab)) api_error(400, "derived_set", "These values are not available for the chosen solution.")
  src <- body$source %||% list()
  kind <- as.character(src$kind %||% "")
  if (!kind %in% names(DERIVED_KINDS)) api_error(400, "derived_kind", "Unknown kind of derived variable.")
  cols <- body$columns %||% list()
  if (!length(cols)) api_error(400, "derived_columns", "Choose at least one variable to save.")
  names_new <- vapply(cols, function(c) trimws(as.character(c$name %||% "")), "")
  bad <- names_new[!nzchar(names_new) | nchar(names_new) > 64]
  if (length(bad)) api_error(400, "derived_name", "Variable names must have 1 to 64 characters.")
  if (anyDuplicated(names_new)) api_error(400, "derived_name", "Each new variable needs its own name.")
  clash <- intersect(names_new, names(ds$data))
  if (length(clash)) api_error(409, "derived_name", sprintf("The dataset already has a column named %s.", paste(sprintf("'%s'", clash), collapse = ", ")),
                               "Choose another name; uploaded columns are never overwritten.")
  ds$derived <- ds$derived %||% list()
  for (j in seq_along(cols)) {
    from <- as.character(cols[[j]]$from %||% "")
    if (!from %in% names(tab)) api_error(400, "derived_column", sprintf("'%s' is not available in these results.", from))
    vals <- rep(NA_real_, d$nInput)
    vals[d$rows] <- as.numeric(tab[[from]])
    if (kind %in% c("lpa_profile", "fimix_segment") && from %in% c("profile", "segment")) vals <- as.integer(vals)
    ds$derived[[names_new[j]]] <- list(name = names_new[j], values = vals, kind = kind, kindLabel = unname(DERIVED_KINDS[kind]),
                                       analysisId = as.character(src$analysisId %||% ""), analysisName = as.character(src$analysisName %||% ""),
                                       resultId = rid, set = set, from = from, label = as.character(cols[[j]]$label %||% from),
                                       nValues = sum(!is.na(vals)), createdAt = now_iso())
  }
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

remove_derived_variable <- function(proj, did, name) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  if (is.null(ds$derived[[name]])) api_error(404, "derived_not_found", sprintf("There is no derived variable named '%s'.", name))
  ds$derived[[name]] <- NULL
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

# ---- Cell edits (jamovi-style spreadsheet editing) -------------------------------------------------------------
# Edits are kept as a list of changed cells next to the uploaded data, applied when the data are read for an
# analysis (dataset_with_derived) and listed, highlighted and revertible in the app. The uploaded file itself is
# never changed, and every edit is the user's explicit action.

#' The uploaded data with the user's cell edits applied.
apply_edits <- function(data, edits) {
  for (e in edits %||% list()) {
    col <- e$column
    if (!col %in% names(data) || e$row < 1 || e$row > nrow(data)) next
    x <- data[[col]]
    v <- e$value
    if (is.null(v)) { x[e$row] <- NA }
    else if (is.factor(x)) { if (!v %in% levels(x)) levels(x) <- c(levels(x), v); x[e$row] <- v }
    else x[e$row] <- v
    data[[col]] <- x
  }
  data
}

#' Validate and store the complete set of edits (replacing the previous set). `edits`: list(list(row, column, value)).
set_dataset_edits <- function(proj, did, edits) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  n <- nrow(ds$data)
  out <- list()
  for (e in edits %||% list()) {
    row <- suppressWarnings(as.integer(e$row %||% NA))[1]
    col <- as.character(e$column %||% "")[1]
    if (is.na(row) || row < 1 || row > n) api_error(400, "edit_row", sprintf("Row %s is not in the dataset.", format(e$row)))
    if (!col %in% names(ds$data)) api_error(400, "edit_column", sprintf("'%s' is not an uploaded column; computed and derived variables are not edited by hand.", col))
    x <- ds$data[[col]]
    raw <- e$value
    if (is.list(raw)) raw <- unlist(raw)
    val <- if (is.null(raw) || !length(raw) || (is.character(raw) && !nzchar(trimws(raw[1])))) NULL else raw[1]
    if (!is.null(val)) {
      if (inherits(x, c("Date", "POSIXt"))) api_error(400, "edit_type", sprintf("'%s' holds dates, which cannot be edited here.", col))
      if (is.numeric(x)) {
        num <- suppressWarnings(as.numeric(val))
        if (is.na(num)) api_error(400, "edit_type", sprintf("'%s' is numeric: enter a number, or leave the cell empty for a missing value.", col))
        val <- if (is.integer(x) && num == round(num) && abs(num) < .Machine$integer.max) as.integer(num) else num
        if (is.integer(x) && !is.integer(val)) val <- num
      } else if (is.logical(x)) {
        l <- toupper(as.character(val))
        if (!l %in% c("TRUE", "FALSE", "1", "0")) api_error(400, "edit_type", sprintf("'%s' holds TRUE / FALSE values.", col))
        val <- l %in% c("TRUE", "1")
      } else val <- as.character(val)
    }
    orig <- x[row]
    same <- if (is.null(val)) is.na(orig) else !is.na(orig) && identical(as.character(orig), as.character(val))
    if (same) next   # back to the uploaded value: no edit
    out[[paste(row, col, sep = "\r")]] <- list(row = row, column = col, value = val, original = if (is.na(orig)) NULL else if (is.factor(orig)) as.character(orig) else orig)
  }
  # Integer columns receiving decimals become numeric when the edits are applied.
  ds$edits <- unname(out)
  ds <- recompute_computed(ds)
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

#' Recompute computed variables after the data changed: one computed from another (a mean of means) comes after it,
#' otherwise the order they were defined in.
recompute_computed <- function(ds) {
  for (nm in derived_order(ds$derived %||% list())) {
    v <- ds$derived[[nm]]
    if (!v$kind %in% c("computed", "filter")) next
    full <- dataset_with_derived(ds)$data
    full[[nm]] <- NULL
    cv <- tryCatch(compute_values(full, v$formula), error = function(e) NULL)
    if (is.null(cv)) next
    if (identical(v$kind, "filter")) { cv$values <- tryCatch(filter_values(cv$values), error = function(e) NULL); if (is.null(cv$values)) next; v$nTrue <- sum(cv$values == 1) }
    v$values <- cv$values; v$nValues <- sum(!is.na(cv$values)); v$createdAt <- now_iso()
    ds$derived[[nm]] <- v
  }
  ds
}

#' Names of the derived variables, each after the derived variables it uses (a cycle keeps the defined order).
derived_order <- function(derived) {
  left <- names(derived); out <- character()
  while (length(left)) {
    ready <- left[vapply(left, function(n) !any(setdiff(unlist(derived[[n]]$uses), n) %in% left), logical(1))]
    if (!length(ready)) ready <- left
    out <- c(out, ready); left <- setdiff(left, ready)
  }
  out
}

edits_payload <- function(ds)unname(lapply(ds$edits %||% list(), function(e) list(row = e$row, column = e$column, value = e$value, original = e$original)))

# ---- Data structure (jamovi's Add / Delete variables and rows) ------------------------------------------------
# The uploaded data are kept as `ds$base`; added data variables, appended rows and deleted rows are replayed on a copy
# (`ds$data`), so the upload itself never changes. Cell edits refer to the rows and columns after these changes.

normalize_structure <- function(raw) {
  raw <- raw %||% list()
  cols <- Filter(function(c) is.list(c) && nzchar(trimws(as.character(c$name %||% ""))), raw$addColumns %||% list())
  ty <- raw$types %||% list()
  ty <- ty[vapply(ty, function(t) is.character(t) && length(t) == 1 && t %in% c("integer", "decimal", "text"), logical(1))]
  # jamovi's Missing values: per variable, the values that mean "missing" (e.g. 99, -1, "N/A").
  mv <- lapply(raw$missing %||% list(), function(v) { v <- unique(trimws(as.character(unlist(v)))); utils::head(v[!is.na(v) & nzchar(v)], 20) })
  mv <- mv[vapply(mv, length, 0L) > 0]
  # jamovi's rename: the original (uploaded or added) column name -> its current name.
  rn <- raw$renames %||% list()
  rn <- lapply(rn[nzchar(names(rn))], function(v) substr(trimws(as.character(v)[1]), 1, 64))
  rn <- rn[vapply(rn, function(v) !is.na(v) && nzchar(v), logical(1)) & names(rn) != unlist(rn)]
  list(addColumns = lapply(cols, function(c) list(name = substr(trimws(as.character(c$name)), 1, 64), type = if (identical(c$type, "text")) "text" else "numeric")),
       addRows = max(0L, min(100000L, as.integer(raw$addRows %||% 0L))),
       deleteRows = sort(unique(as.integer(unlist(raw$deleteRows %||% list())))),
       types = ty, missing = mv, renames = rn)
}

#' Column names after the renames (all at once, so two variables can swap names). Names must stay unique.
renamed_columns <- function(nm, renames) {
  idx <- match(names(renames), nm)
  out <- nm
  out[idx[!is.na(idx)]] <- unlist(renames)[!is.na(idx)]
  clash <- out[duplicated(out)]
  if (length(clash)) api_error(409, "structure_rename", sprintf("There is already a variable called '%s'.", clash[1]), "Choose another name.")
  out
}

#' Values of x that a "missing values" list declares missing: numbers compare as numbers (99 matches 99.0), text as text.
declared_missing <- function(x, values) {
  if (!length(values) || inherits(x, c("Date", "POSIXt"))) return(rep(FALSE, length(x)))
  if (is.numeric(x)) { v <- suppressWarnings(as.numeric(values)); v <- v[!is.na(v)]; return(!is.na(x) & x %in% v) }
  !is.na(x) & trimws(as.character(x)) %in% values
}

#' A column converted to a data type (jamovi's Integer / Decimal / Text). Text to number reads "3,5" as 3.5 and blanks
#' as missing; anything else that is not a number becomes missing. Integer rounds to whole numbers.
convert_data_type <- function(x, type) {
  if (inherits(x, c("Date", "POSIXt"))) return(if (type == "text") format(x) else x)
  if (type == "text") { out <- if (is.numeric(x)) format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE) else as.character(x); out[is.na(x)] <- NA_character_; return(out) }
  num <- if (is.numeric(x) || is.logical(x)) as.numeric(x) else {
    t <- trimws(as.character(x)); t[!is.na(t) & t == ""] <- NA
    t <- sub("^([+-]?[0-9]*),([0-9]+)$", "\\1.\\2", t)
    suppressWarnings(as.numeric(t))
  }
  if (type == "integer") { r <- round(num); if (all(is.na(r) | abs(r) < .Machine$integer.max)) as.integer(r) else r } else num
}
type_change_note <- function(old, new, type) {
  lost <- sum(!is.na(old) & is.na(new))
  rounded <- if (type == "integer" && is.numeric(old)) sum(!is.na(old) & !is.na(new) & abs(as.numeric(old) - as.numeric(new)) > 1e-9) else 0L
  list(type = type, lost = as.integer(lost), rounded = as.integer(rounded))
}

apply_structure <- function(base, st) {
  d <- base
  if (st$addRows > 0) { extra <- d[rep(NA_integer_, st$addRows), , drop = FALSE]; rownames(extra) <- NULL; d <- rbind(d, extra); rownames(d) <- NULL }
  for (c in st$addColumns) {
    if (c$name %in% names(d)) api_error(409, "structure_column", sprintf("The dataset already has a column named '%s'.", c$name))
    d[[c$name]] <- if (c$type == "text") rep(NA_character_, nrow(d)) else rep(NA_real_, nrow(d))
  }
  # Renames come before data types and missing values, which refer to the current names.
  if (length(st$renames)) names(d) <- renamed_columns(names(d), st$renames)
  notes <- list()
  for (nm in intersect(names(st$types %||% list()), names(d))) {
    new <- convert_data_type(d[[nm]], st$types[[nm]])
    notes[[nm]] <- type_change_note(d[[nm]], new, st$types[[nm]])
    d[[nm]] <- new
  }
  attr(d, "type_notes") <- notes
  for (nm in intersect(names(st$missing %||% list()), names(d))) d[[nm]][declared_missing(d[[nm]], st$missing[[nm]])] <- NA
  del <- st$deleteRows[st$deleteRows >= 1 & st$deleteRows <= nrow(d)]
  if (length(del)) { tn <- attr(d, "type_notes"); d <- d[-del, , drop = FALSE]; rownames(d) <- NULL; attr(d, "type_notes") <- tn }
  if (!nrow(d)) api_error(422, "structure_rows", "A dataset needs at least one row.")
  d
}

set_dataset_structure <- function(proj, did, raw) {
  ds <- proj$datasets[[did]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  st <- normalize_structure(raw)
  if (is.null(ds$base)) ds$base <- ds$data
  for (c in st$addColumns) if (c$name %in% names(ds$derived %||% list())) api_error(409, "structure_column", sprintf("'%s' is already a computed or derived variable.", c$name))
  for (to in unlist(st$renames)) if (to %in% names(ds$derived %||% list())) api_error(409, "structure_rename", sprintf("'%s' is already a computed or derived variable.", to), "Choose another name.")
  ds$data <- apply_structure(ds$base, st)
  ds$meta$typeNotes <- attr(ds$data, "type_notes") %||% list()
  attr(ds$data, "type_notes") <- NULL
  ds$structure <- st
  # Keep only edits that still point at a cell.
  ds$edits <- Filter(function(e) e$row <= nrow(ds$data) && e$column %in% names(ds$data), ds$edits %||% list())
  # Derived columns from analyses are row-aligned with the old data; computed columns and filters are recomputed.
  for (nm in names(ds$derived %||% list())) {
    v <- ds$derived[[nm]]
    if (v$kind %in% c("computed", "filter")) next
    vals <- v$values; n <- nrow(ds$data)
    ds$derived[[nm]]$values <- if (length(vals) >= n) vals[seq_len(n)] else c(vals, rep(NA, n - length(vals)))
  }
  for (nm in names(ds$derived %||% list())) if (ds$derived[[nm]]$kind %in% c("computed", "filter")) ds$derived[[nm]]$values <- rep(NA, nrow(ds$data))
  ds <- recompute_computed(ds)
  ds$meta$nRows <- nrow(ds$data); ds$meta$nCols <- ncol(ds$data)
  ds <- refresh_derived_profile(ds)
  proj$datasets[[did]] <- ds
  dataset_with_derived(ds)
}

#' Statistics data: rows excluded by the active filters are dropped and frequency weights are attached (jamovi's
#' Filters and Weights). `filters`: names of active filter columns; `weights`: a numeric column or NULL.
stats_prefilter <- function(df, filters = NULL, weights = NULL) {
  f <- intersect(as.character(unlist(filters %||% list())), names(df))
  keep <- rep(TRUE, nrow(df))
  for (nm in f) keep <- keep & !is.na(df[[nm]]) & df[[nm]] == 1
  w <- as.character(weights %||% "")[1]
  out <- df[keep, , drop = FALSE]
  rownames(out) <- NULL
  if (!is.na(w) && nzchar(w)) {
    if (!w %in% names(df) || !is.numeric(df[[w]])) api_error(400, "weights", sprintf("The weights variable '%s' must be numeric.", w))
    attr(out, "pl-weights") <- df[[w]][keep]; attr(out, "pl-weights-name") <- w
  }
  attr(out, "pl-filtered") <- sum(!keep)
  out
}

#' Frequency weights as repeated rows (as jmvcore does for jamovi analyses): weights are rounded; rows with a weight
#' below 1 or missing are dropped.
expand_weights <- function(d) {
  w <- attr(d, "jmv-weights", exact = TRUE)
  if (is.null(w)) return(d)
  w <- round(as.numeric(w)); w[is.na(w) | w < 1] <- 0
  out <- d[rep(seq_len(nrow(d)), w), , drop = FALSE]
  rownames(out) <- NULL
  out
}
