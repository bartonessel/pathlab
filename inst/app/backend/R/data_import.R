# Data import, profiling and validation.
#
# Principle: the uploaded data are never changed here. Reading follows fixed,
# documented rules (empty cells and the literal text "NA" are read as missing), and
# the exact same call is written into the exported R code so results can be
# reproduced outside PathLab.

CSV_NA_STRINGS <- c("", "NA")

detect_delimiter <- function(path) {
  first <- tryCatch(readLines(path, n = 5, warn = FALSE, encoding = "UTF-8"), error = function(e) "")
  first <- paste(first, collapse = "\n")
  counts <- c("," = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
              ";" = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
              "\t" = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE))))
  if (all(counts == 0)) return(if (grepl("[ ]", first)) "" else ",")   # "" = any white space (read.table rule)
  names(counts)[which.max(counts)]
}

# Supported data files. Each kind is read with one documented call that the exported R code repeats.
DATA_FORMATS <- list(
  csv    = c("csv", "tsv", "txt", "dat"),        # delimited text (comma, semicolon, tab or white space)
  xlsx   = c("xlsx", "xls"),                     # Excel (readxl)
  ods    = c("ods"),                             # OpenDocument spreadsheet (readODS)
  spss   = c("sav", "zsav", "por"),              # SPSS (haven)
  stata  = c("dta"),                             # Stata (haven)
  sas    = c("sas7bdat", "xpt"),                 # SAS (haven)
  jamovi = c("omv"),                             # jamovi (jmvReadWrite)
  arff   = c("arff"),                            # WEKA attribute-relation file (foreign::read.arff)
  rdata  = c("rds", "rdata", "rda")              # R data files (data frames only; no code is run)
)
supported_extensions <- function() unname(unlist(DATA_FORMATS))

file_kind <- function(filename) {
  ext <- tolower(tools::file_ext(filename))
  for (k in names(DATA_FORMATS)) if (ext %in% DATA_FORMATS[[k]]) return(k)
  if (ext %in% c("r", "rmd", "qmd")) {
    api_error(415, "unsupported_file",
              "R scripts contain code, not data, and PathLab never runs uploaded code.",
              "In R, save the data frame with saveRDS(data, \"data.rds\") or write.csv(data, \"data.csv\", row.names = FALSE) and upload that file.")
  }
  api_error(415, "unsupported_file",
            sprintf("Files of type '.%s' are not supported.", ext),
            paste0("Supported: ", paste0(".", supported_extensions(), collapse = ", "), "."))
}

need_pkg <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    api_error(501, "reader_unavailable", sprintf("Reading %s files needs the R package '%s', which is not installed on the analysis server.", what, pkg),
              "Run Rscript backend/install_packages.R on the server, or save the data as CSV.")
  }
}

reader_failed <- function(what, e) {
  api_error(422, "file_parse_failed", sprintf("The %s file could not be read.", what),
            "Check that the file is not damaged or password protected, then try again (or save the data as CSV).",
            sanitize_message(conditionMessage(e)))
}

# Turn labelled / factor columns from statistical packages into plain columns, reporting every
# conversion as an issue (nothing is changed silently). Numeric codes are kept as they are.
plain_columns <- function(df, notes) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  labelled <- character(0); value_labels <- list(); fac_num <- character(0); fac_txt <- character(0); other <- character(0)
  for (j in seq_along(df)) {
    x <- df[[j]]; nm <- names(df)[j]
    if (inherits(x, "haven_labelled")) {
      lab <- attr(x, "labels")
      if (length(lab)) {
        labelled <- c(labelled, nm)
        # The value labels become the variable's level labels (as SPSS shows them); the data keep their codes.
        if (is.numeric(lab) && !is.null(names(lab))) {
          keep <- !is.na(lab) & nzchar(trimws(names(lab)))
          value_labels[[nm]] <- unname(Map(function(v, l) list(value = as.character(v), label = trimws(l)), unname(lab[keep]), names(lab)[keep]))
        }
      }
      x <- haven::zap_labels(x)
    }
    if (is.factor(x)) {
      lv <- levels(x)
      if (length(lv) && all(grepl("^\\s*-?[0-9]+(\\.[0-9]+)?\\s*$", lv))) { x <- as.numeric(lv)[x]; fac_num <- c(fac_num, nm) }
      else { x <- as.character(x); fac_txt <- c(fac_txt, nm) }
    }
    if (is.list(x) || !(is.atomic(x))) { x <- vapply(x, function(v) paste(format(v), collapse = " "), ""); other <- c(other, nm) }
    if (!inherits(x, c("Date", "POSIXt"))) x <- as.vector(x)   # drop format/label attributes; values unchanged
    df[[j]] <- x
  }
  if (length(labelled)) notes[[length(notes) + 1]] <- issue("info", "value_labels_kept",
    sprintf("%d column(s) have value labels; the numeric codes are kept and the labels are shown as their level labels.", length(labelled)), labelled)
  if (length(fac_num)) notes[[length(notes) + 1]] <- issue("info", "factor_codes_numeric",
    sprintf("%d categorical column(s) with numeric levels were read as numbers (the level values, not their positions).", length(fac_num)), fac_num)
  if (length(fac_txt)) notes[[length(notes) + 1]] <- issue("info", "factor_as_text",
    sprintf("%d categorical column(s) with text levels were read as text.", length(fac_txt)), fac_txt)
  if (length(other)) notes[[length(notes) + 1]] <- issue("warning", "complex_columns_as_text",
    sprintf("%d column(s) of an unusual type were converted to text and cannot be used as indicators.", length(other)), other)
  list(data = df, notes = notes, valueLabels = value_labels)
}

# Returns list(data = data.frame, reader = list(...describing the exact call...), sheets, notes, declared); declared is
# the label set of each nominal attribute as the file declares it (ARFF headers), NULL for other formats
read_dataset <- function(path, filename, sheet = NULL) {
  kind <- file_kind(filename)
  ext <- tolower(tools::file_ext(filename))
  if (kind %in% c("ods", "spss", "stata", "sas", "jamovi", "arff", "rdata")) return(read_other_format(path, kind, ext, sheet))
  if (kind == "csv") {
    delim <- detect_delimiter(path)
    df <- tryCatch(
      utils::read.csv(path, sep = delim, check.names = FALSE, na.strings = CSV_NA_STRINGS,
                      stringsAsFactors = FALSE, strip.white = TRUE, fileEncoding = "UTF-8-BOM"),
      error = function(e) api_error(422, "csv_parse_failed",
                                    "The CSV file could not be read.",
                                    "Check that the file is a plain-text CSV with a header row and a consistent delimiter (comma, semicolon or tab).",
                                    sanitize_message(conditionMessage(e))),
      warning = function(w) api_error(422, "csv_parse_failed",
                                      "The CSV file could not be read cleanly.",
                                      "Check for rows with a different number of fields than the header, or for an unusual text encoding (save as UTF-8).",
                                      sanitize_message(conditionMessage(w)))
    )
    reader <- list(kind = "csv", delimiter = delim, naStrings = arr(CSV_NA_STRINGS))
    if (identical(delim, "")) df <- tryCatch(  # white-space separated text (.dat / .txt)
      utils::read.table(path, header = TRUE, sep = "", check.names = FALSE, na.strings = CSV_NA_STRINGS,
                        stringsAsFactors = FALSE, strip.white = TRUE, fileEncoding = "UTF-8-BOM", comment.char = "", quote = "\"'"),
      error = function(e) api_error(422, "csv_parse_failed", "The text file could not be read.",
                                    "Check that the first row holds column names and every row has the same number of values.",
                                    sanitize_message(conditionMessage(e))))
    sheets <- list()
  } else {
    sheets <- tryCatch(readxl::excel_sheets(path),
                       error = function(e) api_error(422, "xlsx_parse_failed",
                                                     "The Excel workbook could not be opened.",
                                                     "Check that the file is a valid Excel workbook and is not password protected."))
    if (length(sheets) == 0) api_error(422, "xlsx_empty", "The workbook has no sheets.")
    if (is.null(sheet) || !nzchar(sheet)) sheet <- sheets[1]
    if (!sheet %in% sheets) api_error(400, "unknown_sheet", "The selected sheet does not exist in this workbook.")
    df <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")),
      error = function(e) api_error(422, "xlsx_parse_failed",
                                    "The selected sheet could not be read.",
                                    "Make sure the first row contains column names and the data start in the second row.",
                                    sanitize_message(conditionMessage(e)))
    )
    reader <- list(kind = "xlsx", sheet = sheet)
    sheets <- as.list(sheets)
  }
  check_shape(df)
  list(data = df, reader = reader, sheets = sheets, notes = list())
}

#' The labels of each nominal attribute in an ARFF header, in header order: named list (attribute -> labels).
#' Quoted names and labels ('10-19', "a b") are unquoted; anything unreadable gives an empty list.
arff_header_labels <- function(path) {
  tryCatch({
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    end <- grep("^\\s*@data\\b", lines, ignore.case = TRUE)[1]
    if (!is.na(end)) lines <- lines[seq_len(end - 1)]
    out <- list()
    for (l in grep("^\\s*@attribute\\s", lines, ignore.case = TRUE, value = TRUE)) {
      m <- regmatches(l, regexec("^\\s*@attribute\\s+('[^']*'|\"[^\"]*\"|\\S+)\\s*\\{(.*)\\}\\s*$", l, ignore.case = TRUE))[[1]]
      if (length(m) != 3) next
      nm <- sub("^(['\"])(.*)\\1$", "\\2", m[2])
      out[[nm]] <- scan(text = m[3], what = "", sep = ",", quote = "'\"", strip.white = TRUE, quiet = TRUE)
    }
    out
  }, error = function(e) list())
}

check_shape <- function(df) {
  if (ncol(df) == 0) api_error(422, "no_columns", "No columns were found in the file.", "Check that the first row contains column names.")
  if (nrow(df) == 0) api_error(422, "no_rows", "The file has column names but no data rows.")
}

# SPSS, Stata, SAS, jamovi, WEKA (ARFF), OpenDocument and R data files.
read_other_format <- function(path, kind, ext, sheet) {
  notes <- list(); sheets <- list(); declared <- NULL
  if (kind == "ods") {
    need_pkg("readODS", "OpenDocument (.ods)")
    sh <- tryCatch(readODS::list_ods_sheets(path), error = function(e) reader_failed("OpenDocument", e))
    if (!length(sh)) api_error(422, "xlsx_empty", "The spreadsheet has no sheets.")
    if (is.null(sheet) || !nzchar(sheet)) sheet <- sh[1]
    if (!sheet %in% sh) api_error(400, "unknown_sheet", "The selected sheet does not exist in this spreadsheet.")
    df <- tryCatch(readODS::read_ods(path, sheet = sheet, na = CSV_NA_STRINGS, .name_repair = "minimal"), error = function(e) reader_failed("OpenDocument", e))
    reader <- list(kind = "ods", sheet = sheet); sheets <- as.list(sh)
  } else if (kind == "spss") {
    need_pkg("haven", "SPSS")
    raw <- tryCatch(if (ext == "por") haven::read_por(path, user_na = TRUE) else haven::read_sav(path, user_na = TRUE),
                    error = function(e) reader_failed("SPSS", e))
    # SPSS user-defined missing values are treated as missing, as SPSS itself does; the counts are reported.
    # is.na() counts user-defined missing values for labelled_spss columns; unclass() shows the stored codes.
    um <- vapply(raw, function(x) sum(is.na(x) & !is.na(unclass(x))), 0)
    if (any(um > 0)) notes[[length(notes) + 1]] <- issue("info", "spss_user_missing",
      sprintf("%d value(s) declared as user-defined missing in the SPSS file were read as missing.", sum(um)), names(um)[um > 0])
    df <- haven::zap_missing(raw)
    reader <- list(kind = "spss", fn = if (ext == "por") "read_por" else "read_sav")
  } else if (kind == "stata") {
    need_pkg("haven", "Stata")
    df <- tryCatch(haven::read_dta(path), error = function(e) reader_failed("Stata", e))
    reader <- list(kind = "stata", fn = "read_dta")
  } else if (kind == "sas") {
    need_pkg("haven", "SAS")
    df <- tryCatch(if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path), error = function(e) reader_failed("SAS", e))
    reader <- list(kind = "sas", fn = if (ext == "xpt") "read_xpt" else "read_sas")
  } else if (kind == "jamovi") {
    need_pkg("jmvReadWrite", "jamovi (.omv)")
    df <- tryCatch(suppressMessages(jmvReadWrite::read_omv(path)), error = function(e) reader_failed("jamovi", e))
    reader <- list(kind = "jamovi")
  } else if (kind == "arff") {
    need_pkg("foreign", "WEKA (.arff)")
    df <- tryCatch(foreign::read.arff(path), error = function(e) reader_failed("WEKA ARFF", e))
    # Nominal attributes stay categories: they are read as text (also when their labels are numbers, e.g. {1,2,3}),
    # so they get the nominal measurement level; WEKA's "?" is missing.
    nominal <- names(df)[vapply(df, is.factor, TRUE)]
    # The labels declared in the header, in header order (also those no case has), as WEKA's Preprocess panel lists them.
    # read.arff keeps only the labels that occur, sorted, so they come from the header itself (when it declares
    # every label read.arff found).
    hdr <- arff_header_labels(path)
    declared <- lapply(setNames(nominal, nominal), function(nm) {
      lev <- levels(df[[nm]]); h <- hdr[[nm]]
      if (!is.null(h) && all(lev %in% h) && !anyDuplicated(h)) h else lev
    })
    df[nominal] <- lapply(df[nominal], as.character)
    if (length(nominal)) notes[[length(notes) + 1]] <- issue("info", "arff_nominal",
      sprintf("%d nominal attribute(s) were kept as categories (text columns, nominal measurement level).", length(nominal)), nominal)
    reader <- list(kind = "arff")
  } else {   # rdata: deserialises data only; no uploaded code is executed
    if (ext == "rds") {
      obj <- tryCatch(readRDS(path), error = function(e) reader_failed("R data", e))
      if (!is.data.frame(obj)) api_error(422, "not_a_data_frame", "The .rds file does not contain a data frame.",
                                         "Save a data frame, for example saveRDS(data, \"data.rds\").")
      df <- obj; reader <- list(kind = "rdata", fn = "readRDS")
    } else {
      env <- new.env(parent = emptyenv())
      tryCatch(load(path, envir = env), error = function(e) reader_failed("R data", e))
      frames <- Filter(function(n) is.data.frame(env[[n]]), sort(ls(env)))
      if (!length(frames)) api_error(422, "not_a_data_frame", "The R data file contains no data frame.",
                                     "Save a data frame, for example save(data, file = \"data.RData\").")
      if (is.null(sheet) || !nzchar(sheet)) sheet <- frames[1]
      if (!sheet %in% frames) api_error(400, "unknown_sheet", "The selected data frame does not exist in this file.")
      df <- env[[sheet]]; reader <- list(kind = "rdata", fn = "load", sheet = sheet); sheets <- as.list(frames)
    }
  }
  p <- plain_columns(df, notes)
  check_shape(p$data)
  list(data = p$data, reader = reader, sheets = sheets, notes = p$notes, declared = declared, valueLabels = p$valueLabels)
}

column_type <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return("date")
  if (is.logical(x)) {
    if (all(is.na(x))) return("empty")
    return("logical")
  }
  if (is.numeric(x)) {
    v <- x[!is.na(x)]
    if (length(v) == 0) return("empty")
    if (all(abs(v - round(v)) < 1e-9)) return("integer")
    return("numeric")
  }
  if (all(is.na(x))) return("empty")
  "text"
}

looks_numeric_text <- function(x) {
  v <- x[!is.na(x) & nzchar(x)]
  if (length(v) == 0) return(FALSE)
  mean(grepl("^\\s*-?[0-9]+([.,][0-9]+)?\\s*$", v)) > 0.9
}

#' Sample skewness and excess kurtosis with the usual small-sample corrections (as in SPSS and Excel's SKEW/KURT).
shape_stats <- function(v) {
  n <- length(v)
  s <- if (n > 1) stats::sd(v) else NA
  if (n < 3 || !is.finite(s) || s == 0) return(list(skew = NA, kurt = NA))
  z <- (v - mean(v)) / s
  skew <- n / ((n - 1) * (n - 2)) * sum(z^3)
  kurt <- if (n < 4) NA else n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(z^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  zap <- function(x) if (is.finite(x) && abs(x) < 1e-12) 0 else x   # rounding noise would show as -0.000
  list(skew = zap(skew), kurt = zap(kurt))
}

#' Cramér-von Mises normality test p value (nortest; needs at least 8 non-missing, non-constant values).
cvm_p <- function(v) {
  if (length(v) < 8 || length(unique(v)) < 2 || !requireNamespace("nortest", quietly = TRUE)) return(NA)
  tryCatch(suppressWarnings(nortest::cvm.test(v)$p.value), error = function(e) NA)
}

profile_dataset <- function(df) {
  nm <- names(df)
  dup <- nm[duplicated(nm) | duplicated(nm, fromLast = TRUE)]
  lapply(seq_along(nm), function(j) {
    x <- df[[j]]
    type <- column_type(x)
    is_num <- type %in% c("integer", "numeric")
    v <- if (is_num) x[!is.na(x)] else NULL
    n_unique <- length(unique(x[!is.na(x)]))
    sd_v <- if (is_num && length(v) > 1) stats::sd(v) else NA
    shape <- if (is_num) shape_stats(v) else list(skew = NA, kurt = NA)
    is_constant <- n_unique <= 1
    looks_id <- type == "integer" && n_unique == nrow(df) && nrow(df) > 2 &&
      isTRUE(all(diff(sort(v)) == 1))
    id_name <- grepl("(^id$|_id$|^id_|respondent|record|case|^no\\.?$|^row)", tolower(nm[j]))
    list(
      index = j,
      name = nm[j],
      type = type,
      nMissing = sum(is.na(x)),
      nUnique = n_unique,
      mean = if (is_num && length(v)) mean(v) else NULL,
      median = if (is_num && length(v)) stats::median(v) else NULL,
      sd = num_or_null(sd_v),
      min = if (is_num && length(v)) min(v) else NULL,
      max = if (is_num && length(v)) max(v) else NULL,
      skewness = num_or_null(shape$skew),
      kurtosis = num_or_null(shape$kurt),
      cvmP = if (is_num) num_or_null(cvm_p(v)) else NULL,
      isConstant = is_constant,
      looksLikeId = isTRUE(looks_id) || (id_name && n_unique == nrow(df)),
      isDuplicateName = nm[j] %in% dup,
      isBlankName = !nzchar(trimws(nm[j])),
      levels = column_levels(x),
      usable = is_num && !is_constant && !(nm[j] %in% dup) && nzchar(trimws(nm[j])),
      suggestedRole = if (is_num && !is_constant && !(isTRUE(looks_id) || (id_name && n_unique == nrow(df))) &&
                          !(nm[j] %in% dup) && nzchar(trimws(nm[j]))) "indicator" else "excluded"
    )
  })
}

issue <- function(severity, code, message, columns = character(0), hint = NULL) {
  out <- list(severity = severity, code = code, message = message, columns = arr(columns))
  if (!is.null(hint)) out$hint <- hint
  out
}

validate_dataset <- function(df, profile, reader) {
  issues <- list()
  add <- function(x) issues[[length(issues) + 1]] <<- x
  nm <- names(df)

  dup <- unique(nm[duplicated(nm)])
  if (length(dup)) add(issue("error", "duplicate_names",
                             sprintf("%d column name(s) appear more than once. These columns cannot be used as indicators until the names are made unique in the source file.", length(dup)),
                             dup, "Rename the duplicated columns in the original file and upload it again."))
  blank <- which(!nzchar(trimws(nm)))
  if (length(blank)) add(issue("error", "blank_names",
                               sprintf("%d column(s) have no name (positions %s). They cannot be used as indicators.", length(blank), paste(blank, collapse = ", ")),
                               character(0), "Add a header for every column in the source file."))

  types <- vapply(profile, `[[`, "", "type")
  names_v <- vapply(profile, `[[`, "", "name")
  text_cols <- names_v[types == "text"]
  if (length(text_cols)) {
    numeric_like <- text_cols[vapply(text_cols, function(cn) looks_numeric_text(df[[cn]]), logical(1))]
    other_text <- setdiff(text_cols, numeric_like)
    if (length(numeric_like)) add(issue("warning", "numeric_as_text",
                                        sprintf("%d column(s) look numeric but were read as text (for example, decimal commas or stray characters). They cannot be used as indicators as-is.", length(numeric_like)),
                                        numeric_like,
                                        "Fix the values in the source file (for example, use a dot as the decimal separator) and upload again. PathLab does not convert values automatically."))
    if (length(other_text)) add(issue("info", "text_columns",
                                      sprintf("%d text column(s) were found. Text columns cannot be indicators and are excluded from analysis by default.", length(other_text)),
                                      other_text))
  }
  empty_cols <- names_v[types == "empty"]
  if (length(empty_cols)) add(issue("warning", "empty_columns", "Some columns contain no values at all.", empty_cols))
  date_cols <- names_v[types %in% c("date", "logical")]
  if (length(date_cols)) add(issue("info", "non_numeric_columns", "Date or logical (TRUE/FALSE) columns cannot be indicators and are excluded by default.", date_cols))

  const <- names_v[vapply(profile, function(p) isTRUE(p$isConstant) && p$type %in% c("integer", "numeric"), logical(1))]
  if (length(const)) add(issue("error", "constant_columns",
                               "These numeric columns have the same value in every row (zero variance). PLS-SEM cannot standardise them, so they cannot be indicators.",
                               const, "Exclude these columns from the model."))

  ids <- names_v[vapply(profile, function(p) isTRUE(p$looksLikeId), logical(1))]
  if (length(ids)) add(issue("info", "id_columns",
                             "These columns look like identifiers (a unique value in every row) and are excluded from analysis by default.",
                             ids, "If a column is really a measurement item, mark it as an analysis variable in the Data tab."))

  miss <- vapply(profile, `[[`, 0, "nMissing")
  miss_cols <- names_v[miss > 0]
  if (length(miss_cols)) {
    rows_any <- sum(!stats::complete.cases(df))
    add(issue("warning", "missing_values",
              sprintf("%d column(s) contain missing values (%d cells in total; %d of %d rows have at least one missing value). Before estimation you must choose how missing values are handled; nothing is imputed or removed automatically.",
                      length(miss_cols), sum(miss), rows_any, nrow(df)),
              miss_cols,
              "Review the missing-data options in Analysis settings."))
  }
  empty_rows <- sum(apply(df, 1, function(r) all(is.na(r))))
  if (empty_rows > 0) add(issue("warning", "empty_rows",
                                sprintf("%d row(s) are completely empty.", empty_rows),
                                character(0),
                                "Remove empty rows in the source file, or choose listwise deletion in the missing-data options."))

  num_cols <- names_v[types %in% c("integer", "numeric") & !(names_v %in% c(const, dup))]
  if (length(num_cols) >= 2) {
    cm <- suppressWarnings(stats::cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs"))
    cm[lower.tri(cm, diag = TRUE)] <- NA
    hits <- which(abs(cm) > 0.9999, arr.ind = TRUE)
    if (nrow(hits)) {
      pairs <- unique(c(num_cols[hits[, 1]], num_cols[hits[, 2]]))
      add(issue("warning", "perfect_correlation",
                "Some numeric columns are perfectly correlated (|r| = 1). Using them together in a Mode B (formative) construct will make estimation fail; in other constructs they add no information.",
                pairs, "Check whether one column is a copy or a linear transformation of another."))
    }
  }
  if (nrow(df) < 30) add(issue("warning", "small_sample",
                               sprintf("The dataset has only %d rows. PLS-SEM estimates and bootstrap results can be unstable with very small samples.", nrow(df))))
  name_odd <- names_v[grepl("->|\\*", names_v)]
  if (length(name_odd)) add(issue("warning", "reserved_characters",
                                  "Some column names contain '->' or '*', which SEMinR uses in its own output labels. These columns cannot be used as indicators.",
                                  name_odd, "Rename these columns in the source file."))
  if (identical(reader$kind, "csv")) add(issue("info", "csv_reading_rules",
                                               "Empty cells and the text 'NA' were read as missing values. No other values were changed."))
  issues
}

dataset_preview <- function(df, n = 50) dataset_rows_json(df, 0L, n)

#' The same rows as dataset_rows(), already written as JSON (an array of row arrays, NA -> null) and marked as such, so
#' the API's JSON serializer inserts it as it is (run.R registers it with json_verbatim). Written column by column, it
#' takes milliseconds where encoding the lists cell by cell takes seconds for wide data (500 rows x 120 columns: 6 s).
dataset_rows_json <- function(df, offset = 0L, limit = 500L) {
  offset <- max(0L, as.integer(offset)); limit <- max(0L, as.integer(limit))
  idx <- seq_len(nrow(df)); idx <- idx[idx > offset & idx <= offset + limit]
  d <- df[idx, , drop = FALSE]
  for (j in seq_along(d)) {
    if (inherits(d[[j]], c("Date", "POSIXt"))) d[[j]] <- format(d[[j]])
    else if (is.factor(d[[j]])) d[[j]] <- as.character(d[[j]])
  }
  json <- if (!nrow(d)) "[]" else as.character(jsonlite::toJSON(d, dataframe = "values", na = "null", digits = NA))
  structure(json, class = "json")
}

# Rows offset+1 .. offset+limit as JSON-ready lists (NA -> null). Values are shown exactly as read.
dataset_rows <- function(df, offset = 0L, limit = 500L) {
  offset <- max(0L, as.integer(offset)); limit <- max(0L, as.integer(limit))
  idx <- seq_len(nrow(df)); idx <- idx[idx > offset & idx <= offset + limit]
  d <- df[idx, , drop = FALSE]
  lapply(seq_len(nrow(d)), function(i) {
    lapply(seq_len(ncol(d)), function(j) {
      v <- d[[j]][i]
      if (is.na(v)) NULL else if (inherits(v, c("Date", "POSIXt"))) format(v) else v
    })
  })
}

# Summarise what a missing-data choice will change, without changing anything.
missing_impact <- function(df, indicators, missing_code = NULL) {
  indicators <- intersect(indicators, names(df))
  d <- df[, indicators, drop = FALSE]
  recoded <- 0L
  per <- lapply(indicators, function(cn) {
    x <- d[[cn]]
    n_code <- if (!is.null(missing_code) && is.numeric(x)) sum(!is.na(x) & x == missing_code) else 0L
    list(indicator = cn, nMissing = sum(is.na(x)), nCoded = n_code, nTotal = sum(is.na(x)) + n_code)
  })
  if (!is.null(missing_code)) {
    for (cn in indicators) if (is.numeric(d[[cn]])) {
      hit <- !is.na(d[[cn]]) & d[[cn]] == missing_code
      recoded <- recoded + sum(hit)
      d[[cn]][hit] <- NA
    }
  }
  complete <- stats::complete.cases(d)
  list(
    nRows = nrow(d),
    cellsRecoded = recoded,
    cellsMissing = sum(is.na(d)),
    rowsWithMissing = sum(!complete),
    rowsRemainingListwise = sum(complete),
    perIndicator = per
  )
}

validation_csv <- function(issues, profile, meta) {
  lines <- c(
    "section,severity,code,message,columns",
    vapply(issues, function(i) {
      paste(c("issue", i$severity, i$code, i$message, paste(unclass(i$columns), collapse = "; ")) |>
              vapply(function(s) paste0("\"", gsub("\"", "\"\"", s), "\""), ""), collapse = ",")
    }, "")
  )
  lines <- c(lines, "", "column,no,type,missing,unique,mean,median,sd,min,max,excess_kurtosis,skewness,cvm_p,constant,looks_like_id")
  for (p in profile) {
    f <- function(v) if (is.null(v)) "" else format(v, digits = 15)
    lines <- c(lines, paste(paste0("\"", gsub("\"", "\"\"", p$name), "\""), p$index, p$type, p$nMissing, p$nUnique,
                            f(p$mean), f(p$median), f(p$sd), f(p$min), f(p$max), f(p$kurtosis), f(p$skewness), f(p$cvmP),
                            p$isConstant, p$looksLikeId, sep = ","))
  }
  header <- c(sprintf("# PathLab SEM data validation summary - %s", meta$filename),
              sprintf("# Rows: %d, Columns: %d, SHA-256: %s, Generated: %s", meta$nRows, meta$nCols, meta$checksum, now_iso()))
  paste(c(header, lines), collapse = "\n")
}
