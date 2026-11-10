# Shared helpers: errors, ids, JSON-friendly conversion, sanitisation.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Structured API error. `status` is the HTTP status; `hint` suggests a next step.
api_error <- function(status, code, message, hint = NULL, details = NULL) {
  cond <- structure(
    class = c("pathlab_error", "error", "condition"),
    list(message = message, call = NULL, status = status, code = code,
         hint = hint, details = details)
  )
  stop(cond)
}

#' R packages of `pkgs` that are not installed (an empty vector when all are).
missing_packages <- function(pkgs) pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

#' Stop with a clear message (HTTP 501) when a feature's optional R packages are not installed, instead of an internal
#' error. Installs that leave a feature out on purpose (a smaller desktop build, a minimal server) rely on this.
need_packages <- function(pkgs, feature, code = "feature_unavailable") {
  miss <- missing_packages(pkgs)
  if (length(miss)) {
    q <- paste0('"', miss, '"', collapse = ", ")
    api_error(501, code,
              sprintf("%s needs the R package%s %s, which %s not installed.", feature, if (length(miss) > 1) "s" else "",
                      paste(miss, collapse = ", "), if (length(miss) > 1) "are" else "is"),
              sprintf("Install %s in R with install.packages(%s) and restart PathLab.", if (length(miss) > 1) "them" else "it",
                      if (length(miss) > 1) paste0("c(", q, ")") else q))
  }
  invisible(TRUE)
}

new_id <- function(prefix = "") {
  paste0(prefix, paste(sample(c(letters, 0:9), 12, replace = TRUE), collapse = ""))
}

new_token <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 40, replace = TRUE), collapse = "")
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Remove anything from an R condition message that could echo data values or
# file-system paths, and keep it short. Used for every user-facing technical detail.
sanitize_message <- function(msg, max_chars = 300) {
  if (is.null(msg) || length(msg) == 0) return("")
  msg <- paste(as.character(msg), collapse = " ")
  msg <- gsub("[A-Za-z]:[\\\\/][^ '\"]*", "<path>", msg)       # Windows paths
  msg <- gsub("(?<![A-Za-z0-9])/[^ '\"]*/[^ '\"]*", "<path>", msg, perl = TRUE) # POSIX paths
  msg <- gsub("\\s+", " ", msg)
  msg <- trimws(msg)
  if (nchar(msg) > max_chars) msg <- paste0(substr(msg, 1, max_chars), "...")
  msg
}

# Wrap a character vector so jsonlite always serialises it as an array.
arr <- function(x) {
  if (is.null(x)) return(list())
  I(as.vector(x))
}

#' Congruence Index: Tucker's (1951) congruence coefficient between the loading patterns of every pair of
#' constructs, from the indicator-by-construct cross-loading matrix L:
#'   phi(i, j) = sum_k L[k, i] L[k, j] / sqrt(sum_k L[k, i]^2 * sum_k L[k, j]^2).
#' 1 means two constructs relate to all indicators in the same proportions (they are not distinct); values of
#' 0.95 or more are taken to mean the two are equal, 0.85-0.94 fair similarity (Lorenzo-Seva & ten Berge, 2006).
#' Returns a lower-triangular construct x construct matrix (upper triangle NA, diagonal 1).
congruence_index <- function(cross_loadings) {
  L <- unclass(cross_loadings)
  L <- suppressWarnings(matrix(as.numeric(L), nrow = nrow(L), dimnames = dimnames(L)))
  L[!is.finite(L)] <- 0
  ss <- sqrt(colSums(L^2))
  phi <- crossprod(L) / outer(ss, ss)
  phi[upper.tri(phi)] <- NA
  diag(phi) <- 1
  phi
}

num_or_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 1 && (is.na(x) || !is.finite(x))) return(NULL)
  x
}

# Convert a matrix (possibly with "." placeholders from SEMinR print classes) to a
# table object: list(columns = [...], rows = [ {...}, ... ]).
# `row_key` names the column that holds the row names.
matrix_to_table <- function(m, row_key = "row", row_label = "", col_labels = NULL,
                            drop_zero_cols = FALSE) {
  m <- unclass(m)
  if (is.null(dim(m))) m <- matrix(m, nrow = 1, dimnames = list("value", names(m)))
  storage <- suppressWarnings(matrix(as.numeric(m), nrow = nrow(m), dimnames = dimnames(m)))
  rn <- rownames(storage) %||% as.character(seq_len(nrow(storage)))
  cn <- colnames(storage) %||% paste0("V", seq_len(ncol(storage)))
  columns <- list(list(key = row_key, label = row_label, type = "text"))
  for (j in seq_along(cn)) {
    columns[[length(columns) + 1]] <- list(
      key = cn[j], label = if (!is.null(col_labels)) col_labels[j] else cn[j], type = "number"
    )
  }
  rows <- lapply(seq_along(rn), function(i) {
    r <- list()
    r[[row_key]] <- rn[i]
    for (j in seq_along(cn)) {
      v <- storage[i, j]
      r[[cn[j]]] <- if (is.na(v) || !is.finite(v)) NULL else v
    }
    r
  })
  list(columns = columns, rows = rows)
}

# Build a table from a data.frame with explicit column metadata.
# cols: list of list(key, label, type, description?)
df_to_table <- function(df, cols) {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r <- list()
    for (cdef in cols) {
      v <- df[[cdef$key]][i]
      if (cdef$type == "number") {
        v <- suppressWarnings(as.numeric(v))
        r[[cdef$key]] <- if (is.na(v) || !is.finite(v)) NULL else v
      } else {
        r[[cdef$key]] <- if (is.na(v)) NULL else as.character(v)
      }
    }
    r
  })
  list(columns = cols, rows = rows)
}

col <- function(key, label, type = "number", description = NULL) {
  out <- list(key = key, label = label, type = type)
  if (!is.null(description)) out$description <- description
  out
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# Quote a string as an R string literal (safe for generated code).
r_str <- function(x) {
  vapply(x, function(s) encodeString(s, quote = "\""), character(1), USE.NAMES = FALSE)
}

r_vec <- function(x) {
  if (length(x) == 1) return(r_str(x))
  paste0("c(", paste(r_str(x), collapse = ", "), ")")
}

parse_json_body <- function(req) {
  body <- req$postBody
  if (is.null(body) || !nzchar(body)) return(list())
  tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) api_error(400, "invalid_json", "The request body is not valid JSON.")
  )
}

# Normalise a JSON array parsed with simplifyVector = FALSE into a character vector.
as_chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  vapply(x, function(v) as.character(v %||% NA_character_), character(1))
}

seminr_version <- function() as.character(utils::packageVersion("seminr"))

#' A possibly missing JSON value as a number (NA when absent).
num_of <- function(x) if (is.null(x) || !length(x)) NA_real_ else suppressWarnings(as.numeric(x))
