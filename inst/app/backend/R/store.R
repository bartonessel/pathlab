# In-memory project store. Uploaded data live only in a per-project temporary
# directory and in this process's memory; both are removed when the project is
# deleted or when it has been idle longer than PATHLAB_SESSION_TTL_HOURS.
# Nothing is written to persistent storage.

.store <- new.env(parent = emptyenv())
.store$projects <- new.env(parent = emptyenv())

data_root <- function() {
  root <- Sys.getenv("PATHLAB_DATA_DIR", "")
  if (!nzchar(root)) root <- file.path(tempdir(), "pathlab")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
}

session_ttl_seconds <- function() {
  h <- suppressWarnings(as.numeric(Sys.getenv("PATHLAB_SESSION_TTL_HOURS", "6")))
  if (is.na(h) || h <= 0) h <- 6
  h * 3600
}

create_project <- function(name) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) name <- "Untitled project"
  if (nchar(name) > 120) name <- substr(name, 1, 120)
  id <- new_id("p")
  dir <- file.path(data_root(), id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  proj <- new.env(parent = emptyenv())
  proj$id <- id
  proj$token <- new_token()
  proj$name <- name
  proj$createdAt <- now_iso()
  proj$lastAccess <- Sys.time()
  proj$dir <- dir
  proj$datasets <- list()
  proj$documents <- list()
  proj$jobs <- list()
  proj$pubjobs <- list()
  proj$results <- list()   # resultId -> path to result.json
  assign(id, proj, envir = .store$projects)
  proj
}

get_project <- function(id, token) {
  if (is.null(id) || !exists(id, envir = .store$projects, inherits = FALSE)) {
    api_error(404, "project_not_found", "This project session no longer exists on the server.",
              "Sessions expire after a period of inactivity or when the server restarts. Reopen your saved project file and upload the dataset again.")
  }
  proj <- get(id, envir = .store$projects)
  if (is.null(token) || !identical(token, proj$token)) {
    api_error(403, "forbidden", "You do not have access to this project.")
  }
  proj$lastAccess <- Sys.time()
  proj
}

delete_project <- function(proj) {
  for (j in c(proj$jobs, proj$pubjobs)) if (!is.null(j$process) && j$process$is_alive()) try(j$process$kill_tree(), silent = TRUE)
  unlink(proj$dir, recursive = TRUE, force = TRUE)
  rm(list = proj$id, envir = .store$projects)
  invisible(TRUE)
}

cleanup_expired_projects <- function() {
  ttl <- session_ttl_seconds()
  for (id in ls(.store$projects)) {
    proj <- get(id, envir = .store$projects)
    busy <- any(vapply(proj$jobs, function(j) !is.null(j$process) && j$process$is_alive(), logical(1)))
    if (!busy && as.numeric(difftime(Sys.time(), proj$lastAccess, units = "secs")) > ttl) {
      delete_project(proj)
    }
  }
}

get_dataset <- function(proj, dataset_id) {
  ds <- proj$datasets[[dataset_id]]
  if (is.null(ds)) api_error(404, "dataset_not_found", "The dataset was not found in this project.", "Upload the dataset again.")
  dataset_with_derived(ds)
}

dataset_payload <- function(ds, include_preview = TRUE) {
  out <- list(
    id = ds$id,
    meta = ds$meta,
    sheets = ds$sheets,
    columns = ds$profile,
    issues = ds$issues,
    derived = unname(lapply(ds$derived %||% list(), derived_meta)),
    edits = edits_payload(ds)
  )
  if (include_preview) out$preview <- list(columns = arr(names(ds$data)), rows = dataset_preview(ds$data))
  out
}

store_dataset <- function(proj, raw, filename, sheet = NULL, dataset_id = NULL) {
  dataset_id <- dataset_id %||% new_id("d")
  ddir <- file.path(proj$dir, dataset_id)
  dir.create(ddir, showWarnings = FALSE, recursive = TRUE)
  safe_name <- gsub("[^A-Za-z0-9._ -]", "_", basename(filename))
  path <- file.path(ddir, paste0("upload.", tolower(tools::file_ext(safe_name))))
  if (!is.null(raw)) writeBin(raw, path)
  parsed <- read_dataset(path, safe_name, sheet)
  df <- parsed$data
  profile <- profile_dataset(df)
  meta <- list(
    filename = safe_name,
    sizeBytes = file.info(path)$size,
    checksum = digest::digest(file = path, algo = "sha256"),
    nRows = nrow(df),
    nCols = ncol(df),
    reader = parsed$reader,
    uploadedAt = now_iso()
  )
  issues <- c(parsed$notes %||% list(), validate_dataset(df, profile, parsed$reader))
  ds <- list(id = dataset_id, path = path, data = df, profile = profile, issues = issues,
             meta = meta, sheets = parsed$sheets, declared = parsed$declared)
  proj$datasets[[dataset_id]] <- ds
  ds
}
