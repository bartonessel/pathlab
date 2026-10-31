# Asynchronous analysis jobs. Each analysis runs in its own background R process
# (callr::r_bg) so the API stays responsive, progress can be polled, and a job can
# be cancelled by killing its process tree (including SEMinR's bootstrap workers).

BACKEND_DIR <- NULL  # set by plumber.R

max_jobs <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("PATHLAB_MAX_JOBS", "2")))
  if (is.na(v) || v < 1) 2L else v
}

running_job_count <- function() {
  n <- 0L
  for (id in ls(.store$projects)) {
    proj <- get(id, envir = .store$projects)
    for (j in proj$jobs) if (identical(j$status, "running") && !is.null(j$process) && j$process$is_alive()) n <- n + 1L
  }
  n
}

# QCA runs in a background job too, so a large model never blocks the server; a QCA job that runs longer than this is
# stopped (QCA's C code cannot always be interrupted from inside the process, so the whole worker is killed).
QCA_JOB_SECONDS <- 120

#' A job's status is only updated when it is polled; one whose page was reloaded (or closed) mid-run is never polled
#' again. Bring every running job up to date, so that a finished run does not block the next one.
refresh_running_jobs <- function(proj) {
  for (id in names(proj$jobs)) if (identical(proj$jobs[[id]]$status, "running")) try(refresh_job(proj, id), silent = TRUE)
  invisible(proj)
}

start_analysis_job <- function(proj, ds, model, settings, model_json, kind = "plssem", extra = list(), exclusive = TRUE) {
  refresh_running_jobs(proj)
  if (exclusive && any(vapply(proj$jobs, function(j) identical(j$status, "running") && !identical(j$kind, "qca"), logical(1)))) {
    api_error(409, "job_running", "An analysis is already running for this project.", "Wait for it to finish or cancel it first.")
  }
  if (running_job_count() >= max_jobs()) {
    api_error(503, "server_busy", "The server is already running the maximum number of analyses.", "Try again in a minute.")
  }
  # Fix random seeds before starting so the exported R code records the exact values.
  if (isTRUE(settings$bootstrap$enabled) && is.null(settings$bootstrap$seed)) settings$bootstrap$seed <- sample.int(1e6, 1)
  if (isTRUE(settings$predict$enabled) && is.null(settings$predict$seed)) settings$predict$seed <- sample.int(1e6, 1)
  if (isTRUE(settings$nca$enabled) && settings$nca$testRep > 0 && is.null(settings$nca$seed)) settings$nca$seed <- sample.int(1e6, 1)
  if (isTRUE(settings$mga$enabled) && is.null(settings$mga$seed)) settings$mga$seed <- sample.int(1e6, 1)

  job_id <- new_id("j")
  result_id <- new_id("r")
  job_dir <- file.path(proj$dir, "jobs", job_id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(c(list(data = ds$data, model = model, settings = settings, datasetMeta = ds$meta,
               modelJson = model_json, resultId = result_id, datasetId = ds$id,
               submittedAt = now_iso(), kind = kind), extra),
          file.path(job_dir, "input.rds"))
  proc <- callr::r_bg(
    function(backend_dir, job_dir) {
      for (f in c("util.R", "data_import.R", "model_spec.R", "advanced.R", "analysis.R", "cbsem.R", "gsca.R", "three_step.R", "lpa.R", "derived.R", "ml.R", "exports.R", "reports_extra.R", "qca.R")) source(file.path(backend_dir, "R", f), local = FALSE)
      suppressPackageStartupMessages(library(seminr))
      worker_main(job_dir)
    },
    args = list(backend_dir = BACKEND_DIR, job_dir = job_dir),
    stdout = NULL, stderr = file.path(job_dir, "worker.log"),
    supervise = TRUE
  )
  job <- list(id = job_id, resultId = result_id, datasetId = ds$id, dir = job_dir, process = proc, kind = kind,
              status = "running", submittedAt = now_iso(), started = Sys.time(), settings = settings)
  proj$jobs[[job_id]] <- job
  job
}

read_progress <- function(job_dir) {
  p <- file.path(job_dir, "progress.json")
  if (!file.exists(p)) return(list(stage = "starting", message = "Starting the R worker", fraction = 0))
  tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) list(stage = "running", message = "Running"))
}

refresh_job <- function(proj, job_id) {
  job <- proj$jobs[[job_id]]
  if (is.null(job)) api_error(404, "job_not_found", "The analysis job was not found.")
  if (!identical(job$status, "running")) return(job)
  res_path <- file.path(job$dir, "result.json")
  if (file.exists(res_path)) {
    # Wait for the worker process to exit so its bootstrap cluster is fully stopped.
    try(job$process$wait(5000), silent = TRUE)
    head <- tryCatch(jsonlite::fromJSON(res_path, simplifyVector = FALSE)[c("status", "error")],
                     error = function(e) list(status = "failed", error = list(message = "The analysis result could not be read.")))
    if (identical(head$status, "completed")) {
      job$status <- "completed"
      proj$results[[job$resultId]] <- res_path
    } else {
      job$status <- "failed"
      job$error <- head$error
    }
  } else if (identical(job$kind, "qca") && as.numeric(difftime(Sys.time(), job$started, units = "secs")) > QCA_JOB_SECONDS) {
    try(job$process$kill_tree(), silent = TRUE)
    job$status <- "failed"
    job$error <- list(stage = "minimisation", message = sprintf("The QCA took longer than %d seconds and was stopped.", QCA_JOB_SECONDS),
                      hint = "This happens with many conditions (the truth table doubles with each one) or severe model ambiguity. Use fewer conditions, or switch off the sensitivity analysis.")
  } else if (!job$process$is_alive()) {
    job$status <- "failed"
    job$error <- list(stage = "worker", message = "The R analysis process stopped unexpectedly before producing results.",
                      hint = "This can happen if the server ran out of memory. Try fewer bootstrap samples or fewer cores.")
  }
  if (!identical(job$status, "running")) job$finishedAt <- now_iso()
  proj$jobs[[job_id]] <- job
  job
}

job_payload <- function(job) {
  out <- list(id = job$id, status = job$status, submittedAt = job$submittedAt,
              datasetId = job$datasetId,
              bootstrap = list(enabled = isTRUE(job$settings$bootstrap$enabled),
                               nboot = job$settings$bootstrap$nboot, cores = job$settings$bootstrap$cores))
  if (identical(job$status, "running")) out$progress <- read_progress(job$dir)
  out$kind <- job$kind %||% "plssem"
  if (identical(job$status, "completed")) out$resultId <- job$resultId
  if (identical(job$status, "failed")) out$error <- job$error
  if (!is.null(job$finishedAt)) out$finishedAt <- job$finishedAt
  out
}

#' Cancel the project's running QCA jobs (a newer run replaces them).
cancel_qca_jobs <- function(proj) {
  for (id in names(proj$jobs)) if (identical(proj$jobs[[id]]$kind, "qca") && identical(proj$jobs[[id]]$status, "running")) cancel_job(proj, id)
}

cancel_job <- function(proj, job_id) {
  job <- refresh_job(proj, job_id)
  if (identical(job$status, "running")) {
    try(job$process$kill_tree(), silent = TRUE)
    job$status <- "cancelled"
    job$finishedAt <- now_iso()
    proj$jobs[[job_id]] <- job
  }
  job
}

read_result_json <- function(proj, result_id) {
  path <- proj$results[[result_id]]
  if (is.null(path) || !file.exists(path)) api_error(404, "result_not_found", "The analysis result was not found.", "Run the analysis again.")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

read_result <- function(proj, result_id) {
  jsonlite::fromJSON(read_result_json(proj, result_id), simplifyVector = FALSE)
}
