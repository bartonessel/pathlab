# Start the PathLab SEM API.
#
#   Rscript backend/run.R            (from the repository root)
#
# Environment variables:
#   PATHLAB_HOST               default 127.0.0.1
#   PATHLAB_PORT               default 8000
#   PATHLAB_MAX_UPLOAD_MB      default 25
#   PATHLAB_SESSION_TTL_HOURS  idle hours before a project's data are deleted (default 6)
#   PATHLAB_MAX_JOBS           concurrent analysis processes (default 2)
#   PATHLAB_MAX_CORES          max cores a bootstrap may use (default 2)
#   PATHLAB_DATA_DIR           temporary data directory (default: R session tempdir)
#   PATHLAB_STATIC_DIR         built frontend to serve at / (default: frontend/dist if present)
#   PATHLAB_CORS_ORIGIN        allowed browser origin for cross-origin use (default: none)
#   ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, AZURE_OPENAI_API_KEY + AZURE_OPENAI_ENDPOINT
#                              optional AI assistant keys for every session (users can instead paste their own key,
#                              which is kept in memory for their project session only)
#   PATHLAB_AI_MOCK            "1" adds an offline test provider (for tests and demos without a key)

suppressPackageStartupMessages({
  library(plumber)
  library(seminr)
  library(jsonlite)
})

args_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
backend_dir <- if (nzchar(Sys.getenv("PATHLAB_BACKEND_DIR"))) {
  normalizePath(Sys.getenv("PATHLAB_BACKEND_DIR"))
} else if (length(args_file)) normalizePath(dirname(args_file[1])) else normalizePath("backend")
repo_dir <- dirname(backend_dir)

for (f in c("util.R", "data_import.R", "model_spec.R", "advanced.R", "analysis.R", "store.R", "jobs.R", "exports.R", "pubtables.R", "efa.R", "regression.R", "reports_extra.R", "fimix.R", "cbsem.R", "gsca.R", "biblio.R", "biblio_more.R", "invariance.R", "lpa.R", "derived.R", "qca.R", "ml.R", "stats.R", "panel.R", "three_step.R", "latent_more.R", "stats_more.R", "gpower.R", "soper.R", "screening.R", "explore.R", "compute.R", "stats_report.R", "ggbuilder.R", "gg3d.R", "text.R", "qda.R", "qda_tools.R", "qda_report.R", "media.R", "text_sources.R", "qda_ai.R", "qda_visual.R", "text_quant.R", "ai.R")) {
  source(file.path(backend_dir, "R", f))
}
BACKEND_DIR <- backend_dir
SAMPLE_DIR <- file.path(repo_dir, "sample")

# Recordings (Text model) may be larger than data files.
options(plumber.maxRequestSize = (max(as.numeric(Sys.getenv("PATHLAB_MAX_UPLOAD_MB", "25")), as.numeric(Sys.getenv("PATHLAB_MAX_MEDIA_MB", "500"))) + 1) * 1024^2)

redact_route <- function(path) gsub("/(p|d|j|r)[a-z0-9]{12}", "/:\\1id", path %||% "")

build_router <- function() {
  pr <- plumber::pr(file.path(backend_dir, "plumber.R"), envir = globalenv())

  pr$setErrorHandler(function(req, res, err) {
    if (inherits(err, "pathlab_error")) {
      res$status <- err$status
      body <- list(error = list(code = err$code, message = conditionMessage(err)))
      if (!is.null(err$hint)) body$error$hint <- err$hint
      if (!is.null(err$details)) body$error$details <- err$details
    } else {
      # Never echo raw R errors (they can contain data values); log a sanitised line.
      message(sprintf("[%s] ERROR %s %s: %s", now_iso(), req$REQUEST_METHOD, redact_route(req$PATH_INFO),
                      sanitize_message(conditionMessage(err), 200)))
      res$status <- 500
      body <- list(error = list(code = "internal_error", message = "The server could not complete the request.",
                                hint = "Try again. If the problem persists, restart the backend and check its log."))
    }
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    res$body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    res
  })
  pr$set404Handler(function(req, res) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    res$body <- jsonlite::toJSON(list(error = list(code = "not_found", message = "Unknown API route.")), auto_unbox = TRUE)
    res
  })

  # Request log: method, route, status, duration only (no bodies, no tokens).
  pr$registerHooks(list(
    preroute = function(req) { req$.pl_start <- Sys.time() },
    postserialize = function(req, res) {
      if (startsWith(req$PATH_INFO %||% "", "/api/") && !grepl("/jobs/", req$PATH_INFO)) {
        ms <- round(as.numeric(difftime(Sys.time(), req$.pl_start %||% Sys.time(), units = "secs")) * 1000)
        message(sprintf("[%s] %s %s -> %s (%d ms)", now_iso(), req$REQUEST_METHOD, redact_route(req$PATH_INFO), res$status, ms))
      }
    }
  ))

  cors <- Sys.getenv("PATHLAB_CORS_ORIGIN", "")
  if (nzchar(cors)) {
    pr$filter("cors", function(req, res) {
      res$setHeader("Access-Control-Allow-Origin", cors)
      res$setHeader("Access-Control-Allow-Headers", "Content-Type, X-PathLab-Token")
      res$setHeader("Access-Control-Allow-Methods", "GET, POST, PATCH, DELETE, OPTIONS")
      if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 204; return(list()) }
      plumber::forward()
    })
  }

  static_dir <- Sys.getenv("PATHLAB_STATIC_DIR", file.path(repo_dir, "frontend", "dist"))
  if (dir.exists(static_dir)) {
    # Mount only /assets so /api/* (JSON 404s) and /__docs__/ keep working.
    pr$mount("/assets", plumber::PlumberStatic$new(file.path(static_dir, "assets")))
    serve_file <- function(name, type) {
      force(name)
      function(req, res) {
        res$setHeader("Content-Type", type)
        res$setHeader("Cache-Control", "no-cache")
        res$body <- readBin(file.path(static_dir, name), "raw", file.info(file.path(static_dir, name))$size)
        res
      }
    }
    pr$handle("GET", "/", serve_file("index.html", "text/html; charset=utf-8"), serializer = plumber::serializer_octet())
    for (f in c("favicon.png", "plssem_icon.png")) if (file.exists(file.path(static_dir, f))) {
      pr$handle("GET", paste0("/", f), serve_file(f, "image/png"), serializer = plumber::serializer_octet())
    }
    message("Serving frontend from ", static_dir)
  }
  pr
}

schedule_cleanup <- function() {
  later::later(function() {
    try(cleanup_expired_projects(), silent = TRUE)
    schedule_cleanup()
  }, delay = 600)
}

if (sys.nframe() == 0L || identical(Sys.getenv("PATHLAB_AUTOSTART"), "1")) {
  host <- Sys.getenv("PATHLAB_HOST", "127.0.0.1")
  port <- as.integer(Sys.getenv("PATHLAB_PORT", "8000"))
  schedule_cleanup()
  message(sprintf("PathLab SEM API (seminr %s, %s) on http://%s:%d  - docs at /__docs__/",
                  seminr_version(), R.version.string, host, port))
  build_router()$run(host = host, port = port, docs = TRUE, quiet = TRUE)
}
