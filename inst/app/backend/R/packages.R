# Installing an optional R package from inside the app, for people who do not work in R. A feature that needs a
# package it has not got answers 501 with the package names in `details` (need_packages() in util.R); the app then
# offers a button instead of a sentence telling the user to type install.packages().
#
# The rules are deliberately narrow, so this never disturbs the R installation the user already has:
#   - only the packages on PKG_OPTIONAL below, never a name the browser sends,
#   - only packages that are absent; a package already installed is never touched, so nothing is upgraded,
#     re-compiled or replaced, and the "cannot remove prior installation" trap cannot arise,
#   - into the library the app itself loads from (.libPaths()[1]), and only when that is writable,
#   - in a separate R process, so the options it sets (ready-built packages, a longer download limit) live and die
#     with that process and never reach the user's own R session,
#   - update.packages() is never called.
# Everything else about the machine's R is left exactly as it was.

PKG_OPTIONAL <- list(
  bibliometrix     = list(feature = "Bibliometric analysis",         syslib = "Poppler on Linux (poppler-cpp-devel or libpoppler-cpp-dev)"),
  bibliometrixData = list(feature = "the sample bibliographic collection", syslib = NULL),
  cSEM             = list(feature = "GSCA analyses",                 syslib = "GMP on Linux (gmp-devel or libgmp-dev)"),
  pdftools         = list(feature = "reading PDF documents",         syslib = "Poppler on Linux (poppler-cpp-devel or libpoppler-cpp-dev)"),
  av               = list(feature = "audio and video recordings",    syslib = "FFmpeg"),
  blavaan          = list(feature = "Bayesian CB-SEM",               syslib = NULL),
  RWeka            = list(feature = "the WEKA learners",             syslib = "a 64-bit Java"),
  rJava            = list(feature = "the WEKA learners",             syslib = "a 64-bit Java"),
  chromote         = list(feature = "the help book as a PDF",        syslib = "Google Chrome or Chromium"),
  openalexR        = list(feature = "searching OpenAlex",            syslib = NULL),
  pubmedR          = list(feature = "searching PubMed",              syslib = NULL),
  maps             = list(feature = "the world map",                 syslib = NULL),
  udpipe           = list(feature = "parts of speech",               syslib = NULL)
)

#' The library the app loads from, and whether this user may write to it.
pkg_library <- function() {
  lib <- .libPaths()[1]
  list(path = lib, writable = !is.na(file.access(lib, 2)) && file.access(lib, 2) == 0)
}

pkg_repos <- function() {
  r <- getOption("repos")
  cran <- if (is.null(r) || is.na(r["CRAN"]) || !nzchar(r["CRAN"]) || identical(unname(r["CRAN"]), "@CRAN@")) "https://cloud.r-project.org" else unname(r["CRAN"])
  c(CRAN = cran)
}

#' What the app may install, and what is already there (the Settings view and the install panel read this).
pkg_optional_status <- function() {
  lib <- pkg_library()
  list(library = lib$path, writable = lib$writable,
       packages = unname(lapply(names(PKG_OPTIONAL), function(p) {
         ok <- requireNamespace(p, quietly = TRUE)
         list(name = p, feature = PKG_OPTIONAL[[p]]$feature, installed = ok,
              version = if (ok) as.character(utils::packageVersion(p)) else NULL,
              systemLibrary = PKG_OPTIONAL[[p]]$syslib)
       })))
}

#' The packages of a request that this may install: on the list, and not already installed.
pkg_requested <- function(packages) {
  want <- unique(as.character(unlist(packages %||% list())))
  want <- want[nzchar(want)]
  if (!length(want)) api_error(422, "pkg_none", "No package was named.")
  unknown <- setdiff(want, names(PKG_OPTIONAL))
  if (length(unknown))
    api_error(422, "pkg_not_optional", sprintf("PathLab does not install %s from here.", paste(unknown, collapse = ", ")),
              "Only the optional packages of a PathLab feature can be installed from the app.")
  want[!vapply(want, requireNamespace, logical(1), quietly = TRUE)]
}

pkg_progress <- function(job_dir, fraction, message) {
  tmp <- file.path(job_dir, "progress.tmp"); jsonlite::write_json(list(fraction = fraction, message = message), tmp, auto_unbox = TRUE)
  file.rename(tmp, file.path(job_dir, "progress.json"))
}

#' Start the install in its own R process. One at a time, so two panels cannot install at once.
pkg_install_start <- function(proj, packages) {
  todo <- pkg_requested(packages)
  if (!length(todo)) return(list(id = NA, status = "completed", packages = list(), installed = TRUE,
                                 progress = list(fraction = 1, message = "Already installed.")))
  lib <- pkg_library()
  if (!lib$writable)
    api_error(409, "pkg_library_readonly", sprintf("PathLab cannot write to the R library at %s.", lib$path),
              "Ask whoever looks after this computer to install the package, or run PathLab with a library of your own.")
  if (any(vapply(proj$pkgJobs %||% list(), function(j) identical(j$status, "running") && j$process$is_alive(), TRUE)))
    api_error(409, "pkg_job_running", "PathLab is already installing a package.", "Wait for it to finish.")
  job_id <- new_id("p"); job_dir <- file.path(proj$dir, "jobs", job_id); dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(packages = todo, lib = lib$path, repos = pkg_repos()), file.path(job_dir, "input.rds"))
  proc <- callr::r_bg(function(backend_dir, job_dir) {
    for (f in c("util.R", "packages.R")) source(file.path(backend_dir, "R", f), local = FALSE)
    pkg_worker(job_dir)
  }, args = list(backend_dir = BACKEND_DIR, job_dir = job_dir), stdout = NULL, stderr = file.path(job_dir, "worker.log"), supervise = TRUE)
  proj$pkgJobs <- proj$pkgJobs %||% list()
  proj$pkgJobs[[job_id]] <- list(id = job_id, dir = job_dir, process = proc, status = "running", packages = todo, started = Sys.time())
  list(id = job_id, status = "running", packages = as.list(todo),
       progress = list(fraction = 0.02, message = sprintf("Starting to install %s", paste(todo, collapse = ", "))))
}

pkg_job_status <- function(proj, job_id) {
  j <- proj$pkgJobs[[job_id]]; if (is.null(j)) api_error(404, "pkg_job_missing", "The install was not found.")
  out_path <- file.path(j$dir, "out.rds")
  if (identical(j$status, "running")) {
    if (file.exists(out_path)) {
      try(j$process$wait(3000), silent = TRUE)
      o <- readRDS(out_path)
      j$status <- o$status
      if (!identical(o$status, "completed")) j$error <- o$error
    } else if (!j$process$is_alive()) {
      log <- tryCatch(paste(utils::tail(readLines(file.path(j$dir, "worker.log"), warn = FALSE), 4), collapse = " "), error = function(e) "")
      j$status <- "failed"
      j$error <- list(message = "The install stopped unexpectedly.", hint = if (nzchar(log)) sanitize_message(log, 300) else NULL)
    }
    proj$pkgJobs[[job_id]] <- j
  }
  prog <- tryCatch(jsonlite::read_json(file.path(j$dir, "progress.json")), error = function(e) NULL)
  list(id = j$id, status = j$status, packages = as.list(j$packages), error = j$error,
       installed = identical(j$status, "completed"),
       progress = prog %||% list(fraction = if (identical(j$status, "completed")) 1 else 0, message = ""))
}

pkg_job_cancel <- function(proj, job_id) {
  j <- proj$pkgJobs[[job_id]]; if (is.null(j)) api_error(404, "pkg_job_missing", "The install was not found.")
  if (identical(j$status, "running")) try(j$process$kill_tree(), silent = TRUE)
  j$status <- "cancelled"; proj$pkgJobs[[job_id]] <- j
  list(id = job_id, status = "cancelled")
}

pkg_worker <- function(job_dir) {
  inp <- readRDS(file.path(job_dir, "input.rds"))
  out <- tryCatch(list(status = "completed", result = pkg_install_run(inp, job_dir)),
                  error = function(e) list(status = "failed", error = list(message = conditionMessage(e), hint = attr(e, "hint"))))
  saveRDS(out, file.path(job_dir, "out.rds"))
}

#' Install the packages, one at a time so the progress says which. This runs in its own process: the options set
#' here never reach the app's R session or the user's.
pkg_install_run <- function(inp, job_dir) {
  win_or_mac <- .Platform$OS.type == "windows" || identical(Sys.info()[["sysname"]], "Darwin")
  if (win_or_mac) options(pkgType = "binary")          # ready-built, so nothing is compiled and no build tools are needed
  options(install.packages.compile.from.source = "never", timeout = 900)
  n <- length(inp$packages)
  for (i in seq_len(n)) {
    p <- inp$packages[i]
    pkg_progress(job_dir, (i - 1) / n + 0.05, sprintf("Installing %s (%d of %d). This can take a minute.", p, i, n))
    # Only `p` and the dependencies it has not got: install.packages leaves installed packages alone.
    try(utils::install.packages(p, lib = inp$lib, repos = inp$repos, dependencies = c("Depends", "Imports", "LinkingTo")), silent = TRUE)
    if (!nzchar(system.file(package = p, lib.loc = c(inp$lib, .libPaths())))) {
      note <- PKG_OPTIONAL[[p]]$syslib
      e <- simpleError(sprintf("%s could not be installed.", p))
      attr(e, "hint") <- if (!is.null(note) && .Platform$OS.type != "windows") sprintf("On this system %s needs %s. Install that first, then try again.", p, note)
                         else "Check the internet connection and try again."
      stop(e)
    }
  }
  pkg_progress(job_dir, 1, sprintf("%s installed.", paste(inp$packages, collapse = ", ")))
  list(installed = as.list(inp$packages))
}
