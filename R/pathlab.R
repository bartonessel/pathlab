# PathLab SEM as an R package that runs on the user's computer and opens in the browser, in the way
# bibliometrix::biblioshiny() does. The app itself (the R backend, the built web interface and the sample data) is
# installed under inst/app with the same layout as the repository, and run() starts it in a separate R process, so the
# user's own R session stays clean and the app behaves exactly as it does in development.

.pathlab <- new.env(parent = emptyenv())

# Packages that cannot be required at install time: WEKA learners need a 64-bit Java (rJava, RWeka); av needs FFmpeg and
# pdftools needs Poppler on Linux. Each feature explains in the app when its package is missing.
EXTRAS <- c(RWeka = "WEKA learners (Machine learning); needs a 64-bit Java",
            rJava = "Java bridge for the WEKA learners",
            av = "audio and video recordings (Text model)",
            pdftools = "reading PDF documents (Text model)",
            blavaan = "Bayesian CB-SEM; installs rstan (a large download)")

app_dir <- function() {
  d <- system.file("app", package = "pathlab")
  if (!nzchar(d) || !file.exists(file.path(d, "backend", "run.R")))
    stop("The PathLab app files are missing from the installed package. Reinstall pathlab.", call. = FALSE)
  d
}

free_port <- function(host) {
  for (i in seq_len(20)) {
    p <- httpuv::randomPort(min = 8001L, max = 8999L, host = host)
    if (!is.null(p)) return(p)
  }
  stop("No free port was found between 8001 and 8999. Give one with run(port = ...).", call. = FALSE)
}

alive <- function(url) {
  h <- curl::new_handle(timeout = 2, connecttimeout = 2)
  r <- tryCatch(curl::curl_fetch_memory(paste0(url, "/api/health"), handle = h), error = function(e) NULL)
  !is.null(r) && r$status_code == 200
}

# RStudio Server and Posit Cloud reach a port of the server through their own address (…/p/<id>/): the address to show
# the user and open in their browser. Elsewhere, the local address itself.
user_url <- function(url) {
  if (!identical(Sys.getenv("RSTUDIO_PROGRAM_MODE"), "server") || !requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) return(url)
  u <- tryCatch(rstudioapi::translateLocalUrl(paste0(url, "/"), absolute = TRUE), error = function(e) url)
  sub("/?$", "/", u)   # the app's paths are relative to this address
}

# In the IDE's Viewer pane (RStudio, Positron) when asked for and available; otherwise in the web browser.
open_app <- function(url, viewer) {
  v <- getOption("viewer")
  if (isTRUE(viewer) && is.function(v)) v(url) else utils::browseURL(user_url(url))
}

log_tail <- function(file, n = 15) {
  if (!file.exists(file)) return("")
  x <- readLines(file, warn = FALSE)
  paste(utils::tail(x, n), collapse = "\n")
}

#' Start PathLab SEM
#'
#' Starts the app on this computer and opens it in the web browser. See ?run.
run <- function(port = NULL, host = "127.0.0.1", launch.browser = TRUE, viewer = FALSE, background = FALSE, timeout = 180) {
  if (!is.null(.pathlab$proc) && .pathlab$proc$is_alive()) {
    message("PathLab is already running at ", user_url(.pathlab$url), ". Opening it again.")
    if (isTRUE(launch.browser)) open_app(.pathlab$url, viewer)
    return(invisible(.pathlab$url))
  }
  app <- app_dir()
  port <- if (is.null(port)) free_port(host) else as.integer(port)
  url <- sprintf("http://%s:%d", host, port)
  work <- file.path(tempdir(), "pathlab")
  dir.create(work, showWarnings = FALSE, recursive = TRUE)
  log <- file.path(work, "pathlab.log")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  # The app reads its settings from environment variables; anything the user has set (an AI key, a data folder, …)
  # is passed on as well.
  # The same package libraries as this session (where pathlab and its packages were installed).
  env <- c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
           PATHLAB_HOST = host, PATHLAB_PORT = as.character(port),
           PATHLAB_BACKEND_DIR = file.path(app, "backend"),
           PATHLAB_STATIC_DIR = file.path(app, "frontend", "dist"),
           PATHLAB_DATA_DIR = Sys.getenv("PATHLAB_DATA_DIR", file.path(work, "data")))
  message("Starting PathLab SEM ", utils::packageVersion("pathlab"), " (the first start can take a minute while R loads the packages)...")
  proc <- processx::process$new(rscript, c("--no-save", "--no-restore", file.path(app, "backend", "run.R")), env = env, wd = work,
                                stdout = log, stderr = "2>&1", cleanup_tree = TRUE)
  waited <- 0
  while (!alive(url)) {
    if (!proc$is_alive()) {
      stop("PathLab could not start. Last lines of its log (", log, "):\n", log_tail(log),
           "\n\nRun pathlab::check_setup() to see which packages are missing.", call. = FALSE)
    }
    if (waited >= timeout) {
      proc$kill_tree()
      stop("PathLab did not answer within ", timeout, " seconds. Its log is in ", log, call. = FALSE)
    }
    Sys.sleep(0.5); waited <- waited + 0.5
  }
  .pathlab$proc <- proc; .pathlab$url <- url; .pathlab$log <- log
  message("PathLab SEM is running at ", user_url(url))
  if (isTRUE(launch.browser)) open_app(url, viewer)
  if (isTRUE(background)) {
    message("It keeps running in the background; stop it with pathlab::stop_app().")
    return(invisible(url))
  }
  message("Press Esc (RStudio) or Ctrl+C (console) to stop it.")
  on.exit(stop_app(quiet = TRUE), add = TRUE)
  tryCatch({ while (proc$is_alive()) Sys.sleep(0.5) }, interrupt = function(e) invisible(NULL))
  if (!proc$is_alive() && proc$get_exit_status() %in% c(0L, NA)) message("PathLab has stopped.")
  invisible(url)
}

#' RStudio addins (Addins menu): start PathLab in the web browser or in the Viewer pane, R staying free for other work.
addin_start <- function() run(background = TRUE)
addin_viewer <- function() run(background = TRUE, viewer = TRUE)

#' Stop PathLab SEM started with run(background = TRUE)
stop_app <- function(quiet = FALSE) {
  p <- .pathlab$proc
  if (is.null(p) || !p$is_alive()) {
    if (!quiet) message("PathLab is not running.")
    return(invisible(FALSE))
  }
  p$kill_tree()
  .pathlab$proc <- NULL
  if (!quiet) message("PathLab has stopped. Projects you did not save to a file are gone; saved .plj files open again.")
  invisible(TRUE)
}

#' Check that everything PathLab needs is installed
check_setup <- function() {
  desc <- utils::packageDescription("pathlab")
  pkgs <- function(field) {
    x <- trimws(strsplit(gsub("\\([^)]*\\)", "", desc[[field]] %||% ""), ",")[[1]])
    x[nzchar(x)]
  }
  have <- function(p) nzchar(system.file(package = p))   # installed (without loading it)
  core <- setdiff(pkgs("Imports"), c("stats", "utils", "graphics", "grDevices", "tools", "methods", "parallel"))
  miss <- core[!vapply(core, have, logical(1))]
  extra <- names(EXTRAS)[!vapply(names(EXTRAS), have, logical(1))]
  java <- have("rJava") && isTRUE(tryCatch({ rJava::.jinit(); TRUE }, error = function(e) FALSE))
  # Where the app looks for Quarto (backend/R/pubtables.R quarto_bin()), including the copy that comes with RStudio.
  qc <- c(Sys.getenv("PATHLAB_QUARTO"), Sys.which("quarto"), "C:/Program Files/Quarto/bin/quarto.exe",
          file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "quarto.exe"),
          "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe", "C:/Program Files/Posit/RStudio/resources/app/bin/quarto/bin/quarto.exe",
          "/usr/local/bin/quarto", "/opt/quarto/bin/quarto", "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto")
  quarto <- any(nzchar(qc) & file.exists(qc))
  ok <- function(b) if (b) "OK     " else "MISSING"
  cat(sprintf("PathLab SEM %s on %s\n\n", desc$Version, R.version.string))
  cat(ok(!length(miss)), " required packages", if (length(miss)) paste0(": ", paste(miss, collapse = ", ")), "\n", sep = "")
  cat(ok(!length(extra)), " optional packages", if (length(extra)) paste0(": ", paste(extra, collapse = ", "), "  (pathlab::install_extras())"), "\n", sep = "")
  cat(ok(java), " Java for the WEKA learners", if (!java) "  (optional: install a 64-bit Java, e.g. Eclipse Temurin, then install_extras())", "\n", sep = "")
  cat(ok(quarto), " Quarto for Word reports", if (!quarto) "  (optional: https://quarto.org/docs/get-started/)", "\n", sep = "")
  if (length(miss)) cat("\nInstall the missing required packages with:\n  install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))\n", sep = "")
  invisible(list(missing = miss, extras = extra, java = java, quarto = quarto))
}

#' Install the optional packages
install_extras <- function(which = c("RWeka", "rJava", "av", "pdftools", "blavaan"), repos = getOption("repos")) {
  which <- intersect(which, names(EXTRAS))
  todo <- which[!vapply(which, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(todo)) { message("All optional packages are installed."); return(invisible(character())) }
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  message("Installing: ", paste(sprintf("%s (%s)", todo, EXTRAS[todo]), collapse = "; "))
  utils::install.packages(todo, repos = repos)
  if (any(c("RWeka", "rJava") %in% todo)) message("The WEKA learners also need a 64-bit Java. Check with pathlab::check_setup().")
  invisible(todo)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
