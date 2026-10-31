# Audio and video documents with transcription (Text model), as in NVivo, ATLAS.ti and MAXQDA. A recording is imported
# as a document whose text is its transcript; the file stays in the project's session folder (never in the project
# file, which keeps the transcript). Transcription runs in a background R process (callr) with progress, either
#   - offline with whisper.cpp (whisper-cli) on this computer, with a Whisper model the user downloads once, or
#   - with OpenAI's whisper-1 (cloud, the user's OpenAI key from AI settings; the audio is sent to OpenAI).
# av (FFmpeg) converts any audio or video to what the engine needs. The transcript keeps a timestamp per segment, so the
# player follows the text and coded passages can be played. Segments can be corrected and given speakers ("Name: …"),
# which the speaker autocoding then picks up.

MEDIA_TYPES <- c(mp3 = "audio/mpeg", wav = "audio/wav", m4a = "audio/mp4", aac = "audio/aac", ogg = "audio/ogg", oga = "audio/ogg", opus = "audio/ogg",
                 flac = "audio/flac", wma = "audio/x-ms-wma", mp4 = "video/mp4", m4v = "video/mp4", mov = "video/quicktime", webm = "video/webm",
                 mkv = "video/x-matroska", avi = "video/x-msvideo")
WHISPER_MODELS <- list(tiny = 75, base = 142, small = 466, medium = 1500, `large-v3-turbo` = 1600)   # approximate download sizes (MB)
WHISPER_MODEL_URL <- "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-%s.bin"
OPENAI_CHUNK_SECONDS <- 600

media_is <- function(filename) tolower(tools::file_ext(filename)) %in% names(MEDIA_TYPES)
pathlab_data_dir <- function(...) { d <- file.path(Sys.getenv("PATHLAB_DATA", file.path(path.expand("~"), ".pathlab")), ...); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
whisper_model_file <- function(size) file.path(pathlab_data_dir("whisper", "models"), sprintf("ggml-%s.bin", size))

#' The whisper.cpp command-line program: PATHLAB_WHISPER_CLI, the path set in the app, the PATH, or the PathLab data folder.
whisper_cli <- function() {
  cfg <- file.path(pathlab_data_dir("whisper"), "cli.txt")
  set <- if (file.exists(cfg)) trimws(readLines(cfg, warn = FALSE)[1]) else ""
  cands <- c(Sys.getenv("PATHLAB_WHISPER_CLI"), set, unname(Sys.which(c("whisper-cli", "whisper-cpp"))),
             file.path(pathlab_data_dir("whisper"), c("whisper-cli.exe", "whisper-cli", file.path("Release", "whisper-cli.exe"))))
  cands <- cands[!is.na(cands) & nzchar(cands) & file.exists(cands)]
  if (length(cands)) normalizePath(cands[1], winslash = "/") else ""
}

#' What can transcribe here (shown in the app before the user chooses).
transcribe_status <- function(proj) {
  cli <- whisper_cli()
  list(av = requireNamespace("av", quietly = TRUE),
       engines = list(
         list(id = "local", label = "whisper.cpp on this computer (offline)", available = nzchar(cli), cli = cli, dataDir = normalizePath(pathlab_data_dir("whisper")),
              models = lapply(names(WHISPER_MODELS), function(m) list(id = m, mb = WHISPER_MODELS[[m]], downloaded = file.exists(whisper_model_file(m))))),
         list(id = "openai", label = "OpenAI whisper-1 (cloud)", available = !is.null(ai_key(proj, "openai")))),
       mock = identical(Sys.getenv("PATHLAB_TRANSCRIBE_MOCK"), "1"))
}

whisper_set_cli <- function(proj, path) {
  path <- trimws(as.character(path %||% ""))
  cfg <- file.path(pathlab_data_dir("whisper"), "cli.txt")
  if (!nzchar(path)) { unlink(cfg); return(transcribe_status(proj)) }
  if (!file.exists(path) || dir.exists(path)) api_error(422, "whisper_cli", "That file does not exist.", "Give the full path of whisper-cli (whisper-cli.exe on Windows).")
  if (!grepl("whisper", basename(path), ignore.case = TRUE)) api_error(422, "whisper_cli", "That does not look like the whisper.cpp program (whisper-cli).")
  writeLines(normalizePath(path, winslash = "/"), cfg)
  transcribe_status(proj)
}

# ---- Import ------------------------------------------------------------------------------------------------------
#' A recording becomes a document with an empty transcript; the file is kept in the session folder.
text_import_media <- function(proj, raw, filename) {
  if (!requireNamespace("av", quietly = TRUE)) api_error(501, "media_unavailable", "The av R package (FFmpeg) is not installed on the analysis server.")
  ext <- tolower(tools::file_ext(filename))
  dir <- file.path(proj$dir, "media"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- text_new_doc(proj, tools::file_path_sans_ext(basename(filename)), filename, ext, "")
  path <- file.path(dir, paste0(d$id, ".", ext)); writeBin(raw, path)
  info <- tryCatch(av::av_media_info(path), error = function(e) NULL)
  if (is.null(info) || is.null(info$audio) || !NROW(info$audio)) {
    unlink(path); proj$documents[[d$id]] <- NULL
    api_error(422, "media_no_audio", sprintf("%s has no audio track that can be read.", filename))
  }
  d$media <- list(file = path, ext = ext, mime = unname(MEDIA_TYPES[[ext]]), duration = as.numeric(info$duration), video = !is.null(info$video) && NROW(info$video) > 0)
  d$segments <- list()
  proj$documents[[d$id]] <- d
  d
}

#' The public part of a document's media (no file path).
media_meta <- function(d) if (is.null(d$media)) NULL else c(d$media[intersect(names(d$media), c("ext", "mime", "duration", "video", "engine", "model", "language", "transcribedAt"))],
                                                          list(available = !is.null(d$media$file) && file.exists(d$media$file)))

# ---- Transcript ----------------------------------------------------------------------------------------------------
#' Text of a transcript (one line per segment, "Speaker: text" when a speaker is set) and each segment's position in it
#' (UTF-16 units, as the coding tools use).
transcript_build <- function(segs) {
  segs <- Filter(function(s) nzchar(trimws(as.character(s$text %||% ""))), segs)
  lines <- vapply(segs, function(s) { sp <- trimws(as.character(s$speaker %||% "")); paste0(if (nzchar(sp)) paste0(sp, ": ") else "", trimws(gsub("[\r\n]+", " ", as.character(s$text)))) }, "")
  text <- paste(lines, collapse = "\n")
  starts <- cumsum(c(0L, nchar(lines) + 1L))[seq_along(lines)]
  js <- if (length(lines)) qda_cp2js(text, c(starts, starts + nchar(lines))) else integer(0)
  n <- length(lines)
  list(text = text, segments = lapply(seq_len(n), function(i) list(start = as.numeric(segs[[i]]$start), end = as.numeric(segs[[i]]$end), text = trimws(as.character(segs[[i]]$text)),
                                                                     speaker = trimws(as.character(segs[[i]]$speaker %||% "")), from = js[i], to = js[n + i])))
}

#' Corrected segments (text and speakers) from the transcript editor; the timestamps are kept.
transcript_save <- function(proj, tid, segs) {
  d <- proj$documents[[tid]]; if (is.null(d)) api_error(404, "text_doc_missing", "The document is not in this session.")
  if (is.null(d$media)) api_error(422, "media_not_media", "This document is not a recording.")
  if (!length(segs)) api_error(422, "transcript_empty", "The transcript has no segments.")
  t <- transcript_build(lapply(segs, function(s) list(start = suppressWarnings(as.numeric(s$start)), end = suppressWarnings(as.numeric(s$end)), text = substr(as.character(s$text %||% ""), 1, 5000), speaker = substr(as.character(s$speaker %||% ""), 1, 60))))
  if (nchar(t$text) > TEXT_MAX_CHARS) api_error(413, "text_too_long", "The transcript is too long.")
  d$text <- t$text; d$segments <- t$segments
  proj$documents[[tid]] <- d
  c(text_meta(d), list(text = d$text, segments = d$segments))
}

# ---- Background jobs (transcription and model downloads) -----------------------------------------------------------
media_progress <- function(job_dir, fraction, message) {
  tmp <- file.path(job_dir, "progress.tmp"); jsonlite::write_json(list(fraction = fraction, message = message), tmp, auto_unbox = TRUE)
  file.rename(tmp, file.path(job_dir, "progress.json"))
}

start_media_job <- function(proj, task, input, env = character(0)) {
  if (any(vapply(proj$mediaJobs %||% list(), function(j) identical(j$status, "running") && j$process$is_alive(), TRUE))) api_error(409, "media_job_running", "A transcription or download is already running.", "Wait for it to finish or cancel it.")
  job_id <- new_id("m"); job_dir <- file.path(proj$dir, "jobs", job_id); dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(c(list(task = task), input), file.path(job_dir, "input.rds"))
  proc <- callr::r_bg(function(backend_dir, job_dir) {
    for (f in c("util.R", "qda.R", "media.R")) source(file.path(backend_dir, "R", f), local = FALSE)
    media_worker(job_dir)
  }, args = list(backend_dir = BACKEND_DIR, job_dir = job_dir), env = c(callr::rcmd_safe_env(), env), stdout = NULL, stderr = file.path(job_dir, "worker.log"), supervise = TRUE)
  proj$mediaJobs <- proj$mediaJobs %||% list()
  proj$mediaJobs[[job_id]] <- list(id = job_id, task = task, docId = input$docId, dir = job_dir, process = proc, status = "running", started = Sys.time(), input = input[c("engine", "model", "language")])
  list(id = job_id, task = task, status = "running", progress = list(fraction = 0, message = "Starting"))
}

#' Poll a job; when a transcription finishes, its transcript becomes the document's text.
media_job_status <- function(proj, job_id) {
  j <- proj$mediaJobs[[job_id]]; if (is.null(j)) api_error(404, "media_job_missing", "The job was not found.")
  out_path <- file.path(j$dir, "out.rds")
  if (identical(j$status, "running")) {
    if (file.exists(out_path)) {
      try(j$process$wait(3000), silent = TRUE)
      o <- readRDS(out_path)
      if (identical(o$status, "completed")) {
        j$status <- "completed"
        if (identical(j$task, "transcribe")) {
          d <- proj$documents[[j$docId]]
          if (!is.null(d)) {
            t <- transcript_build(o$result$segments)
            if (!nzchar(t$text)) { j$status <- "failed"; j$error <- list(message = "No speech was recognised in this recording.", hint = "Check the language, or try a larger model.") }
            else {
              d$text <- t$text; d$segments <- t$segments
              d$media$engine <- j$input$engine; d$media$model <- j$input$model; d$media$language <- o$result$language %||% j$input$language; d$media$transcribedAt <- now_iso()
              proj$documents[[j$docId]] <- d
            }
          }
        }
      } else { j$status <- "failed"; j$error <- o$error }
    } else if (!j$process$is_alive()) {
      j$status <- "failed"
      log <- tryCatch(paste(utils::tail(readLines(file.path(j$dir, "worker.log"), warn = FALSE), 3), collapse = " "), error = function(e) "")
      j$error <- list(message = "The background process stopped unexpectedly.", hint = if (nzchar(log)) sanitize_message(log) else "Try again, or a smaller model.")
    }
    proj$mediaJobs[[job_id]] <- j
  }
  out <- list(id = j$id, task = j$task, status = j$status, docId = j$docId)
  if (identical(j$status, "running")) out$progress <- tryCatch(jsonlite::fromJSON(file.path(j$dir, "progress.json")), error = function(e) list(fraction = 0, message = "Starting"))
  if (identical(j$status, "failed")) out$error <- j$error
  out
}

media_job_cancel <- function(proj, job_id) {
  j <- proj$mediaJobs[[job_id]]; if (is.null(j)) api_error(404, "media_job_missing", "The job was not found.")
  if (identical(j$status, "running")) { try(j$process$kill_tree(), silent = TRUE); j$status <- "cancelled"; proj$mediaJobs[[job_id]] <- j }
  list(id = j$id, status = j$status)
}

#' Start transcribing a recording. b: engine ("local" / "openai"), model (whisper.cpp size), language ("auto" or a code),
#' translate (to English), consent (required for the cloud).
transcribe_start <- function(proj, tid, b) {
  d <- proj$documents[[tid]]; if (is.null(d)) api_error(404, "text_doc_missing", "The document is not in this session.")
  if (is.null(d$media) || is.null(d$media$file) || !file.exists(d$media$file)) api_error(422, "media_missing", "The recording is not in this session.", "Import the audio or video file again.")
  engine <- as.character(b$engine %||% "local")
  lang <- tolower(as.character(b$language %||% "auto")); if (!grepl("^(auto|[a-z]{2,3})$", lang)) lang <- "auto"
  input <- list(docId = tid, path = d$media$file, duration = d$media$duration, language = lang, translate = isTRUE(b$translate), engine = engine)
  if (identical(Sys.getenv("PATHLAB_TRANSCRIBE_MOCK"), "1") && identical(engine, "mock")) return(start_media_job(proj, "transcribe", c(input, list(model = "mock"))))
  if (engine == "local") {
    cli <- whisper_cli(); if (!nzchar(cli)) api_error(422, "whisper_cli_missing", "whisper.cpp is not installed on this computer.", "Download whisper.cpp (whisper-cli) and set its location in Transcribe → Settings.")
    size <- as.character(b$model %||% "base"); if (!size %in% names(WHISPER_MODELS)) size <- "base"
    mf <- whisper_model_file(size); if (!file.exists(mf)) api_error(422, "whisper_model_missing", sprintf("The %s Whisper model has not been downloaded yet.", size), "Download it first (once).")
    return(start_media_job(proj, "transcribe", c(input, list(model = size, modelFile = mf, cli = cli, threads = max(1L, parallel::detectCores() - 1L)))))
  }
  if (engine == "openai") {
    if (!isTRUE(b$consent)) api_error(400, "ai_consent", "Confirm that the recording may be sent to OpenAI first.")
    k <- ai_key(proj, "openai"); if (is.null(k)) api_error(422, "ai_key_missing", "No OpenAI API key is set.", "Add it in the AI assistant's settings, or transcribe offline with whisper.cpp.")
    return(start_media_job(proj, "transcribe", c(input, list(model = "whisper-1")), env = c(PATHLAB_TRANSCRIBE_KEY = k$key)))
  }
  api_error(400, "transcribe_engine", "Unknown transcription engine.")
}

whisper_model_start <- function(proj, size) {
  size <- as.character(size %||% "base"); if (!size %in% names(WHISPER_MODELS)) api_error(400, "whisper_model", "Unknown Whisper model.")
  start_media_job(proj, "model", list(model = size, url = sprintf(WHISPER_MODEL_URL, size), file = whisper_model_file(size)))
}

# ---- Worker (runs in the background R process) ---------------------------------------------------------------------
media_worker <- function(job_dir) {
  inp <- readRDS(file.path(job_dir, "input.rds"))
  out <- tryCatch(list(status = "completed", result = switch(inp$task, transcribe = transcribe_run(inp, job_dir), model = whisper_fetch_model(inp, job_dir))),
                  error = function(e) list(status = "failed", error = list(message = conditionMessage(e), code = e$code %||% "media_failed", hint = e$hint)))
  saveRDS(out, file.path(job_dir, "out.rds"))
}

whisper_fetch_model <- function(inp, job_dir) {
  media_progress(job_dir, 0.01, sprintf("Downloading the %s model (about %d MB) from Hugging Face", inp$model, WHISPER_MODELS[[inp$model]]))
  tmp <- paste0(inp$file, ".part")
  h <- curl::new_handle(); curl::handle_setopt(h, noprogress = FALSE, xferinfofunction = function(total, now, ...) {
    if (total > 0) media_progress(job_dir, now / total, sprintf("Downloading the %s model: %d of %d MB", inp$model, round(now / 1e6), round(total / 1e6))); 0L })
  curl::curl_download(inp$url, tmp, handle = h, quiet = TRUE)
  if (file.info(tmp)$size < 1e6) { unlink(tmp); stop("The download was incomplete.") }
  file.rename(tmp, inp$file)
  list(model = inp$model)
}

transcribe_run <- function(inp, job_dir) {
  dur <- as.numeric(inp$duration %||% 0)
  if (identical(inp$engine, "mock")) {   # tests: fixed segments every 5 seconds
    n <- max(1, ceiling(dur / 5)); media_progress(job_dir, 0.5, "Transcribing")
    return(list(segments = lapply(seq_len(n), function(i) list(start = (i - 1) * 5, end = min(dur, i * 5), text = sprintf("Mock sentence %d about fees.", i))), language = "en"))
  }
  if (identical(inp$engine, "local")) {
    wav <- file.path(job_dir, "audio.wav")
    media_progress(job_dir, 0.02, "Converting the audio (16 kHz mono)")
    av::av_audio_convert(inp$path, wav, channels = 1, sample_rate = 16000, verbose = FALSE)
    args <- c("-m", inp$modelFile, "-f", wav, "-oj", "-of", file.path(job_dir, "whisper"), "-pp", "-l", inp$language, "-t", as.character(inp$threads), if (isTRUE(inp$translate)) "-tr")
    p <- processx::process$new(inp$cli, args, stdout = "|", stderr = "|")
    err <- character(0)
    while (p$is_alive()) {
      p$wait(1000)
      lines <- c(p$read_error_lines(), p$read_output_lines()); err <- utils::tail(c(err, lines), 20)
      pr <- regmatches(lines, regexpr("progress = *[0-9]+%", lines))
      if (length(pr)) { v <- as.numeric(gsub("[^0-9]", "", utils::tail(pr, 1))) / 100; media_progress(job_dir, 0.05 + 0.9 * v, sprintf("Transcribing with whisper.cpp (%d%%)", round(100 * v))) }
    }
    if (!identical(p$get_exit_status(), 0L)) stop(paste("whisper.cpp stopped with an error:", paste(utils::tail(c(err, p$read_all_error_lines()), 3), collapse = " ")))
    j <- jsonlite::fromJSON(file.path(job_dir, "whisper.json"), simplifyVector = FALSE)
    segs <- lapply(j$transcription %||% list(), function(s) list(start = as.numeric(s$offsets$from) / 1000, end = as.numeric(s$offsets$to) / 1000, text = trimws(s$text)))
    return(list(segments = segs, language = j$result$language %||% inp$language))
  }
  if (identical(inp$engine, "openai")) {
    key <- Sys.getenv("PATHLAB_TRANSCRIBE_KEY"); if (!nzchar(key)) stop("No OpenAI API key.")
    starts <- seq(0, max(0, dur - 0.5), by = OPENAI_CHUNK_SECONDS); segs <- list(); lang <- inp$language
    for (k in seq_along(starts)) {
      media_progress(job_dir, (k - 1) / length(starts), sprintf("Sending part %d of %d to OpenAI", k, length(starts)))
      f <- file.path(job_dir, sprintf("part%03d.mp3", k))
      av::av_audio_convert(inp$path, f, channels = 1, sample_rate = 16000, bit_rate = 32000, start_time = starts[k], total_time = OPENAI_CHUNK_SECONDS, verbose = FALSE)
      body <- list(file = curl::form_file(f, type = "audio/mpeg"), model = "whisper-1", response_format = "verbose_json", `timestamp_granularities[]` = "segment")
      if (lang != "auto") body$language <- lang
      req <- httr2::req_timeout(httr2::req_auth_bearer_token(httr2::request(if (isTRUE(inp$translate)) "https://api.openai.com/v1/audio/translations" else "https://api.openai.com/v1/audio/transcriptions"), key), 900)
      req <- httr2::req_error(do.call(httr2::req_body_multipart, c(list(req), body)), is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      r <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
      if (httr2::resp_status(resp) >= 400) stop(sprintf("OpenAI returned an error (HTTP %d): %s", httr2::resp_status(resp), gsub(key, "<key>", r$error$message %||% "", fixed = TRUE)))
      for (s in r$segments %||% list()) segs[[length(segs) + 1]] <- list(start = starts[k] + as.numeric(s$start), end = starts[k] + as.numeric(s$end), text = trimws(s$text))
      if (lang == "auto" && !is.null(r$language)) lang <- r$language
    }
    return(list(segments = segs, language = lang))
  }
  stop("Unknown transcription engine.")
}
