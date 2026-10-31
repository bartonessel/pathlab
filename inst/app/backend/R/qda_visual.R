# Images and PDF pages in the Text model: coding rectangular regions (NVivo / ATLAS.ti / MAXQDA picture and PDF region
# coding). An image is a document without text; a PDF keeps its extracted text (coded as text) and its pages (coded as
# regions). Regions are relative to the page (0–1), so they do not depend on the size at which a page is shown. Pages
# of a PDF are rendered once (pdftools) and cached in the session folder.

VISUAL_TYPES <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif", webp = "image/webp", bmp = "image/bmp")
visual_is <- function(filename) tolower(tools::file_ext(filename)) %in% names(VISUAL_TYPES)
visual_dims <- function(path) { v <- tryCatch(av::av_media_info(path)$video, error = function(e) NULL); if (NROW(v)) c(as.integer(v$width[1]), as.integer(v$height[1])) else c(NA_integer_, NA_integer_) }

text_import_image <- function(proj, raw, filename) {
  ext <- tolower(tools::file_ext(filename))
  dir <- file.path(proj$dir, "media"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- text_new_doc(proj, tools::file_path_sans_ext(basename(filename)), filename, ext, "")
  path <- file.path(dir, paste0(d$id, ".", ext)); writeBin(raw, path)
  wh <- visual_dims(path)
  if (is.na(wh[1])) { unlink(path); proj$documents[[d$id]] <- NULL; api_error(422, "image_unreadable", sprintf("%s could not be read as an image.", filename)) }
  d$visual <- list(type = "image", file = path, mime = unname(VISUAL_TYPES[[ext]]), ext = ext, pages = 1L, width = wh[1], height = wh[2])
  proj$documents[[d$id]] <- d
  d
}

#' Keep a PDF's file with its document, so its pages can be shown and coded as regions.
text_attach_pdf <- function(proj, d, raw) {
  if (is.null(proj$dir)) return(d)   # no session folder: the text is imported, the pages are not kept
  dir <- file.path(proj$dir, "media"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(d$id, ".pdf")); writeBin(raw, path)
  n <- tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NA_integer_)
  if (is.na(n) || n < 1) { unlink(path); return(d) }
  d$visual <- list(type = "pdf", file = path, mime = "application/pdf", ext = "pdf", pages = as.integer(n))
  proj$documents[[d$id]] <- d
  d
}

visual_meta <- function(d) if (is.null(d$visual)) NULL else list(type = d$visual$type, pages = d$visual$pages, available = !is.null(d$visual$file) && file.exists(d$visual$file))

#' One page as an image: the image itself, or a PDF page rendered at 110 dpi (cached).
visual_page <- function(proj, tid, page) {
  d <- proj$documents[[tid]]
  if (is.null(d$visual) || is.null(d$visual$file) || !file.exists(d$visual$file)) api_error(404, "visual_missing", "The image or PDF is not in this session.", "Import the file again.")
  page <- suppressWarnings(as.integer(page)); if (is.na(page) || page < 1 || page > d$visual$pages) api_error(404, "visual_page", "There is no such page.")
  if (d$visual$type == "image") return(list(path = d$visual$file, mime = d$visual$mime))
  out <- sub("\\.pdf$", sprintf("_p%03d.png", page), d$visual$file)
  if (!file.exists(out)) suppressWarnings(pdftools::pdf_convert(d$visual$file, format = "png", pages = page, dpi = 110, filenames = out, verbose = FALSE))
  list(path = out, mime = "image/png")
}

#' Whether two coded segments overlap: text spans, or rectangles on the same page.
qda_overlap <- function(a, b) {
  ra <- a$region; rb <- b$region
  if (is.null(ra) && is.null(rb)) return(a$start < b$end && b$start < a$end)
  if (is.null(ra) || is.null(rb)) return(FALSE)
  ra$page == rb$page && ra$x < rb$x + rb$w && rb$x < ra$x + ra$w && ra$y < rb$y + rb$h && rb$y < ra$y + ra$h
}
