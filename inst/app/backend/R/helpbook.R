# PDF of the help center's ebook. The browser builds the book as paginated HTML (paged.js: cover, contents with page
# numbers, chapters, running headers); Chrome, Chromium or Edge prints it here through chromote. Without chromote or a
# browser the app prints the same pages from the user's browser instead (Save as PDF).

help_book_browser <- function() {
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (!is.null(chrome) && length(chrome) && nzchar(chrome)) return(chrome)
  edge <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe", "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
            "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge", "/usr/bin/microsoft-edge", "/usr/bin/chromium", "/usr/bin/chromium-browser")
  edge <- edge[file.exists(edge)]
  if (length(edge)) edge[1] else NULL
}

#' The book's PDF (raw bytes) from its paginated HTML; waits until the page sets `window.__bookReady`.
help_book_pdf <- function(html, timeout = 240) {
  if (!requireNamespace("chromote", quietly = TRUE)) stop("PDF export needs the R package chromote: install.packages(\"chromote\").")
  browser <- help_book_browser()
  if (is.null(browser)) stop("PDF export needs Chrome, Chromium or Microsoft Edge on this computer.")
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) Sys.setenv(CHROMOTE_CHROME = browser)
  if (!nzchar(html) || nchar(html, type = "bytes") > 60 * 1024^2) stop("The book could not be prepared.")
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  writeLines(enc2utf8(html), f, useBytes = TRUE)
  b <- chromote::ChromoteSession$new(width = 1240, height = 1754)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Page$navigate(paste0("file:///", sub("^/", "", normalizePath(f, winslash = "/"))), wait_ = TRUE, timeout_ = 60)
  t0 <- Sys.time()
  repeat {
    ok <- tryCatch(isTRUE(b$Runtime$evaluate("window.__bookReady === true", timeout_ = 10)$result$value), error = function(e) FALSE)
    if (ok) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("Laying out the book took too long.")
    Sys.sleep(0.5)
  }
  r <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE, displayHeaderFooter = FALSE, timeout_ = timeout)
  jsonlite::base64_dec(r$data)
}
