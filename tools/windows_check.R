# =============================================================================
# tools/windows_check.R: one-command check that the dashboard works on this
# computer (written for Windows; works anywhere).
# Usage, from the project folder (temporary-workers-dashboard):
#   Rscript tools/windows_check.R
# Steps:
#   1. Prints R, OS and locale details.
#   2. Checks the date helpers print English dates without "-d"/"-I" glitches.
#   3. Downloads the IRCC test files if needed (tools/get_testdata.R).
#   4. Runs the full test suite (tests/testthat.R; browser tests need Chrome
#      or Edge; set CHROMOTE_CHROME to Edge's path if Chrome isn't installed).
#   5. Starts the app in the background, waits until it answers, and checks
#      the page header shows the data status with a clean date.
# =============================================================================
required_packages <- c("shiny", "testthat", "callr", "curl")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
Sys.setenv(NOT_CRAN = "true")
ok <- TRUE
say <- function(...) cat(sprintf(...), "\n")

# 1. Environment
say("R %s on %s | LC_TIME = %s | encoding UTF-8: %s", getRversion(), R.version$platform,
    Sys.getlocale("LC_TIME"), l10n_info()$`UTF-8`)

# 2. Date helpers (the cause of "October -d, 2026" on Windows in older code)
source("R/calc.R")
dt <- fmt_datetime(as.POSIXct("2026-10-16 23:50:00", tz = "UTC"))
say("fmt_datetime test: %s", dt)
if (!identical(dt, "October 16, 2026, 4:50 PM PT")) { ok <- FALSE; say("FAIL: date helper") }

# 3. Test data
if (!dir.exists("testdata") || length(list.files("testdata")) < 4) {
  say("Downloading IRCC test files ...")
  system2(file.path(R.home("bin"), "Rscript"), "tools/get_testdata.R")
}

# 4. Full test suite (runs in a separate R process, output shown here)
say("Running the full test suite (about 10-20 minutes) ...")
status <- system2(file.path(R.home("bin"), "Rscript"), "tests/testthat.R")
say("Test suite exit status: %s", status)
if (!identical(as.integer(status), 0L)) { ok <- FALSE; say("FAIL: test suite (see output above)") }

# 5. Headless app start
port <- 8765L
say("Starting the app on port %d ...", port)
app <- callr::r_bg(function(port) shiny::runApp(".", port = port, launch.browser = FALSE),
                   args = list(port = port))
html <- NULL; t0 <- Sys.time()
while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 180 && app$is_alive()) {
  res <- tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", port)),
                  error = function(e) NULL)
  if (!is.null(res) && res$status_code == 200) { html <- rawToChar(res$content); break }
  Sys.sleep(2)
}
secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
if (is.null(html)) {
  ok <- FALSE; say("FAIL: the app did not answer within 180 s")
  if (!app$is_alive()) cat(app$read_all_error())
} else {
  Encoding(html) <- "UTF-8"
  status_line <- regmatches(html, regexpr("Data last retrieved:.{0,200}?Latest month available:[^<]*<[^>]*>[^<]*", html))
  say("App answered after %d s. Header: %s", secs, gsub("<[^>]+>|\\s+", " ", status_line))
  if (grepl("-d,|-I:|%", status_line)) { ok <- FALSE; say("FAIL: date glitch in the header") }
  if (grepl("saved copy", html)) say("Note: the app is using the saved copy (IRCC not reachable from here).")
}
app$kill()
say(if (ok) "ALL CHECKS PASSED" else "SOME CHECKS FAILED (see above)")
