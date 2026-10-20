# =============================================================================
# tools/run_local.R: start the dashboard on this computer and open it in the
# browser. Installs nothing.
#   Rscript tools/run_local.R         the IRCC release of September 15, 2026 (default)
#   Rscript tools/run_local.R live    download IRCC's current files instead; a download
#                                     must pass the check against the shipped release,
#                                     otherwise the saved copy is shown with a notice
# Windows: run_dashboard.bat does this with a double-click (run_dashboard.bat live
# from a Command Prompt for live data). If a library/ folder is next to app.R (the
# offline Windows package), only that library and R's own packages are used, never
# the user's personal library. Set TW_PORT to choose the port, TW_NO_BROWSER=1 to
# not open a browser.
# =============================================================================
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
proj <- if (length(file_arg)) normalizePath(file.path(dirname(file_arg[1]), "..")) else getwd()
setwd(proj)
lib <- file.path(proj, "library")
if (dir.exists(lib)) .libPaths(lib)   # bundled packages + R's own (no user library)

need <- c("shiny", "bslib", "bsicons", "plotly", "DT", "dplyr", "tidyr", "readr",
          "lubridate", "tibble", "curl", "httr2", "jsonlite", "commonmark",
          "shinyWidgets", "shinycssloaders", "scales", "htmltools")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  stop("Missing R packages: ", paste(miss, collapse = ", "),
       ".\nUse the offline Windows package, or install them once with",
       " install.packages(\"renv\"); renv::restore()  (see README.md).", call. = FALSE)
}
if ("live" %in% tolower(commandArgs(trailingOnly = TRUE))) Sys.setenv(TW_DATA_SOURCE = "live")
port <- as.integer(Sys.getenv("TW_PORT", "")); if (is.na(port)) port <- httpuv::randomPort()
cat(sprintf("\nR %s, packages from: %s\n", getRversion(), paste(.libPaths(), collapse = "; ")))
cat(sprintf("Data: %s\n", if (Sys.getenv("TW_DATA_SOURCE") == "live") "live IRCC download (checked)" else
            "IRCC open data release of September 15, 2026"))
cat(sprintf("Dashboard: http://127.0.0.1:%d  (close this window or press Ctrl+C to stop)\n\n", port))
shiny::runApp(proj, host = "127.0.0.1", port = port,
              launch.browser = Sys.getenv("TW_NO_BROWSER", "") == "")
