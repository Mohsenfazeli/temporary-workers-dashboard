# =============================================================================
# tools/deploy.R: publish the dashboard to shinyapps.io
# Usage (from the project folder), after setting three environment variables
# with the values from shinyapps.io > Account > Tokens:
#   SHINYAPPS_ACCOUNT=...  SHINYAPPS_TOKEN=...  SHINYAPPS_SECRET=...  Rscript tools/deploy.R
# (or with an account already registered on this computer: Rscript tools/deploy.R).
# Optional: GEMINI_API_KEY=... for the Help assistant (see section 2).
# Nothing secret is stored in this file.
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("rsconnect")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(rsconnect)

# ---- 1. The shinyapps.io account ---------------------------------------------
# Either set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET, or use an
# account already registered on this computer (rsconnect::accounts()); then
# only SHINYAPPS_ACCOUNT is needed (default: the only registered account).
account <- Sys.getenv("SHINYAPPS_ACCOUNT")
token   <- Sys.getenv("SHINYAPPS_TOKEN")
secret  <- Sys.getenv("SHINYAPPS_SECRET")
saved   <- tryCatch(rsconnect::accounts(server = "shinyapps.io")$name, error = function(e) character())
if (!nzchar(account) && length(saved) == 1) account <- saved
if (nzchar(token) && nzchar(secret) && nzchar(account)) {
  rsconnect::setAccountInfo(name = account, token = token, secret = secret)   # once per machine
} else if (!account %in% saved) {
  stop("Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET (shinyapps.io > Account > Tokens), ",
       "or register the account on this computer first.")
}

# ---- 1b. The Downloads tab's PDFs, built from the data before every upload ----
# tools/build_exports.R makes the three PDFs and www/exports/manifest.json (no
# AI; needs Chrome, python-docx and LibreOffice on this computer, which the free
# shinyapps.io servers do not offer). If it fails, nothing is deployed.
rscript <- file.path(R.home("bin"), "Rscript")
build_status <- system2(rscript, "tools/build_exports.R")
if (!identical(as.integer(build_status), 0L) || !file.exists("www/exports/manifest.json")) {
  stop("Building the Downloads PDFs failed (tools/build_exports.R), so nothing was deployed.")
}

# ---- 2. Optional: the Help assistant's Gemini API key --------------------------
# If GEMINI_API_KEY is set here, it is written to a temporary .Renviron that is
# uploaded with the app (R reads it when the app starts on shinyapps.io) and
# deleted again straight after the upload. It is never stored in the code or
# in git (.Renviron is in .gitignore). Without it, the Help tab says the
# assistant is not available and the rest of the app is unchanged.
renv_file <- ".Renviron"
if (file.exists(renv_file)) stop(".Renviron already exists here; move it away first (it is never deployed or committed).")
key <- Sys.getenv("GEMINI_API_KEY")
extra_files <- character()
if (nzchar(key)) {
  opts <- c("TW_GEMINI_MODEL", "TW_GEMINI_FALLBACK", "TW_GEMINI_TIMEOUT", "TW_GEMINI_THINKING",
            "TW_ASSISTANT_SESSION_MAX", "TW_ASSISTANT_DAILY_MAX")
  set <- opts[nzchar(Sys.getenv(opts))]
  writeLines(c(paste0("GEMINI_API_KEY=", key), if (length(set)) paste0(set, "=", Sys.getenv(set))), renv_file)
  Sys.chmod(renv_file, "0600")
  extra_files <- renv_file
  cat("Help assistant: key found, it will be uploaded in .Renviron (deleted locally afterwards).\n")
} else {
  cat("Help assistant: GEMINI_API_KEY not set, so the Help tab will say the assistant is not available.\n")
}

# ---- 3. Deploy only the files the app needs ----------------------------------
# The raw IRCC files are NOT uploaded. By default the app shows the saved IRCC
# release (data/baseline.rds); with TW_DATA_SOURCE=live it downloads the files at
# startup and uses data/cache.rds only if the download fails or fails its checks.
app_files <- c("app.R",
               list.files("R", pattern = "\\.R$", full.names = TRUE),
               "www/styles.css",
               list.files("www/exports", full.names = TRUE),   # Downloads tab (built above)
               "data/cache.rds",
               "data/baseline.rds",
               extra_files)
# tryCatch(finally = ...) deletes the temporary .Renviron even if the upload
# fails or times out (on.exit() does not run at the top level of a script).
tryCatch(
  rsconnect::deployApp(appDir = ".", appFiles = app_files,
                       appName = "temporary-workers-dashboard",
                       appTitle = "Temporary Workers in Canada",
                       account = account, server = "shinyapps.io",
                       forceUpdate = TRUE, launch.browser = FALSE),
  finally = unlink(renv_file))
cat("Deployed: https://", account, ".shinyapps.io/temporary-workers-dashboard/\n", sep = "")
