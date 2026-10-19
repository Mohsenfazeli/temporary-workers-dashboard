# =============================================================================
# tools/deploy.R: publish the dashboard to shinyapps.io
# Usage (from the project folder), after setting three environment variables
# with the values from shinyapps.io > Account > Tokens:
#   SHINYAPPS_ACCOUNT=...  SHINYAPPS_TOKEN=...  SHINYAPPS_SECRET=...  Rscript tools/deploy.R
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

# ---- 1. Read the account details from environment variables -----------------
account <- Sys.getenv("SHINYAPPS_ACCOUNT")
token   <- Sys.getenv("SHINYAPPS_TOKEN")
secret  <- Sys.getenv("SHINYAPPS_SECRET")
if (!nzchar(account) || !nzchar(token) || !nzchar(secret)) {
  stop("Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN and SHINYAPPS_SECRET before running this script.")
}

# ---- 2. Register the account on this computer (once per machine) ------------
rsconnect::setAccountInfo(name = account, token = token, secret = secret)

# ---- 3. Deploy only the files the app needs ----------------------------------
# The IRCC files are NOT uploaded: the app downloads them live at startup and
# uses data/cache.rds only if the download fails or does not pass its checks.
# data/baseline.rds is the verified copy new downloads are checked against.
app_files <- c("app.R",
               list.files("R", pattern = "\\.R$", full.names = TRUE),
               "www/styles.css",
               "data/cache.rds",
               "data/baseline.rds")
rsconnect::deployApp(appDir = ".", appFiles = app_files,
                     appName = "temporary-workers-dashboard",
                     appTitle = "Temporary Workers in Canada",
                     account = account, server = "shinyapps.io",
                     forceUpdate = TRUE, launch.browser = FALSE)

cat("Deployed: https://", account, ".shinyapps.io/temporary-workers-dashboard/\n", sep = "")
