# =============================================================================
# tools/get_testdata.R: download the two IRCC CSV files into testdata/ so the
# full test suite can run (testdata/ is gitignored).
# Usage (from the project folder): Rscript tools/get_testdata.R
# =============================================================================
source("R/data.R")   # for IRCC_URLS and BROWSER_UA

dir.create("testdata", showWarnings = FALSE)
urls <- IRCC_URLS
for (u in urls) {
  dest <- file.path("testdata", basename(u))
  cat("Downloading", basename(u), "...\n")
  # mode = "wb" keeps the files byte-for-byte
  utils::download.file(u, dest, mode = "wb", quiet = TRUE,
                       headers = c("User-Agent" = BROWSER_UA))
}
cat("Done. Now run: NOT_CRAN=true Rscript tests/testthat.R\n")
