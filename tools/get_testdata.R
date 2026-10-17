# =============================================================================
# tools/get_testdata.R: download the four IRCC files into testdata/ so the
# full test suite can run (the files are not part of the submission).
# Usage (from the project folder): Rscript tools/get_testdata.R
# =============================================================================
source("R/data.R")   # for IRCC_URLS, IRCC_REFERENCE_URLS and BROWSER_UA

dir.create("testdata", showWarnings = FALSE)
urls <- c(IRCC_URLS, IRCC_REFERENCE_URLS)
for (u in urls) {
  dest <- file.path("testdata", basename(u))
  cat("Downloading", basename(u), "...\n")
  # mode = "wb" keeps the files byte-for-byte (needed for the Excel tables)
  utils::download.file(u, dest, mode = "wb", quiet = TRUE,
                       headers = c("User-Agent" = BROWSER_UA))
}
cat("Done. Now run: NOT_CRAN=true Rscript tests/testthat.R\n")
