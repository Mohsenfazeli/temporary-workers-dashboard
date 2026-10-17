# Runs every test. From the project folder:  Rscript tests/testthat.R
required_packages <- c("testthat", "shinytest2", "withr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
Sys.setenv(NOT_CRAN = "true")       # shinytest2 skips browser tests "on CRAN" otherwise
library(testthat)
res <- testthat::test_dir("tests/testthat", reporter = testthat::ProgressReporter$new(show_praise = FALSE),
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\nSUMMARY: %d tests, %d passed expectations, %d failed, %d skipped, %d errors\n",
            nrow(df), sum(df$passed), sum(df$failed), sum(df$skipped), sum(df$error)))
# Optional: save the results table for tools/windows_check.R to read.
out <- Sys.getenv("TW_TEST_RESULTS", "")
if (nzchar(out)) saveRDS(df[, c("file", "test", "nb", "passed", "failed", "skipped", "error")], out)
# Exit with status 1 if any test failed or stopped with an error, so a script
# calling this file can tell (errors are not "failures" in testthat's counts).
bad <- df$failed > 0 | df$error
if (any(bad)) {
  cat("Tests with failures or errors:\n")
  cat(sprintf("  - %s: %s\n", df$file[bad], df$test[bad]), sep = "")
  quit(status = 1)
}
