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
