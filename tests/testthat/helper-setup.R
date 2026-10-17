# Shared set-up for all tests: load the app's R code and build test fixtures.
required_packages <- c("testthat", "shiny", "withr", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Browser tests: give headless Chrome up to 60 s per command (default 10 s).
# The first page load waits for the app to read its data, which can take
# longer than 10 s on a slower computer (seen on a Windows PC: about 18 s).
options(chromote.timeout = 60)

# Project root = two levels above tests/testthat.
PROJECT <- tryCatch(normalizePath(file.path(testthat::test_path(), "..", "..")),
                    error = function(e) normalizePath(file.path(getwd(), "..", "..")))
for (f in list.files(file.path(PROJECT, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

# Local copies of the IRCC files (same bytes as the live files, downloaded
# Oct 16, 2026). Needed because the test machine cannot reach IRCC.
TESTDATA <- file.path(PROJECT, "testdata")
LOCAL_CORE <- c(TFWP = file.path(TESTDATA, "ODP-TR-Work-TFWP-PT_program.csv"),
                IMP  = file.path(TESTDATA, "ODP-TR-Work-IMP-PT_program.csv"))
LOCAL_REF  <- c(TFWP = file.path(TESTDATA, "EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx"),
                IMP  = file.path(TESTDATA, "EN_ODP-TR-Work-IMP_PT_program_sign.xlsx"))
have_testdata <- all(file.exists(c(LOCAL_CORE, LOCAL_REF)))

# A temporary copy of the cache, so tests never overwrite data/cache.rds.
temp_cache <- function() {
  f <- tempfile(fileext = ".rds")
  file.copy(file.path(PROJECT, "data", "cache.rds"), f)
  f
}

# Load once from the local files and reuse in many tests.
if (have_testdata && !exists("BUNDLE")) {
  BUNDLE <- load_data(LOCAL_CORE, LOCAL_REF, cache_path = temp_cache(),
                      write_cache = FALSE)
}

# Fixture: the same files with one extra month (August 2026) appended, made
# by copying the July 2026 rows. Used to check that everything extends.
make_extra_month_dir <- function() {
  dir <- file.path(tempdir(), paste0("extra_month_", as.integer(runif(1, 1, 1e6))))
  dir.create(dir)
  for (p in names(LOCAL_CORE)) {
    lines <- readLines(LOCAL_CORE[[p]], encoding = "UTF-8")
    jul <- grep("^2026\tQ3\tJul\t", lines, value = TRUE)
    aug <- sub("^2026\tQ3\tJul\t2026\tT3\tjuil\\.", "2026\tQ3\tAug\t2026\tT3\tao\u00fbt", jul)
    writeLines(c(lines, aug), file.path(dir, basename(LOCAL_CORE[[p]])), useBytes = TRUE)
  }
  file.copy(LOCAL_REF, dir)
  dir
}

# Fixture: a copy of the TFWP file with a renamed column (layout change).
make_layout_changed_file <- function() {
  f <- tempfile(fileext = ".csv")
  lines <- readLines(LOCAL_CORE[["TFWP"]], encoding = "UTF-8")
  lines[1] <- sub("EN_PROVINCE_TERRITORY", "EN_PROV_TERR", lines[1])
  writeLines(lines, f, useBytes = TRUE)
  f
}

# Can this machine reach IRCC? (Quick check used to skip live tests.)
ircc_reachable <- function() {
  res <- tryCatch(fetch_many(c(x = IRCC_URLS[["TFWP"]]), timeout = 15, tries = 1)[[1]],
                  error = function(e) "error")
  !is.character(res)
}
