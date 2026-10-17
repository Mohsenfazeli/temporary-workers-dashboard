# Live-data tests. Tests that need the IRCC server are skipped (with a clear
# message) on machines that cannot reach it; the others always run.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")

test_that("LIVE: reading both IRCC URLs works and the latest month matches the IRCC file", {
  skip_if_not(ircc_reachable(),
    "IRCC server not reachable from this machine; run this test where www.cic.gc.ca is reachable")
  cache <- tempfile(fileext = ".rds")
  t0 <- Sys.time()
  b <- load_data(cache_path = cache)                        # live URLs (defaults)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(b$meta$source, "live")
  expect_true(file.exists(cache))                           # cache refreshed
  raw <- readr::read_tsv(IRCC_URLS[["TFWP"]], col_types = readr::cols(.default = "c"),
                         na = character(), progress = FALSE)
  raw_latest <- max(as.Date(sprintf("%s-%02d-01", raw$EN_YEAR, match(raw$EN_MONTH, month.abb))))
  expect_equal(b$meta$latest_month, raw_latest)
  expect_false(is.null(b$reference$table))                  # Excel reference also live
  message(sprintf("Live read took %.1f seconds", elapsed))
  # Realistic bound for a real connection: about 16.6 MB of CSVs plus 0.5 MB
  # of Excel files. Parsing takes under a second; a home connection took
  # about 36 s, so 90 s leaves room for slower networks.
  expect_lt(elapsed, 90)
})

test_that("a failed download (simulated) falls back to the cache with a notice", {
  cache <- temp_cache()
  bad <- c(TFWP = "https://127.0.0.1:9/ODP-TR-Work-TFWP-PT_program.csv",
           IMP  = "https://127.0.0.1:9/ODP-TR-Work-IMP-PT_program.csv")
  expect_warning(b <- load_data(bad, NULL, cache_path = cache, timeout = 5, tries = 1),
                 "download failed")
  expect_equal(b$meta$source, "cache")
  expect_equal(b$meta$problem_type, "download")
  expect_match(b$meta$cache_note, "saved copy")
  expect_equal(b$data, readRDS(cache)$data)
})

test_that("a failed reference download keeps the core data and hides the reference", {
  bad_ref <- c(TFWP = "https://127.0.0.1:9/a.xlsx", IMP = "https://127.0.0.1:9/b.xlsx")
  b <- load_data(LOCAL_CORE, bad_ref, cache_path = temp_cache(), timeout = 5, tries = 1,
                 write_cache = FALSE)
  expect_equal(b$meta$source, "local file")
  expect_null(b$reference$table)
  expect_match(b$reference$note, "not available")
})

test_that("the reachability probe spots an unreachable server quickly", {
  t0 <- Sys.time()
  r <- hosts_reachable(c("https://127.0.0.1:9/", "https://10.255.255.1/"),
                       connect_timeout = 2, total_timeout = 3)
  expect_equal(unname(r), c(FALSE, FALSE))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 6)   # probed in parallel
})

test_that("an unreachable IRCC host falls back to the cache within 15 seconds", {
  cache <- temp_cache()
  bad <- c(TFWP = "https://10.255.255.1/ODP-TR-Work-TFWP-PT_program.csv",
           IMP  = "https://10.255.255.1/ODP-TR-Work-IMP-PT_program.csv")
  t0 <- Sys.time()
  expect_warning(b <- load_data(bad, NULL, cache_path = cache, timeout = 60, tries = 2),
                 "download failed")
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 15)
  expect_equal(b$meta$source, "cache")
})
