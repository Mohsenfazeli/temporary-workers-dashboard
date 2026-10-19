# Tests for the baseline check (R/data.R, check_against_baseline): a download
# with the right layout but wrong numbers must never reach the dashboard or
# overwrite the saved copy. Added October 18, 2026 after IRCC served such a file.
skip_if_not(have_testdata, "IRCC test files not available")

test_that("the IRCC files that match the verified copy pass the baseline check", {
  b <- load_data(LOCAL_CORE, cache_path = temp_cache(), baseline_path = BASELINE,
                 write_cache = FALSE)
  expect_equal(b$meta$source, "local file")
  expect_equal(b$meta$baseline_check$cell_share, 0)
  expect_equal(b$meta$baseline_check$max_year_gap, 0)
})

test_that("a damaged file with the normal layout is rejected and the saved copy is shown", {
  dir <- make_damaged_dir()
  cache <- temp_cache()
  before <- tools::md5sum(cache)
  expect_warning(
    b <- load_data(damaged_core(dir), cache_path = cache, baseline_path = BASELINE,
                   write_cache = TRUE),
    "did not match the checked copy")
  expect_equal(b$meta$source, "cache")
  expect_equal(b$meta$problem_type, "validation")
  expect_match(b$meta$warning, "Validation failed.*cells differ")
  expect_equal(b$data, readRDS(BASELINE)$data)            # correct numbers shown
  expect_equal(unname(tools::md5sum(cache)), unname(before))   # cache not overwritten
  # Headline figures stay right: Canada Jan-Jul 2026 = 673,280; B.C. = 109,805.
  ytd <- function(g) kpi_total(filter_selection(b$data, as.Date("2026-01-01"),
                                                as.Date("2026-07-01"), g, c("TFWP", "IMP")))
  expect_equal(ytd("Canada"), 673280)
  expect_equal(ytd("British Columbia"), 109805)
})

test_that("without a cache, a damaged file falls back to the baseline copy", {
  expect_warning(
    b <- load_data(damaged_core(make_damaged_dir()),
                   cache_path = tempfile(fileext = ".rds"), baseline_path = BASELINE),
    "did not match")
  expect_equal(b$meta$fallback_file, "baseline.rds")
  expect_equal(b$data, readRDS(BASELINE)$data)
})

test_that("a new month (later release) still passes the baseline check", {
  dir <- make_extra_month_dir()
  b <- load_data(c(TFWP = file.path(dir, basename(LOCAL_CORE[["TFWP"]])),
                   IMP  = file.path(dir, basename(LOCAL_CORE[["IMP"]]))),
                 cache_path = temp_cache(), baseline_path = BASELINE, write_cache = FALSE)
  expect_equal(b$meta$source, "local file")
  expect_equal(b$meta$latest_month, as.Date("2026-08-01"))
})

test_that("small revisions to recent months are allowed; missing settled months are not", {
  base <- readRDS(BASELINE)$data
  rev <- base
  recent <- rev$month >= as.Date("2026-01-01")
  rev$value[recent] <- round(rev$value[recent] * 1.10)      # recent months revised by 10%
  expect_silent(check_against_baseline(rev, base))
  short <- base[base$month >= as.Date("2016-01-01"), ]       # 2015 missing
  expect_error(check_against_baseline(short, base), "Validation failed: the new data cover")
})

test_that("the app shows a damaged download as saved data with the right totals", {
  skip_on_cran()
  library(shinytest2)
  withr::local_envvar(TW_LOCAL_DIR = make_damaged_dir(), TW_TRIES = "1", TW_TIMEOUT = "5")
  app <- AppDriver$new(PROJECT, load_timeout = 90000, timeout = 30000, width = 1366, height = 768)
  on.exit(app$stop())
  bar <- gsub("\\s+", " ", app$get_text(".status-bar"))
  expect_match(bar, "Using saved data")
  expect_match(bar, "did not match the checked copy")
  expect_equal(app$get_value(output = "overview-kpi_ytd"), "673,280")
})
