# Data tests: coverage, keys, geography, aggregation, masking, layout
# changes, the two-file rule, and new months.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
d <- BUNDLE$data

test_that("coverage runs from January 2015 to the latest month, with no gaps", {
  expect_equal(min(d$month), as.Date("2015-01-01"))
  expect_equal(max(d$month), BUNDLE$meta$latest_month)
  expect_equal(length(unique(d$month)),
               length(seq(as.Date("2015-01-01"), max(d$month), by = "month")))
  # latest month in metadata = latest month in the raw file
  raw <- readr::read_tsv(LOCAL_CORE[["IMP"]], col_types = readr::cols(.default = "c"),
                         na = character(), progress = FALSE)
  raw_latest <- max(as.Date(sprintf("%s-%02d-01", raw$EN_YEAR, match(raw$EN_MONTH, month.abb))))
  expect_equal(BUNDLE$meta$latest_month, raw_latest)
})

test_that("there are no duplicate keys (one row per month x geography x program x category)", {
  expect_equal(anyDuplicated(d[c("month", "geography", "program", "category")]), 0L)
})

test_that("Canada and every province and territory are present in every month", {
  expect_setequal(as.character(unique(d$geography)),
                  c("Canada", PROVINCES_TERRITORIES, NOT_STATED_GEO))
  per_month <- tapply(as.character(d$geography), d$month, function(x) length(unique(x)))
  expect_true(all(per_month == 15))
})

test_that("annual figures equal the sum of monthly figures (flow measure)", {
  sel <- filter_selection(d, "2015-01-01", max(d$month), "British Columbia", c("TFWP", "IMP"))
  ann <- summarise_series(sel, "Annual", by = "program", latest_month = max(d$month))
  manual <- aggregate(value ~ year + program, data = sel, FUN = sum)
  m <- merge(ann, manual, by = c("year", "program"))
  expect_equal(m$value.x, m$value.y)
  expect_true(any(grepl("^2026 \\(Jan", ann$label)))   # latest year is labelled with its months
  expect_true(all(ann$partial[grepl("^2026 \\(Jan", ann$label)]))
})

test_that("Canada equals the sum of provinces, territories and 'not stated' (within rounding)", {
  can <- aggregate(value ~ month + program + category, data = d[d$geography == "Canada", ], FUN = sum)
  pts <- aggregate(value ~ month + program + category, data = d[d$geography != "Canada", ], FUN = sum)
  m <- merge(can, pts, by = c("month", "program", "category"))
  expect_lte(max(abs(m$value.x - m$value.y)), 5)       # exact here; tolerance 5
})

test_that("TFWP plus IMP equals the 'both' option", {
  both <- kpi_total(filter_selection(d, "2020-01-01", "2024-12-01", "Ontario", programs_from_choice("Both")))
  t <- kpi_total(filter_selection(d, "2020-01-01", "2024-12-01", "Ontario", "TFWP"))
  i <- kpi_total(filter_selection(d, "2020-01-01", "2024-12-01", "Ontario", "IMP"))
  expect_equal(both, t + i)
})

test_that("masked cells ('--') count as 0 and are flagged, as documented", {
  det <- BUNDLE$detail
  expect_true(all(det$value[det$masked] == 0))
  raw_masked <- sum(vapply(LOCAL_CORE, function(f) {
    r <- readr::read_tsv(f, col_types = readr::cols(.default = "c"), na = character(), progress = FALSE)
    sum(r$TOTAL == "--")
  }, numeric(1)))
  expect_equal(sum(d$masked_cells[d$geography == "Canada"]), raw_masked)
  expect_equal(sum(BUNDLE$meta$masked_cells), raw_masked)
  expect_true(all(d$any_masked == (d$masked_cells > 0)))
})

test_that("Latin-1 encoding, comma delimiter, title row and footnotes are handled", {
  lines <- readLines(LOCAL_CORE[["TFWP"]], encoding = "UTF-8")
  f <- tempfile(fileext = ".csv")
  con <- file(f, "wb")
  writeBin(iconv(paste(c("A title row", gsub("\t", ",", lines), "", "Note: a footnote"),
                       collapse = "\r\n"), "UTF-8", "latin1", toRaw = TRUE)[[1]], con)
  close(con)
  b <- load_data(c(TFWP = f, IMP = LOCAL_CORE[["IMP"]]),
                 cache_path = temp_cache(), write_cache = FALSE)
  expect_equal(b$meta$files$TFWP$encoding, "Latin-1")
  expect_equal(b$meta$files$TFWP$delimiter, "comma")
  expect_equal(b$meta$files$TFWP$header_row, 2L)
  expect_equal(b$meta$files$TFWP$footnotes, "Note: a footnote")
  expect_equal(b$data, d)
})

test_that("a changed column layout shows a clear warning and falls back to the cache", {
  cache <- temp_cache()
  expect_warning(
    b <- load_data(c(TFWP = make_layout_changed_file(), IMP = LOCAL_CORE[["IMP"]]),
                   cache_path = cache, write_cache = FALSE),
    "layout has changed")
  expect_equal(b$meta$source, "cache")
  expect_equal(b$meta$problem_type, "layout")
  expect_match(b$meta$warning, "EN_PROVINCE_TERRITORY")   # names the problem
  expect_gt(nrow(b$data), 0)
})

test_that("a changed layout with no cache stops with a clear message", {
  expect_error(
    load_data(c(TFWP = make_layout_changed_file(), IMP = LOCAL_CORE[["IMP"]]),
              cache_path = tempfile(fileext = ".rds"), write_cache = FALSE),
    "layout has changed.*EN_PROVINCE_TERRITORY.*No saved copy")
})

test_that("unexpected values in the count column are treated as a layout change", {
  f <- tempfile(fileext = ".csv")
  lines <- readLines(LOCAL_CORE[["TFWP"]], encoding = "UTF-8")
  lines[2] <- sub("\t[0-9]+$", "\tx", lines[2])
  writeLines(lines, f, useBytes = TRUE)
  expect_warning(b <- load_data(c(TFWP = f, IMP = LOCAL_CORE[["IMP"]]),
                                cache_path = temp_cache(), write_cache = FALSE),
                 "Unexpected value")
  expect_equal(b$meta$source, "cache")
})

test_that("a new month in the IRCC files extends the data, slider and latest month automatically", {
  dir <- make_extra_month_dir()
  b <- load_data(c(TFWP = file.path(dir, basename(LOCAL_CORE[["TFWP"]])),
                   IMP  = file.path(dir, basename(LOCAL_CORE[["IMP"]]))), cache_path = temp_cache(), write_cache = FALSE)
  expect_equal(b$meta$latest_month, BUNDLE$meta$latest_month %m+% months(1))
  expect_equal(length(month_choices(b$data)), length(month_choices(d)) + 1)
  ann <- summarise_series(filter_selection(b$data, "2026-01-01", b$meta$latest_month),
                          "Annual", by = character(0), latest_month = b$meta$latest_month)
  expect_equal(ann$label, "2026 (Jan\u2013Aug)")
})

test_that("the cache is written after a successful read when requested", {
  cache <- tempfile(fileext = ".rds")
  b <- load_data(LOCAL_CORE, cache_path = cache, write_cache = TRUE)
  expect_true(file.exists(cache))
  expect_equal(readRDS(cache)$data, b$data)
  # local test reads do not touch the cache by default
  cache2 <- tempfile(fileext = ".rds")
  load_data(LOCAL_CORE, cache_path = cache2)
  expect_false(file.exists(cache2))
})

# ---- Only the two IRCC CSV files are used ----

test_that("the data come only from the two IRCC CSV files", {
  expect_setequal(names(formals(load_data)),
                  c("sources", "cache_path", "baseline_path", "timeout", "tries",
                    "source_label", "retrieved_at", "write_cache"))
  expect_equal(unname(IRCC_URLS),
               c("https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv",
                 "https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv"))
  expect_false(exists("IRCC_REFERENCE_URLS"))
  expect_setequal(names(BUNDLE), c("data", "detail", "meta"))
  expect_null(BUNDLE$meta$reference_urls)
  x <- readRDS(file.path(PROJECT, "data", "baseline.rds"))
  expect_setequal(names(x), c("data", "detail", "meta"))
  expect_true(all(grepl("\\.csv$", x$meta$urls)))
  expect_false(file.exists(file.path(PROJECT, "data", "cache.rds")))
  # No source file of the app reads Excel or any other data file
  code <- unlist(lapply(c(file.path(PROJECT, "app.R"),
                          list.files(file.path(PROJECT, "R"), full.names = TRUE)), readLines))
  expect_false(any(grepl("readxl|read_excel|\\.xlsx|statcan|17-10-0009", code, ignore.case = TRUE)))
})
