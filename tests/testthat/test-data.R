# Data tests: coverage, keys, geography, aggregation, masking, layout
# changes, the IRCC reference, and new months.
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
  expect_true(any(grepl("YTD", ann$label)))           # latest year is YTD
  expect_true(all(ann$partial[grepl("YTD", ann$label)]))
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
  b <- load_data(c(TFWP = f, IMP = LOCAL_CORE[["IMP"]]), NULL,
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
                   LOCAL_REF, cache_path = cache, write_cache = FALSE),
    "layout has changed")
  expect_equal(b$meta$source, "cache")
  expect_equal(b$meta$problem_type, "layout")
  expect_match(b$meta$warning, "EN_PROVINCE_TERRITORY")   # names the problem
  expect_gt(nrow(b$data), 0)
})

test_that("a changed layout with no cache stops with a clear message", {
  expect_error(
    load_data(c(TFWP = make_layout_changed_file(), IMP = LOCAL_CORE[["IMP"]]), NULL,
              cache_path = tempfile(fileext = ".rds"), write_cache = FALSE),
    "layout has changed.*EN_PROVINCE_TERRITORY.*No saved copy")
})

test_that("unexpected values in the count column are treated as a layout change", {
  f <- tempfile(fileext = ".csv")
  lines <- readLines(LOCAL_CORE[["TFWP"]], encoding = "UTF-8")
  lines[2] <- sub("\t[0-9]+$", "\tx", lines[2])
  writeLines(lines, f, useBytes = TRUE)
  expect_warning(b <- load_data(c(TFWP = f, IMP = LOCAL_CORE[["IMP"]]), NULL,
                                cache_path = temp_cache(), write_cache = FALSE),
                 "Unexpected value")
  expect_equal(b$meta$source, "cache")
})

test_that("a new month in the IRCC files extends the data, slider and latest month automatically", {
  dir <- make_extra_month_dir()
  b <- load_data(c(TFWP = file.path(dir, basename(LOCAL_CORE[["TFWP"]])),
                   IMP  = file.path(dir, basename(LOCAL_CORE[["IMP"]]))),
                 NULL, cache_path = temp_cache(), write_cache = FALSE)
  expect_equal(b$meta$latest_month, BUNDLE$meta$latest_month %m+% months(1))
  expect_equal(length(month_choices(b$data)), length(month_choices(d)) + 1)
  ann <- summarise_series(filter_selection(b$data, "2026-01-01", b$meta$latest_month),
                          "Annual", by = character(0), latest_month = b$meta$latest_month)
  expect_match(ann$label, "YTD \\(Jan\u2013Aug\\)")
})

test_that("the cache is written after a successful read when requested", {
  cache <- tempfile(fileext = ".rds")
  b <- load_data(LOCAL_CORE, LOCAL_REF, cache_path = cache, write_cache = TRUE)
  expect_true(file.exists(cache))
  expect_equal(readRDS(cache)$data, b$data)
  # local test reads do not touch the cache by default
  cache2 <- tempfile(fileext = ".rds")
  load_data(LOCAL_CORE, NULL, cache_path = cache2)
  expect_false(file.exists(cache2))
})

# ---- IRCC distinct-people reference (Excel tables) ----

test_that("the reference parses IRCC's annual distinct-people totals for Canada only", {
  ref <- BUNDLE$reference
  expect_false(is.null(ref$table))
  expect_setequal(unique(ref$table$geography), "Canada")
  expect_setequal(unique(ref$table$program), c("TFWP", "IMP"))
  expect_setequal(unique(ref$table$year), unique(d$year))
  expect_equal(ref$table$people[ref$table$program == "TFWP" & ref$table$year == 2023], 183180)
  expect_equal(ref$table$people[ref$table$program == "IMP" & ref$table$year == 2023], 761200)
  expect_match(ref$notes, "total unique count may not equal")
})

test_that("the reference is 1-4% below the sum of months in full years (sanity check)", {
  ref <- BUNDLE$reference$table
  csv <- aggregate(value ~ program + year, data = d[d$geography == "Canada", ], FUN = sum)
  m <- merge(ref, csv, by = c("program", "year"))
  m <- m[m$year < 2026, ]
  gap <- m$people / m$value - 1
  expect_true(all(gap < 0 & gap > -0.05))
})

test_that("a missing reference file hides the reference but the data still load", {
  b <- load_data(LOCAL_CORE, c(TFWP = "nope.xlsx", IMP = "nope2.xlsx"),
                 cache_path = temp_cache(), write_cache = FALSE)
  expect_null(b$reference$table)
  expect_match(b$reference$note, "not available")
  expect_equal(b$data, d)
  expect_equal(b$meta$source, "local file")
})

test_that("a corrupted reference file is handled gracefully", {
  bad <- tempfile(fileext = ".xlsx"); writeLines("not an excel file", bad)
  b <- load_data(LOCAL_CORE, c(TFWP = bad, IMP = LOCAL_REF[["IMP"]]),
                 cache_path = temp_cache(), write_cache = FALSE)
  expect_null(b$reference$table)
  expect_equal(b$data, d)
})
