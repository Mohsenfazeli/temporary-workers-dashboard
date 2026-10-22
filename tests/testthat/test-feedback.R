# Tests for the reviewer feedback of October 18, 2026: frozen release as the
# default source, full-year and year-to-date cards, same-months comparison,
# sourced event markers, "What the data show", the Unknown IMP wording and
# known-program shares, the IRCC program-name table and on-screen wording.
skip_if_not(have_testdata, "IRCC test files not available")
d <- BUNDLE$data; latest <- BUNDLE$meta$latest_month
both <- c("TFWP", "IMP")

test_that("the frozen release is labelled with IRCC's release date and has no warning", {
  r <- load_release(BASELINE)
  expect_equal(r$meta$source, "release")
  expect_equal(r$meta$release_date, as.Date("2026-09-15"))
  expect_equal(r$meta$source_label, "IRCC open data release of September 15, 2026")
  expect_null(r$meta$warning)
  expect_equal(r$data, readRDS(BASELINE)$data)
  expect_equal(r$data, d)                                  # same numbers as the IRCC files
  expect_match(data_date_text(r$meta), "release of September 15, 2026 (data to July 2026)", fixed = TRUE)
  # One release phrase everywhere (header, About, Overview, written notes)
  expect_equal(r$meta$release_phrase, "IRCC open data release of September 15, 2026 (data to July 2026)")
  expect_error(load_release(tempfile()), "not found")
})

test_that("card 1 leads with the latest full calendar year; card 2 compares year to date", {
  fy <- kpi_full_year(d, latest, "Canada", both)
  expect_equal(fy$year, 2025L); expect_equal(fy$value, 754915); expect_equal(fy$prev_value, 924235)
  expect_equal(kpi_full_year(d, as.Date("2024-12-01"), "Canada", both)$year, 2024L)
  expect_true(is.na(kpi_full_year(d, as.Date("2015-06-01"), "Canada", both)$value))
  yt <- kpi_ytd_compare(d, latest, "Canada", both)
  expect_equal(c(yt$value, yt$prev_value), c(673280, 461760))
  expect_equal(fmt_pct(yt$change), "+45.8%")
  expect_equal(yt$label, "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025")
  bc <- kpi_ytd_compare(d, latest, "British Columbia", both)
  expect_equal(c(bc$value, bc$prev_value), c(109805, 84165))
  expect_equal(kpi_ytd_compare(d, as.Date("2025-12-01"), "Canada", both)$label, "2025 vs 2024")
})

test_that("'same months each year' compares January to July of every year", {
  expect_equal(same_months_limit(TRUE, "Annual", latest), 7L)
  expect_null(same_months_limit(TRUE, "Monthly", latest))
  expect_null(same_months_limit(FALSE, "Annual", latest))
  expect_null(same_months_limit(TRUE, "Annual", as.Date("2025-12-01")))
  sm <- filter_selection(d, min(d$month), latest, "Canada", both, last_moy = 7)
  expect_true(all(as.integer(format(sm$month, "%m")) <= 7))
  ser <- summarise_series(sm, "Annual", by = character(0), latest_month = latest)
  expect_false(any(ser$partial))                           # every year comparable
  expect_equal(ser$value[ser$year == 2026], 673280)
  expect_equal(ser$value[ser$year == 2025], 461760)
  expect_match(ser$label[ser$year == 2019], "2019 (Jan\u2013Jul)", fixed = TRUE)
  # Without the option the latest year is still a part-year
  ser2 <- summarise_series(filter_selection(d, min(d$month), latest, "Canada", both), "Annual",
                           by = character(0), latest_month = latest)
  expect_true(ser2$partial[ser2$year == 2026])
})

test_that("event markers: six dated events, each with an official source", {
  expect_equal(nrow(KEY_EVENTS), 6)
  expect_equal(format(KEY_EVENTS$date, "%Y-%m"),
               c("2020-03", "2024-01", "2024-03", "2024-09", "2024-11", "2025-01"))
  expect_true(all(grepl("^https://www\\.(canada\\.ca|pm\\.gc\\.ca)/", KEY_EVENTS$url)))
  expect_true(all(nzchar(KEY_EVENTS$source)))
  html <- as.character(event_notes_html(KEY_EVENTS))
  for (u in KEY_EVENTS$url) expect_match(html, u, fixed = TRUE)
  expect_match(html, "do not imply cause")
  expect_match(IN_CANADA_NOTE$url, "supplementary-immigration-levels-2026-2028", fixed = TRUE)
})

test_that("'What the data show' points are computed from the data and match the Q2 note", {
  h <- release_highlights(d, latest)
  expect_length(h, 4)
  expect_match(h[1], "about 4 in 5 work permits: 77.9% from January 2015 to July 2026, and 83.2%", fixed = TRUE)
  expect_match(h[2], "179,755 (85.0%) of the 211,520 increase", fixed = TRUE)
  expect_match(h[2], "largest category in every year since 2018", fixed = TRUE)
  expect_match(h[3], "178,695 in 2023 to 127,030 in 2025", fixed = TRUE)
  expect_match(h[3], "16.1% below the same months of 2025", fixed = TRUE)
  expect_match(h[4], "Ontario (42.6%) and British Columbia (16.3%)", fixed = TRUE)
  # Neutral voice: no causal or judgemental words
  expect_no_match(paste(h, collapse = " "),
                  "\\b(because|caused|due to|drove|driven|led to|impact|surge|plunge|should)\\b")
})

test_that("Unknown IMP note says IRCC does not identify the program, and shares can exclude them", {
  ca <- unknown_imp_note_text(d, "Canada", latest)
  expect_match(ca, "does not identify the program", fixed = TRUE)
  expect_match(ca, "8.4% to 26.4% of IMP permits", fixed = TRUE)   # 2021-2025, checked against the data
  expect_no_match(ca, "record", ignore.case = TRUE)
  imp <- filter_selection(d, "2022-01-01", "2025-12-01", "Canada", "IMP")
  sh <- composition_shares(imp, "Annual", latest, known_only = TRUE)
  expect_false("Unknown or other IMP" %in% as.character(sh$category))
  expect_equal(as.numeric(tapply(sh$share, sh$year, sum)), rep(1, 4))
  full <- composition_shares(imp, "Annual", latest)
  u <- full$share[full$category == "Unknown or other IMP"]
  expect_equal(round(range(u), 3), c(0.192, 0.264))               # 2022-2025: 19.2% to 26.4%
})

test_that("the IRCC program-name table adds up to the dashboard totals", {
  t <- category_detail_table(BUNDLE$detail, min(d$month), latest, "Canada", both)
  expect_equal(sum(t$Permits), kpi_total(filter_selection(d, min(d$month), latest, "Canada", both)))
  bc <- category_detail_table(BUNDLE$detail, "2026-01-01", latest, "British Columbia", "IMP")
  expect_equal(sum(bc$Permits), kpi_total(filter_selection(d, "2026-01-01", latest, "British Columbia", "IMP")))
  expect_true(all(c("Spouses of skilled workers", "Spouses of students") %in% t$`IRCC program name`))
  expect_false(any(t$`IRCC program name` == "Program not stated" & t$Category == "Post-graduation work permits"))
})

test_that("on-screen text stays free of hiring/assignment framing", {
  ui <- paste(as.character(htmltools::renderTags(htmltools::tagList(
    overview_ui("o"), trends_ui("t"), geography_ui("g"), composition_ui("c"), data_ui("d")))$html),
    collapse = "")
  expect_no_match(ui, "assignment|competition|written test|Question 1|Question 2", ignore.case = TRUE)
  ab <- NULL
  shiny::testServer(function(input, output, session) about_server("x", d, load_release(BASELINE)$meta), {
    ab <<- paste(format(output[["x-about"]]), collapse = "")
  })
  expect_no_match(ab, "assignment|competition|written test|Question 1|Question 2", ignore.case = TRUE)
  expect_match(ab, "IRCC open data release of September 15, 2026 (data to July 2026)", fixed = TRUE)
  expect_match(ab, "these two files are IRCC's official monthly series", fixed = TRUE)
  expect_match(ab, "supplementary-immigration-levels-2026-2028", fixed = TRUE)
  expect_no_match(ab, "xlsx|Excel|distinct people per year", ignore.case = TRUE)
})

test_that("the app opens on the frozen release by default, with the right cards and new controls", {
  skip_on_cran()
  library(shinytest2)
  withr::local_envvar(TW_LOCAL_DIR = NA, TW_DATA_SOURCE = NA)
  app <- AppDriver$new(PROJECT, load_timeout = 90000, timeout = 30000, width = 1366, height = 900)
  on.exit(app$stop())
  bar <- gsub("\\s+", " ", app$get_text(".status-bar"))
  expect_match(bar, "Data: IRCC open data release of September 15, 2026 (data to July 2026)", fixed = TRUE)
  expect_no_match(bar, "Using saved data")
  # One mention of the latest month in the header (no repeated "Latest month available")
  expect_no_match(bar, "Latest month available")
  expect_equal(app$get_value(output = "overview-kpi_ytd_title"), "Work permits issued, Jan\u2013Jul 2026")
  expect_equal(app$get_value(output = "overview-kpi_ytd"), "673,280")
  expect_equal(app$get_value(output = "overview-kpi_change_title"), "Change from the same months of 2025")
  expect_equal(app$get_value(output = "overview-kpi_change"), "+45.8%")
  expect_equal(app$get_value(output = "overview-kpi_split"), "83% / 17%")
  expect_equal(app$get_value(output = "overview-kpi_latest_title"), "Latest month: July 2026")
  expect_equal(app$get_value(output = "overview-kpi_latest"), "72,495")
  app$click("qp_12m"); app$wait_for_idle(800)
  expect_equal(app$get_value(input = "range"), c("Aug 2025", "Jul 2026"))
  expect_equal(app$get_value(output = "overview-kpi_ytd"), "673,280")          # the end month is unchanged
  app$click("qp_full"); app$wait_for_idle(800)
  app$set_inputs(same_months = TRUE); app$wait_for_idle(800)
  expect_identical(app$get_value(output = "overview-headline_title"),
                   "Work permits issued by program, Canada, January\u2013July of each year, 2015 to 2026")
  # Plain years on the axis, unrotated (every bar is January-July, as the title says)
  pj <- jsonlite::fromJSON(app$get_value(output = "overview-headline_plot"), simplifyVector = FALSE)$x
  expect_identical(unique(unlist(lapply(pj$data, function(tr) unlist(tr$x)))), as.character(2015:2026))
  expect_identical(as.numeric(pj$layout$xaxis$tickangle), 0)
  expect_no_match(app$get_value(output = "overview-headline_title"), "shown separately")
})

test_that("contribution to change: B.C.'s part of Canada's Jan-Jul 2026 increase is 12.1%", {
  x <- contribution_to_change(d, latest, both)
  expect_equal(attr(x, "label"), "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025")
  expect_equal(attr(x, "canada_change"), 673280 - 461760)
  bc <- x[x$geography == "British Columbia", ]
  expect_equal(c(bc$value, bc$prev_value), c(109805, 84165))
  expect_equal(fmt_share(bc$contribution), "12.1%")
  expect_equal(x$geography[1], "Ontario"); expect_equal(fmt_share(x$contribution[1]), "52.9%")
  expect_equal(nrow(x), 13)                                 # provinces and territories only
  # Independent check with base R: province change / Canada change
  ytd <- function(g, y) sum(d$value[d$geography == g & d$year == y & as.integer(format(d$month, "%m")) <= 7])
  expect_equal(bc$contribution, (ytd("British Columbia", 2026) - ytd("British Columbia", 2025)) /
                 (ytd("Canada", 2026) - ytd("Canada", 2025)))
  # The 13 parts plus "not stated" add up to Canada's change
  ns <- ytd("Province/territory not stated", 2026) - ytd("Province/territory not stated", 2025)
  expect_equal(sum(x$change) + ns, attr(x, "canada_change"))
  # A decrease (TFWP) is shared out the same way; B.C. has the largest part
  tf <- contribution_to_change(d, latest, "TFWP")
  expect_lt(attr(tf, "canada_change"), 0)
  expect_equal(tf$geography[1], "British Columbia")
})
