# App tests in a real (headless Chrome) browser with shinytest2.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
skip_on_cran()
library(shinytest2)

TABS <- c("Overview", "Trends", "Geography", "Composition", "Data", "About the data")

# Start the app with the local copies of the IRCC files (this machine cannot
# reach IRCC). `local_dir` can point at another folder (e.g. one extra month).
start_app <- function(local_dir = TESTDATA, ...) {
  withr::local_envvar(TW_LOCAL_DIR = local_dir, TW_TRIES = "1", TW_TIMEOUT = "5",
                      .local_envir = parent.frame())
  AppDriver$new(PROJECT, load_timeout = 90000, timeout = 30000,
                width = 1366, height = 768, ...)
}

# Status bar text with whitespace tidied.
status_text <- function(app) gsub("\\s+", " ", app$get_text(".status-bar"))

# Move the date-range slider like a user would (through the slider widget).
set_range <- function(app, from_label, to_label) {
  labs <- format(month_choices(BUNDLE$data), "%b %Y")
  app$run_js(sprintf("$('#range').data('ionRangeSlider').update({from: %d, to: %d});",
                     match(from_label, labs) - 1, match(to_label, labs) - 1))
  app$wait_for_idle(800)
}

# Browser-side errors (ignoring a harmless message from plotly's bundled
# typedarray polyfill, which modern browsers do not need).
browser_errors <- function(app) {
  lg <- app$get_logs()
  bad <- lg[lg$level %in% c("error") | grepl("Error", lg$message), ]
  bad[!grepl("GLOBAL is not defined|moduleExporter|typedarray", bad$message), ]
}

test_that("the app launches and every tab renders", {
  app <- start_app()
  on.exit(app$stop())
  expect_match(status_text(app), "Data last retrieved")
  expect_match(status_text(app), "Latest month available: July 2026")
  for (t in TABS) {
    app$set_inputs(nav = t)
    app$wait_for_idle(500)
    expect_equal(app$get_value(input = "nav"), t)
  }
  expect_match(app$get_value(output = "overview-kpi_total"), "^[0-9,]+$")
  expect_match(app$get_value(output = "trends-trend_title"), "permits")
  expect_match(app$get_value(output = "geography-rank_title"), "received")
  expect_match(app$get_value(output = "composition-mix_title"), "^Post-graduation work permits \\(IMP\\) was the largest category: 24% of permits")
  expect_match(app$get_value(output = "data-title"), "rows")
  # plotly charts on the visible tab produced figures
  app$set_inputs(nav = "Trends"); app$wait_for_idle(1000)
  expect_true(app$get_js("document.querySelectorAll('#trends-trend_plot .main-svg').length > 0"))
  expect_equal(nrow(browser_errors(app)), 0)
})

test_that("every geography and program combination works in the browser without errors", {
  app <- start_app()
  on.exit(app$stop())
  app$set_inputs(nav = "Overview")
  for (geo in c("Canada", PROVINCES_TERRITORIES)) for (prog in c("Both", "TFWP", "IMP")) {
    app$set_inputs(geography = geo, program = prog)
    v <- app$get_value(output = "overview-kpi_total")
    expect_match(v, "^[0-9,]+$", info = paste(geo, prog))
    expect_match(app$get_value(output = "overview-headline_title"), geo, fixed = TRUE)
  }
  expect_equal(nrow(browser_errors(app)), 0)
})

test_that("the slider and the monthly/annual toggle update the outputs; reset restores defaults", {
  app <- start_app()
  on.exit(app$stop())
  total_all <- app$get_value(output = "overview-kpi_total")
  set_range(app, "Jan 2020", "Dec 2022")
  expect_equal(app$get_value(input = "range"), c("Jan 2020", "Dec 2022"))
  expect_false(identical(app$get_value(output = "overview-kpi_total"), total_all))
  app$set_inputs(nav = "Data"); app$wait_for_idle(800)
  expect_match(app$get_value(output = "data-title"), "Jan 2020\u2013Dec 2022")
  app$set_inputs(resolution = "Monthly")
  expect_match(app$get_value(output = "data-title"), "^Monthly data")
  app$set_inputs(nav = "Trends"); app$wait_for_idle(800)
  expect_match(app$get_value(output = "trends-trend_title"), "by month")
  app$set_inputs(nav = "Overview")
  app$click("reset"); app$wait_for_idle(1500)
  expect_equal(app$get_value(input = "resolution"), "Annual")
  expect_equal(app$get_value(input = "range"), c("Jan 2015", "Jul 2026"))
  expect_equal(app$get_value(output = "overview-kpi_total"), total_all)
})

test_that("the CSV download works and matches the selection", {
  app <- start_app()
  on.exit(app$stop())
  app$set_inputs(geography = "British Columbia", resolution = "Annual", nav = "Data")
  app$wait_for_idle(800)
  f <- app$get_download("data-download")
  csv <- readr::read_csv(f, show_col_types = FALSE)
  expect_equal(nrow(csv), 12 * 12)                      # 12 years x 12 categories
  expect_true(all(csv$Geography == "British Columbia"))
  expect_true("Work permits that took effect" %in% names(csv))
  bc <- kpi_total(filter_selection(BUNDLE$data, min(BUNDLE$data$month),
                                   BUNDLE$meta$latest_month, "British Columbia", c("TFWP", "IMP")))
  expect_equal(sum(csv$`Work permits that took effect`), bc)
})

test_that("when the live read fails the app falls back to the cache and says so", {
  app <- start_app(local_dir = file.path(tempdir(), "does-not-exist"))
  on.exit(app$stop())
  bar <- status_text(app)
  expect_match(bar, "Using saved data")
  expect_match(bar, "download failed")
  expect_match(app$get_value(output = "overview-kpi_total"), "^[0-9,]+$")
})

test_that("a file with an extra month extends the slider, header and charts automatically", {
  app <- start_app(local_dir = make_extra_month_dir())
  on.exit(app$stop())
  expect_match(status_text(app), "Latest month available: August 2026")
  expect_equal(app$get_value(input = "range")[2], "Aug 2026")
  expect_match(app$get_value(output = "overview-kpi_latest_title"), "August 2026")
  app$set_inputs(nav = "Data"); app$wait_for_idle(800)
  expect_match(app$get_value(output = "data-title"), "Jul|Aug 2026")
})

test_that("clicking a bar in the ranking chart selects that province (and again returns to Canada)", {
  app <- start_app()
  on.exit(app$stop())
  app$set_inputs(nav = "Geography"); app$wait_for_idle(1500)
  # Simulate the plotly click event the browser sends for the Alberta bar.
  click <- function() app$set_inputs(`plotly_click-geography-rank` = '[{"customdata":"Alberta"}]',
                                     allow_no_input_binding_ = TRUE)
  click(); app$wait_for_idle(1500)
  expect_equal(app$get_value(input = "geography"), "Alberta")
  expect_match(app$get_value(output = "geography-compare_title"), "Alberta")
  app$set_inputs(`plotly_click-geography-rank` = NULL, allow_no_input_binding_ = TRUE)
  click(); app$wait_for_idle(1500)
  expect_equal(app$get_value(input = "geography"), "Canada")
  expect_equal(nrow(browser_errors(app)), 0)
})

test_that("charts settle on a wide screen (no endless redraw) and on a narrow one", {
  for (w in c(1920, 420)) {
    withr::local_envvar(TW_LOCAL_DIR = TESTDATA, TW_TRIES = "1", TW_TIMEOUT = "5")
    app <- AppDriver$new(PROJECT, load_timeout = 90000, timeout = 30000, width = w, height = 1000)
    app$set_inputs(nav = "Trends"); app$wait_for_idle(2000)
    app$set_inputs(nav = "Geography"); app$wait_for_idle(2000)
    app$set_inputs(nav = "Trends"); app$wait_for_idle(2000)
    # After Shiny is idle, count how many times the charts are re-sent in 4 seconds.
    n_before <- sum(grepl("shiny:value (trends-trend_plot|geography-rank_plot)", app$get_logs()$message))
    Sys.sleep(4)
    n_after <- sum(grepl("shiny:value (trends-trend_plot|geography-rank_plot)", app$get_logs()$message))
    expect_equal(n_after, n_before, info = paste("width", w))
    # The trend chart has drawn its lines
    expect_gt(app$get_js("document.querySelectorAll('#trends-trend_plot path.js-line').length"), 0)
    app$stop()
  }
})
