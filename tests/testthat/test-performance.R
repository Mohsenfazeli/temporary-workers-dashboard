# Performance: cold start under 10 seconds, filter changes under 1 second.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
skip_on_cran()
library(shinytest2)

test_that("cold start (data read + app ready) is under 10 seconds", {
  withr::local_envvar(TW_LOCAL_DIR = TESTDATA)
  t0 <- Sys.time()
  app <- AppDriver$new(PROJECT, load_timeout = 60000, width = 1366, height = 768)
  on.exit(app$stop())
  app$wait_for_idle(500)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("Cold start with local files: %.1f s (live download timed in test-live.R)", elapsed))
  expect_lt(elapsed, 10)
})

test_that("filter changes update in under 1 second", {
  withr::local_envvar(TW_LOCAL_DIR = TESTDATA)
  app <- AppDriver$new(PROJECT, load_timeout = 60000, width = 1366, height = 768)
  on.exit(app$stop())
  times <- c()
  for (tab in c("Overview", "Trends", "Geography", "Composition")) {
    app$set_inputs(nav = tab); app$wait_for_idle(500)
    for (chg in list(list(geography = "Ontario"), list(program = "IMP"),
                     list(resolution = "Monthly"), list(geography = "Canada", program = "Both",
                                                         resolution = "Annual"))) {
      t0 <- Sys.time()
      do.call(app$set_inputs, c(chg, list(wait_ = TRUE)))
      times <- c(times, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    }
  }
  message(sprintf("Filter change: median %.2f s, max %.2f s", median(times), max(times)))
  expect_lt(max(times), 1)
})

test_that("calculations for one filter change take well under 1 second (server side)", {
  d <- BUNDLE$data
  t0 <- Sys.time()
  sel <- filter_selection(d, "2015-01-01", BUNDLE$meta$latest_month, "Ontario", c("TFWP", "IMP"))
  invisible(list(summarise_series(sel, "Annual"), kpi_period_change(sel),
                 geo_ranking(d, "2015-01-01", BUNDLE$meta$latest_month, c("TFWP", "IMP")),
                 composition_shares(sel, "Annual"),
                 make_takeaways(d, "2015-01-01", BUNDLE$meta$latest_month, "Ontario", "Both")))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
})
