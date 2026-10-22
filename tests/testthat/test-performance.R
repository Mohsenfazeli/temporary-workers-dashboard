# Performance targets: the app is ready in under 10 seconds and a filter change
# updates in under 1 second.
# What is measured, and why:
#  - Start-up is timed from the saved copy bundled with the app
#    (data/baseline.rds via the live-read fallback), so the number reflects the
#    app itself, not the network. A live start also downloads about 17 MB from
#    IRCC, so its time depends on the connection; test-live.R times that
#    download separately.
#  - Filter speed uses the MEDIAN of 16 changes (a typical change), with a
#    looser guard on the slowest one, because a single slow change can come
#    from the browser or the computer being busy rather than from the app.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
skip_on_cran()
library(shinytest2)

# Start the app from the bundled saved copy: pointing TW_LOCAL_DIR at a folder
# without the IRCC files makes the read fail at once, so the app loads
# data/baseline.rds (the same path it takes when IRCC can't be reached and no
# cache.rds exists yet). The baseline is never overwritten here.
start_from_cache <- function() {
  withr::local_envvar(TW_LOCAL_DIR = file.path(tempdir(), "no-ircc-files-here"),
                      TW_TRIES = "1", TW_TIMEOUT = "5", .local_envir = parent.frame())
  AppDriver$new(PROJECT, load_timeout = 60000, timeout = 30000, width = 1366, height = 768)
}

test_that("cold start from the bundled saved copy (app ready) is under 10 seconds", {
  t0 <- Sys.time()
  app <- start_from_cache()
  on.exit(app$stop())
  app$wait_for_idle(500)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # Check it really started from the saved copy, with the full data range
  bar <- gsub("\\s+", " ", paste(app$get_text(".status-bar"), collapse = " "))  # tidy spaces
  expect_match(bar, "Using saved data")
  expect_match(bar, "Latest month available: [A-Z][a-z]+ 20[0-9]{2}")
  message(sprintf("Cold start from saved data (baseline/cache fallback): %.1f s (a live start adds the IRCC download; see test-live.R)", elapsed))
  expect_lt(elapsed, 10)
})

test_that("filter changes update in under 1 second (median), none over 2 seconds", {
  app <- start_from_cache()
  on.exit(app$stop())
  times <- c()
  for (tab in c("Overview", "Trends", "Geography", "Composition")) {
    app$set_inputs(nav = tab); app$wait_for_idle(500)
    for (chg in list(list(geography = "Ontario"), list(program = "IMP"),
                     list(resolution = "Monthly"), list(geography = "Canada", program = "Both",
                                                         resolution = "Annual"))) {
      t0 <- Sys.time()
      do.call(app$set_inputs, c(chg, list(wait_ = TRUE)))   # waits until outputs update
      times <- c(times, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    }
  }
  message(sprintf("Filter change (%d changes): median %.2f s, max %.2f s",
                  length(times), median(times), max(times)))
  expect_lt(median(times), 1)   # the brief's target, for a typical change
  expect_lt(max(times), 2)      # guard: no change is badly slow
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
