# Server-side module tests: every geography x program x view combination
# renders every output without errors or warnings, and the slider and the
# monthly/annual toggle change every chart.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
d <- BUNDLE$data; meta <- BUNDLE$meta
first <- min(d$month); latest <- meta$latest_month

make_sel <- function(geo = "Canada", prog = "Both", res = "Annual",
                     start = first, end = latest) {
  list(start = as.Date(start), end = as.Date(end), resolution = res, geography = geo,
       program_choice = prog, programs = programs_from_choice(prog))
}

# The outputs each module produces.
OUTPUTS <- list(
  overview = c("kpi_total", "kpi_split", "kpi_latest", "kpi_change", "headline_title",
               "headline_plot", "takeaways", "kpi_total_sub", "kpi_latest_sub", "kpi_change_sub",
               "kpi_split_title", "kpi_split_sub", "kpi_change_title",
               "kpi_year", "kpi_year_title", "kpi_year_sub", "highlights"),
  trends = c("trend_title", "trend_plot", "yoy_title", "yoy_plot", "trend_source"),
  geography = c("rank_title", "rank_plot", "rank_table", "compare_title", "compare_plot"),
  composition = c("share_tfwp_title", "share_tfwp_plot", "share_imp_title", "share_imp_plot",
                  "mix_title", "mix_plot", "unknown_note", "detail_title", "detail_table"),
  data = c("title", "table")
)
SERVERS <- list(
  overview = function(sel) overview_server("x", sel, d, meta),
  trends = function(sel) trends_server("x", sel, d, meta),
  geography = function(sel) geography_server("x", sel, d, meta),
  composition = function(sel) composition_server("x", sel, d, meta, BUNDLE$detail),
  data = function(sel) data_server("x", sel, d, meta)
)

# Render all outputs of one module for one selection; return them as text.
# Any warning raised while rendering (e.g. plotly dropping missing values)
# is collected and returned in attr(out, "warnings").
render_all <- function(mod, s) {
  out <- list(); warns <- character()
  sel <- shiny::reactiveVal(s)
  withCallingHandlers(
    shiny::testServer(function(input, output, session) SERVERS[[mod]](sel), {
      for (o in OUTPUTS[[mod]]) {
        out[[o]] <<- paste(format(output[[paste0("x-", o)]]), collapse = "")
      }
    }),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  attr(out, "warnings") <- warns
  stopifnot(length(out) == length(OUTPUTS[[mod]]))   # every output was rendered
  out
}

test_that("every geography and program combination renders every tab without errors or warnings", {
  geos <- c("Canada", PROVINCES_TERRITORIES)
  n <- 0
  for (geo in geos) for (prog in c("Both", "TFWP", "IMP")) for (res in c("Annual", "Monthly")) {
    for (mod in names(SERVERS)) {
      expect_no_error(out <- render_all(mod, make_sel(geo, prog, res)))
      expect_length(attr(out, "warnings"), 0)
      expect_true(all(nzchar(unlist(out)) | names(out) == "unknown_note"),
                  info = paste(mod, geo, prog, res))
      n <- n + 1
    }
  }
  expect_equal(n, 14 * 3 * 2 * 5)
})

test_that("edge selections (one month, 2026 only, territory) render without errors", {
  for (s in list(make_sel("Nunavut", "TFWP", "Monthly", latest, latest),
                 make_sel("Yukon", "Both", "Annual", "2026-01-01", latest),
                 make_sel("Canada", "IMP", "Annual", latest, latest),
                 make_sel("Northwest Territories", "Both", "Monthly", "2015-01-01", "2015-02-01"))) {
    for (mod in names(SERVERS)) {
      expect_no_error(out <- render_all(mod, s))
      expect_length(attr(out, "warnings"), 0)
    }
  }
})

test_that("the date-range slider changes every chart and title", {
  for (mod in names(SERVERS)) {
    a <- render_all(mod, make_sel("British Columbia", start = first, end = latest))
    b <- render_all(mod, make_sel("British Columbia", start = "2020-01-01", end = "2022-12-01"))
    changed <- names(a)[unlist(a) != unlist(b)]
    charts <- grep("plot|table|title$|^kpi_total$", names(a), value = TRUE)
    # KPI card labels (e.g. "Split: IMP vs TFWP") name what is measured, not
    # the value, so they rightly stay the same when only the dates change
    charts <- setdiff(charts, c("kpi_split_title", "kpi_change_title"))
    expect_true(all(charts %in% changed), info = paste(mod, "unchanged:",
                paste(setdiff(charts, changed), collapse = ", ")))
  }
})

test_that("the monthly/annual toggle changes every chart that has a time axis", {
  for (mod in names(SERVERS)) {
    a <- render_all(mod, make_sel("British Columbia", res = "Annual"))
    b <- render_all(mod, make_sel("British Columbia", res = "Monthly"))
    # The geography ranking (rank_plot/rank_table) shows totals for the whole
    # selected period, so it is the same in both views by design (its title
    # says "period totals"); every chart with a time axis must change.
    timed <- intersect(names(a), c("headline_plot", "trend_plot", "trend_title", "yoy_plot",
                                   "share_tfwp_plot", "share_imp_plot", "compare_plot",
                                   "compare_title", "table", "title"))
    changed <- names(a)[unlist(a) != unlist(b)]
    expect_true(all(timed %in% changed), info = paste(mod, "unchanged:",
                paste(setdiff(timed, changed), collapse = ", ")))
  }
})

test_that("the Trends tab shows only data from the two IRCC CSV files", {
  sel <- shiny::reactiveVal(make_sel("Canada", "Both", "Annual"))
  shiny::testServer(function(input, output, session) trends_server("x", sel, d, meta), {
    expect_no_match(output$`x-trend_plot`, "distinct people|diamond|Excel", ignore.case = TRUE)
    expect_no_match(paste(format(output$`x-trend_source`), collapse = ""), "xlsx|Excel", ignore.case = TRUE)
  })
  ui <- as.character(htmltools::renderTags(trends_ui("t"))$html)
  expect_no_match(ui, "show_ref|distinct people", ignore.case = TRUE)
})

test_that("the split card follows the program filter", {
  both <- render_all("overview", make_sel("Canada", "Both", "Annual"))
  expect_match(both$kpi_split_title, "Split: IMP vs TFWP")
  tf <- render_all("overview", make_sel("Canada", "TFWP", "Annual"))
  expect_match(tf$kpi_split_title, "TFWP share of all work permits")
  sp <- kpi_split(d, "2015-01-01", meta$latest_month, "Canada")
  expect_match(tf$kpi_split, fmt_share(sp$share[sp$program == "TFWP"], 0), fixed = TRUE)
  expect_no_match(tf$kpi_split_sub, "IMP [0-9]")
  expect_match(tf$kpi_change_title, "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025", fixed = TRUE)
})

test_that("the composition mix can be shown as numbers, and the glossary is shown", {
  sel <- shiny::reactiveVal(make_sel("Canada", "Both", "Annual"))
  shiny::testServer(function(input, output, session) composition_server("x", sel, d, meta), {
    pct <- paste(format(output[["x-share_imp_plot"]]), collapse = "")
    session$setInputs(`x-measure` = "number")
    num <- paste(format(output[["x-share_imp_plot"]]), collapse = "")
    expect_false(identical(pct, num))
    expect_match(num, "Work permits that took effect (number)", fixed = TRUE)
    expect_match(pct, "Share of the program", fixed = TRUE)
  })
  ui <- as.character(composition_ui("x"))
  expect_match(ui, "What the categories mean"); expect_match(ui, "Labour Market Impact Assessment")
})

test_that("the 'Reading the IMP mix' note follows the selected geography", {
  note_for <- function(geo, prog = "Both") {
    sel <- shiny::reactiveVal(make_sel(geo, prog, "Annual"))
    txt <- NULL
    shiny::testServer(function(input, output, session) composition_server("x", sel, d, meta), {
      txt <<- paste(format(output[["x-unknown_note"]]), collapse = "")
      sel(make_sel("British Columbia", prog, "Annual"))      # change the filter
      session$flushReact()
      attr(txt, "after_bc") <<- paste(format(output[["x-unknown_note"]]), collapse = "")
    })
    txt
  }
  ca <- note_for("Canada"); bc <- attr(ca, "after_bc")
  expect_match(ca, "across Canada", fixed = TRUE)
  expect_match(bc, "in British Columbia", fixed = TRUE)
  expect_false(identical(ca, bc))                             # reacts to the filter
  # The peak shown is BC's own peak, not Canada's
  peak_of <- function(geo) {
    u <- d[d$geography == geo & d$category == "Unknown or other IMP" &
             d$year < as.integer(format(latest, "%Y")), ]
    max(tapply(u$value, u$year, sum))
  }
  expect_match(bc, paste("a peak of", fmt_n(peak_of("British Columbia"))), fixed = TRUE)
  expect_no_match(bc, fmt_n(peak_of("Canada")), fixed = TRUE)
  expect_no_match(note_for("British Columbia", "TFWP"), "Reading the IMP mix")  # no IMP, no note
})

test_that("About states the data rules, the IMP subset caveat, event sources and a glossary", {
  ab <- NULL
  shiny::testServer(function(input, output, session) about_server("x", d, meta), {
    ab <<- paste(format(output[["x-about"]]), collapse = "")
  })
  expect_match(ab, "two")                                       # two assignment CSV files
  expect_match(ab, "subset", ignore.case = TRUE)
  expect_match(ab, "preliminary", ignore.case = TRUE)
  expect_match(ab, "Glossary")
  for (u in KEY_EVENTS$url) expect_match(ab, u, fixed = TRUE)
})

test_that("event lines on the trend chart carry their sources", {
  out <- render_all("trends", make_sel("Canada", "Both", "Monthly"))
  expect_match(out$trend_source, "do not imply cause")
  for (u in KEY_EVENTS$url) expect_match(out$trend_source, u, fixed = TRUE)
  expect_no_match(out$trend_source, "\\(\\s+<a|</a>\\s+\\)")    # no stray spaces around links
})

test_that("external feedback: YoY shows both programs, merged event label, table headers, card precision", {
  tr <- render_all("trends", make_sel("Canada", "Both", "Annual"))
  expect_match(tr$yoy_plot, "Both programs", fixed = TRUE)
  expect_match(tr$yoy_title, "both programs (up|down)")
  expect_match(tr$trend_plot, "2024: study cap & TR target & TFWP limits & PGWP rules", fixed = TRUE)
  tf <- render_all("trends", make_sel("Canada", "TFWP", "Annual"))
  expect_no_match(tf$yoy_plot, "Both programs", fixed = TRUE)
  geo <- render_all("geography", make_sel("Canada", "Both", "Annual"))
  expect_match(geo$rank_table, "Province / territory", fixed = TRUE)
  expect_match(geo$rank_table, "Note*", fixed = TRUE)
  expect_no_match(geo$rank_table, "<caption", fixed = TRUE)          # no duplicate Growth note
  ov <- render_all("overview", make_sel("Canada", "Both", "Monthly"))
  num <- sub("^\\+", "", ov$kpi_change)                              # e.g. "45.8%"
  expect_match(ov$takeaways, num, fixed = TRUE)                       # same number, same precision
})
