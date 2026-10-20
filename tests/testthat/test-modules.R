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
  overview = c("kpi_intro", "kpi_ytd", "kpi_ytd_title", "kpi_ytd_sub", "kpi_change",
               "kpi_change_title", "kpi_change_sub", "kpi_split", "kpi_split_title",
               "kpi_split_sub", "kpi_latest", "kpi_latest_title", "kpi_latest_sub",
               "headline_title", "headline_plot", "takeaways", "highlights"),
  trends = c("trend_title", "trend_plot", "yoy_title", "yoy_plot", "trend_source"),
  geography = c("rank_title", "rank_plot", "rank_table_title", "rank_table", "compare_title", "compare_plot",
                "contrib_title", "contrib_plot"),
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
    # Every chart, table and title changes, including the cards and the
    # contribution chart, which follow the last month of the period.
    charts <- grep("plot|table|title$|^kpi_", names(a), value = TRUE)
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
  expect_match(both$kpi_split_title, "IMP / TFWP split, Jan\u2013Jul 2026", fixed = TRUE)
  tf <- render_all("overview", make_sel("Canada", "TFWP", "Annual"))
  expect_match(tf$kpi_split_title, "TFWP share of all work permits, Jan\u2013Jul 2026", fixed = TRUE)
  sp <- kpi_split(d, "2026-01-01", meta$latest_month, "Canada")         # same months as card 1
  expect_match(tf$kpi_split, fmt_share(sp$share[sp$program == "TFWP"], 0), fixed = TRUE)
  expect_no_match(tf$kpi_split_sub, "IMP [0-9]")
  expect_match(tf$kpi_change_title, "Change from the same months of 2025", fixed = TRUE)
})

test_that("the four cards cover the same months and follow place, program and the END of the period", {
  ov <- render_all("overview", make_sel("Canada", "Both", "Annual"))
  expect_match(ov$kpi_intro, "Key numbers for Canada (TFWP and IMP), January to July 2026.", fixed = TRUE)
  expect_match(ov$kpi_ytd_title, "Work permits issued, Jan\u2013Jul 2026", fixed = TRUE)
  expect_match(ov$kpi_ytd, "673,280", fixed = TRUE)
  expect_match(ov$kpi_change, "+45.8%", fixed = TRUE)
  expect_match(ov$kpi_change_sub, "673,280 in Jan\u2013Jul 2026, compared with 461,760 in Jan\u2013Jul 2025", fixed = TRUE)
  expect_match(ov$kpi_split, "83% / 17%", fixed = TRUE)
  expect_match(ov$kpi_latest_title, "Latest month: July 2026", fixed = TRUE)
  expect_match(ov$kpi_latest, "72,495", fixed = TRUE)
  # Independent check (base R): Jan-Jul 2026 for B.C., both programs
  bc <- render_all("overview", make_sel("British Columbia", "Both", "Annual"))
  v <- sum(d$value[d$geography == "British Columbia" & d$year == 2026])
  expect_match(bc$kpi_ytd, fmt_n(v), fixed = TRUE)
  # The start of the period does not change the cards; the end does
  late <- render_all("overview", make_sel("Canada", "Both", "Annual", start = "2024-01-01"))
  expect_identical(late[grep("^kpi_", names(late))], ov[grep("^kpi_", names(ov))])
  y24 <- render_all("overview", make_sel("Canada", "Both", "Annual", end = "2024-12-01"))
  expect_match(y24$kpi_ytd_title, "Work permits issued in 2024", fixed = TRUE)
  expect_match(y24$kpi_ytd, fmt_n(sum(d$value[d$geography == "Canada" & d$year == 2024])), fixed = TRUE)
  expect_match(y24$kpi_latest_title, "Last month selected: December 2024", fixed = TRUE)
  expect_match(y24$kpi_change_title, "Change from 2023", fixed = TRUE)
  # The contribution chart (Geography) uses the same months as the cards
  g <- render_all("geography", make_sel("Canada", "Both", "Annual"))
  expect_match(g$contrib_title, "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025", fixed = TRUE)
  g24 <- render_all("geography", make_sel("Canada", "Both", "Annual", end = "2024-12-01"))
  expect_match(g24$contrib_title, "2024 vs 2023", fixed = TRUE)
})

test_that("the composition mix can be shown as numbers, and the glossary is shown", {
  sel <- shiny::reactiveVal(make_sel("Canada", "Both", "Annual"))
  shiny::testServer(function(input, output, session) composition_server("x", sel, d, meta), {
    pct <- paste(format(output[["x-share_imp_plot"]]), collapse = "")
    session$setInputs(`x-measure` = "number")
    num <- paste(format(output[["x-share_imp_plot"]]), collapse = "")
    expect_false(identical(pct, num))
    expect_match(num, "Work permits issued (number)", fixed = TRUE)
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
  expect_no_match(note_for("British Columbia", "TFWP"), "Reading the IMP chart")  # no IMP, no note
})

test_that("About states the data rules, the IMP subset caveat, event sources and a glossary", {
  ab <- NULL
  shiny::testServer(function(input, output, session) about_server("x", d, meta), {
    ab <<- paste(format(output[["x-about"]]), collapse = "")
  })
  expect_match(ab, "two")                                       # the two IRCC CSV files
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
  expect_match(tr$yoy_title, "^Change from a year earlier in work permits issued, by program, Canada, ")
  expect_no_match(tr$yoy_title, "%")
  # Events in one year share one numbered box (key under the chart), not a long label
  expect_match(tr$trend_plot, '"text":"2\u20135"', fixed = TRUE)
  expect_no_match(tr$trend_plot, "study cap &", fixed = TRUE)
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

test_that("plain language: no 'YTD', 'took effect' or other jargon in any tab", {
  jargon <- "YTD|took effect|taking effect|vermillion|by the author|part-year|<br>part\\b|year-to-date|vs a year earlier"
  for (s in list(make_sel("Canada", "Both", "Annual"), make_sel("British Columbia", "IMP", "Monthly"),
                 make_sel("Alberta", "TFWP", "Annual", start = "2019-03-01"))) {
    for (mod in names(SERVERS)) {
      txt <- paste(unlist(render_all(mod, s)), collapse = " ")
      expect_no_match(txt, jargon, info = paste(mod, s$geography, s$program_choice))
    }
  }
  ab <- NULL
  shiny::testServer(function(input, output, session) about_server("x", d, meta), {
    ab <<- paste(format(output[["x-about"]]), collapse = "")
  })
  # IRCC's own file titles ("... became effective") are quoted in Sources; the
  # explanatory text uses "issued"
  expect_no_match(ab, jargon)
  expect_match(ab, "became effective", fixed = TRUE)
  ui <- as.character(htmltools::renderTags(htmltools::tagList(
    overview_ui("o"), trends_ui("t"), geography_ui("g"), composition_ui("c"), data_ui("d")))$html)
  expect_no_match(ui, jargon)
  # TFWP, IMP, LMIA and PGWP are each defined in plain words in the glossary
  for (t in c("TFWP", "IMP", "LMIA", "PGWP")) expect_true(t %in% GLOSSARY$term)
})

test_that("feedback Oct 20: part year not joined, labelled change, numbered events, clearer table", {
  tr <- render_all("trends", make_sel("Canada", "Both", "Annual"))
  pl <- jsonlite::fromJSON(tr$trend_plot, simplifyVector = FALSE)$x
  # The lines (lines+markers) never include the part year; it is a separate open circle
  lines <- Filter(function(t) identical(t$mode, "lines+markers"), pl$data)
  expect_length(lines, 3)
  for (t in lines) expect_false(any(grepl("Jan\u2013", unlist(t$x))))
  opens <- Filter(function(t) identical(t$mode, "markers"), pl$data)
  expect_length(opens, 3)
  for (t in opens) expect_equal(t$marker$symbol, "circle-open")
  # Label: change from the same months a year earlier (same number as the Overview card)
  card <- overview_cards(d, latest, "Canada", "Both")$cards[[2]]$value
  expect_match(tr$trend_plot, sprintf("Jan\u2013Jul 2026: %s<br>vs Jan\u2013Jul 2025", card), fixed = TRUE)
  # Numbered key under the chart
  expect_match(tr$trend_source, "1. <a href", fixed = TRUE)
  expect_match(tr$trend_source, "6. <a href", fixed = TRUE)
  expect_match(tr$trend_source, "not joined to the full years", fixed = TRUE)
  # Monthly view: plain lines, no markers, no part-year label
  mo <- render_all("trends", make_sel("Canada", "Both", "Monthly"))
  mp <- jsonlite::fromJSON(mo$trend_plot, simplifyVector = FALSE)$x
  expect_true(all(vapply(mp$data, function(t) identical(t$mode, "lines"), TRUE)))
  expect_no_match(mo$trend_plot, "vs Jan\u2013Jul 2025", fixed = TRUE)
  # One program: label on that program's point
  tf <- render_all("trends", make_sel("Canada", "TFWP", "Annual"))
  expect_match(tf$trend_plot, "Jan\u2013Jul 2026: ", fixed = TRUE)
  # Geography table: Growth header names the periods; plain flag with a tooltip
  geo <- render_all("geography", make_sel("Canada", "Both", "Annual"))
  expect_match(geo$rank_table, sprintf("Growth, %s\u2013%s vs 2015", fmt_month_short(latest %m-% months(11)), fmt_month_short(latest)), fixed = TRUE)
  expect_match(geo$rank_table, "Hidden counts", fixed = TRUE)
  expect_match(geo$rank_table, "hidden-flag", fixed = TRUE)
  expect_no_match(geo$rank_table, "Small", fixed = TRUE)
  src <- list()
  sel <- shiny::reactiveVal(make_sel("Canada", "Both", "Annual"))
  shiny::testServer(function(input, output, session) geography_server("x", sel, d, meta), {
    for (o in c("table_source", "contrib_source")) src[[o]] <<- paste(format(output[[paste0("x-", o)]]), collapse = "")
  })
  expect_match(src$table_source, "many hidden counts", fixed = TRUE)
  expect_no_match(src$table_source, "Small n", fixed = TRUE)
  # Short common source line with a link to About the data
  expect_match(src$contrib_source, "about-link", fixed = TRUE)
  expect_match(src$contrib_source, "Permits, not people", fixed = TRUE)
  # Composition: the Unknown IMP option says what happens to the 100%
  ui <- as.character(htmltools::renderTags(composition_ui("c"))$html)
  expect_match(ui, "count only permits with a known category", fixed = TRUE)
  expect_no_match(ui, "leave out Unknown IMP", fixed = TRUE)
})

test_that("feedback Oct 20: sidebar definitions fold away, filters start closed on a phone, notes tab follows the selection", {
  app_src <- paste(readLines(file.path(PROJECT, "app.R"), warn = FALSE), collapse = "\n")
  expect_match(app_src, 'mobile = "closed"', fixed = TRUE)
  expect_match(app_src, "tags$details(class = \"sidebar-help\"", fixed = TRUE)
  expect_match(app_src, "not individual workers", fixed = TRUE)
  expect_match(app_src, "a.about-link", fixed = TRUE)
  ov_src <- paste(readLines(file.path(PROJECT, "R", "mod_overview.R"), warn = FALSE), collapse = "\n")
  expect_match(ov_src, 'bslib::nav_select("notes_tab"', fixed = TRUE)
})
