# Function tests: key numbers and growth calculations, including edge cases.
skip_if_not(have_testdata, "Local IRCC test files not found in testdata/ (run Rscript tools/get_testdata.R first)")
d <- BUNDLE$data
latest <- BUNDLE$meta$latest_month

test_that("pct_change handles division by zero and missing values", {
  expect_equal(pct_change(110, 100), 0.1)
  expect_true(is.na(pct_change(5, 0)))
  expect_true(is.na(pct_change(5, NA)))
  expect_true(is.na(pct_change(0, 0)))
  expect_equal(fmt_pct(NA), "n/a")
  expect_equal(fmt_pct(0.1234), "+12.3%")
  expect_equal(fmt_pct(-0.05), "-5.0%")
  expect_equal(fmt_n(1234567), "1,234,567")
  expect_equal(fmt_n(NA), "n/a")
})

test_that("KPI total, split and latest month match direct calculations", {
  sel <- filter_selection(d, "2024-01-01", "2024-12-01", "Canada", c("TFWP", "IMP"))
  expect_equal(kpi_total(sel), 194680 + 729555)
  sp <- kpi_split(d, "2024-01-01", "2024-12-01", "Canada")
  expect_equal(sum(sp$share), 1)
  expect_equal(sp$value[sp$program == "TFWP"], 194680)
  lt <- kpi_latest(d, "2026-07-01", "Canada", c("TFWP", "IMP"))
  expect_equal(lt$value, 72495)                       # July 2026, spot-checked
  expect_equal(lt$prev_month, as.Date("2025-07-01"))
  expect_equal(lt$change, pct_change(72495, lt$prev_value))
})

test_that("period change uses 12-month blocks, single months, or nothing", {
  long <- filter_selection(d, "2015-01-01", latest, "Canada", "TFWP")
  pc <- kpi_period_change(long)
  m <- aggregate(value ~ month, data = long, FUN = sum)
  n <- nrow(m)
  expect_equal(pc$method, "12m")
  expect_equal(pc$change, sum(m$value[(n - 11):n]) / sum(m$value[1:12]) - 1)
  short <- filter_selection(d, "2026-01-01", latest, "Canada", "TFWP")
  expect_equal(kpi_period_change(short)$method, "month")
})

test_that("edge case: a one-month range works everywhere", {
  one <- filter_selection(d, latest, latest, "Canada", c("TFWP", "IMP"))
  expect_equal(nrow(summarise_series(one, "Monthly")), 2)
  ann <- summarise_series(one, "Annual", latest_month = latest)
  expect_true(all(ann$partial))
  expect_match(ann$label[1], "^2026 \\(Jul\\)$")
  pc <- kpi_period_change(one)
  expect_true(is.na(pc$change))
  expect_equal(fmt_pct(pc$change), "n/a")
  expect_gte(length(make_takeaways(d, latest, latest, "Canada", "Both")), 2)
  expect_equal(nrow(geo_ranking(d, latest, latest, c("TFWP", "IMP"))), 13)
})

test_that("edge case: a range inside 2026 only is year-to-date and compared with the same months of 2025", {
  ytd <- filter_selection(d, "2026-01-01", latest, "Canada", "IMP")
  ann <- summarise_series(ytd, "Annual", latest_month = latest)
  expect_equal(nrow(ann), 1)
  expect_equal(ann$label, "2026 (Jan\u2013Jul)")
  y <- yoy_series(d[d$geography == "Canada" & d$program == "IMP", ], "2026-01-01", latest,
                  "Annual", latest_month = latest)
  jan_jul_2025 <- sum(d$value[d$geography == "Canada" & d$program == "IMP" &
                                d$month >= as.Date("2025-01-01") & d$month <= as.Date("2025-07-01")])
  expect_equal(y$prev_value, jan_jul_2025)                 # not all of 2025
  expect_equal(y$yoy, 560490 / jan_jul_2025 - 1)
})

test_that("partial years (cut by the slider) are labelled and compared like-for-like", {
  y <- yoy_series(d[d$geography == "Canada", ], "2019-03-01", "2020-12-01", "Annual",
                  by = "program", latest_month = latest)
  expect_true(any(grepl("2019 \\(Mar\u2013Dec\\)", y$label)))
  v18 <- sum(d$value[d$geography == "Canada" & d$program == "TFWP" &
                       d$month >= as.Date("2018-03-01") & d$month <= as.Date("2018-12-01")])
  expect_equal(y$prev_value[y$program == "TFWP" & grepl("2019", y$label)], v18)
})

test_that("edge case: territories with mostly masked values do not break calculations", {
  nu <- filter_selection(d, "2026-01-01", latest, "Nunavut", "TFWP")
  expect_gt(sum(nu$masked_cells), 0)
  expect_silent(kpi_period_change(nu))
  tk <- make_takeaways(d, "2026-01-01", latest, "Nunavut", "TFWP")
  expect_true(any(grepl("suppressed for privacy", tk)))
  lt <- kpi_latest(d, latest, "Nunavut", "TFWP")
  expect_true(is.na(lt$change) || is.finite(lt$change))
  cp <- composition_period(nu)
  expect_equal(sum(cp$share), 1)
})

test_that("edge case: a selection with zero permits returns NA shares, not errors", {
  z <- filter_selection(d, "2015-01-01", "2015-03-01", "Nunavut", "TFWP")
  z$value <- 0
  expect_true(all(is.na(composition_period(z)$share)))
  expect_true(is.na(kpi_period_change(z)$change))
  zz <- d; zz$value <- 0
  sp <- kpi_split(zz, "2015-01-01", "2015-12-01", "Yukon")
  expect_true(all(is.na(sp$share)))
  expect_match(make_takeaways(zz, "2015-01-01", "2015-12-01", "Yukon", "TFWP"), "^No ")
})

test_that("geography ranking: 13 provinces/territories, shares of Canada including 'not stated'", {
  r <- geo_ranking(d, "2015-01-01", latest, c("TFWP", "IMP"))
  expect_equal(nrow(r), 13)
  expect_false(NOT_STATED_GEO %in% r$geography)
  canada <- kpi_total(filter_selection(d, "2015-01-01", latest, "Canada", c("TFWP", "IMP")))
  not_stated <- kpi_total(filter_selection(d, "2015-01-01", latest, NOT_STATED_GEO, c("TFWP", "IMP")))
  expect_equal(sum(r$share), 1 - not_stated / canada)
  expect_equal(r$rank, 1:13)
  expect_true(all(diff(r$value) <= 0))
})

test_that("comparison with Canada gives shares and year-over-year for both", {
  c <- compare_with_canada(d, "2015-01-01", latest, "British Columbia", c("TFWP", "IMP"), latest)
  expect_true(all(c("value_pt", "value_canada", "yoy_pt", "yoy_canada", "share") %in% names(c)))
  expect_true(all(c$share > 0 & c$share < 1))
  expect_true(is.na(c$yoy_pt[1]))                 # 2015 has no earlier year
})

test_that("composition shares add to 100% in every period", {
  s <- composition_shares(filter_selection(d, "2015-01-01", latest, "Canada", "IMP"), "Annual", latest)
  tot <- tapply(s$share, s$period, sum)
  expect_true(all(abs(tot - 1) < 1e-9))
})

test_that("download table matches the selection and resolution", {
  sel <- filter_selection(d, "2025-01-01", "2025-12-01", "Alberta", c("TFWP", "IMP"))
  m <- table_for_download(sel, "Monthly", latest, "Alberta")
  a <- table_for_download(sel, "Annual", latest, "Alberta")
  expect_equal(nrow(m), 12 * 12)
  expect_equal(nrow(a), 12)
  expect_equal(sum(m$`Work permits issued`), sum(a$`Work permits issued`))
  expect_equal(sum(a$`Work permits issued`), kpi_total(sel))
})

test_that("takeaways are plain sentences with thousands separators", {
  tk <- make_takeaways(d, "2015-01-01", latest, "Canada", "Both")
  expect_length(tk, 5)
  expect_match(tk[1], "^[0-9]{1,3}(,[0-9]{3})+ TFWP and IMP work permits were issued across Canada from January 2015 to July 2026, an average of [0-9,]+ a month\\. These are permits, not people")
})

test_that("takeaways tell a story: peak and change since, year to date, mix, where", {
  tk <- make_takeaways(d, "2015-01-01", latest, "Canada", "Both")
  # Point 2 is checked against the annual totals computed independently
  ann <- tapply(d$value[d$geography == "Canada"], d$year[d$geography == "Canada"], sum)
  full <- ann[names(ann) < format(latest, "%Y")]
  peak <- names(full)[which.max(full)]
  expect_match(tk[2], sprintf("peak of %s in %s", fmt_n(max(full)), peak), fixed = TRUE)
  expect_match(tk[3], sprintf("^In %s so far \\(Jan", format(latest, "%Y")))
  expect_match(tk[4], "largest single category was .+ \\((TFWP|IMP)\\), at [0-9]+% of all permits")
  expect_match(tk[5], "^Ontario, .+ together received [0-9]+% of Canada's permits")
  # A province: its share of Canada and its rank
  bc <- make_takeaways(d, "2015-01-01", latest, "British Columbia", "IMP")
  expect_match(bc[5], "ranking [0-9]+ of 13 provinces and territories")
  # A range ending in December compares the last month with a year earlier
  y24 <- make_takeaways(d, "2024-01-01", "2024-12-01", "Canada", "Both")
  expect_true(any(grepl("^In December 2024, .+ from December 2023", y24)))
})

test_that("the change card label names the comparison used", {
  expect_equal(kpi_period_change(filter_selection(d, "2015-01-01", latest))$label,
               "Last 12 months vs first 12 months")
  expect_equal(kpi_period_change(filter_selection(d, "2026-01-01", latest))$label, "Jul 2026 vs Jan 2026")
  expect_equal(kpi_period_change(filter_selection(d, latest, latest))$label, "Change over the period")
})

test_that("undercount_flag marks figures where suppressed cells could add over 10%", {
  expect_equal(undercount_flag(c(1000, 1000, 0, 0, 40), c(10, 30, 0, 1, 1)),
               c(FALSE, TRUE, FALSE, TRUE, TRUE))       # 50 = 5% no; 150 = 15% yes
  t <- table_for_download(filter_selection(d, "2015-01-01", latest, "Nunavut", "TFWP"),
                          "Annual", latest, "Nunavut")
  expect_true("Possible undercount over 10%" %in% names(t))
  expect_true(any(t$`Possible undercount over 10%` == "Yes"))   # a territory, mostly suppressed
  expect_true(all(t$`Possible undercount over 10%` %in% c("Yes", "")))
})

test_that("share_over_time: 5 largest provinces, shares of Canada between 0 and 1", {
  d <- BUNDLE$data
  latest <- max(d$month)
  sh <- share_over_time(d, "2015-01-01", latest, c("TFWP", "IMP"), "Annual", top = 5,
                        latest_month = latest)
  expect_equal(levels(sh$place)[1:3], c("Ontario", "British Columbia", "Quebec"))
  expect_equal(length(unique(sh$place)), 5)
  expect_true(all(sh$share > 0 & sh$share < 1))
  # Share is computed against the Canada total for the same period
  on15 <- sh[sh$place == "Ontario" & sh$label == "2015", ]
  can15 <- sum(d$value[d$geography == "Canada" & d$year == 2015])
  expect_equal(on15$share, on15$value / can15)
  # The five shares together never exceed 100%
  tot <- tapply(sh$share, sh$label, sum)
  expect_true(all(tot <= 1))
  # Monthly view works too and has one row per month per province
  shm <- share_over_time(d, "2025-01-01", "2025-12-01", "TFWP", "Monthly", top = 5)
  expect_equal(nrow(shm), 60)
})

test_that("fmt_share shows tiny shares as '<1%' rather than '0%'", {
  expect_equal(fmt_share(c(0.001, 0.004, 0.006, 0.5), 0), c("<1%", "<1%", "1%", "50%"))
  expect_equal(fmt_share(0, 0), "0%")
  expect_equal(fmt_share(NA), "n/a")
})

test_that("date helpers are Windows-safe and language-independent", {
  t <- as.POSIXct("2026-10-16 23:50:00", tz = "UTC")          # 4:50 PM in Vancouver
  expect_equal(fmt_datetime(t), "October 16, 2026, 4:50 PM PT")
  expect_equal(fmt_date(t), "October 16, 2026")
  expect_equal(fmt_date(as.Date("2026-03-01")), "March 1, 2026")   # no leading zero
  expect_equal(fmt_datetime(as.POSIXct("2026-01-05 08:05:00", tz = "UTC")),
               "January 5, 2026, 12:05 AM PT")                    # midnight hour = 12 AM
  expect_equal(fmt_datetime(as.POSIXct("2026-07-01 19:00:00", tz = "UTC")),
               "July 1, 2026, 12:00 PM PT")                       # noon = 12 PM (PDT)
  expect_equal(fmt_month(as.Date("2026-07-01")), "July 2026")
  expect_equal(fmt_month_short(as.Date(c("2015-01-01", "2026-12-01"))), c("Jan 2015", "Dec 2026"))
})

test_that("no R file uses non-portable date format codes (Windows prints them literally)", {
  files <- c(list.files(file.path(PROJECT, "R"), "\\.R$", full.names = TRUE),
             file.path(PROJECT, "app.R"),
             list.files(file.path(PROJECT, "tools"), "\\.R$", full.names = TRUE))
  # %-d, %-I etc. (Linux/macOS only); %l, %k, %P and %e are not supported on Windows
  bad <- "%-[A-Za-z]|%[lkPe]"
  hits <- unlist(lapply(files, function(f) {
    x <- sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8"))   # ignore comments
    i <- grep(bad, x)
    if (length(i)) paste0(basename(f), ":", i, ": ", trimws(x[i]))
  }))
  expect_identical(hits, NULL, info = paste(hits, collapse = "\n"))
})

test_that("chart titles name what is shown and follow the filters", {
  s <- list(start = as.Date("2015-01-01"), end = latest, resolution = "Annual",
            geography = "Canada", program_choice = "Both", last_moy = NULL)
  mo <- month.abb[as.integer(format(latest, "%m"))]; yr <- format(latest, "%Y")
  # Full years and the part year named separately (not "2015 to 2026 (Jan-Jul)")
  per <- sprintf("2015 to %d (full years) and %s (Jan\u2013%s)", as.integer(yr) - 1L, yr, mo)
  expect_equal(chart_title("headline", s), paste0("Work permits issued by program, Canada, ", per))
  expect_equal(chart_title("trend", s), paste0("Work permits issued, both programs and each program, Canada, ", per))
  expect_equal(chart_title("yoy", s), paste0("Change from a year earlier in work permits issued, by program, Canada, ", per))
  expect_equal(chart_title("rank", s), sprintf("Work permits issued by province and territory, TFWP and IMP, Jan 2015 to %s %s", mo, yr))
  expect_equal(chart_title("compare", s), paste0("Share of Canada's work permits issued, five largest provinces, TFWP and IMP, ", per))
  lab <- sprintf("Jan\u2013%s %s vs Jan\u2013%s %d", mo, yr, mo, as.integer(yr) - 1)
  expect_equal(chart_title("contrib", s, label = lab),
               paste0("Share of Canada's change in work permits issued, by province and territory, TFWP and IMP, ", lab))
  expect_equal(chart_title("share", s, prog = "TFWP"), paste0("TFWP work permits issued by category, Canada, ", per))
  expect_equal(chart_title("share", s, prog = "IMP", as_number = FALSE, known_only = TRUE),
               paste0("Share of IMP work permits issued by category (Unknown IMP left out), Canada, ", per))
  # Filters: province, one program, same months, monthly view, one part year
  s2 <- utils::modifyList(s, list(geography = "Ontario", program_choice = "TFWP", last_moy = 7L))
  expect_equal(chart_title("headline", s2), sprintf("TFWP work permits issued, Ontario, Jan\u2013Jul of each year, 2015 to %s", yr))
  expect_equal(chart_title("yoy", s2), sprintf("Change from a year earlier in TFWP work permits issued, Ontario, Jan\u2013Jul of each year, 2015 to %s", yr))
  expect_match(chart_title("compare", utils::modifyList(s2, list(last_moy = NULL))),
               "^Change from a year earlier in work permits issued, Ontario and Canada, TFWP, 2015 to ")
  expect_equal(chart_title("trend", utils::modifyList(s, list(resolution = "Monthly"))),
               sprintf("Work permits issued, both programs and each program, Canada, Jan 2015 to %s %s, by month", mo, yr))
  expect_equal(chart_title("headline", utils::modifyList(s, list(start = as.Date(paste0(yr, "-01-01"))))),
               sprintf("Work permits issued by program, Canada, %s (Jan\u2013%s)", yr, mo))
  # Change-from-a-year-earlier titles start where the comparison can start
  expect_equal(chart_title("yoy", yoy_title_sel(s, min(d$month))),
               sprintf("Change from a year earlier in work permits issued, by program, Canada, 2016 to %d (full years) and %s (Jan\u2013%s)", as.integer(yr) - 1L, yr, mo))
  expect_equal(yoy_title_sel(utils::modifyList(s, list(start = as.Date("2018-03-01"))), min(d$month))$start, as.Date("2018-03-01"))
  expect_equal(chart_title("headline", utils::modifyList(s, list(end = as.Date("2024-12-01")))),
               "Work permits issued by program, Canada, 2015 to 2024")
})

test_that("annual period text says which years are full and which are part years", {
  j <- function(a, b) period_text(as.Date(a), as.Date(b), "Annual")
  expect_equal(j("2015-01-01", "2026-07-01"), "2015 to 2025 (full years) and 2026 (Jan\u2013Jul)")
  expect_equal(j("2015-01-01", "2024-12-01"), "2015 to 2024")
  expect_equal(j("2025-01-01", "2026-07-01"), "2025 (full year) and 2026 (Jan\u2013Jul)")
  expect_equal(j("2025-03-01", "2026-07-01"), "2025 (Mar\u2013Dec) and 2026 (Jan\u2013Jul)")
  expect_equal(j("2019-03-01", "2026-07-01"), "2019 (Mar\u2013Dec), 2020 to 2025 (full years) and 2026 (Jan\u2013Jul)")
  expect_equal(j("2019-03-01", "2024-12-01"), "2019 (Mar\u2013Dec) and 2020 to 2024 (full years)")
  expect_equal(j("2026-01-01", "2026-07-01"), "2026 (Jan\u2013Jul)")
  # Same months each year and monthly view are unchanged
  expect_equal(period_text(as.Date("2015-01-01"), as.Date("2026-07-01"), "Annual", 7L), "Jan\u2013Jul of each year, 2015 to 2026")
  expect_equal(period_text(as.Date("2015-01-01"), as.Date("2026-07-01"), "Monthly"), "Jan 2015 to Jul 2026, by month")
})

test_that("growth comparison has a short name for the Geography table header", {
  g <- kpi_period_change(filter_selection(d, min(d$month), latest, "Canada", c("TFWP", "IMP")))
  expect_equal(g$short, sprintf("%s\u2013%s vs 2015", fmt_month_short(latest %m-% months(11)), fmt_month_short(latest)))
  g2 <- kpi_period_change(filter_selection(d, as.Date("2026-01-01"), latest, "Canada", c("TFWP", "IMP")))
  expect_equal(g2$short, sprintf("%s vs Jan 2026", fmt_month_short(latest)))
  g3 <- kpi_period_change(filter_selection(d, as.Date("2015-01-01"), as.Date("2016-12-01"), "Canada", "IMP"))
  expect_equal(g3$short, "2016 vs 2015")
})

test_that("chart titles carry no numbers other than dates and no trend wording", {
  base <- list(start = as.Date("2015-01-01"), end = latest, resolution = "Annual",
               geography = "Canada", program_choice = "Both", last_moy = NULL)
  sels <- list(base, utils::modifyList(base, list(geography = "British Columbia", program_choice = "IMP")),
               utils::modifyList(base, list(resolution = "Monthly")),
               utils::modifyList(base, list(last_moy = 7L)))
  kinds <- c("headline", "trend", "yoy", "rank", "table", "compare", "contrib", "mix", "detail", "data")
  tt <- unlist(lapply(sels, function(s) c(
    vapply(kinds, chart_title, "", s = s, label = "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025"),
    chart_title("share", s, prog = "TFWP"), chart_title("share", s, prog = "IMP", as_number = FALSE, known_only = TRUE))))
  expect_no_match(tt, "%")
  # the only numbers are four-digit years
  expect_no_match(gsub("\\b(19|20)[0-9]{2}\\b", "", tt), "[0-9]")
  expect_no_match(tt, "\\b(higher|lower|up|down|rose|fell|grew|increase|decrease|largest category|most|made up|received|went from|was|were)\\b",
                  ignore.case = TRUE)
})

test_that("takeaways say 'so far' only for a true year to date", {
  h <- make_takeaways(d, "2015-01-01", "2025-07-01", "Canada", "Both")      # historical end
  expect_true(any(grepl("^In the selected months of 2025 \\(Jan\u2013Jul\\)", h)))
  expect_false(any(grepl("so far", h)))
  m <- make_takeaways(d, "2026-03-01", latest, "Canada", "Both")            # not from January
  expect_true(any(grepl("^In the selected months of 2026 \\(Mar\u2013", m)))
  # The figure is the selected months vs the same months a year earlier
  cur <- sum(d$value[d$geography == "Canada" & d$month >= as.Date("2025-01-01") & d$month <= as.Date("2025-07-01")])
  expect_true(any(grepl(paste0(fmt_n(cur), " permits were issued"), h, fixed = TRUE)))
})

test_that("chart colours have at least 3:1 contrast with the white background", {
  lum <- function(h) { v <- grDevices::col2rgb(h)[, 1] / 255
    v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v) }
  contrast <- vapply(c(CATEGORY_COLOURS, PROGRAM_COLOURS), function(h) 1.05 / (lum(h) + 0.05), numeric(1))
  expect_true(all(contrast >= 3), info = paste(names(contrast)[contrast < 3], collapse = ", "))
})

test_that("short axis labels for crowded year axes", {
  expect_equal(short_year_label(c("2015", "2026 (Jan\u2013Jul)", "2015 (Mar\u2013Dec)")),
               c("2015", "2026<br>Jan\u2013Jul", "2015<br>Mar\u2013Dec"))
})

test_that("the Unknown-IMP note uses the selected geography and handles edge cases", {
  # Expected numbers computed straight from the data for each geography
  expected <- function(geo) {
    u <- d[d$geography == geo & d$category == "Unknown or other IMP" &
             d$year < as.integer(format(latest, "%Y")), ]
    by_year <- tapply(u$value, u$year, sum)
    list(peak = max(by_year), peak_year = names(by_year)[which.max(by_year)],
         early = sum(by_year[as.integer(names(by_year)) <= 2020]))
  }
  ca <- unknown_imp_note_text(d, "Canada", latest)
  bc <- unknown_imp_note_text(d, "British Columbia", latest)
  e_ca <- expected("Canada"); e_bc <- expected("British Columbia")
  expect_match(ca, "across Canada", fixed = TRUE)
  expect_match(bc, "in British Columbia", fixed = TRUE)
  expect_match(bc, sprintf("a peak of %s in %s", fmt_n(e_bc$peak), e_bc$peak_year), fixed = TRUE)
  expect_match(bc, "does not identify the program", fixed = TRUE)
  expect_no_match(bc, "record", ignore.case = TRUE)                # no claim about recording
  expect_match(ca, sprintf("a peak of %s in %s", fmt_n(e_ca$peak), e_ca$peak_year), fixed = TRUE)
  expect_true(e_bc$peak < e_ca$peak)                       # BC is not Canada's number
  expect_no_match(bc, fmt_n(e_ca$peak), fixed = TRUE)
  expect_match(unknown_imp_note_text(d, NOT_STATED_GEO, latest), "not stated")
  # Edge cases: never NA, -Inf or "n/a" in the sentence
  only_partial <- d[d$year == as.integer(format(latest, "%Y")), ]
  zeros <- d; zeros$value[zeros$category == "Unknown or other IMP"] <- 0
  for (txt in c(unknown_imp_note_text(only_partial, "Yukon", latest),
                unknown_imp_note_text(zeros, "Yukon", latest),
                vapply(c("Canada", PROVINCES_TERRITORIES, NOT_STATED_GEO),
                       function(g) unknown_imp_note_text(d, g, latest), character(1)))) {
    expect_no_match(txt, "NA|Inf|n/a")
  }
  expect_match(unknown_imp_note_text(only_partial, "Yukon", latest), "no full calendar year")
  expect_match(unknown_imp_note_text(zeros, "Yukon", latest), "No IMP permits in Yukon")
  expect_no_match(unknown_imp_note_text(zeros, "Yukon", latest), "growing")
})
