# =============================================================================
# R/calc.R: calculation functions (no Shiny code here, so they can be tested)
# -----------------------------------------------------------------------------
# Every number shown in the dashboard is produced by one of these functions.
# They take the tidy table from load_data() (one row per month x geography x
# program x category) and return small summary tables or single values.
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("dplyr", "tidyr", "lubridate", "scales")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# ---- 1. Formatting helpers --------------------------------------------------

# Whole number with thousands separators, e.g. 12345 -> "12,345".
fmt_n <- function(x) ifelse(is.na(x), "n/a",
                            formatC(round(x), format = "d", big.mark = ","))

# Percentage change with a sign, e.g. 0.123 -> "+12.3%". NA -> "n/a".
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x) | !is.finite(x), "n/a",
         paste0(ifelse(x > 0, "+", ""), formatC(100 * x, format = "f",
                                                 digits = digits), "%"))
}

# Share without a sign, e.g. 0.42 -> "42.0%".
fmt_share <- function(x, digits = 1) {
  # Shares that would round to 0 are shown as "<1%" (or "<0.1%"), not "0%".
  smallest <- 10^(-digits) / 100
  ifelse(is.na(x) | !is.finite(x), "n/a",
         ifelse(x > 0 & x < smallest / 2,
                paste0("<", formatC(100 * smallest, format = "f", digits = digits), "%"),
                paste0(formatC(100 * x, format = "f", digits = digits), "%")))
}

# Month label, e.g. as.Date("2026-07-01") -> "July 2026".
# Dates are built from R's built-in English month.name / month.abb and plain
# numbers, NOT from format codes like "%B", "%p" or "%-d": "%-d"/"%-I" only
# work on Linux/macOS (Windows prints "-d"), and "%B"/"%b"/"%p" follow the
# computer's language setting (e.g. French month names on a French Windows).
fmt_month <- function(d) {
  d <- as.Date(d)
  paste(month.name[as.integer(format(d, "%m"))], format(d, "%Y"))
}
fmt_month_short <- function(d) {
  d <- as.Date(d)
  paste(month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))
}

# Pacific time for a date-time (BC's time zone; the "PT" labels assume it).
to_pacific <- function(t) lubridate::with_tz(as.POSIXct(t), "America/Vancouver")

# Date, e.g. "October 16, 2026" (no leading zero). A date-time is first
# converted to Pacific time so the date matches the "PT" times shown.
fmt_date <- function(t) {
  if (inherits(t, "POSIXt")) t <- to_pacific(t)
  d <- as.Date(format(t, "%Y-%m-%d"))
  paste0(month.name[as.integer(format(d, "%m"))], " ",
         as.integer(format(d, "%d")), ", ", format(d, "%Y"))
}

# Date and time, e.g. "October 16, 2026, 4:50 PM PT" (no leading zeros).
fmt_datetime <- function(t) {
  t <- to_pacific(t)
  h24 <- as.integer(format(t, "%H"))
  h12 <- ifelse(h24 %% 12 == 0, 12L, h24 %% 12)
  paste0(fmt_date(t), ", ", h12, ":", format(t, "%M"), " ",
         ifelse(h24 < 12, "AM", "PM"), " PT")
}

# Words for direction of change, e.g. +0.05 -> "up", -0.05 -> "down".
dir_word <- function(x) ifelse(is.na(x), "unchanged", ifelse(x >= 0, "up", "down"))

# ---- 2. Safe percentage change ----------------------------------------------
# (new - old) / old. Returns NA when the comparison is impossible
# (old is missing or zero), which avoids division-by-zero errors.
pct_change <- function(new, old) {
  ifelse(is.na(old) | is.na(new) | old == 0, NA_real_, new / old - 1)
}

# ---- 3. Selection helpers ---------------------------------------------------

# Program choice in the sidebar -> vector of programs.
programs_from_choice <- function(choice) {
  if (identical(choice, "Both")) c("TFWP", "IMP") else choice
}

# Plain-language program label used in sentences.
program_label <- function(choice) {
  switch(choice,
         TFWP = "TFWP",
         IMP  = "IMP",
         Both = "TFWP and IMP",
         choice)
}

# Month choices for the date-range slider, built from the data itself.
month_choices <- function(data) {
  seq(min(data$month), max(data$month), by = "month")
}

# Keep rows for one geography, the chosen programs and a month range.
# `last_moy` (optional, 1-12): keep only January to that month in every year,
# for the "same months each year" comparison in the annual view.
filter_selection <- function(data, start, end, geography = "Canada",
                             programs = c("TFWP", "IMP"), last_moy = NULL) {
  out <- data |>
    filter(month >= as.Date(start), month <= as.Date(end),
           geography == !!geography, program %in% programs)
  if (!is.null(last_moy)) out <- out[as.integer(format(out$month, "%m")) <= last_moy, ]
  out
}

# The months of year to keep for the "same months each year" option: January
# to the latest month of the latest (partial) year, e.g. 7 for Jan-Jul.
# NULL when the option is off, the view is monthly, or the latest year is full.
same_months_limit <- function(on, resolution, latest_month) {
  m <- as.integer(format(as.Date(latest_month), "%m"))
  if (isTRUE(on) && identical(resolution, "Annual") && m < 12) m else NULL
}

# ---- 4. Period labels for the annual view -----------------------------------
# A year is "full" only if all 12 months are selected. Any other year is a
# part year and its label names the months it covers, e.g. "2026 (Jan-Jul)"
# for the latest year (data to July) or "2019 (Mar-Dec)" for a year cut by the
# slider. Plain month names are used instead of "YTD" or "part", so a part
# year is never read as a full year.
#' Short axis version of a year label, for crowded category axes: the months
#' go on a second line, e.g. "2026 (Jan\u2013Jul)" -> "2026<br>Jan\u2013Jul";
#' full years are unchanged.
short_year_label <- function(label) {
  label <- as.character(label)
  sub("^(\\d{4}) \\((.*)\\)$", "\\1<br>\\2", label)
}

year_label <- function(year, first_m, last_m, latest_month) {
  n_months  <- last_m - first_m + 1
  months_tx <- ifelse(first_m == last_m, month.abb[first_m],
                      paste0(month.abb[first_m], "\u2013", month.abb[last_m]))
  # latest_month is kept as an argument for callers; every part year,
  # including the latest one, is labelled with its months.
  dplyr::if_else(n_months == 12, as.character(year), paste0(year, " (", months_tx, ")"))
}

# ---- 5. Time series at monthly or annual resolution --------------------------
#' @param df filtered data (one geography)
#' @param resolution "Monthly" or "Annual"
#' @param by extra grouping columns, e.g. "program" or c("program","category")
#' @param latest_month latest month in the full data
#' @return tibble with period (Date: first month of the period), label,
#'   value, months (number of months included), partial (TRUE/FALSE) and
#'   masked_cells.
summarise_series <- function(df, resolution = "Monthly", by = "program",
                             latest_month = max(df$month)) {
  if (nrow(df) == 0) {
    return(tibble(period = as.Date(character()), label = character(),
                  value = numeric(), months = integer(),
                  partial = logical(), masked_cells = numeric()))
  }
  if (resolution == "Monthly") {
    df |>
      group_by(period = month, across(all_of(by))) |>
      summarise(value = sum(value), masked_cells = sum(masked_cells),
                .groups = "drop") |>
      mutate(label = fmt_month_short(period), months = 1L, partial = FALSE)
  } else {
    df |>
      group_by(year, across(all_of(by))) |>
      summarise(value = sum(value), masked_cells = sum(masked_cells),
                first_m = min(lubridate::month(month)),
                last_m  = max(lubridate::month(month)),
                .groups = "drop") |>
      mutate(period  = lubridate::make_date(year, 1, 1),
             months  = as.integer(last_m - first_m + 1),
             partial = months < 12,
             label   = year_label(year, first_m, last_m, latest_month)) |>
      # When every year shown covers the same months (e.g. Jan-Jul of every
      # year, the "same months each year" option), the years are comparable,
      # so none is marked as a part-year.
      mutate(partial = if (dplyr::n_distinct(year) > 1 && dplyr::n_distinct(first_m) == 1 &&
                           dplyr::n_distinct(last_m) == 1) FALSE else partial) |>
      select(-first_m, -last_m)
  }
}

# ---- 6. Year-over-year change -----------------------------------------------
# Monthly: each month vs the same month one year earlier.
# Annual: each year vs the SAME MONTHS of the previous year. So a full year
# is compared with the full previous year, and 2026 (Jan-Jul) is
# compared with Jan-Jul 2025, never with all of 2025.
#' @param full_df data for one geography and the chosen programs, ALL months
#'   (so the year before the slider start is available for comparison)
yoy_series <- function(full_df, start, end, resolution = "Monthly",
                       by = "program", latest_month = max(full_df$month)) {
  start <- as.Date(start); end <- as.Date(end)
  monthly <- full_df |>
    group_by(month, across(all_of(by))) |>
    summarise(value = sum(value), .groups = "drop")
  prev <- monthly |>
    mutate(month = month %m+% months(12)) |>      # shift last year forward
    rename(prev_value = value)
  joined <- monthly |>
    left_join(prev, by = c("month", by)) |>
    filter(month >= start, month <= end)
  # A comparison is only valid if the year-earlier month exists in the data.
  first_month <- min(full_df$month)
  joined <- joined |>
    mutate(prev_value = ifelse(month %m-% months(12) < first_month,
                               NA_real_, prev_value))
  if (resolution == "Monthly") {
    joined |>
      mutate(period = month, label = fmt_month_short(month),
             yoy = pct_change(value, prev_value)) |>
      select(period, label, all_of(by), value, prev_value, yoy)
  } else {
    joined |>
      mutate(year = lubridate::year(month)) |>
      group_by(year, across(all_of(by))) |>
      summarise(value = sum(value),
                prev_value = if (any(is.na(prev_value))) NA_real_ else sum(prev_value),
                first_m = min(lubridate::month(month)),
                last_m  = max(lubridate::month(month)), .groups = "drop") |>
      mutate(period = lubridate::make_date(year, 1, 1),
             label  = year_label(year, first_m, last_m, latest_month),
             yoy    = pct_change(value, prev_value)) |>
      select(period, label, all_of(by), value, prev_value, yoy)
  }
}

# ---- 7. Key numbers (Overview cards) ----------------------------------------

#' Total permits in the selection.
kpi_total <- function(df) sum(df$value)

#' TFWP vs IMP split for the selected place and period (both programs,
#' regardless of the program filter, so the split is always meaningful).
kpi_split <- function(data, start, end, geography) {
  d <- filter_selection(data, start, end, geography, c("TFWP", "IMP"))
  tot <- sum(d$value)
  tibble(program = c("TFWP", "IMP")) |>
    left_join(d |> group_by(program) |> summarise(value = sum(value)),
              by = "program") |>
    mutate(value = coalesce(value, 0),
           share = if (tot > 0) value / tot else NA_real_)
}

#' Latest month in the selected range and its change from a year earlier.
kpi_latest <- function(data, end, geography, programs) {
  end <- as.Date(end)
  cur  <- filter_selection(data, end, end, geography, programs)
  prev_month <- end %m-% months(12)
  prev <- filter_selection(data, prev_month, prev_month, geography, programs)
  has_prev <- prev_month >= min(data$month)
  list(month = end, value = sum(cur$value),
       prev_month = prev_month,
       prev_value = if (has_prev) sum(prev$value) else NA_real_,
       change = if (has_prev) pct_change(sum(cur$value), sum(prev$value)) else NA_real_)
}

#' Latest full calendar year up to the end of the selection (Overview card 1).
#' E.g. a selection ending July 2026 gives 2025; one ending December 2024
#' gives 2024. Returns the year, its total and the change from the year before.
kpi_full_year <- function(data, end, geography, programs) {
  end <- as.Date(end)
  yr <- as.integer(format(end, "%Y")) - if (format(end, "%m") == "12") 0L else 1L
  first_year <- as.integer(format(min(data$month), "%Y")) +
    if (format(min(data$month), "%m") == "01") 0L else 1L
  if (yr < first_year) return(list(year = NA_integer_, value = NA_real_, prev_value = NA_real_, change = NA_real_))
  tot <- function(y) sum(filter_selection(data, lubridate::make_date(y, 1, 1),
                                          lubridate::make_date(y, 12, 1), geography, programs)$value)
  v <- tot(yr); pv <- if (yr - 1 >= first_year) tot(yr - 1) else NA_real_
  list(year = yr, value = v, prev_value = pv, change = pct_change(v, pv))
}

#' Year to date at the end of the selection vs the same months a year
#' earlier (Overview card 2), e.g. Jan-Jul 2026 vs Jan-Jul 2025. When the
#' selection ends in December this is the full year vs the year before.
kpi_ytd_compare <- function(data, end, geography, programs) {
  end <- as.Date(end)
  yr <- as.integer(format(end, "%Y")); m <- as.integer(format(end, "%m"))
  cur_start <- lubridate::make_date(yr, 1, 1); prev_start <- lubridate::make_date(yr - 1, 1, 1)
  prev_end <- lubridate::make_date(yr - 1, m, 1)
  months_tx <- if (m == 1) "Jan" else paste0("Jan\u2013", month.abb[m])
  cur <- sum(filter_selection(data, cur_start, end, geography, programs)$value)
  has_prev <- prev_start >= min(data$month)
  prev <- if (has_prev) sum(filter_selection(data, prev_start, prev_end, geography, programs)$value) else NA_real_
  list(year = yr, months = months_tx, value = cur, prev_value = prev,
       change = pct_change(cur, prev),
       label = if (m == 12) sprintf("%d vs %d", yr, yr - 1)
               else sprintf("%s %d vs %s %d", months_tx, yr, months_tx, yr - 1))
}

#' The four Overview cards, as plain text (title, value, note), for the
#' selection's geography and program and the LAST month of the selected
#' period (by default the latest month IRCC has published, e.g. July 2026):
#'   1. permits issued from January to that month (year to date)
#'   2. change from the same months a year earlier
#'   3. IMP / TFWP split over the same months (with one program: its share)
#'   4. that single month, with the change from the same month a year earlier
#' All four cover the same months, so they can be read together; the intro
#' line names the place, the programs and the months.
overview_cards <- function(data, end, geography, program_choice) {
  programs <- programs_from_choice(program_choice)
  end <- as.Date(end)
  yr <- as.integer(format(end, "%Y")); m <- as.integer(format(end, "%m"))
  is_latest <- end == max(data$month)
  ytd <- kpi_ytd_compare(data, end, geography, programs)
  months_long <- if (m == 1) "January" else paste("January to", month.name[m])
  period <- if (m == 12) as.character(yr) else paste(ytd$months, yr)            # "Jan\u2013Jul 2026"
  prev_period <- if (m == 12) as.character(yr - 1) else paste(ytd$months, yr - 1)
  sp <- kpi_split(data, lubridate::make_date(yr, 1, 1), end, geography)
  lt <- kpi_latest(data, end, geography, programs)
  c1 <- list(
    title = if (m == 12) sprintf("Work permits issued in %d", yr) else sprintf("Work permits issued, %s", period),
    value = fmt_n(ytd$value),
    note  = if (m == 12) "Full calendar year."
            else if (is_latest) sprintf("%d so far: %s (latest month published).", yr, months_long)
            else sprintf("%s %d.", months_long, yr))
  c2 <- list(
    title = if (m == 12) sprintf("Change from %d", yr - 1) else sprintf("Change from the same months of %d", yr - 1),
    value = fmt_pct(ytd$change),
    note  = if (is.na(ytd$prev_value)) "No data for the same months a year earlier."
            else sprintf("%s in %s, compared with %s in %s.", fmt_n(ytd$value), period,
                         fmt_n(ytd$prev_value), prev_period))
  c3 <- if (length(programs) == 2) list(
    title = sprintf("IMP / TFWP split, %s", period),
    value = sprintf("%s / %s", fmt_share(sp$share[sp$program == "IMP"], 0), fmt_share(sp$share[sp$program == "TFWP"], 0)),
    note  = sprintf("IMP %s \u00b7 TFWP %s permits.", fmt_n(sp$value[sp$program == "IMP"]),
                    fmt_n(sp$value[sp$program == "TFWP"])))
  else list(
    title = sprintf("%s share of all work permits, %s", programs, period),
    value = fmt_share(sp$share[sp$program == programs], 0),
    note  = sprintf("%s %s of %s permits (TFWP and IMP).", programs,
                    fmt_n(sp$value[sp$program == programs]), fmt_n(sum(sp$value))))
  c4 <- list(
    title = sprintf("%s: %s", if (is_latest) "Latest month" else "Last month selected", fmt_month(end)),
    value = fmt_n(lt$value),
    note  = if (is.na(lt$change)) "No data for the same month a year earlier."
            else sprintf("%s from %s (%s).", fmt_pct(lt$change), fmt_month(lt$prev_month), fmt_n(lt$prev_value)))
  intro <- sprintf("Key numbers for %s (%s), %s %d. They follow the Geography and Program filters and the last month of the Period.",
                   geography, program_label(program_choice), months_long, yr)
  list(intro = intro, cards = list(c1, c2, c3, c4))
}

#' Change over the selected period.
#' - 24 months or more: total of the LAST 12 months vs the FIRST 12 months
#'   of the range (12-month blocks remove seasonal swings).
#' - 2 to 23 months: last month vs first month (seasonal swings possible).
#' - 1 month: not defined (NA).
kpi_period_change <- function(df) {
  m <- df |> group_by(month) |> summarise(value = sum(value), .groups = "drop") |>
    arrange(month)
  n <- nrow(m)
  if (n >= 24) {
    first <- sum(m$value[1:12]); last <- sum(m$value[(n - 11):n])
    # Short name of each 12-month block for a column header: "2015" when it
    # is a calendar year, otherwise "Aug 2025\u2013Jul 2026"
    blk <- function(a, b) if (format(a, "%m") == "01" && format(a, "%Y") == format(b, "%Y")) format(a, "%Y")
                          else paste0(fmt_month_short(a), "\u2013", fmt_month_short(b))
    list(change = pct_change(last, first), method = "12m",
         label = "Last 12 months vs first 12 months",
         short = sprintf("%s vs %s", blk(m$month[n - 11], m$month[n]), blk(m$month[1], m$month[12])),
         first_value = first, last_value = last,
         text = sprintf("the last 12 months (%s\u2013%s) compared with the first 12 months (%s\u2013%s) of the selected period",
                        fmt_month_short(m$month[n - 11]), fmt_month_short(m$month[n]),
                        fmt_month_short(m$month[1]), fmt_month_short(m$month[12])))
  } else if (n >= 2) {
    list(change = pct_change(m$value[n], m$value[1]), method = "month",
         label = sprintf("%s vs %s", fmt_month_short(m$month[n]), fmt_month_short(m$month[1])),
         short = sprintf("%s vs %s", fmt_month_short(m$month[n]), fmt_month_short(m$month[1])),
         first_value = m$value[1], last_value = m$value[n],
         text = sprintf("%s compared with %s (single months, so seasonal ups and downs show)",
                        fmt_month_short(m$month[n]), fmt_month_short(m$month[1])))
  } else {
    list(change = NA_real_, method = "none", label = "Change over the period", short = "",
         first_value = NA, last_value = NA,
         text = "select at least two months to see a change")
  }
}

# ---- 8. Geography -----------------------------------------------------------

#' Ranking of the 13 provinces and territories for the selected period.
#' Share of Canada uses the Canada total, which includes "not stated".
#' Growth uses the same rule as kpi_period_change().
geo_ranking <- function(data, start, end, programs) {
  d <- data |> filter(month >= as.Date(start), month <= as.Date(end),
                      program %in% programs)
  canada_total <- sum(d$value[d$geography == "Canada"])
  pts <- d |> filter(geo_type == "province/territory")
  growth <- pts |> group_by(geography) |>
    group_modify(~ tibble(growth = kpi_period_change(.x)$change)) |>
    ungroup()
  pts |>
    group_by(geography) |>
    summarise(value = sum(value), masked_cells = sum(masked_cells),
              .groups = "drop") |>
    left_join(growth, by = "geography") |>
    mutate(share = if (canada_total > 0) value / canada_total else NA_real_,
           geography = as.character(geography)) |>
    arrange(desc(value)) |>
    mutate(rank = row_number())
}

#' Contribution to change (Geography tab): each province or territory's part
#' of Canada's change in the latest year to date compared with the same months
#' a year earlier (e.g. Jan-Jul 2026 vs Jan-Jul 2025), for the selected programs.
#' contribution = province change / Canada change. Canada includes permits with
#' no province/territory stated, which are not listed, so the parts add up to
#' slightly less (or more) than 100%. Uses kpi_ytd_compare(), the same rule as
#' the Overview year-to-date card.
#' @return one row per province/territory (largest contribution first), with
#'   attributes `label` (period wording) and `canada_change`.
contribution_to_change <- function(data, latest_month, programs) {
  geos <- unique(as.character(data$geography[data$geo_type == "province/territory"]))
  ca <- kpi_ytd_compare(data, latest_month, "Canada", programs)
  ca_change <- ca$value - ca$prev_value
  out <- do.call(rbind, lapply(geos, function(g) {
    k <- kpi_ytd_compare(data, latest_month, g, programs)
    data.frame(geography = g, value = k$value, prev_value = k$prev_value,
               change = k$value - k$prev_value, stringsAsFactors = FALSE)
  }))
  # Undefined when Canada did not change (or there is no year-earlier period)
  out$contribution <- if (isTRUE(ca_change != 0)) out$change / ca_change else NA_real_
  out <- out[order(-out$contribution, -out$change), ]
  rownames(out) <- NULL
  attr(out, "label") <- ca$label
  attr(out, "canada_change") <- ca_change
  out
}

#' Share of Canada's permits over time for the largest provinces/territories
#' (used on the Geography tab when "Canada" is selected).
#' @param top how many provinces/territories to show (largest in the period)
#' @return one row per period and place: value, canada (Canada total), share
share_over_time <- function(data, start, end, programs, resolution = "Annual",
                            top = 5, latest_month = max(data$month)) {
  d <- data |> filter(month >= as.Date(start), month <= as.Date(end),
                      program %in% programs)
  # The largest provinces/territories in the period, by total permits.
  leaders <- geo_ranking(data, start, end, programs)$geography[seq_len(top)]
  canada <- summarise_series(d |> filter(geography == "Canada"), resolution,
                             by = character(0), latest_month = latest_month) |>
    select(period, canada = value)
  summarise_series(d |> filter(geography %in% leaders) |>
                     mutate(place = as.character(geography)),
                   resolution, by = "place", latest_month = latest_month) |>
    left_join(canada, by = "period") |>
    # Share is undefined when Canada has no permits in that period.
    mutate(share = if_else(canada > 0, value / canada, NA_real_),
           place = factor(place, levels = leaders)) |>
    arrange(place, period)
}

#' Comparison of one province/territory with Canada (annual or monthly):
#' its share of Canada and the year-over-year change of both.
compare_with_canada <- function(data, start, end, geography, programs,
                                latest_month = max(data$month),
                                resolution = "Annual") {
  full <- data |> filter(program %in% programs,
                         geography %in% c(!!geography, "Canada"))
  yoy <- yoy_series(full |> mutate(place = as.character(geography)),
                    start, end, resolution, by = "place",
                    latest_month = latest_month)
  wide <- yoy |>
    select(period, label, place, value, yoy) |>
    pivot_wider(names_from = place, values_from = c(value, yoy))
  names(wide) <- sub(paste0("_", geography, "$"), "_pt", names(wide), fixed = FALSE)
  names(wide) <- sub("_Canada$", "_canada", names(wide))
  wide |> mutate(share = ifelse(value_canada > 0, value_pt / value_canada, NA_real_))
}

# ---- 9. Composition -----------------------------------------------------------

#' Category shares within each period (for stacked 100% bars).
#' known_only = TRUE leaves out permits whose program IRCC does not identify
#' ("Unknown or other IMP"), so shares are of permits with a known program.
composition_shares <- function(df, resolution = "Annual",
                               latest_month = max(df$month), known_only = FALSE) {
  if (known_only) df <- df[df$category != "Unknown or other IMP", ]
  summarise_series(df, resolution, by = c("program", "category"),
                   latest_month = latest_month) |>
    group_by(period, program) |>
    mutate(share = if (sum(value) > 0) value / sum(value) else NA_real_) |>
    ungroup()
}

#' IRCC's own program names inside each grouped category, with permits, for
#' the Composition detail table. `detail` is the cell-level table from
#' load_data() (provinces, territories and "not stated"; Canada = all of them).
#' The name shown is the most detailed IRCC level given ("Program not stated"
#' levels are skipped).
category_detail_table <- function(detail, start, end, geography, programs) {
  d <- detail[detail$month >= as.Date(start) & detail$month <= as.Date(end) &
                detail$program %in% programs, ]
  if (geography != "Canada") d <- d[d$geography == geography, ]
  # Most detailed level that is given (vectorised: level 5, else 4, 3, 2)
  given <- function(x) !is.na(x) & nzchar(x) & x != "Program not stated"
  d$ircc_name <- ifelse(given(d$level_5), d$level_5, ifelse(given(d$level_4), d$level_4,
                   ifelse(given(d$level_3), d$level_3, ifelse(given(d$level_2), d$level_2, "Program not stated"))))
  out <- stats::aggregate(value ~ program + category + ircc_name, data = d, FUN = sum)
  out <- out[order(out$program, out$category, -out$value), ]
  data.frame(Program = out$program, Category = as.character(out$category),
             `IRCC program name` = out$ircc_name, Permits = out$value,
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' Category totals for the whole selected period, largest first.
composition_period <- function(df) {
  df |> group_by(program, category) |>
    summarise(value = sum(value), masked_cells = sum(masked_cells),
              .groups = "drop") |>
    group_by(program) |>
    mutate(share = if (sum(value) > 0) value / sum(value) else NA_real_) |>
    ungroup() |> arrange(program, desc(value))
}

# ---- 10. Data table for display/download ------------------------------------
#' Flag figures that suppression could make noticeably too low.
#' Each suppressed IRCC cell ("--") hides a true value of 0 to 5 that is
#' counted as 0, so the true figure could be up to 5 x masked_cells higher.
#' Flag when that possible gap is more than `threshold` (default 10%) of the
#' figure shown (or when the figure is 0 but some cells were suppressed).
undercount_flag <- function(value, masked_cells, threshold = 0.10) {
  masked_cells > 0 & (5 * masked_cells > threshold * value)
}

table_for_download <- function(df, resolution = "Monthly",
                               latest_month = max(df$month), geography = "") {
  summarise_series(df, resolution, by = c("program", "category"),
                   latest_month = latest_month) |>
    arrange(period, program, category) |>          # chronological order
    transmute(Period = label,
              Geography = geography,
              Program = program,
              Category = as.character(category),
              `Work permits issued` = value,
              `Months included` = months,
              `Hidden small counts (counted as 0)` = masked_cells,
              # "Yes" when suppression could understate the figure by over 10%
              `Possible undercount over 10%` = if_else(undercount_flag(value, masked_cells),
                                                       "Yes", ""))
}

# ---- 11. Plain-language takeaways (Overview) --------------------------------
#' A five-point story that updates with the filters: scale, the yearly
#' trend (peak and change since), the most recent months, the mix, and where.
make_takeaways <- function(data, start, end, geography, program_choice) {
  programs <- programs_from_choice(program_choice)
  start <- as.Date(start); end <- as.Date(end)
  sel <- filter_selection(data, start, end, geography, programs)
  if (nrow(sel) == 0 || sum(sel$value) == 0) {
    return(sprintf("No %s work permits were issued %s in the selected period.",
                   program_label(program_choice), geo_phrase(geography)))
  }
  out <- character()
  n_months <- length(unique(sel$month))

  # 1. Scale: the total and the monthly average
  out <- c(out, sprintf("%s %s work permits were issued %s from %s to %s, an average of %s a month. These are permits, not people: one person can get more than one permit.",
                        fmt_n(sum(sel$value)), program_label(program_choice), geo_phrase(geography),
                        fmt_month(start), fmt_month(end), fmt_n(round(sum(sel$value) / n_months))))
  n_masked <- sum(sel$masked_cells)
  if (n_masked > 0 && n_masked * 5 >= 0.05 * sum(sel$value)) {
    out[1] <- paste0(out[1], sprintf(" Many figures here are small and suppressed for privacy, so the true total could be up to %s higher.",
                                     fmt_n(5 * n_masked)))
  }

  # 2. Trend over full calendar years: start, peak and (if any) the fall since
  ann <- summarise_series(sel, "Annual", by = character(0),
                          latest_month = max(data$month)) |> filter(!partial)
  if (nrow(ann) >= 2) {
    first <- ann[1, ]; last <- ann[nrow(ann), ]; pk <- ann[which.max(ann$value), ]
    if (pk$label == last$label) {
      out <- c(out, sprintf("Yearly totals grew from %s in %s to %s in %s, the highest full year selected.",
                            fmt_n(first$value), first$label, fmt_n(last$value), last$label))
    } else if (pk$label == first$label) {
      out <- c(out, sprintf("Yearly totals were highest in %s (%s) and were %s lower by %s (%s).",
                            first$label, fmt_n(first$value),
                            sub("^-", "", fmt_pct(pct_change(last$value, first$value), 0)),
                            last$label, fmt_n(last$value)))
    } else {
      out <- c(out, sprintf("Yearly totals grew from %s in %s to a peak of %s in %s, then %s %s to %s in %s.",
                            fmt_n(first$value), first$label, fmt_n(pk$value), pk$label,
                            if (last$value < pk$value) "fell" else "rose",
                            sub("^[+-]", "", fmt_pct(pct_change(last$value, pk$value), 0)),
                            fmt_n(last$value), last$label))
    }
  }

  # 3. Most recent: the selected months of the last year vs the same months a
  #    year earlier (if the range ends part-way through a year; called "so far"
  #    only for a true year to date), otherwise the last month vs a year earlier
  end_year <- as.integer(format(end, "%Y"))
  ytd_months <- sort(unique(sel$month[sel$year == end_year]))
  prev_months <- ytd_months %m-% months(12)
  if (length(ytd_months) < 12 && length(ytd_months) >= 2 && min(prev_months) >= min(data$month)) {
    cur <- sum(sel$value[sel$year == end_year])
    prev <- sum(filter_selection(data, min(prev_months), max(prev_months), geography, programs)$value)
    ch <- pct_change(cur, prev)
    months_tx <- sprintf("%s\u2013%s", month.abb[as.integer(format(min(ytd_months), "%m"))],
                         month.abb[as.integer(format(max(ytd_months), "%m"))])
    # "So far" only when the range runs from January to the latest month IRCC
    # has published (a true year to date); otherwise name the selected months.
    is_ytd <- end == max(data$month) && format(min(ytd_months), "%m") == "01"
    lead <- if (is_ytd) sprintf("In %s so far (%s)", end_year, months_tx)
            else sprintf("In the selected months of %s (%s)", end_year, months_tx)
    out <- c(out, sprintf("%s, %s permits were issued, %s %s on the same months of %s.",
                          lead, fmt_n(cur), dir_word(ch), sub("^[+-]", "", fmt_pct(ch)),
                          end_year - 1))
  } else {
    lt <- kpi_latest(data, end, geography, programs)
    if (!is.na(lt$change)) {
      out <- c(out, sprintf("In %s, %s permits were issued, %s %s from %s.",
                            fmt_month(lt$month), fmt_n(lt$value), dir_word(lt$change),
                            sub("^[+-]", "", fmt_pct(lt$change)), fmt_month(lt$prev_month)))
    }
  }

  # 4. Mix: program split and the largest category
  cp <- composition_period(sel) |> arrange(desc(value))
  top_cat <- sprintf("%s%s", as.character(cp$category[1]),
                     if (length(programs) == 2) paste0(" (", cp$program[1], ")") else "")
  if (length(programs) == 2) {
    sp <- kpi_split(data, start, end, geography)
    out <- c(out, sprintf("The International Mobility Program (IMP) accounted for %s of permits and the Temporary Foreign Worker Program (TFWP) for %s; the largest single category was %s, at %s of all permits.",
                          fmt_share(sp$share[sp$program == "IMP"], 0),
                          fmt_share(sp$share[sp$program == "TFWP"], 0),
                          top_cat, fmt_share(cp$value[1] / sum(cp$value), 0)))
  } else {
    out <- c(out, sprintf("The largest category was %s, at %s of %s permits.",
                          top_cat, fmt_share(cp$value[1] / sum(cp$value), 0), program_choice))
  }

  # 5. Where: concentration (Canada) or the selected province's share and rank
  rk <- geo_ranking(data, start, end, programs)
  if (geography == "Canada") {
    out <- c(out, sprintf("%s, %s and %s together received %s of Canada's permits.",
                          rk$geography[1], rk$geography[2], rk$geography[3],
                          fmt_share(sum(rk$share[1:3]), 0)))
  } else if (geography %in% rk$geography) {
    r <- rk[rk$geography == geography, ]
    out <- c(out, sprintf("%s received %s of Canada's %s permits in the period, ranking %s of 13 provinces and territories.",
                          geography, fmt_share(r$share), program_label(program_choice), r$rank))
  }
  unname(out[!is.na(out)])
}

# ---- 12. Descriptive figure titles --------------------------------------------
# Every chart and table title names WHAT is shown (measure, breakdown, place,
# program and period), never the trend: no numbers and no "higher", "up",
# "largest" etc. The point of each chart is told in the "What the data show"
# panel and the source lines, not in the title.

#' Months as text: "Jan 2015 to Jul 2026" (or "Jul 2026" for one month).
range_text <- function(start, end) {
  if (format(as.Date(start), "%Y-%m") == format(as.Date(end), "%Y-%m")) return(fmt_month_short(start))
  paste(fmt_month_short(start), "to", fmt_month_short(end))
}

#' Period as shown on a chart, following the Annual/Monthly view:
#'   Annual             "2015 to 2025 (full years) and 2026 (Jan\u2013Jul)"
#'   Annual, same months "Jan\u2013Jul of each year, 2015 to 2026"
#'   Monthly            "Jan 2015 to Jul 2026, by month"
period_text <- function(start, end, resolution = "Annual", last_moy = NULL) {
  start <- as.Date(start); end <- as.Date(end)
  sy <- as.integer(format(start, "%Y")); ey <- as.integer(format(end, "%Y"))
  sm <- as.integer(format(start, "%m")); em <- as.integer(format(end, "%m"))
  if (resolution != "Annual") return(paste0(range_text(start, end), ", by month"))
  if (!is.null(last_moy)) {
    mtx <- if (last_moy == 1) "Jan" else paste0("Jan\u2013", month.abb[last_moy])
    return(if (sy == ey) paste(mtx, sy) else sprintf("%s of each year, %d to %d", mtx, sy, ey))
  }
  if (sy == ey) return(year_label(sy, sm, em, end))
  # Say which years are full and which are part years, so "2015 to 2026
  # (Jan-Jul)" is not read as Jan-Jul of every year:
  #   "2015 to 2025 (full years) and 2026 (Jan-Jul)"
  first_full <- sm == 1; last_full <- em == 12
  if (first_full && last_full) return(paste(sy, "to", ey))
  fy <- if (first_full) sy else sy + 1L        # first full year
  ly <- if (last_full) ey else ey - 1L         # last full year
  parts <- c(if (!first_full) year_label(sy, sm, 12, end),
             if (fy == ly) paste0(fy, " (full year)")
             else if (fy < ly) sprintf("%d to %d (full years)", fy, ly),
             if (!last_full) year_label(ey, 1, em, end))
  if (length(parts) == 2) paste(parts[1], "and", parts[2])
  else paste0(parts[1], ", ", parts[2], " and ", parts[3])
}

#' The same-months period with full month names, for the Overview headline
#' chart, whose bars are labelled with plain years in this view:
#'   "January\u2013July of each year, 2015 to 2026" (or "January 2026" for one year).
same_months_period_long <- function(start, end, last_moy) {
  sy <- as.integer(format(as.Date(start), "%Y")); ey <- as.integer(format(as.Date(end), "%Y"))
  mtx <- if (last_moy == 1) "January" else paste0("January\u2013", month.name[last_moy])
  if (sy == ey) paste(mtx, sy) else sprintf("%s of each year, %d to %d", mtx, sy, ey)
}

#' Change-from-a-year-earlier charts start one year after the data do (the
#' first year has nothing to compare with), so their titles start there too.
#' Returns the selection with `start` moved to that month when needed.
yoy_title_sel <- function(s, first_month) {
  first_yoy <- seq(as.Date(first_month), by = "12 months", length.out = 2)[2]
  if (as.Date(s$start) < first_yoy) s$start <- first_yoy
  s
}

#' Title for every chart and table, from the sidebar selection `s` (start,
#' end, resolution, geography, program_choice, last_moy).
#'   kind  "headline" (Overview), "trend", "yoy" (Trends), "rank", "table",
#'         "compare", "contrib" (Geography), "share" (Composition, with
#'         `prog`, `as_number`, `known_only`), "mix", "detail", "data"
#'   label for "contrib": the months compared, e.g. "Jan\u2013Jul 2026 vs Jan\u2013Jul 2025"
chart_title <- function(kind, s, prog = NULL, as_number = TRUE, known_only = FALSE, label = NULL) {
  geo <- s$geography; pl <- program_label(s$program_choice)
  both <- identical(s$program_choice, "Both")
  per <- period_text(s$start, s$end, s$resolution, s$last_moy)
  rng <- range_text(s$start, s$end)
  permits <- if (both) "Work permits issued" else paste(s$program_choice, "work permits issued")
  switch(kind,
    # Same months each year: the bars carry plain years, so the title names
    # the months in full, e.g. "January\u2013July of each year, 2015 to 2026".
    headline = { if (identical(s$resolution, "Annual") && !is.null(s$last_moy)) per <- same_months_period_long(s$start, s$end, s$last_moy)
                 if (both) sprintf("Work permits issued by program, %s, %s", geo, per)
                 else sprintf("%s, %s, %s", permits, geo, per) },
    trend    = if (both) sprintf("Work permits issued, both programs and each program, %s, %s", geo, per)
               else sprintf("%s, %s, %s", permits, geo, per),
    yoy      = if (both) sprintf("Change from a year earlier in work permits issued, by program, %s, %s", geo, per)
               else sprintf("Change from a year earlier in %s work permits issued, %s, %s", s$program_choice, geo, per),
    rank     = sprintf("Work permits issued by province and territory, %s, %s", pl, rng),
    table    = sprintf("Work permits issued, share of Canada and growth, by province and territory, %s, %s", pl, rng),
    compare  = if (geo == "Canada") sprintf("Share of Canada's work permits issued, five largest provinces, %s, %s", pl, per)
               else sprintf("Change from a year earlier in work permits issued, %s and Canada, %s, %s", geo, pl, per),
    contrib  = sprintf("Share of Canada's change in work permits issued, by province and territory, %s, %s", pl, label),
    share    = sprintf("%s%s work permits issued by category%s, %s, %s",
                       if (as_number) "" else "Share of ", prog,
                       if (!as_number && known_only) " (Unknown IMP left out)" else "", geo, per),
    mix      = sprintf("Work permits issued by category and program, %s, %s, %s", geo, pl, rng),
    detail   = sprintf("IRCC program names in each category, %s, %s", geo, rng),
    data     = sprintf("Work permits issued by %s, program and category, %s, %s, %s",
                       if (s$resolution == "Annual") "year" else "month", geo, pl, rng),
    stop("unknown title kind: ", kind))
}

# -----------------------------------------------------------------------------
# Composition tab note on "Unknown International Mobility Program" permits,
# computed for the geography selected in the sidebar (full calendar years only,
# so the partial latest year does not look like a drop).
#   data          the tidy data (one row per month x geography x program x category)
#   geography     the selected geography, e.g. "British Columbia" or "Canada"
#   latest_month  the latest month in the data (its year is partial)
# Returns one sentence. Suppressed IRCC cells ("--", 0 to 5 permits) are
# counted as 0, as everywhere else in the dashboard, and the note says so.
# -----------------------------------------------------------------------------
geo_phrase <- function(geography) {
  if (geography == "Canada") return("across Canada")
  if (geography == "Province/territory not stated") return("where the province or territory was not stated")
  paste("in", geography)
}

unknown_imp_note_text <- function(data, geography, latest_month) {
  where <- geo_phrase(geography)
  # IMP rows for the selected geography, full calendar years only
  imp <- data[data$geography == geography & data$program == "IMP", ]
  latest_year <- as.integer(format(latest_month, "%Y"))
  imp <- imp[imp$year != latest_year, ]
  lead <- "For some IMP permits IRCC does not identify the program (IRCC's label: \"Unknown International Mobility Program\"; grey in the IMP chart)"
  if (nrow(imp) == 0) {
    return(sprintf("%s; there is no full calendar year of data %s to count them. ", lead, where))
  }
  is_u <- imp$category == "Unknown or other IMP"
  u_by   <- tapply(imp$value * is_u, imp$year, sum)       # unknown-program permits per year
  all_by <- tapply(imp$value, imp$year, sum)              # all IMP permits per year
  masked <- any(imp$any_masked[is_u])                     # any suppressed cell used?
  mask_note <- if (masked) " IRCC's hidden small counts (0 to 5) are counted as 0." else ""
  yrs <- names(u_by)
  if (all(u_by == 0)) {
    return(sprintf("No IMP permits %s are in IRCC's \"Unknown International Mobility Program\" group in %s\u2013%s.%s ",
                   where, yrs[1], yrs[length(yrs)], mask_note))
  }
  # Years from the first year in which they were at least 1% of IMP permits
  # to the last full year (tiny early counts are described, not ranged)
  share_all <- ifelse(all_by > 0, u_by / all_by, 0)
  first_u <- which(share_all >= 0.01)[1]
  if (is.na(first_u)) first_u <- which(u_by > 0)[1]
  span <- first_u:length(u_by)
  share <- share_all[span]
  none_before <- if (first_u == 1) sprintf("%s, in ", sub("^i", "I", sub("^a", "A", where)))
    else if (all(u_by[seq_len(first_u - 1)] == 0)) sprintf("There were none %s in %s\u2013%s; in ", where, yrs[1], yrs[first_u - 1])
    else sprintf("They were under 1%% of IMP permits %s in %s\u2013%s; in ", where, yrs[1], yrs[first_u - 1])
  peak <- yrs[which.max(u_by)]
  range_tx <- if (length(span) > 1) sprintf("%s\u2013%s they were %s to %s of IMP permits each year",
                                            yrs[first_u], yrs[length(yrs)], fmt_share(min(share)), fmt_share(max(share)))
              else sprintf("%s they were %s of IMP permits", yrs[first_u], fmt_share(share[1]))
  sprintf("%s. %s%s (a peak of %s in %s).%s To see the mix without them, tick \"IMP shares: count only permits with a known category\" above the charts. ",
          lead, none_before, range_tx, fmt_n(max(u_by)), peak, mask_note)
}

# ---- 13. "What the data show": fixed points for the release --------------------
#' Three or four short, neutral points for Canada, computed from the data and
#' dated to the release (Overview). A point is only included when the data
#' support it; each number is printed exactly as computed here.
#' @return character vector of sentences
release_highlights <- function(data, latest_month) {
  latest_month <- as.Date(latest_month)
  ca <- data[data$geography == "Canada", ]
  ly <- as.integer(format(latest_month, "%Y")); lm <- as.integer(format(latest_month, "%m"))
  months_tx <- if (lm == 1) "January" else paste(month.name[1], "to", month.name[lm])
  ytd <- function(y) ca[ca$year == y & as.integer(format(ca$month, "%m")) <= lm, ]
  cur <- ytd(ly); prev <- ytd(ly - 1)
  out <- character()

  # 1. IMP share of all permits (whole period and latest year to date)
  imp_all <- sum(ca$value[ca$program == "IMP"]) / sum(ca$value)
  imp_cur <- sum(cur$value[cur$program == "IMP"]) / sum(cur$value)
  about <- if (imp_all >= 0.75 && imp_all < 0.85) "about 4 in 5 work permits: " else ""
  out <- c(out, sprintf("The International Mobility Program (IMP) accounts for %s%s from %s to %s, and %s in %s %d.",
                        about, fmt_share(imp_all), fmt_month(min(ca$month)), fmt_month(latest_month),
                        fmt_share(imp_cur), months_tx, ly))

  # 2. The largest category and its part in the latest change
  by_cat <- function(x) tapply(x$value, as.character(x$category), sum)
  top_by_year <- vapply(sort(unique(ca$year)), function(y) names(which.max(by_cat(ca[ca$year == y, ]))), "")
  names(top_by_year) <- sort(unique(ca$year))
  top_now <- names(which.max(by_cat(cur)))
  run <- rev(cumprod(rev(top_by_year == top_now)))           # years in a row up to the latest
  since <- names(top_by_year)[which(run == 1)[1]]
  chg <- by_cat(cur) - by_cat(prev)[names(by_cat(cur))]
  tot_chg <- sum(cur$value) - sum(prev$value)
  if (tot_chg > 0 && names(which.max(chg)) == top_now) {
    out <- c(out, sprintf("%s are the largest category in every year since %s. They made up %s (%s) of the %s increase in %s %d compared with the same months of %d.",
                          if (top_now == "Post-graduation work permits") "Post-graduation work permits (PGWPs)" else top_now,
                          since, fmt_n(chg[top_now]), fmt_share(chg[top_now] / tot_chg),
                          fmt_n(tot_chg), months_tx, ly, ly - 1))
  } else {
    out <- c(out, sprintf("%s are the largest category in %s %d.", top_now, months_tx, ly))
  }

  # 3. Spouse and family permits: peak year, latest full year, latest months
  sp <- ca[ca$category == "Spouses and family members", ]
  sp_y <- tapply(sp$value[sp$year < ly], sp$year[sp$year < ly], sum)
  pk <- names(sp_y)[which.max(sp_y)]
  sp_cur <- sum(ytd(ly)$value[ytd(ly)$category == "Spouses and family members"])
  sp_prev <- sum(ytd(ly - 1)$value[ytd(ly - 1)$category == "Spouses and family members"])
  if (as.integer(pk) < ly - 1 && sp_y[length(sp_y)] < max(sp_y) && sp_cur < sp_prev) {
    out <- c(out, sprintf("Spouse and family permits fell from %s in %s to %s in %s, and %s in %s %d was %s below the same months of %d. Eligibility for family open work permits was narrowed in January 2025 (marked on the Trends tab for context).",
                          fmt_n(max(sp_y)), pk, fmt_n(sp_y[length(sp_y)]), names(sp_y)[length(sp_y)],
                          fmt_n(sp_cur), months_tx, ly, sub("^-", "", fmt_pct(pct_change(sp_cur, sp_prev))), ly - 1))
  }

  # 4. Where: the largest provincial shares in the latest year to date
  pt <- data[data$geo_type == "province/territory" & data$year == ly &
               as.integer(format(data$month, "%m")) <= lm, ]
  sh <- sort(tapply(pt$value, as.character(pt$geography), sum), decreasing = TRUE) / sum(cur$value)
  out <- c(out, sprintf("%s (%s) and %s (%s) received the largest shares of permits in %s %d, followed by %s (%s).",
                        names(sh)[1], fmt_share(sh[1]), names(sh)[2], fmt_share(sh[2]),
                        months_tx, ly, names(sh)[3], fmt_share(sh[3])))
  out
}
