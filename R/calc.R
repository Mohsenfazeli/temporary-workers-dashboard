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
filter_selection <- function(data, start, end, geography = "Canada",
                             programs = c("TFWP", "IMP")) {
  data |>
    filter(month >= as.Date(start), month <= as.Date(end),
           geography == !!geography, program %in% programs)
}

# ---- 4. Period labels for the annual view -----------------------------------
# A year is "full" only if all 12 months are selected. The latest year in
# the data (2026) is always labelled year-to-date (YTD), e.g.
# "2026 YTD (Jan-Jul)", and a year cut by the slider is labelled partial,
# e.g. "2019 (Mar-Dec)". This stops a part-year being read as a full year.
#' Short axis version of a year label, for crowded category axes:
#' "2026 YTD (Jan\u2013Jul)" -> "2026<br>YTD"; full years are unchanged; other
#' part-years ("2015 (Mar\u2013Dec)") -> "2015<br>part".
short_year_label <- function(label) {
  label <- as.character(label)
  out <- sub("^(\\d{4}) YTD.*$", "\\1<br>YTD", label)
  sub("^(\\d{4}) \\(.*\\)$", "\\1<br>part", out)
}

year_label <- function(year, first_m, last_m, latest_month) {
  n_months  <- last_m - first_m + 1
  months_tx <- ifelse(first_m == last_m, month.abb[first_m],
                      paste0(month.abb[first_m], "\u2013", month.abb[last_m]))
  is_latest <- year == lubridate::year(latest_month) &
    lubridate::month(latest_month) < 12
  dplyr::case_when(
    is_latest & first_m == 1 & last_m == lubridate::month(latest_month)
      ~ paste0(year, " YTD (", months_tx, ")"),
    n_months == 12 ~ as.character(year),
    TRUE ~ paste0(year, " (", months_tx, ")")
  )
}

# ---- 5. Time series at monthly or annual resolution --------------------------
#' @param df filtered data (one geography)
#' @param resolution "Monthly" or "Annual"
#' @param by extra grouping columns, e.g. "program" or c("program","category")
#' @param latest_month latest month in the full data (for YTD labels)
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
      select(-first_m, -last_m)
  }
}

# ---- 6. Year-over-year change -----------------------------------------------
# Monthly: each month vs the same month one year earlier.
# Annual: each year vs the SAME MONTHS of the previous year. So a full year
# is compared with the full previous year, and 2026 YTD (Jan-Jul) is
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
    list(change = pct_change(last, first), method = "12m",
         label = "Last 12 months vs first 12 months",
         first_value = first, last_value = last,
         text = sprintf("last 12 months (%s\u2013%s) vs first 12 months (%s\u2013%s) of the period",
                        fmt_month_short(m$month[n - 11]), fmt_month_short(m$month[n]),
                        fmt_month_short(m$month[1]), fmt_month_short(m$month[12])))
  } else if (n >= 2) {
    list(change = pct_change(m$value[n], m$value[1]), method = "month",
         label = sprintf("%s vs %s", fmt_month_short(m$month[n]), fmt_month_short(m$month[1])),
         first_value = m$value[1], last_value = m$value[n],
         text = sprintf("%s vs %s (single months; seasonal swings possible)",
                        fmt_month_short(m$month[n]), fmt_month_short(m$month[1])))
  } else {
    list(change = NA_real_, method = "none", label = "Change over the period",
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
composition_shares <- function(df, resolution = "Annual",
                               latest_month = max(df$month)) {
  summarise_series(df, resolution, by = c("program", "category"),
                   latest_month = latest_month) |>
    group_by(period, program) |>
    mutate(share = if (sum(value) > 0) value / sum(value) else NA_real_) |>
    ungroup()
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
              `Work permits that took effect` = value,
              `Months included` = months,
              `Suppressed cells (counted as 0)` = masked_cells,
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
    return(sprintf("No %s work permits took effect in %s in the selected period.",
                   program_label(program_choice), geography))
  }
  out <- character()
  n_months <- length(unique(sel$month))

  # 1. Scale: the total and the monthly average
  out <- c(out, sprintf("%s %s work permits took effect in %s between %s and %s, an average of %s a month.",
                        fmt_n(sum(sel$value)), program_label(program_choice), geography,
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
    out <- c(out, sprintf("%s, %s permits took effect, %s %s on the same months of %s.",
                          lead, fmt_n(cur), dir_word(ch), sub("^[+-]", "", fmt_pct(ch)),
                          end_year - 1))
  } else {
    lt <- kpi_latest(data, end, geography, programs)
    if (!is.na(lt$change)) {
      out <- c(out, sprintf("In %s, %s permits took effect, %s %s from %s.",
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

# ---- 12. Point-stating chart titles -----------------------------------------
#' Title for the year-over-year chart, naming every plotted series for the
#' latest period, e.g. "2026 YTD (Jan-Jul): IMP up 63.3%, TFWP down 4.8% (both
#' programs up 45.8%) vs a year earlier". `y` is yoy_series() output with a
#' `program` column ("TFWP", "IMP" and, with both programs, "Total").
yoy_title_text <- function(y) {
  y <- y[!is.na(y$yoy), ]
  if (nrow(y) == 0) return("Year-over-year change (not available for this period)")
  last <- y[y$period == max(y$period), ]
  part <- function(pr) {
    r <- last[last$program == pr, ]
    if (nrow(r) == 0) return(NULL)
    paste(dir_word(r$yoy), sub("^[+-]", "", fmt_pct(r$yoy)))
  }
  progs <- intersect(c("IMP", "TFWP"), last$program)
  each <- paste(vapply(progs, function(pr) paste(pr, part(pr)), character(1)), collapse = ", ")
  tot <- if ("Total" %in% last$program) sprintf(" (both programs %s)", part("Total")) else ""
  sprintf("%s: %s%s vs a year earlier", last$label[1], each, tot)
}


#' Headline for the trend chart, e.g. "Permits that took effect rose 25%
#' between the first and last 12 months of the period".
trend_title <- function(df, geography, program_choice, resolution = "Monthly",
                        latest_month = max(df$month)) {
  what <- sprintf("%s permits in %s", program_label(program_choice), geography)
  # "264.7% above", "12.0% below" or "about the same as" (within +/-2%).
  # One decimal, the same precision as the change card on the Overview.
  rel_text <- function(ch) {
    size <- sub("^[+-]", "", fmt_pct(ch, 1))
    if (ch >= 0.02) paste(size, "above") else if (ch <= -0.02) paste(size, "below")
    else "about the same as"
  }
  # Annual view: compare calendar years that are visible on the chart, i.e.
  # the last full year with the first full year (part-years are noted).
  if (resolution == "Annual") {
    ann <- summarise_series(df, "Annual", by = character(0), latest_month = latest_month)
    full <- ann[!ann$partial, ]
    if (nrow(full) >= 2) {
      ch <- pct_change(full$value[nrow(full)], full$value[1])
      if (!is.na(ch)) {
        note <- if (any(ann$partial)) sprintf(" (%s shown separately)",
                                              paste(ann$label[ann$partial], collapse = ", ")) else ""
        return(sprintf("%s: %s was %s %s%s", what, full$label[nrow(full)], rel_text(ch),
                       full$label[1], note))
      }
    }
  }
  # Monthly view (or fewer than two full years): the rule used by the
  # change card (last 12 vs first 12 months, or last vs first month).
  pc <- kpi_period_change(df)
  if (is.na(pc$change)) {
    return(sprintf("%s work permits that took effect in %s",
                   program_label(program_choice), geography))
  }
  if (pc$method == "12m") {
    sprintf("%s: the last 12 months were %s the first 12 months selected", what, rel_text(pc$change))
  } else {
    sprintf("%s: %s was %s %s", what, sub(" vs .*", "", pc$label), rel_text(pc$change),
            sub(".* vs ", "", pc$label))
  }
}
