# =============================================================================
# R/mod_trends.R: Trends tab (trend by program, year-over-year change,
# neutral notes for key events)
# =============================================================================
required_packages <- c("shiny", "bslib", "plotly")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

trends_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    chart_card(ns, "trend", height = "400px"),
    chart_card(ns, "yoy", height = "360px")
  )
}

trends_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    sel_df <- shiny::reactive({
      s <- sel(); filter_selection(data, s$start, s$end, s$geography, s$programs, s$last_moy)
    })

    output$trend_title <- shiny::renderText({
      chart_title("trend", sel())      # names what is shown (R/calc.R, tested)
    })

    # Is the chart narrow (under 600 px, e.g. a phone)? A reactiveVal only
    # signals a change when its value actually changes, so resizing the
    # window redraws the chart only when it crosses the 600 px line.
    is_narrow <- shiny::reactiveVal(FALSE)
    shiny::observe({
      w <- session$clientData[[paste0("output_", session$ns("trend_plot"), "_width")]]
      # A hidden chart (e.g. while the spinner shows) reports width 0; that
      # is ignored, otherwise hiding and showing the chart would loop forever.
      if (!is.null(w) && w > 0) is_narrow(w < 600)
    })

    output$trend_plot <- plotly::renderPlotly({
      s <- sel()
      ser <- summarise_series(sel_df(), s$resolution, by = "program",
                              latest_month = meta$latest_month)
      if (nrow(ser) == 0) return(empty_plot("No data for this selection"))
      # Add a total line when both programs are selected.
      if (length(s$programs) == 2) {
        tot <- ser |> dplyr::group_by(period, label, months, partial) |>
          dplyr::summarise(value = sum(value), masked_cells = sum(masked_cells),
                           .groups = "drop") |> dplyr::mutate(program = "Total")
        ser <- dplyr::bind_rows(ser, tot)
      }
      ser <- ser[order(ser$period), ]
      ser$hover <- sprintf("%s: %s%s", ser$program, fmt_n(ser$value),
                           ifelse(ser$partial, " (part of the year only, not comparable with full years)", ""))
      p <- plotly::plot_ly()
      # One line per program. Each trace gets its own data frame via `data =`
      # and formulas (~), because plotly evaluates arguments lazily and a
      # plain loop variable would end up pointing at the last program.
      ser$x <- if (s$resolution == "Monthly") ser$period else ser$label
      for (pr in intersect(c("Total", "TFWP", "IMP"), unique(ser$program))) {
        d <- ser[ser$program == pr, ]
        p <- p |> plotly::add_trace(
          data = d, x = ~x, y = ~value, type = "scatter",
          mode = if (s$resolution == "Monthly") "lines" else "lines+markers",
          name = if (pr == "Total") "Both programs" else pr,
          line = list(color = PROGRAM_COLOURS[[pr]], width = if (pr == "Total") 3 else 2),
          marker = list(color = PROGRAM_COLOURS[[pr]], size = 8,
                        symbol = ifelse(d$partial, "circle-open", "circle")),
          text = ~hover, hoverinfo = "text")
      }
      p <- style_plot(p,
        if (s$resolution == "Monthly") "Month issued" else "Year issued",
        if (s$resolution == "Monthly") "Work permits issued per month"
        else "Work permits issued per year",
        x_type = if (s$resolution == "Monthly") "date" else "category") |>
        # Legend above the chart (never collides with long year labels) and
        # headroom at the top (50% monthly, 35% annual) so the event notes never sit on a line.
        plotly::layout(legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom"),
                       margin = list(t = 70, b = 70),
                       yaxis = list(range = c(0, (if (s$resolution == "Monthly") 1.5 else 1.35) * max(ser$value, na.rm = TRUE))))
      # Neutral event notes (only those inside the selected period).
      ev <- KEY_EVENTS[KEY_EVENTS$date >= s$start & KEY_EVENTS$date <= s$end, ]
      # Width of the chart in the browser (pixels). On a narrow screen the
      # short notes would overlap, so only the dotted lines are drawn there;
      # the full notes are always in the source line under the chart.
      narrow <- is_narrow()
      if (nrow(ev) > 0) {
        # Monthly: place the note at the date. Annual (category axis): use the
        # category POSITION (0 = first year shown), because plotly would read
        # a label like "2020" as position 2020 and squash the chart.
        yrs_shown <- sort(unique(as.integer(format(ser$period, "%Y"))))
        if (s$resolution == "Annual") {
          # One line per year: events in the same year share a line and a label
          ev$year <- as.integer(format(ev$date, "%Y"))
          ev <- do.call(rbind, lapply(split(ev, ev$year), function(e) data.frame(
            date = e$date[1], year = e$year[1],
            short = if (nrow(e) > 1) paste0(e$year[1], ": ", paste(e$tiny, collapse = " & "))
                    else e$short[1])))
        }
        xs <- if (s$resolution == "Monthly") ev$date else match(ev$year, yrs_shown) - 1
        p <- p |> plotly::layout(
          shapes = lapply(seq_len(nrow(ev)), function(i) list(
            type = "line", x0 = xs[i], x1 = xs[i], yref = "paper", y0 = 0, y1 = 1,
            line = list(color = "#777777", dash = "dot", width = 1))),
          annotations = if (narrow) list() else lapply(seq_len(nrow(ev)), function(i) list(
            # One height per note (stepping down from the top, inside the
            # headroom), because several 2024-2025 events are close together
            x = xs[i], y = 1 - (if (s$resolution == "Monthly") 0.055 else 0.065) * (i - 1),
            yref = "paper",
            text = ev$short[i], showarrow = FALSE,
            xanchor = "right", yanchor = "top", xshift = -3,
            font = list(size = 11, color = "#444444"),
            bgcolor = "rgba(255,255,255,0.85)")))
      }
      if (s$resolution == "Monthly") p <- p |> plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"))
      p
    })
    output$trend_source <- shiny::renderUI({
      s <- sel()
      ev <- KEY_EVENTS[KEY_EVENTS$date >= s$start & KEY_EVENTS$date <= s$end, ]
      source_line(meta,
        extra = htmltools::tagList(
          if (!is.null(s$last_moy)) sprintf("Every year shows January to %s only. ", month.name[s$last_moy])
          else if (s$resolution == "Annual") "Open circles cover only part of a year. ",
          event_notes_html(ev)))
    })
    # ---- Year-over-year change ----
    yoy <- shiny::reactive({
      s <- sel()
      full <- data |> dplyr::filter(geography == s$geography, program %in% s$programs)
      # Same months each year: compare e.g. Jan-Jul with Jan-Jul a year earlier
      if (!is.null(s$last_moy)) full <- full[as.integer(format(full$month, "%m")) <= s$last_moy, ]
      # With both programs, add their combined total as a third series
      # ("Total"), consistent with the trend chart above.
      if (length(s$programs) == 2) full <- dplyr::bind_rows(full, dplyr::mutate(full, program = "Total"))
      yoy_series(full, s$start, s$end, s$resolution, by = "program",
                 latest_month = meta$latest_month)
    })
    output$yoy_title <- shiny::renderText({
      chart_title("yoy", yoy_title_sel(sel(), min(data$month)))
    })
    output$yoy_plot <- plotly::renderPlotly({
      s <- sel(); y <- yoy()
      y <- y[!is.na(y$yoy), ]
      if (nrow(y) == 0) return(empty_plot("Year-over-year change needs data from a year earlier"))
      y <- y[order(y$period), ]
      y$program <- factor(ifelse(y$program == "Total", "Both programs", y$program),
                          levels = c("TFWP", "IMP", "Both programs"))
      y$program <- droplevels(y$program)
      y$hover <- sprintf("%s %s: %s (%s, a year earlier %s)", y$program, y$label, fmt_pct(y$yoy),
                         fmt_n(y$value), fmt_n(y$prev_value))
      yoy_cols <- c(TFWP = PROGRAM_COLOURS[["TFWP"]], IMP = PROGRAM_COLOURS[["IMP"]],
                    `Both programs` = PROGRAM_COLOURS[["Total"]])[levels(y$program)]
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(y, x = ~period, y = ~yoy, color = ~program,
                             colors = yoy_cols,
                             type = "scatter", mode = "lines", text = ~hover, hoverinfo = "text")
        p <- style_plot(p, "Month", "Change from a year earlier (%)", y_format = ".0%") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"),
                         yaxis = list(rangemode = "normal"))
      } else {
        y$label <- factor(y$label, levels = unique(y$label))
        p <- plotly::plot_ly(y, x = ~label, y = ~yoy, color = ~program,
                             colors = yoy_cols, type = "bar",
                             text = ~hover, hoverinfo = "text", textposition = "none")
        p <- style_plot(p, "Year issued",
                        "Change from a year earlier (%)", y_format = ".0%", x_type = "category") |>
          plotly::layout(barmode = "group", yaxis = list(rangemode = "normal"))
      }
      p
    })
    output$yoy_source <- shiny::renderUI(source_line(meta,
      extra = "Each year is compared with the year before; a part year, such as 2026 (January to July), with the same months a year earlier."))
  })
}
