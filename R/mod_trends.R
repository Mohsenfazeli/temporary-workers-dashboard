# =============================================================================
# R/mod_trends.R: Trends tab (trend by program, year-over-year change,
# neutral notes for key events, IRCC distinct-people reference)
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
    chart_card(ns, "trend", height = "400px",
               footer = htmltools::tagList(
                 # The IRCC Excel reference is OFF by default: the dashboard's
                 # figures all come from the two CSVs named in the assignment.
                 shiny::checkboxInput(ns("show_ref"),
                   "Show IRCC's own annual count of distinct people (supplementary reference; not one of the two assignment data files)",
                   value = FALSE, width = "100%"),
                 shiny::uiOutput(ns("reference_note")))),
    chart_card(ns, "yoy", height = "360px")
  )
}

trends_server <- function(id, sel, data, meta, reference) {
  shiny::moduleServer(id, function(input, output, session) {

    sel_df <- shiny::reactive({
      s <- sel(); filter_selection(data, s$start, s$end, s$geography, s$programs)
    })

    # Should the IRCC distinct-people reference be drawn? Only for Canada,
    # only in the annual view, and only if the reference was read.
    show_ref <- shiny::reactive({
      s <- sel()
      isTRUE(input$show_ref) && s$geography == "Canada" && s$resolution == "Annual" &&
        !is.null(reference$table)
    })

    output$trend_title <- shiny::renderText({
      s <- sel()
      paste(trend_title(sel_df(), s$geography, s$program_choice, s$resolution,
                        meta$latest_month),
            # The annual title already names the years compared
            if (s$resolution == "Annual") "" else "(by month)") |> trimws()
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
                           ifelse(ser$partial, " (part-year, not comparable with full years)", ""))
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
      # IRCC distinct-people reference (Canada, annual view only).
      if (show_ref()) {
        ref <- reference$table
        ser_years <- unique(ser[c("period", "label", "partial")])
        ser_years$year <- as.integer(format(ser_years$period, "%Y"))
        ref <- merge(ref, ser_years, by = "year")
        # Only full years (and the YTD year, whose reference is also Jan-latest month)
        ref <- ref[!ref$partial | grepl("YTD", ref$label), ]
        for (pr in intersect(c("TFWP", "IMP"), s$programs)) {
          r <- ref[ref$program == pr, ]
          r <- r[order(r$year), ]
          if (nrow(r) == 0) next
          r$hover <- sprintf("%s reference: %s distinct people (IRCC Excel table)", pr, fmt_n(r$people))
          p <- p |> plotly::add_trace(
            data = r, x = ~label, y = ~people, type = "scatter", mode = "markers",
            name = paste(pr, "\u2013 IRCC count of distinct people (reference)"),
            marker = list(color = PROGRAM_COLOURS[[pr]], symbol = "diamond-open",
                          size = 11, line = list(width = 2)),
            text = ~hover, hoverinfo = "text")
        }
      }
      p <- style_plot(p,
        if (s$resolution == "Monthly") "Month the permit took effect"
        else "Year the permit took effect",
        if (s$resolution == "Monthly") "Work permits (number per month)"
        else "Work permits (number per year)",
        x_type = if (s$resolution == "Monthly") "date" else "category") |>
        # Legend above the chart (never collides with long year labels) and
        # 25% headroom at the top so the event notes never sit on a line.
        plotly::layout(legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom"),
                       margin = list(t = 70, b = 70),
                       yaxis = list(range = c(0, 1.25 * max(c(ser$value,
                         if (show_ref()) reference$table$people), na.rm = TRUE))))
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
            # Monthly: alternate heights so close notes don't overlap; Annual:
            # one line per year, so all notes sit at the top (above the headroom)
            x = xs[i], y = if (s$resolution == "Monthly") 1 - 0.07 * ((i - 1) %% 2) else 1,
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
      source_line(meta, reference = show_ref(),
        extra = htmltools::tagList(
          if (s$resolution == "Annual") "Open circles are part-years. ",
          event_notes_html(ev)))
    })
    output$reference_note <- shiny::renderUI({
      s <- sel()
      if (!isTRUE(input$show_ref)) {
        NULL    # reference switched off (the default): nothing extra to explain
      } else if (s$geography != "Canada" || s$resolution != "Annual") {
        htmltools::tags$p(class = "source-line",
          "The IRCC reference exists only for Canada as a whole, by year: choose Canada and the Annual view.")
      } else if (is.null(reference$table)) {
        htmltools::tags$p(class = "source-line",
          "IRCC's reference count of distinct people is not available right now, so it is not shown.")
      } else {
        htmltools::tags$p(class = "source-line",
          htmltools::tags$strong("Reading the diamonds: "),
          "lines count work permits that took effect (sum of months, from the two IRCC CSVs). Diamonds show IRCC's own annual count of distinct people for Canada, from IRCC's Excel version of the same tables (a supplementary source, shown only on request). It is 1\u20133% lower because a person with permits in two different months is counted once. See About the data.")
      }
    })

    # ---- Year-over-year change ----
    yoy <- shiny::reactive({
      s <- sel()
      full <- data |> dplyr::filter(geography == s$geography, program %in% s$programs)
      # With both programs, add their combined total as a third series
      # ("Total"), consistent with the trend chart above.
      if (length(s$programs) == 2) full <- dplyr::bind_rows(full, dplyr::mutate(full, program = "Total"))
      yoy_series(full, s$start, s$end, s$resolution, by = "program",
                 latest_month = meta$latest_month)
    })
    output$yoy_title <- shiny::renderText({
      yoy_title_text(yoy())          # names each program (R/calc.R, tested)
    })
    output$yoy_plot <- plotly::renderPlotly({
      s <- sel(); y <- yoy()
      y <- y[!is.na(y$yoy), ]
      if (nrow(y) == 0) return(empty_plot("Year-over-year change needs data from a year earlier"))
      y <- y[order(y$period), ]
      y$program <- factor(ifelse(y$program == "Total", "Both programs", y$program),
                          levels = c("TFWP", "IMP", "Both programs"))
      y$program <- droplevels(y$program)
      y$hover <- sprintf("%s %s: %s (%s vs %s)", y$program, y$label, fmt_pct(y$yoy),
                         fmt_n(y$value), fmt_n(y$prev_value))
      yoy_cols <- c(TFWP = PROGRAM_COLOURS[["TFWP"]], IMP = PROGRAM_COLOURS[["IMP"]],
                    `Both programs` = PROGRAM_COLOURS[["Total"]])[levels(y$program)]
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(y, x = ~period, y = ~yoy, color = ~program,
                             colors = yoy_cols,
                             type = "scatter", mode = "lines", text = ~hover, hoverinfo = "text")
        p <- style_plot(p, "Month", "Change vs a year earlier", y_format = ".0%") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"),
                         yaxis = list(rangemode = "normal"))
      } else {
        y$label <- factor(y$label, levels = unique(y$label))
        p <- plotly::plot_ly(y, x = ~label, y = ~yoy, color = ~program,
                             colors = yoy_cols, type = "bar",
                             text = ~hover, hoverinfo = "text", textposition = "none")
        p <- style_plot(p, "Year the permit took effect",
                        "Change vs a year earlier", y_format = ".0%", x_type = "category") |>
          plotly::layout(barmode = "group", yaxis = list(rangemode = "normal"))
      }
      p
    })
    output$yoy_source <- shiny::renderUI(source_line(meta,
      extra = "Part-years (including 2026 year-to-date) are compared with the same months of the previous year."))
  })
}
