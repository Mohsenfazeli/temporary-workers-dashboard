# =============================================================================
# R/mod_overview.R: Overview tab (key numbers, headline chart, "What the data
# show" for the release, and takeaways for the current selection)
# =============================================================================
required_packages <- c("shiny", "bslib", "plotly", "bsicons")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Four key-number cards (built in the server from overview_cards() in
    # R/calc.R). All four cover the same months: January to the last month of
    # the selected period (by default 2026 so far), for the selected place
    # and programs. The intro line says exactly what they cover.
    htmltools::tags$p(class = "kpi-intro", shiny::textOutput(ns("kpi_intro"), inline = TRUE)),
    bslib::layout_column_wrap(
      width = "220px", fill = FALSE, class = "kpi-row",
      bslib::value_box(title = shiny::textOutput(ns("kpi_ytd_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_ytd")),
                       shiny::textOutput(ns("kpi_ytd_sub")),
                       showcase = bsicons::bs_icon("people"), theme = "primary", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_change_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_change")),
                       shiny::textOutput(ns("kpi_change_sub")),
                       showcase = bsicons::bs_icon("graph-up-arrow"), theme = "light", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_split_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_split")),
                       shiny::textOutput(ns("kpi_split_sub")),
                       showcase = bsicons::bs_icon("pie-chart"), theme = "light", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_latest_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_latest")),
                       shiny::textOutput(ns("kpi_latest_sub")),
                       showcase = bsicons::bs_icon("calendar-event"), theme = "light", showcase_layout = "top right")
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
      chart_card(ns, "headline", height = "360px"),
      # Right: fixed points for the release (default) and the automatic
      # takeaways for the current filters.
      bslib::navset_card_tab(
        id = ns("notes_tab"),
        bslib::nav_panel("What the data show", shiny::uiOutput(ns("highlights"))),
        bslib::nav_panel("For your selection",
          shiny::uiOutput(ns("takeaways")),
          htmltools::tags$p(class = "source-line",
            "Written automatically from the data; updates with the filters on the left.")))
    )
  )
}

overview_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    # Data for the current selection (one place, chosen programs, period;
    # with "same months each year", only those months of every year).
    sel_df <- shiny::reactive({
      s <- sel()
      filter_selection(data, s$start, s$end, s$geography, s$programs, s$last_moy)
    })

    # ---- The four cards: same months, same place and programs ----
    cards <- shiny::reactive({ s <- sel(); overview_cards(data, s$end, s$geography, s$program_choice) })
    output$kpi_intro <- shiny::renderText(cards()$intro)
    ids <- c("kpi_ytd", "kpi_change", "kpi_split", "kpi_latest")
    lapply(seq_along(ids), function(i) {
      id <- ids[i]
      output[[paste0(id, "_title")]] <- shiny::renderText(cards()$cards[[i]]$title)
      output[[id]]                   <- shiny::renderText(cards()$cards[[i]]$value)
      output[[paste0(id, "_sub")]]   <- shiny::renderText(cards()$cards[[i]]$note)
    })

    # ---- Headline chart: stacked by program ----
    output$headline_title <- shiny::renderText({
      s <- sel(); trend_title(sel_df(), s$geography, s$program_choice, s$resolution,
                              meta$latest_month)   # basis matches the plotted view
    })
    output$headline_plot <- plotly::renderPlotly({
      s <- sel()
      ser <- summarise_series(sel_df(), s$resolution, by = "program",
                              latest_month = meta$latest_month)
      if (nrow(ser) == 0) return(empty_plot("No data for this selection"))
      ser$program <- factor(ser$program, levels = c("TFWP", "IMP"))
      ser <- ser[order(ser$program, ser$period), ]
      ser$hover <- sprintf("%s: %s%s", ser$program, fmt_n(ser$value),
                           ifelse(ser$partial, " (part of the year only)", ""))
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(ser, x = ~period, y = ~value, color = ~program,
                             colors = PROGRAM_COLOURS[c("TFWP", "IMP")],
                             type = "scatter", mode = "lines", stackgroup = "one",
                             text = ~hover, hoverinfo = "text", line = list(width = 1))
        style_plot(p, "Month issued", "Work permits issued per month") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"))
      } else {
        ser$label <- factor(ser$label, levels = unique(ser$label[order(ser$period)]))
        p <- plotly::plot_ly(ser, x = ~label, y = ~value, color = ~program,
                             colors = PROGRAM_COLOURS[c("TFWP", "IMP")], type = "bar",
                             text = ~hover, hoverinfo = "text", textposition = "none",
                             marker = list(opacity = ifelse(ser$partial, 0.55, 1)))
        style_plot(p, "Year issued",
                   "Work permits issued per year", x_type = "category") |>
          plotly::layout(barmode = "stack")
      }
    })
    output$headline_source <- shiny::renderUI({
      s <- sel()
      source_line(meta, extra = if (!is.null(s$last_moy)) sprintf("Every year shows January to %s only, so the years can be compared.", month.name[s$last_moy])
                                else if (s$resolution == "Annual") "Lighter bars cover only part of a year.")
    })

    # ---- What the data show: fixed for the release (Canada, all months) ----
    output$highlights <- shiny::renderUI({
      tx <- release_highlights(data, meta$latest_month)
      htmltools::tagList(
        htmltools::tags$p(class = "highlights-date",
          htmltools::tags$strong(paste0("Canada, ", meta$release_phrase %||%
                                          paste0("IRCC open data (data to ", fmt_month(meta$latest_month), ")")))),
        htmltools::tags$ul(class = "takeaways", lapply(tx, htmltools::tags$li)),
        htmltools::tags$p(class = "source-line", IN_CANADA_NOTE$text, " Source: ",
          htmltools::tags$a(href = IN_CANADA_NOTE$url, target = "_blank", IN_CANADA_NOTE$source, .noWS = "after"), "."))
    })

    # ---- Takeaways for the current selection ----
    output$takeaways <- shiny::renderUI({
      s <- sel()
      tx <- make_takeaways(data, s$start, s$end, s$geography, s$program_choice)
      htmltools::tags$ul(class = "takeaways", lapply(tx, htmltools::tags$li))
    })
  })
}
