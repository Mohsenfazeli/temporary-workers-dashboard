# =============================================================================
# R/mod_geography.R: Geography tab (ranking, share of Canada, growth,
# comparison of one province/territory with Canada)
# =============================================================================
required_packages <- c("shiny", "bslib", "plotly", "DT", "jsonlite")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

geography_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      chart_card(ns, "rank", height = "470px",
                 footer = htmltools::tags$p(class = "source-line",
                   htmltools::tags$strong("Tip: "), "click a bar to select that province or territory on every tab.")),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(htmltools::tags$h2(class = "chart-title",
          "Provinces and territories: size, share of Canada and growth")),
        bslib::card_body(DT::DTOutput(ns("rank_table"))),
        bslib::card_footer(shiny::uiOutput(ns("table_source")))
      )
    ),
    chart_card(ns, "compare", height = "340px")
  )
}

geography_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    rank <- shiny::reactive({ s <- sel(); geo_ranking(data, s$start, s$end, s$programs) })

    output$rank_title <- shiny::renderText({
      r <- rank(); s <- sel()
      top3 <- sum(r$share[1:3], na.rm = TRUE)
      sprintf("%s, %s and %s received %s of %s permits in the period (period totals)",
              r$geography[1], r$geography[2], r$geography[3], fmt_share(top3, 0),
              program_label(s$program_choice))
    })
    output$rank_plot <- plotly::renderPlotly({
      r <- rank(); s <- sel()
      r <- r[order(r$value), ]
      r$geography <- factor(r$geography, levels = r$geography)
      highlight <- r$geography == s$geography
      r$hover <- sprintf("%s<br>Permits: %s<br>Share of Canada: %s<br>Growth: %s",
                         r$geography, fmt_n(r$value), fmt_share(r$share), fmt_pct(r$growth))
      # source = a name for this chart's click events (see the observer below);
      # customdata carries the province name of each bar.
      p <- plotly::plot_ly(r, y = ~geography, x = ~value, type = "bar", orientation = "h",
                           source = session$ns("rank"), customdata = ~as.character(geography),
                           marker = list(color = ifelse(highlight, GOV_GOLD, GOV_BLUE),
                                         line = list(color = ifelse(highlight, "#7A5A00", GOV_BLUE), width = 1)),
                           text = ~paste0(fmt_n(value), "  (", fmt_share(share, 0), ")"),
                           textposition = "outside", cliponaxis = FALSE,
                           hovertext = ~hover, hoverinfo = "text")
      style_plot(p, NULL, NULL,
                 legend = FALSE, hovermode = "closest") |>
        # x range with 40% headroom so the value labels are never clipped
        # No x axis: every bar is labelled with its number and share, which
        # reads better than an axis at any screen width. 40% headroom keeps
        # the labels inside the chart.
        plotly::layout(xaxis = list(visible = FALSE, range = c(0, 1.4 * max(r$value, 1))),
                       yaxis = list(tickformat = NULL, title = NULL),
                       margin = list(l = 170, r = 90)) |>
        plotly::event_register("plotly_click")
    })

    # Clicking a bar selects that province/territory in the sidebar (clicking
    # the selected one again goes back to Canada). rootScope() is the app's
    # top-level session, where the sidebar's "geography" input lives.
    # Plotly sends a click as the input "plotly_click-<source>" (JSON text).
    # It is read straight from the top-level session to avoid plotly's
    # "event not registered" warning before the chart has been drawn.
    click_id <- paste0("plotly_click-", session$ns("rank"))
    shiny::observeEvent(session$rootScope()$input[[click_id]], {
      click <- jsonlite::fromJSON(session$rootScope()$input[[click_id]])
      place <- click$customdata[[1]]
      if (is.null(place)) return()
      if (identical(place, sel()$geography)) place <- "Canada"
      shiny::updateSelectInput(session$rootScope(), "geography", selected = place)
    }, ignoreInit = TRUE)
    output$rank_source <- shiny::renderUI(source_line(meta,
      extra = "Bars rank totals for the whole selected period (the Monthly/Annual view applies to the time charts only), with the share of Canada in brackets. Share of Canada includes permits with no province/territory stated in the Canada total; that group is not ranked."))

    output$rank_table <- DT::renderDT({
      r <- rank(); s <- sel()
      tbl <- data.frame(Rank = r$rank, `Province/territory` = r$geography,
                        # Share as text so tiny shares read "<0.1%" rather than "0.0%"
                        Permits = r$value, `Share of Canada` = fmt_share(r$share, 1),
                        Growth = r$growth,
                        # Marker where suppressed small values could hide over 10% more
                        `Small numbers*` = ifelse(undercount_flag(r$value, r$masked_cells),
                                                  "Yes", ""), check.names = FALSE)
      DT::datatable(tbl, rownames = FALSE, selection = "none",
                    class = "compact stripe",
                    # scrollX keeps every column reachable in a narrow window
                    options = list(dom = "t", pageLength = 13, ordering = TRUE,
                                   scrollX = TRUE, autoWidth = FALSE),
                    caption = htmltools::tags$caption(style = "caption-side: bottom; font-size: 0.85em;",
                      kpi_period_change(filter_selection(data, s$start, s$end, "Canada", s$programs))$text |>
                        (\(x) paste0("Growth = ", x,
                          ""))())) |>
        DT::formatRound("Permits", digits = 0, mark = ",") |>
        DT::formatPercentage("Growth", digits = 1) |>
        DT::formatStyle("Province/territory", target = "row",
                        backgroundColor = DT::styleEqual(s$geography, "#FFF4D6"))
    })
    output$table_source <- shiny::renderUI(source_line(meta,
      # The growth basis is stated here (a DataTables caption is hidden by scrollX)
      extra = paste0("Growth = ", (\(s) kpi_period_change(filter_selection(
        data, s$start, s$end, "Canada", s$programs))$text)(sel()), ". ",
        "*Small numbers = Yes: suppressed values (0\u20135 each, counted as 0) could add more than 10% to that total, so treat its share and growth with care.")))

    # ---- Compare the selected province/territory with Canada ----
    cmp <- shiny::reactive({
      s <- sel()
      if (s$geography == "Canada") return(NULL)
      compare_with_canada(data, s$start, s$end, s$geography, s$programs, meta$latest_month,
                          s$resolution)
    })
    # When "Canada" is selected there is nothing to compare it with, so the
    # card shows how the share of Canada's permits held by the five largest
    # provinces/territories has changed over time instead.
    shares <- shiny::reactive({
      s <- sel()
      if (s$geography != "Canada") return(NULL)
      share_over_time(data, s$start, s$end, s$programs, s$resolution, top = 5,
                      latest_month = meta$latest_month)
    })
    output$compare_title <- shiny::renderText({
      s <- sel(); c <- cmp()
      if (is.null(c)) {
        sh <- shares()
        lead <- sh[sh$place == levels(sh$place)[1], ]
        if (nrow(lead) < 2) return("Share of Canada's permits held by the five largest provinces")
        return(sprintf("%s's share of Canada's %s permits went from %s (%s) to %s (%s); choose a province in the sidebar to compare it with Canada",
                       lead$place[1], program_label(s$program_choice),
                       fmt_share(lead$share[1], 0), lead$label[1],
                       fmt_share(lead$share[nrow(lead)], 0), lead$label[nrow(lead)]))
      }
      sh <- sum(c$value_pt) / sum(c$value_canada)
      sprintf("%s received %s of Canada's %s permits in the period; %s change compared with Canada",
              s$geography, fmt_share(sh), program_label(s$program_choice),
              if (s$resolution == "Annual") "year-over-year" else "monthly year-over-year")
    })
    output$compare_plot <- plotly::renderPlotly({
      s <- sel(); c <- cmp()
      if (is.null(c)) {
        sh <- shares()
        sh <- sh[!is.na(sh$share), ]
        if (nrow(sh) == 0) return(empty_plot("No data for this selection"))
        sh$x <- if (s$resolution == "Monthly") sh$period else sh$label
        sh$hover <- sprintf("%s, %s: %s of Canada (%s permits)", sh$place, sh$label,
                            fmt_share(sh$share), fmt_n(sh$value))
        # Okabe-Ito colours, one per province (5 largest).
        pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00")
        p <- plotly::plot_ly()
        for (i in seq_along(levels(sh$place))) {
          d <- sh[sh$place == levels(sh$place)[i], ]
          if (nrow(d) == 0) next
          p <- p |> plotly::add_trace(
            data = d, x = ~x, y = ~share, type = "scatter",
            mode = if (s$resolution == "Monthly") "lines" else "lines+markers",
            name = levels(sh$place)[i], line = list(color = pal[i], width = 2),
            marker = list(color = pal[i], size = 7), text = ~hover, hoverinfo = "text")
        }
        return(style_plot(p,
          if (s$resolution == "Monthly") "Month the permit took effect" else "Year the permit took effect",
          "Share of Canada's permits (%)", y_format = ".0%",
          x_type = if (s$resolution == "Monthly") "date" else "category") |>
          plotly::layout(legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom"),
                         margin = list(t = 50)))
      }
      # Long format: one row per year and place; drop years with no valid
      # comparison (e.g. the first year, or a zero base in a small territory).
      lv <- c$label
      long <- rbind(
        data.frame(label = c$label, place = s$geography, yoy = c$yoy_pt),
        data.frame(label = c$label, place = "Canada", yoy = c$yoy_canada))
      long <- long[!is.na(long$yoy) & is.finite(long$yoy), ]
      if (nrow(long) == 0) return(empty_plot("Year-over-year change needs data from a year earlier"))
      long$label <- factor(long$label, levels = lv[lv %in% long$label])
      long$place <- factor(long$place, levels = c(s$geography, "Canada"))
      long$hover <- sprintf("%s %s: %s", long$place, long$label, fmt_pct(long$yoy))
      cols <- stats::setNames(c(GOV_GOLD, GOV_BLUE), c(s$geography, "Canada"))
      if (s$resolution == "Monthly") {
        # Monthly view: lines of the change vs the same month a year earlier.
        long$period <- c$period[match(long$label, c$label)]
        p <- plotly::plot_ly(long, x = ~period, y = ~yoy, color = ~place, colors = cols,
                             type = "scatter", mode = "lines", text = ~hover, hoverinfo = "text")
        return(style_plot(p, "Month", "Change vs same month a year earlier (%)",
                          y_format = ".0%", x_type = "date") |>
                 plotly::layout(yaxis = list(rangemode = "normal"),
                                xaxis = list(tickformat = "%Y", dtick = "M12")))
      }
      p <- plotly::plot_ly(long, x = ~label, y = ~yoy, color = ~place, colors = cols,
                           type = "bar", text = ~hover, hoverinfo = "text",
                           textposition = "none",
                           marker = list(line = list(color = "#333333", width = 0.5)))
      style_plot(p, "Year the permit took effect",
                 "Change vs a year earlier (%)", y_format = ".0%", x_type = "category") |>
        plotly::layout(barmode = "group", yaxis = list(rangemode = "normal"))
    })
    output$compare_source <- shiny::renderUI({
      s <- sel()
      source_line(meta, extra = if (s$geography == "Canada")
        "Share = province/territory permits \u00f7 Canada permits (Canada includes permits with no province/territory stated)."
        else "Part-years (e.g. year-to-date) are compared with the same months a year earlier.")
    })
  })
}
