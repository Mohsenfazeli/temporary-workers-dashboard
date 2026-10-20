# =============================================================================
# R/mod_geography.R: Geography tab (ranking, share of Canada, growth,
# comparison of one province/territory with Canada, contribution to change)
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
          shiny::textOutput(ns("rank_table_title"), inline = TRUE))),
        # Not a fill area: the card grows to show all 13 rows without an
        # inner scroll bar (its taller header names the growth periods)
        bslib::card_body(DT::DTOutput(ns("rank_table")), fillable = FALSE, fill = FALSE),
        bslib::card_footer(shiny::uiOutput(ns("table_source")))
      )
    ),
    chart_card(ns, "compare", height = "340px"),
    # Each province/territory's part of Canada's latest year-to-date change
    chart_card(ns, "contrib", height = "380px")
  )
}

geography_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    rank <- shiny::reactive({ s <- sel(); geo_ranking(data, s$start, s$end, s$programs) })

    output$rank_title <- shiny::renderText({
      chart_title("rank", sel())
    })
    output$rank_table_title <- shiny::renderText({
      chart_title("table", sel())
    })
    # The growth comparison for the selected period, e.g. "Aug 2025\u2013Jul 2026
    # vs 2015", named in the table header and the hover text.
    growth <- shiny::reactive({ s <- sel()
      kpi_period_change(filter_selection(data, s$start, s$end, "Canada", s$programs)) })
    output$rank_plot <- plotly::renderPlotly({
      r <- rank(); s <- sel()
      r <- r[order(r$value), ]
      r$geography <- factor(r$geography, levels = r$geography)
      highlight <- r$geography == s$geography
      r$hover <- sprintf("%s<br>Permits: %s<br>Share of Canada: %s<br>Growth (%s): %s",
                         r$geography, fmt_n(r$value), fmt_share(r$share), growth()$short, fmt_pct(r$growth))
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
      extra = "Each bar is the total for the whole selected period (for the latest year only, choose \u201cLast 12 months\u201d on the left), with its share of Canada in brackets. Canada's total also includes permits with no province or territory given, which have no bar."))

    output$rank_table <- DT::renderDT({
      r <- rank(); s <- sel()
      # Short headers so all six columns fit in the card at 1366 px; the
      # Growth header names the two periods compared, and the source line
      # under the table explains Share, Growth and Note.
      g_col <- if (nzchar(growth()$short)) paste0("Growth, ", growth()$short) else "Growth"
      tbl <- data.frame(`#` = r$rank, `Province / territory` = r$geography,
                        # Share as text so tiny shares read "<0.1%" rather than "0.0%"
                        Permits = r$value, Share = fmt_share(r$share, 1),
                        Growth = r$growth,
                        # Marker where IRCC's hidden small counts could add over 10%
                        # (hover for the meaning)
                        `Note*` = ifelse(undercount_flag(r$value, r$masked_cells),
                                         HIDDEN_COUNTS_FLAG, ""), check.names = FALSE)
      names(tbl)[5] <- g_col
      DT::datatable(tbl, rownames = FALSE, selection = "none",
                    escape = setdiff(names(tbl), "Note*"),   # the flag is HTML (tooltip)
                    class = "compact stripe rank-table",
                    # scrollX keeps every column reachable on a phone
                    options = list(dom = "t", pageLength = 13, ordering = TRUE,
                                   scrollX = TRUE, autoWidth = FALSE)) |>
        DT::formatRound("Permits", digits = 0, mark = ",") |>
        DT::formatPercentage(g_col, digits = 1) |>
        DT::formatStyle("Province / territory", target = "row",
                        backgroundColor = DT::styleEqual(s$geography, "#FFF4D6"))
    }, server = FALSE)   # client-side: 13 rows, data sent with the table
    output$table_source <- shiny::renderUI(source_line(meta,
      # The only explanation of Growth (no duplicate table caption)
      extra = paste0("Share = share of all permits in Canada. Growth = ", growth()$text, ". ",
        "*\u26a0 = many hidden counts: IRCC hides counts of 0\u20135 (shown as 0), and for this place they could add more than 10% to its total, so read its share and growth with care.")))

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
      # This chart ignores the "same months" option, so its title does too
      s <- utils::modifyList(sel(), list(last_moy = NULL))
      # Province view: change from a year earlier, which starts a year after the data
      chart_title("compare", if (s$geography == "Canada") s else yoy_title_sel(s, min(data$month)))
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
          if (s$resolution == "Monthly") "Month issued" else "Year issued",
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
        return(style_plot(p, "Month", "Change from the same month a year earlier (%)",
                          y_format = ".0%", x_type = "date") |>
                 plotly::layout(yaxis = list(rangemode = "normal"),
                                xaxis = list(tickformat = "%Y", dtick = "M12")))
      }
      p <- plotly::plot_ly(long, x = ~label, y = ~yoy, color = ~place, colors = cols,
                           type = "bar", text = ~hover, hoverinfo = "text",
                           textposition = "none",
                           marker = list(line = list(color = "#333333", width = 0.5)))
      style_plot(p, "Year issued",
                 "Change from a year earlier (%)", y_format = ".0%", x_type = "category") |>
        plotly::layout(barmode = "group", yaxis = list(rangemode = "normal"))
    })
    # ---- Contribution to change: latest year to date vs same months a year earlier ----
    # Follows the program filter and the last month of the period slider.
    # Same months as the Overview cards: January to the last month of the
    # selected period (by default 2026 so far) vs the same months a year earlier.
    contrib <- shiny::reactive(contribution_to_change(data, sel()$end, sel()$programs))
    output$contrib_title <- shiny::renderText({
      chart_title("contrib", sel(), label = attr(contrib(), "label"))
    })
    output$contrib_plot <- plotly::renderPlotly({
      x <- contrib(); s <- sel()
      if (is.na(attr(x, "canada_change"))) return(empty_plot("This needs data from a year earlier"))
      if (all(is.na(x$contribution))) return(empty_plot("Canada's total did not change, so shares of the change are not defined"))
      x <- x[order(x$contribution), ]
      x$geography <- factor(x$geography, levels = x$geography)
      highlight <- x$geography == s$geography
      x$hover <- sprintf("%s<br>%s: %s permits (%s a year earlier)<br>Change: %s<br>Share of Canada's change: %s",
                         x$geography, attr(x, "label"), fmt_n(x$value), fmt_n(x$prev_value),
                         fmt_n(x$change), fmt_share(x$contribution))
      p <- plotly::plot_ly(x, y = ~geography, x = ~contribution, type = "bar", orientation = "h",
                           marker = list(color = ifelse(highlight, GOV_GOLD, GOV_BLUE),
                                         line = list(color = ifelse(highlight, "#7A5A00", GOV_BLUE), width = 1)),
                           text = ~fmt_share(contribution), textposition = "outside", cliponaxis = FALSE,
                           hovertext = ~hover, hoverinfo = "text")
      rng <- range(c(0, x$contribution), na.rm = TRUE)
      style_plot(p, "Share of Canada's change (%)", NULL, legend = FALSE, hovermode = "closest") |>
        plotly::layout(xaxis = list(tickformat = ".0%", range = c(min(rng[1] * 1.3, 0), rng[2] * 1.25)),
                       yaxis = list(title = NULL, tickformat = NULL), margin = list(l = 170, r = 40))
    })
    output$contrib_source <- shiny::renderUI(source_line(meta, extra = paste0(
      "Share of Canada's change = the province or territory's change in permits \u00f7 Canada's change, ",
      attr(contrib(), "label"), " (the same months as the Overview cards). ",
      "A negative share means it moved the opposite way to Canada. ",
      "Permits with no province or territory given are not listed, so the shares do not add to exactly 100%.")))

    output$compare_source <- shiny::renderUI({
      s <- sel()
      source_line(meta, extra = if (s$geography == "Canada")
        "Share = the province or territory's permits \u00f7 Canada's permits (Canada also includes permits with no province or territory given)."
        else "Each year is compared with the year before; a part year, such as 2026 (January to July), with the same months a year earlier.")
    })
  })
}
