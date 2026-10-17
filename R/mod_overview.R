# =============================================================================
# R/mod_overview.R: Overview tab (key numbers, headline chart, takeaways)
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
    # Four key-number cards; their contents are built in the server.
    bslib::layout_column_wrap(
      width = "220px", fill = FALSE, class = "kpi-row",
      bslib::value_box(title = "Work permits that took effect",
                       value = shiny::textOutput(ns("kpi_total")),
                       shiny::uiOutput(ns("kpi_total_sub")),
                       showcase = bsicons::bs_icon("people"), theme = "primary", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_split_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_split")),
                       shiny::uiOutput(ns("kpi_split_sub")),
                       showcase = bsicons::bs_icon("pie-chart"), theme = "light", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_latest_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_latest")),
                       shiny::uiOutput(ns("kpi_latest_sub")),
                       showcase = bsicons::bs_icon("calendar-event"), theme = "light", showcase_layout = "top right"),
      bslib::value_box(title = shiny::textOutput(ns("kpi_change_title"), inline = TRUE),
                       value = shiny::textOutput(ns("kpi_change")),
                       shiny::uiOutput(ns("kpi_change_sub")),
                       showcase = bsicons::bs_icon("graph-up-arrow"), theme = "light", showcase_layout = "top right")
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
      chart_card(ns, "headline", height = "360px"),
      bslib::card(
        bslib::card_header(htmltools::tags$h2(class = "chart-title", "Key takeaways")),
        bslib::card_body(shiny::uiOutput(ns("takeaways"))),
        bslib::card_footer(htmltools::tags$p(class = "source-line",
          "Takeaways are written automatically from the data and update with the filters on the left."))
      )
    )
  )
}

overview_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    # Data for the current selection (one place, chosen programs, period).
    sel_df <- shiny::reactive({
      s <- sel()
      filter_selection(data, s$start, s$end, s$geography, s$programs)
    })

    # ---- Key numbers ----
    output$kpi_total <- shiny::renderText(fmt_n(kpi_total(sel_df())))
    output$kpi_total_sub <- shiny::renderUI({
      s <- sel()
      htmltools::tags$span(sprintf("%s, %s, %s\u2013%s", program_label(s$program_choice),
                                   s$geography, fmt_month_short(s$start), fmt_month_short(s$end)))
    })

    split <- shiny::reactive({ s <- sel(); kpi_split(data, s$start, s$end, s$geography) })
    # With both programs: the IMP / TFWP split. With one program: that
    # program's share of all work permits (so the card follows the filter).
    output$kpi_split_title <- shiny::renderText({
      s <- sel()
      if (length(s$programs) == 2) "Split: IMP vs TFWP"
      else paste(s$programs, "share of all work permits")
    })
    output$kpi_split <- shiny::renderText({
      sp <- split(); s <- sel()
      if (length(s$programs) == 2) {
        sprintf("%s / %s", fmt_share(sp$share[sp$program == "IMP"], 0),
                fmt_share(sp$share[sp$program == "TFWP"], 0))
      } else fmt_share(sp$share[sp$program == s$programs], 0)
    })
    output$kpi_split_sub <- shiny::renderUI({
      sp <- split(); s <- sel()
      htmltools::tags$span(if (length(s$programs) == 2) {
        sprintf("IMP %s \u00b7 TFWP %s (selected place and period)",
                fmt_n(sp$value[sp$program == "IMP"]), fmt_n(sp$value[sp$program == "TFWP"]))
      } else {
        sprintf("%s %s of %s TFWP + IMP permits (selected place and period)",
                s$programs, fmt_n(sp$value[sp$program == s$programs]), fmt_n(sum(sp$value)))
      })
    })

    latest <- shiny::reactive({ s <- sel(); kpi_latest(data, s$end, s$geography, s$programs) })
    output$kpi_latest_title <- shiny::renderText(paste("Latest month:", fmt_month(latest()$month)))
    output$kpi_latest <- shiny::renderText(fmt_n(latest()$value))
    output$kpi_latest_sub <- shiny::renderUI({
      lt <- latest()
      htmltools::tags$span(if (is.na(lt$change)) "No comparison available a year earlier"
                           else sprintf("%s vs %s (%s)", fmt_pct(lt$change),
                                        fmt_month_short(lt$prev_month), fmt_n(lt$prev_value)))
    })

    change <- shiny::reactive(kpi_period_change(sel_df()))
    output$kpi_change_title <- shiny::renderText(paste0("Change: ", change()$label))
    output$kpi_change <- shiny::renderText(fmt_pct(change()$change))
    output$kpi_change_sub <- shiny::renderUI(htmltools::tags$span(change()$text))

    # ---- Headline chart: stacked by program ----
    output$headline_title <- shiny::renderText({
      s <- sel(); trend_title(sel_df(), s$geography, s$program_choice)
    })
    output$headline_plot <- plotly::renderPlotly({
      s <- sel()
      ser <- summarise_series(sel_df(), s$resolution, by = "program",
                              latest_month = meta$latest_month)
      if (nrow(ser) == 0) return(empty_plot("No data for this selection"))
      ser$program <- factor(ser$program, levels = c("TFWP", "IMP"))
      ser <- ser[order(ser$program, ser$period), ]
      ser$hover <- sprintf("%s: %s%s", ser$program, fmt_n(ser$value),
                           ifelse(ser$partial, " (part-year)", ""))
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(ser, x = ~period, y = ~value, color = ~program,
                             colors = PROGRAM_COLOURS[c("TFWP", "IMP")],
                             type = "scatter", mode = "lines", stackgroup = "one",
                             text = ~hover, hoverinfo = "text", line = list(width = 1))
        style_plot(p, "Month the permit took effect", "Work permits (number per month)") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"))
      } else {
        ser$label <- factor(ser$label, levels = unique(ser$label[order(ser$period)]))
        p <- plotly::plot_ly(ser, x = ~label, y = ~value, color = ~program,
                             colors = PROGRAM_COLOURS[c("TFWP", "IMP")], type = "bar",
                             text = ~hover, hoverinfo = "text", textposition = "none",
                             marker = list(opacity = ifelse(ser$partial, 0.55, 1)))
        style_plot(p, "Year the permit took effect",
                   "Work permits (number per year)", x_type = "category") |>
          plotly::layout(barmode = "stack")
      }
    })
    output$headline_source <- shiny::renderUI({
      s <- sel()
      source_line(meta, extra = if (s$resolution == "Annual")
        "Lighter bars are part-years (e.g. year-to-date) and are not comparable with full years.")
    })

    # ---- Takeaways ----
    output$takeaways <- shiny::renderUI({
      s <- sel()
      tx <- make_takeaways(data, s$start, s$end, s$geography, s$program_choice)
      htmltools::tags$ul(class = "takeaways", lapply(tx, htmltools::tags$li))
    })
  })
}
