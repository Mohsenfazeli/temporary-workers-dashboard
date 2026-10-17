# =============================================================================
# R/mod_composition.R: Composition tab (mix of TFWP streams and IMP
# sub-programs, and how the mix changed over time)
# =============================================================================
required_packages <- c("shiny", "bslib", "plotly")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

composition_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("unknown_note")),
    # Show the mix as shares (each program adds to 100%) or as numbers of permits
    shiny::radioButtons(ns("measure"), "Show the mix over time as",
                        choices = c("Share (%)" = "share", "Number of permits" = "number"),
                        selected = "share", inline = TRUE),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      chart_card(ns, "share_tfwp", height = "470px"),
      chart_card(ns, "share_imp", height = "470px")
    ),
    chart_card(ns, "mix", height = "380px"),
    # Plain-language meaning of the program and category names (shared with About)
    bslib::card(class = "chart-card",
      bslib::card_header(htmltools::tags$h3("What the categories mean", class = "chart-title")),
      bslib::card_body(glossary_list()))
  )
}

composition_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    sel_df <- shiny::reactive({
      s <- sel(); filter_selection(data, s$start, s$end, s$geography, s$programs)
    })
    period_mix <- shiny::reactive(composition_period(sel_df()))

    # Stacked shares (100%) or stacked numbers over time, one chart per program.
    share_plot <- function(prog) {
      s <- sel()
      as_number <- identical(input$measure, "number")
      if (!prog %in% s$programs) {
        return(empty_plot(sprintf("%s is not selected (see 'Program' in the sidebar)", prog)))
      }
      df <- composition_shares(sel_df() |> dplyr::filter(program == prog),
                               s$resolution, meta$latest_month)
      df <- df[!is.na(df$share), ]            # periods with no permits at all
      if (nrow(df) == 0) return(empty_plot("No permits in this selection"))
      df <- df[order(df$period, df$category), ]
      df$category <- droplevels(df$category)
      df$hover <- sprintf("%s<br>%s: %s (%s permits)", df$label, df$category,
                          fmt_share(df$share), fmt_n(df$value))
      cols <- CATEGORY_COLOURS[levels(df$category)]
      # The plotted value: the share of the program, or the number of permits
      df$y <- if (as_number) df$value else df$share
      y_title <- if (as_number) "Work permits that took effect (number)" else "Share of the program's permits (%)"
      y_fmt <- if (as_number) ",d" else ".0%"
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(df, x = ~period, y = ~y, color = ~category, colors = cols,
                             type = "scatter", mode = "lines", stackgroup = "one",
                             line = list(width = 0.5), text = ~hover, hoverinfo = "text")
        p <- style_plot(p, "Month", y_title, y_format = y_fmt,
                        hovermode = "closest") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"))
      } else {
        df$label <- factor(df$label, levels = unique(df$label[order(df$period)]))
        p <- plotly::plot_ly(df, x = ~label, y = ~y, color = ~category, colors = cols,
                             type = "bar", text = ~hover, hoverinfo = "text", textposition = "none")
        p <- style_plot(p, NULL, y_title,
                        y_format = y_fmt, hovermode = "closest", x_type = "category") |>
          plotly::layout(barmode = "stack")
      }
      p |> plotly::layout(yaxis = if (as_number) list(rangemode = "tozero") else list(range = c(0, 1)),
                          xaxis = list(tickangle = -45),
                          legend = list(orientation = "h", y = -0.3, x = 0, yanchor = "top",
                                        font = list(size = 11), traceorder = "reversed"),
                          margin = list(b = 40))
    }
    share_title <- function(prog) {
      s <- sel()
      if (!prog %in% s$programs) return(sprintf("%s mix over time", prog))
      m <- period_mix() |> dplyr::filter(program == prog)
      if (nrow(m) == 0 || sum(m$value) == 0) return(sprintf("%s mix over time", prog))
      sprintf("%s: %s was the largest group in the period (%s)", prog,
              as.character(m$category[1]), fmt_share(m$share[1], 0))
    }
    output$share_tfwp_title <- shiny::renderText(share_title("TFWP"))
    output$share_imp_title  <- shiny::renderText(share_title("IMP"))
    output$share_tfwp_plot  <- plotly::renderPlotly(share_plot("TFWP"))
    output$share_imp_plot   <- plotly::renderPlotly(share_plot("IMP"))
    output$share_tfwp_source <- shiny::renderUI(source_line(meta,
      extra = "TFWP streams grouped by the author; mapping in About the data."))
    output$share_imp_source <- shiny::renderUI(source_line(meta,
      extra = "IMP sub-programs grouped by the author; mapping in About the data."))

    # Category totals for the whole period.
    output$mix_title <- shiny::renderText({
      s <- sel()
      m <- period_mix()
      if (nrow(m) == 0 || sum(m$value) == 0)
        return(sprintf("Permits by category, %s\u2013%s, %s", fmt_month_short(s$start),
                       fmt_month_short(s$end), s$geography))
      top <- m[which.max(m$value), ]
      # Title states the point: the largest category and its share of all permits.
      sprintf("%s (%s) was the largest category: %s of permits, %s\u2013%s, %s",
              top$category, top$program, fmt_share(top$value / sum(m$value), 0),
              fmt_month_short(s$start), fmt_month_short(s$end), s$geography)
    })
    output$mix_plot <- plotly::renderPlotly({
      m <- period_mix()
      if (nrow(m) == 0 || sum(m$value) == 0) return(empty_plot("No data for this selection"))
      m <- m[m$value > 0 | m$category != "Other TFWP", ]
      m <- m[order(m$value), ]
      m$name <- paste0(m$category, " (", m$program, ")")
      m$name <- factor(m$name, levels = m$name)
      m$hover <- sprintf("%s<br>%s permits, %s of %s", m$name, fmt_n(m$value),
                         fmt_share(m$share), m$program)
      p <- plotly::plot_ly(m, y = ~name, x = ~value, type = "bar", orientation = "h",
                           marker = list(color = unname(PROGRAM_COLOURS[m$program])),
                           text = ~fmt_n(value), textposition = "outside", cliponaxis = FALSE,
                           hovertext = ~hover, hoverinfo = "text")
      style_plot(p, "Work permits that took effect (number)", NULL, legend = FALSE,
                 hovermode = "closest") |>
        plotly::layout(margin = list(l = 330, r = 80),
                       xaxis = list(showgrid = TRUE, gridcolor = "#E5E5E5", tickformat = ",d"),
                       yaxis = list(tickformat = NULL))
    })
    output$mix_source <- shiny::renderUI(source_line(meta,
      extra = "Bar colour shows the program: blue = TFWP, vermillion = IMP."))

    output$unknown_note <- shiny::renderUI({
      s <- sel()
      if (!"IMP" %in% s$programs) return(NULL)
      htmltools::div(class = "alert alert-info caveat", role = "note",
        htmltools::tags$strong("Reading the IMP mix: "),
        # The figures in this note are computed from the data (Canada, full years).
        {
          u <- data[data$geography == "Canada" & data$category == "Unknown or other IMP", ]
          by_year <- tapply(u$value, u$year, sum)
          full <- by_year[names(by_year) != format(meta$latest_month, "%Y")]
          peak <- names(full)[which.max(full)]
          sprintf("IRCC records a growing number of IMP permits as \"Unknown International Mobility Program\" (%s in 2015\u20132020 combined; a peak of %s in %s). ",
                  fmt_n(sum(full[names(full) <= "2020"])), fmt_n(max(full)), peak)
        },
        "Part of the shift in the IMP mix after 2021 reflects how permits were recorded, not only real change.")
    })
  })
}
