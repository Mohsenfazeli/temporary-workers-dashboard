# =============================================================================
# R/mod_composition.R: Composition tab (mix of TFWP streams and IMP
# sub-programs, how the mix changed over time, and the IRCC program names
# inside each grouped category)
# =============================================================================
required_packages <- c("shiny", "bslib", "plotly", "DT")
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
    htmltools::div(class = "d-flex flex-wrap gap-4 align-items-center",
      shiny::radioButtons(ns("measure"), "Show the mix over time as",
                          choices = c("Share (%)" = "share", "Number of permits" = "number"),
                          selected = "share", inline = TRUE),
      # Shares of the permits whose program IRCC identifies (leaves out
      # "Unknown International Mobility Program"); off by default.
      shiny::checkboxInput(ns("known_only"), "IMP shares: leave out Unknown IMP", value = FALSE)),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      chart_card(ns, "share_tfwp", height = "470px"),
      chart_card(ns, "share_imp", height = "470px")
    ),
    chart_card(ns, "mix", height = "380px"),
    # IRCC's own program names inside each grouped category (selected place,
    # programs and period), so the grouping can be checked
    bslib::card(class = "chart-card",
      bslib::card_header(htmltools::tags$h3(shiny::textOutput(ns("detail_title"), inline = TRUE), class = "chart-title")),
      bslib::card_body(DT::DTOutput(ns("detail_table"))),
      bslib::card_footer(shiny::uiOutput(ns("detail_source")))),
    # Plain-language meaning of the program and category names (shared with About)
    bslib::card(class = "chart-card",
      bslib::card_header(htmltools::tags$h3("What the categories mean", class = "chart-title")),
      bslib::card_body(glossary_list()))
  )
}

composition_server <- function(id, sel, data, meta, detail = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    sel_df <- shiny::reactive({
      s <- sel(); filter_selection(data, s$start, s$end, s$geography, s$programs, s$last_moy)
    })
    period_mix <- shiny::reactive({
      s <- sel(); composition_period(filter_selection(data, s$start, s$end, s$geography, s$programs))
    })

    # Stacked shares (100%) or stacked numbers over time, one chart per program.
    share_plot <- function(prog) {
      s <- sel()
      as_number <- identical(input$measure, "number")
      if (!prog %in% s$programs) {
        return(empty_plot(sprintf("%s is not selected (see 'Program' in the sidebar)", prog)))
      }
      known <- prog == "IMP" && isTRUE(input$known_only) && !as_number
      df <- composition_shares(sel_df() |> dplyr::filter(program == prog),
                               s$resolution, meta$latest_month, known_only = known)
      df <- df[!is.na(df$share), ]            # periods with no permits at all
      if (nrow(df) == 0) return(empty_plot("No permits in this selection"))
      df <- df[order(df$period, df$category), ]
      df$category <- droplevels(df$category)
      df$hover <- sprintf("%s<br>%s: %s (%s permits)", df$label, df$category,
                          fmt_share(df$share), fmt_n(df$value))
      cols <- CATEGORY_COLOURS[levels(df$category)]
      # The plotted value: the share of the program, or the number of permits
      df$y <- if (as_number) df$value else df$share
      y_title <- if (as_number) "Work permits issued (number)"
                 else if (known) "Share of IMP permits, Unknown IMP left out (%)"
                 else "Share of the program's permits (%)"
      y_fmt <- if (as_number) ",d" else ".0%"
      if (s$resolution == "Monthly") {
        p <- plotly::plot_ly(df, x = ~period, y = ~y, color = ~category, colors = cols,
                             type = "scatter", mode = "lines", stackgroup = "one",
                             line = list(width = 0.5), text = ~hover, hoverinfo = "text")
        # No x-axis title: the years are self-explanatory and a title would
        # sit on the legend below the chart
        p <- style_plot(p, NULL, y_title, y_format = y_fmt,
                        hovermode = "closest") |>
          plotly::layout(xaxis = list(tickformat = "%Y", dtick = "M12"))
      } else {
        # Short axis labels (the months of a part year on a second line) so they fit without
        # rotating; the hover text keeps the full label with the months.
        # When every year covers the same months ("Show only Jan-Jul of each
        # year"), the months are said once in the source line, not under each bar.
        same <- length(unique(sub("^\\d{4}", "", df$label))) == 1 && grepl("(", df$label[1], fixed = TRUE)
        df$label <- if (same) sub(" \\(.*\\)$", "", df$label) else short_year_label(df$label)
        df$label <- factor(df$label, levels = unique(df$label[order(df$period)]))
        p <- plotly::plot_ly(df, x = ~label, y = ~y, color = ~category, colors = cols,
                             type = "bar", text = ~hover, hoverinfo = "text", textposition = "none")
        p <- style_plot(p, NULL, y_title,
                        y_format = y_fmt, hovermode = "closest", x_type = "category") |>
          plotly::layout(barmode = "stack")
      }
      p |> plotly::layout(yaxis = if (as_number) list(rangemode = "tozero") else list(range = c(0, 1)),
                          xaxis = list(tickangle = 0),
                          # Legend below the chart, clear of the (unrotated) axis labels
                          legend = list(orientation = "h", y = -0.14, x = 0, yanchor = "top",
                                        font = list(size = 11), traceorder = "reversed"),
                          margin = list(b = 40))
    }
    share_title <- function(prog) {
      chart_title("share", sel(), prog = prog, as_number = identical(input$measure, "number"),
                  known_only = prog == "IMP" && isTRUE(input$known_only))
    }
    output$share_tfwp_title <- shiny::renderText(share_title("TFWP"))
    output$share_imp_title  <- shiny::renderText(share_title("IMP"))
    output$share_tfwp_plot  <- plotly::renderPlotly(share_plot("TFWP"))
    output$share_imp_plot   <- plotly::renderPlotly(share_plot("IMP"))
    # One sentence when every year shows the same months (sidebar option)
    months_note <- function() { s <- sel()
      if (!is.null(s$last_moy)) sprintf("Every year shows January to %s only. ", month.name[s$last_moy]) else "" }
    output$share_tfwp_source <- shiny::renderUI(source_line(meta,
      extra = paste0(months_note(), "Streams grouped for this dashboard (IRCC's own names are in the table below).")))
    output$share_imp_source <- shiny::renderUI(source_line(meta,
      extra = paste0(months_note(), if (isTRUE(input$known_only) && !identical(input$measure, "number"))
        "Shares of IMP permits, leaving out Unknown IMP." else "Sub-programs grouped for this dashboard (IRCC's own names are in the table below).")))

    # Category totals for the whole period.
    output$mix_title <- shiny::renderText({
      chart_title("mix", sel())
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
      style_plot(p, "Work permits issued (number)", NULL, legend = FALSE,
                 hovermode = "closest") |>
        plotly::layout(margin = list(l = 330, r = 80),
                       xaxis = list(showgrid = TRUE, gridcolor = "#E5E5E5", tickformat = ",d"),
                       yaxis = list(tickformat = NULL))
    })
    output$mix_source <- shiny::renderUI(source_line(meta,
      extra = "Blue = TFWP, orange = IMP. Totals for the whole selected period."))

    # ---- IRCC program names inside each category ----
    detail_tbl <- shiny::reactive({
      shiny::req(!is.null(detail)); s <- sel()
      category_detail_table(detail, s$start, s$end, s$geography, s$programs)
    })
    output$detail_title <- shiny::renderText({
      chart_title("detail", sel())
    })
    output$detail_table <- DT::renderDT({
      DT::datatable(detail_tbl(), rownames = FALSE,
                    options = list(pageLength = 10, dom = "ftip", order = list(list(3, "desc")),
                                   columnDefs = list(list(className = "dt-right", targets = 3))),
                    class = "compact stripe") |>
        DT::formatRound("Permits", digits = 0, mark = ",")
    }, server = FALSE)   # client-side: small table (at most about 130 rows)
    output$detail_source <- shiny::renderUI(source_line(meta,
      extra = "IRCC program names as published (most detailed level given)."))

    output$unknown_note <- shiny::renderUI({
      s <- sel()
      if (!"IMP" %in% s$programs) return(NULL)
      htmltools::div(class = "alert alert-info caveat", role = "note",
        htmltools::tags$strong("Reading the IMP chart: "),
        # Figures for the selected geography, full calendar years (see
        # unknown_imp_note_text() in R/calc.R); updates when the geography changes.
        unknown_imp_note_text(data, as.character(s$geography), meta$latest_month))
    })
  })
}
