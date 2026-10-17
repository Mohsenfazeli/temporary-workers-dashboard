# =============================================================================
# R/theme.R: look and feel (colours, fonts, chart helpers, source lines)
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("bslib", "plotly", "htmltools")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(bslib)
  library(plotly)
  library(htmltools)
})

# ---- 1. Colours ---------------------------------------------------------------
# Okabe-Ito palette: designed to be distinguishable with common forms of
# colour-vision deficiency. One accent colour per program.
PROGRAM_COLOURS <- c(TFWP = "#0072B2",   # blue
                     IMP  = "#D55E00",   # vermillion
                     Total = "#3A3A3A")  # dark grey

# Category colours (TFWP and IMP each use distinct Okabe-Ito hues; the
# "Unknown" category is neutral grey so it does not draw the eye).
# Category colours: Okabe-Ito hues darkened so every colour has at least
# 3:1 contrast against the white background (WCAG 1.4.11; the lightest is
# 3.9:1), with lightness varied so neighbouring categories stay distinct for
# readers with red-green colour blindness (checked by simulation: the
# closest pair under deuteranopia is more distinct than with the original
# Okabe-Ito colours). The original yellow (#F0E442) was only 1.3:1.
CATEGORY_COLOURS <- c(
  "Seasonal Agricultural Worker Program"        = "#00845F",  # green
  "Other agricultural workers"                  = "#2F80C0",  # mid blue
  "Caregivers"                                  = "#C0559A",  # pink
  "Other TFWP workers with an LMIA"             = "#0B3D6B",  # navy
  "Other TFWP"                                  = "#6B6B6B",  # grey
  "Post-graduation work permits"                = "#C46A00",  # dark orange
  "Spouses and family members"                  = "#2F80C0",  # mid blue
  "International Experience Canada"             = "#00845F",  # green
  "Significant benefit and PR-bridging permits" = "#0B3D6B",  # navy
  "Trade and other agreements"                  = "#C0559A",  # pink
  "Other Canadian interests"                    = "#8A2B0E",  # brick red
  "Vulnerable workers"                          = "#6A5ACD",  # violet
  "Unknown or other IMP"                        = "#6B6B6B"   # grey
)

GOV_BLUE <- "#003366"   # BC Government primary blue
GOV_GOLD <- "#FCBA19"   # BC Government accent gold

# ---- 2. Bootstrap theme -------------------------------------------------------
app_theme <- bslib::bs_theme(
  version = 5,
  primary = GOV_BLUE,
  "navbar-bg" = GOV_BLUE,
  base_font = "'BC Sans', 'Noto Sans', 'Segoe UI', Verdana, Arial, sans-serif",
  heading_font = "'BC Sans', 'Noto Sans', 'Segoe UI', Verdana, Arial, sans-serif",
  font_scale = 0.95,
  "border-radius" = "0.25rem"
)

# ---- 3. Plotly helpers --------------------------------------------------------
# Common layout for every chart: readable fonts, thousands separators,
# unified hover, axis titles with units, and a trimmed toolbar.
style_plot <- function(p, x_title = NULL, y_title = NULL, y_format = ",d",
                       legend = TRUE, hovermode = "x unified",
                       x_type = "-") {
  # x_type = "category" for annual charts: stops plotly reading labels such
  # as "2015" as numbers (which would drop "2026 YTD (Jan-Jul)").
  p |>
    plotly::layout(
      separators = ".,",
      font = list(family = "Noto Sans, Segoe UI, Verdana, Arial, sans-serif",
                  size = 13, color = "#222222"),
      # automargin: plotly grows the margins so labels and titles are never cut off
      xaxis = list(title = list(text = x_title, standoff = 10), showgrid = FALSE, type = x_type,
                   automargin = TRUE,
                   zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(title = list(text = y_title, standoff = 10), tickformat = y_format,
                   automargin = TRUE,
                   gridcolor = "#E5E5E5", zeroline = TRUE,
                   zerolinecolor = "#BBBBBB", fixedrange = TRUE,
                   rangemode = "tozero"),
      hovermode = hovermode,
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.12, xanchor = "left"),
      margin = list(l = 70, r = 20, t = 40, b = 60),
      paper_bgcolor = "white", plot_bgcolor = "white"
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d",
                     "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d",
                     "autoScale2d", "hoverClosestCartesian",
                     "hoverCompareCartesian", "toggleSpikelines"),
                   toImageButtonOptions = list(format = "png", scale = 2))
}

# An empty chart with a message (used when a selection has nothing to show).
empty_plot <- function(message) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = message, showarrow = FALSE,
                              font = list(size = 15, color = "#555555"),
                              x = 0.5, y = 0.5, xref = "paper", yref = "paper"))) |>
    plotly::config(displayModeBar = FALSE)
}

# ---- 4. Source line under every chart ------------------------------------------
source_line <- function(meta, extra = NULL, reference = FALSE) {
  htmltools::tags$p(
    class = "source-line",
    "Source: Immigration, Refugees and Citizenship Canada (IRCC), open data: ",
    htmltools::tags$a(href = meta$urls[["TFWP"]], target = "_blank", "TFWP"),
    " and ",
    htmltools::tags$a(href = meta$urls[["IMP"]], target = "_blank", "IMP"),
    # paste0() keeps the date and the full stop together (no stray space)
    paste0(" work permit holders by province/territory, program and month the permit took effect; retrieved ",
           fmt_date(meta$retrieved_at), ". "),   # Windows-safe helper (R/calc.R)
    if (reference) htmltools::tagList(
      "Reference: IRCC Excel tables, ",
      htmltools::tags$a(href = meta$reference_urls[["TFWP"]], target = "_blank", "TFWP"),
      " and ",
      # .noWS = "after": no space between the link and the full stop
      htmltools::tags$a(href = meta$reference_urls[["IMP"]], target = "_blank", "IMP", .noWS = "after"),
      ". "),
    "Values of 0\u20135 are suppressed by IRCC and counted as 0. ",
    extra
  )
}

# ---- 5. Card with a point-stating title, a chart and a source line -------------
chart_card <- function(ns, id, height = "380px", footer = NULL, full_screen = TRUE) {
  bslib::card(
    full_screen = full_screen,
    bslib::card_header(htmltools::tags$h2(class = "chart-title",
                                          shiny::textOutput(ns(paste0(id, "_title")), inline = TRUE))),
    bslib::card_body(
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns(paste0(id, "_plot")), height = height),
        type = 7, color = GOV_BLUE, size = 0.6),
      padding = c(8, 12)),
    bslib::card_footer(shiny::uiOutput(ns(paste0(id, "_source"))), footer)
  )
}

# ---- 6. Neutral notes for key events (Trends tab) --------------------------------
# Context only: dated, worded neutrally, each with an official source.
# `short` is drawn on the chart; `label` is the full note under the chart.
KEY_EVENTS <- data.frame(
  date  = as.Date(c("2020-03-01", "2024-03-01", "2024-09-01")),
  # Very short names, used to build one label when two events share a year
  # in the annual view (e.g. "2024: temp. resident target & TFWP limits")
  tiny = c("COVID-19 travel limits", "temp. resident target", "TFWP limits"),
  short = c("COVID-19 travel limits (Mar 2020)", "Temporary resident target (Mar 2024)",
            "Low-wage TFWP limits (Sep 2024)"),
  label = c("March 2020: federal COVID-19 travel restrictions on foreign nationals take effect (March 18)",
            "March 2024: federal government announces a target to lower temporary residents' share of the population",
            "September 2024: new limits on low-wage TFWP hiring take effect (September 26)"),
  source = c("Prime Minister of Canada, news release, March 16, 2020",
             "IRCC, Minister's announcement on temporary residents, March 21, 2024",
             "ESDC, news release, August 26, 2024"),
  url = c("https://www.pm.gc.ca/en/news/news-releases/2020/03/16/prime-minister-announces-new-actions-under-canadas-covid-19-response",
          "https://www.canada.ca/en/immigration-refugees-citizenship/news/2024/03/speaking-notes-for-the-honourable-marc-miller-minister-of-immigration-refugees-and-citizenship-announcement-related-to-temporary-residents.html",
          "https://www.canada.ca/en/employment-social-development/news/2024/08/minister-boissonnault-reducing-the-number-of-temporary-foreign-workers-in-canada.html"),
  stringsAsFactors = FALSE
)

# The event notes as HTML for a source line: each note linked to its source.
event_notes_html <- function(ev) {
  if (nrow(ev) == 0) return(NULL)
  # Built as one HTML string so no stray spaces appear around the links
  esc <- htmltools::htmlEscape
  items <- sprintf('%s (<a href="%s" target="_blank">%s</a>)',
                   esc(ev$label), esc(ev$url, attribute = TRUE), esc(ev$source))
  htmltools::HTML(paste0("Dotted lines mark events for context only; they do not imply cause: ",
                         paste(items, collapse = "; "), "."))
}
