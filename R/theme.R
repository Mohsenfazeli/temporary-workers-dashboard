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
# One short line: source, release (or retrieval) date and the one caveat that
# applies everywhere. Chart-specific notes are one short sentence (`extra`);
# the details are in About the data.
data_date_text <- function(meta) {
  if (!is.null(meta$release_date) && identical(meta$source, "release"))
    paste0("release of ", fmt_date(meta$release_date), " (data to ", fmt_month(meta$latest_month), ")")
  else paste0("retrieved ", fmt_date(meta$retrieved_at))   # Windows-safe helper (R/calc.R)
}
source_line <- function(meta, extra = NULL) {
  # .noWS keeps the links tight against the brackets and commas
  a <- function(href, txt) htmltools::tags$a(href = href, target = "_blank", txt, .noWS = c("before", "after"))
  htmltools::tags$p(
    class = "source-line",
    htmltools::HTML(paste0(
      "Source: IRCC open data (", a(meta$urls[["TFWP"]], "TFWP"), ", ", a(meta$urls[["IMP"]], "IMP"),
      "), ", htmltools::htmlEscape(data_date_text(meta)), ". ",
      "Counts are permits issued, not people; IRCC hides counts of 0\u20135, shown here as 0. ")),
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
  date  = as.Date(c("2020-03-01", "2024-01-01", "2024-03-01", "2024-09-01",
                    "2024-11-01", "2025-01-01")),
  # Very short names, used to build one label when events share a year in
  # the annual view (e.g. "2024: study cap & TR target & TFWP limits & PGWP rules")
  tiny = c("COVID-19 travel limits", "study cap", "resident target", "TFWP limits",
           "graduate permit rules", "family permit limits"),
  short = c("COVID-19 travel limits (Mar 2020)", "Study permit cap (Jan 2024)",
            "Temporary resident target (Mar 2024)", "Low-wage TFWP limits (Sep 2024)",
            "Graduate work permit rules (Nov 2024)", "Family work permits narrowed (Jan 2025)"),
  label = c("March 2020: federal COVID-19 travel restrictions on foreign nationals take effect (March 18)",
            "January 2024: IRCC sets a two-year intake cap on new study permit applications (announced January 22)",
            "March 2024: federal government announces a target to lower temporary residents' share of the population",
            "September 2024: new limits on low-wage TFWP hiring take effect (September 26)",
            "November 2024: post-graduation work permit applicants need a minimum language level, and some college graduates a field of study linked to labour shortages (from November 1)",
            "January 2025: family open work permits limited to spouses of some students and skilled workers (from January 21)"),
  source = c("Prime Minister of Canada, news release, March 16, 2020",
             "IRCC, news release, January 22, 2024",
             "IRCC, Minister's announcement on temporary residents, March 21, 2024",
             "ESDC, news release, August 26, 2024",
             "IRCC, news release, September 18, 2024",
             "IRCC, notice, January 14, 2025"),
  url = c("https://www.pm.gc.ca/en/news/news-releases/2020/03/16/prime-minister-announces-new-actions-under-canadas-covid-19-response",
          "https://www.canada.ca/en/immigration-refugees-citizenship/news/2024/01/canada-to-stabilize-growth-and-decrease-number-of-new-international-student-permits-issued-to-approximately-360000-for-2024.html",
          "https://www.canada.ca/en/immigration-refugees-citizenship/news/2024/03/speaking-notes-for-the-honourable-marc-miller-minister-of-immigration-refugees-and-citizenship-announcement-related-to-temporary-residents.html",
          "https://www.canada.ca/en/employment-social-development/news/2024/08/minister-boissonnault-reducing-the-number-of-temporary-foreign-workers-in-canada.html",
          "https://www.canada.ca/en/immigration-refugees-citizenship/news/2024/09/strengthening-temporary-residence-programs-for-sustainable-volumes.html",
          "https://www.canada.ca/en/immigration-refugees-citizenship/news/notices/changes-open-work-permits-family-members-temporary-residents.html"),
  stringsAsFactors = FALSE
)

# Source for the note that the counts include people already in Canada:
# IRCC's levels plan says permit extensions and in-Canada permit applications
# are outside its new-arrival targets, and that PGWPs are treated as an
# extension or change of status, not new arrivals (table footnotes 2 and 3).
IN_CANADA_NOTE <- list(
  text = paste("These are permits issued, not people or new arrivals. They include permits for",
               "people already in Canada who renew a permit or switch to a new one, such as international",
               "students moving to a post-graduation work permit, so the counts can rise",
               "while new arrivals fall."),
  source = "IRCC, Supplementary Information for the 2026\u20132028 Immigration Levels Plan, table footnotes 2 and 3",
  url = "https://www.canada.ca/en/immigration-refugees-citizenship/corporate/immigration-system/levels/supplementary-immigration-levels-2026-2028.html"
)

# The event notes as HTML for a source line: each note linked to its source.
event_notes_html <- function(ev) {
  if (nrow(ev) == 0) return(NULL)
  # One short line: each short event name links to its official source; the
  # full wording and source titles are listed in About the data.
  esc <- htmltools::htmlEscape
  items <- sprintf('<a href="%s" target="_blank" title="%s">%s</a>',
                   esc(ev$url, attribute = TRUE), esc(paste0(ev$label, ". Source: ", ev$source), attribute = TRUE),
                   esc(ev$short))
  htmltools::HTML(paste0("Dotted lines mark policy changes, for context only; they do not imply cause (",
                         paste(items, collapse = "; "), ")."))
}
