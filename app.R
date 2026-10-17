# =============================================================================
# app.R: Temporary Workers in Canada, interactive dashboard (R Shiny)
# -----------------------------------------------------------------------------
# Question 1, BC ECON 24R written assignment (REQ 125819).
# Run locally with: shiny::runApp()   (the IRCC data download automatically)
#
# Structure:
#   R/data.R        live download, cleaning, validation, cache (load_data)
#   R/calc.R        all calculations (testable, no Shiny code)
#   R/theme.R       colours, fonts, chart helpers, source lines
#   R/mod_*.R       one Shiny module per tab
# Shiny automatically sources every file in R/ before running this file.
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("shiny", "bslib", "bsicons", "plotly", "DT", "dplyr",
                       "tidyr", "readr", "readxl", "lubridate", "scales",
                       "shinyWidgets", "shinycssloaders", "htmltools")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

# ---- 1. Load the data ONCE when the app starts --------------------------------
# By default load_data() reads the two IRCC CSVs (and the Excel reference)
# live from IRCC. For testing on a machine that cannot reach IRCC, set the
# environment variable TW_LOCAL_DIR to a folder holding the same four files.
local_dir <- Sys.getenv("TW_LOCAL_DIR", "")
if (nzchar(local_dir)) {
  core_sources <- c(TFWP = file.path(local_dir, "ODP-TR-Work-TFWP-PT_program.csv"),
                    IMP  = file.path(local_dir, "ODP-TR-Work-IMP-PT_program.csv"))
  ref_sources  <- c(TFWP = file.path(local_dir, "EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx"),
                    IMP  = file.path(local_dir, "EN_ODP-TR-Work-IMP_PT_program_sign.xlsx"))
} else {
  core_sources <- IRCC_URLS
  ref_sources  <- IRCC_REFERENCE_URLS
}
bundle <- load_data(sources = core_sources, reference_sources = ref_sources,
                    cache_path = file.path("data", "cache.rds"),
                    timeout = as.numeric(Sys.getenv("TW_TIMEOUT", "60")),   # per download (server reachable)
                    tries = as.integer(Sys.getenv("TW_TRIES", "2")))
DATA <- bundle$data            # tidy table used by every tab
META <- bundle$meta            # retrieval time, latest month, source...
REF  <- bundle$reference       # IRCC distinct-people reference (may be NULL)

# Slider months, years and latest month all come from the data itself.
MONTHS        <- month_choices(DATA)
MONTH_LABELS  <- fmt_month_short(MONTHS)   # English labels on any computer
GEOGRAPHIES   <- c("Canada", PROVINCES_TERRITORIES)

# ---- 2. User interface ----------------------------------------------------------
status_bar <- htmltools::div(
  class = "status-bar", role = "status",
  htmltools::tags$span(htmltools::tags$strong("Data last retrieved: "),
                       fmt_datetime(META$retrieved_at)),
  " \u00b7 ",
  htmltools::tags$span(htmltools::tags$strong("Latest month available: "),
                       fmt_month(META$latest_month)),
  " \u00b7 ",
  htmltools::tags$span("Source: IRCC open data"),
  if (identical(META$source, "cache")) htmltools::div(
    class = "alert alert-warning py-1 px-2 my-1", role = "alert",
    htmltools::tags$strong("Using saved data. "),
    # add a full stop after the problem text if it does not end with one
    sub("([^.])$", "\\1.", META$warning), " ", META$cache_note)
)

sidebar_controls <- bslib::sidebar(
  width = 290, id = "sidebar", title = "Filters (apply to every tab)",
  # Open on desktop; on a phone the filters sit above the charts.
  open = list(desktop = "open", mobile = "always-above"),
  shinyWidgets::sliderTextInput(
    "range", "Period (month the permit took effect)",
    choices = MONTH_LABELS, selected = c(MONTH_LABELS[1], tail(MONTH_LABELS, 1)),
    grid = FALSE, force_edges = TRUE, width = "100%"),
  shiny::radioButtons("resolution", "View", choices = c("Monthly", "Annual"),
                      selected = "Annual", inline = TRUE),
  shiny::selectInput("geography", "Geography", choices = GEOGRAPHIES, selected = "Canada"),
  shiny::radioButtons("program", "Program",
                      choices = c("Both programs" = "Both", "TFWP only" = "TFWP",
                                  "IMP only" = "IMP"), selected = "Both"),
  shiny::actionButton("reset", "Reset filters", icon = shiny::icon("rotate-left"),
                      class = "btn-outline-primary"),
  htmltools::tags$div(class = "sidebar-help",
    htmltools::tags$p(htmltools::tags$strong("TFWP"), ": Temporary Foreign Worker Program. The employer usually needs a Labour Market Impact Assessment (LMIA) showing no Canadian is available."),
    htmltools::tags$p(htmltools::tags$strong("IMP"), ": International Mobility Program. No LMIA needed, e.g. graduates, spouses, working holiday, trade agreements. IRCC's IMP table excludes people here mainly for other reasons (e.g. asylum claimants)."),
    htmltools::tags$p(paste0("Figures count work permits that took effect. ",
                      format(META$latest_month, "%Y"), " is year-to-date to ",
                      month.name[as.integer(format(META$latest_month, "%m"))], ".")))
)

ui <- bslib::page_navbar(
  id = "nav",
  title = paste0("Temporary Workers in Canada, ", format(META$first_month, "%Y"),
                 "\u2013", format(META$latest_month, "%Y")),
  window_title = "Temporary Workers in Canada: TFWP and IMP work permits",
  theme = app_theme,
  lang = "en",
  fillable = FALSE,          # let pages scroll; cards keep their full height
  sidebar = sidebar_controls,
  header = htmltools::tagList(
    htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", href = "styles.css")),
    status_bar),
  bslib::nav_panel("Overview", overview_ui("overview")),
  bslib::nav_panel("Trends", trends_ui("trends")),
  bslib::nav_panel("Geography", geography_ui("geography")),
  bslib::nav_panel("Composition", composition_ui("composition")),
  bslib::nav_panel("Data", data_ui("data")),
  bslib::nav_panel("About the data", about_ui("about"))
)

# ---- 3. Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # One reactive "selection" shared by every tab.
  sel <- shiny::reactive({
    shiny::req(input$range, input$resolution, input$geography, input$program)
    idx <- match(input$range, MONTH_LABELS)            # labels -> dates
    list(start = MONTHS[min(idx)], end = MONTHS[max(idx)],
         resolution = input$resolution, geography = input$geography,
         program_choice = input$program,
         programs = programs_from_choice(input$program))
  })

  # Reset button: back to the full period, Canada, both programs, annual.
  shiny::observeEvent(input$reset, {
    shinyWidgets::updateSliderTextInput(session, "range",
      selected = c(MONTH_LABELS[1], tail(MONTH_LABELS, 1)))
    shiny::updateRadioButtons(session, "resolution", selected = "Annual")
    shiny::updateSelectInput(session, "geography", selected = "Canada")
    shiny::updateRadioButtons(session, "program", selected = "Both")
  })

  overview_server("overview", sel, DATA, META)
  trends_server("trends", sel, DATA, META, REF)
  geography_server("geography", sel, DATA, META)
  composition_server("composition", sel, DATA, META)
  data_server("data", sel, DATA, META)
  about_server("about", DATA, META, REF)
}

shiny::shinyApp(ui, server)
