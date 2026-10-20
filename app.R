# =============================================================================
# app.R: Temporary Workers in Canada, interactive dashboard (R Shiny)
# -----------------------------------------------------------------------------
# Run locally with: shiny::runApp()   (the IRCC open data release of September 15,
# 2026 (data to July 2026) is included in data/baseline.rds; no download needed)
#
# Structure:
#   R/data.R        the included IRCC release (load_release); optional live
#                   download, cleaning, checks against the release, cache (load_data)
#   R/calc.R        all calculations (testable, no Shiny code)
#   R/theme.R       colours, fonts, chart helpers, source lines
#   R/mod_*.R       one Shiny module per tab
#   R/assistant.R   Help tab assistant (optional; needs GEMINI_API_KEY)
# Shiny automatically sources every file in R/ before running this file.
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("shiny", "bslib", "bsicons", "plotly", "DT", "dplyr",
                       "tidyr", "readr", "lubridate", "scales",
                       "shinyWidgets", "shinycssloaders", "htmltools",
                       "httr2", "commonmark")   # Help assistant (R/assistant.R)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(lubridate)   # %m-% for the quick period buttons
})

# ---- 1. Load the data ONCE when the app starts --------------------------------
# Default: the frozen, verified IRCC release shipped with the app
# (data/baseline.rds, IRCC open data release of September 15, 2026), so every
# visitor sees the same checked numbers and the page never depends on IRCC's
# server. Options (environment variables, for maintainers):
#   TW_DATA_SOURCE = "live"  read the two IRCC CSVs live; a download must pass the baseline check or
#                            the saved copy is shown with a notice (R/data.R)
#   TW_LOCAL_DIR = <folder>  read the same two files from a local folder
#                            (testing on a machine that cannot reach IRCC)
local_dir   <- Sys.getenv("TW_LOCAL_DIR", "")
data_source <- tolower(Sys.getenv("TW_DATA_SOURCE", "release"))
read_files <- function(core_sources) {
  load_data(sources = core_sources,
            cache_path = file.path("data", "cache.rds"),
            # verified copy: new downloads must match it for settled
            # months, or the app keeps its saved data (see R/data.R)
            baseline_path = file.path("data", "baseline.rds"),
            timeout = as.numeric(Sys.getenv("TW_TIMEOUT", "60")),   # per download (server reachable)
            tries = as.integer(Sys.getenv("TW_TRIES", "2")))
}
bundle <- if (nzchar(local_dir)) {
  read_files(c(TFWP = file.path(local_dir, "ODP-TR-Work-TFWP-PT_program.csv"),
               IMP  = file.path(local_dir, "ODP-TR-Work-IMP-PT_program.csv")))
} else if (identical(data_source, "live")) {
  read_files(IRCC_URLS)
} else {
  load_release(file.path("data", "baseline.rds"))
}
DATA <- bundle$data            # tidy table used by every tab
META <- bundle$meta            # retrieval time, latest month, source...

# Help assistant: its instructions and context (the dashboard's own text and
# key figures from these data), built once (R/assistant.R). It works only if
# the GEMINI_API_KEY environment variable is set; otherwise the Help tab says
# the assistant is not available and nothing else changes.
ASSISTANT_CONFIG <- assistant_config()
ASSISTANT_PROMPT <- if (ASSISTANT_CONFIG$has_key) assistant_system_prompt(DATA, META) else ""

# Slider months, years and latest month all come from the data itself.
MONTHS        <- month_choices(DATA)
MONTH_LABELS  <- fmt_month_short(MONTHS)   # English labels on any computer
GEOGRAPHIES   <- c("Canada", PROVINCES_TERRITORIES)

# ---- 2. User interface ----------------------------------------------------------
status_bar <- htmltools::div(
  class = "status-bar", role = "status",
  if (identical(META$source, "release")) {
    # Default: the frozen release, labelled plainly with IRCC's release date
    htmltools::tags$span(htmltools::tags$strong("Data: "),
      META$release_phrase,   # e.g. "IRCC open data release of September 15, 2026 (data to July 2026)"
      # What the dashboard counts, said once at the top
      " \u00b7 Counts work permits issued, including renewals, not individual workers.")
  } else {
    htmltools::tags$span(htmltools::tags$strong("Data last retrieved: "),
                         fmt_datetime(META$retrieved_at))
  },
  # The release phrase already names IRCC and the latest month, so the
  # latest month and source are added only for a live read or saved copy.
  if (!identical(META$source, "release")) htmltools::tagList(
    " \u00b7 ",
    htmltools::tags$span(htmltools::tags$strong("Latest month available: "),
                         fmt_month(META$latest_month)),
    " \u00b7 ",
    htmltools::tags$span("Source: IRCC open data")),
  if (identical(META$source, "cache")) htmltools::div(
    class = "alert alert-warning py-1 px-2 my-1", role = "alert",
    htmltools::tags$strong("Using saved data. "),
    # add a full stop after the problem text if it does not end with one
    sub("([^.])$", "\\1.", META$warning), " ", META$cache_note)
)

# Months of the latest (partial) year, e.g. "Jan\u2013Jul", for the
# "same months each year" option in the annual view.
LATEST_M   <- as.integer(format(META$latest_month, "%m"))
YTD_MONTHS <- if (LATEST_M == 1) "Jan" else paste0("Jan\u2013", month.abb[LATEST_M])

sidebar_controls <- bslib::sidebar(
  width = 290, id = "sidebar", title = "Filters (apply to every tab)",
  # Open on desktop; on a phone the filters start closed behind the
  # "Filters" button, so the key numbers and charts come first.
  open = list(desktop = "open", mobile = "closed"),
  shinyWidgets::sliderTextInput(
    "range", "Period (month the permit was issued)",
    choices = MONTH_LABELS, selected = c(MONTH_LABELS[1], tail(MONTH_LABELS, 1)),
    grid = FALSE, force_edges = TRUE, width = "100%"),
  # Quick periods: set the slider in one click
  htmltools::div(class = "quick-periods", role = "group", `aria-label` = "Quick periods",
    shiny::actionButton("qp_full", "Full period", class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("qp_12m", "Last 12 months", class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("qp_ytd", paste(format(META$latest_month, "%Y"), "so far"), class = "btn-sm btn-outline-secondary"),
    shiny::actionButton("qp_2019", "Since 2019", class = "btn-sm btn-outline-secondary")),
  shiny::radioButtons("resolution", "View", choices = c("Monthly", "Annual"),
                      selected = "Annual", inline = TRUE),
  # Annual view only: compare the same months of every year (e.g. Jan-Jul),
  # so the latest part-year is not read against full years.
  if (LATEST_M < 12) shiny::conditionalPanel("input.resolution == 'Annual'",
    shiny::checkboxInput("same_months",
      sprintf("Show only %s of each year, to compare fairly with %s", YTD_MONTHS, format(META$latest_month, "%Y")),
      value = FALSE)),
  shiny::selectInput("geography", "Geography", choices = GEOGRAPHIES, selected = "Canada"),
  shiny::radioButtons("program", "Program",
                      choices = c("Both programs" = "Both", "TFWP only" = "TFWP",
                                  "IMP only" = "IMP"), selected = "Both"),
  shiny::actionButton("reset", "Reset filters", icon = shiny::icon("rotate-left"),
                      class = "btn-outline-primary"),
  # Definitions folded away (one click to open); the full glossary is in
  # About the data.
  htmltools::tags$details(class = "sidebar-help",
    htmltools::tags$summary("What TFWP, IMP and the counts mean"),
    htmltools::tags$p(htmltools::tags$strong("TFWP (Temporary Foreign Worker Program)", .noWS = "after"),
      ": an employer hires a foreign worker for a job, usually after a government check, the Labour Market Impact Assessment (LMIA), shows no Canadian worker is available."),
    htmltools::tags$p(htmltools::tags$strong("IMP (International Mobility Program)", .noWS = "after"),
      ": work permits that do not need that check, for example for graduates of Canadian colleges and universities, spouses of workers or students, working holidays and trade agreements. ",
      "Not included: permits for people in Canada mainly for other reasons, such as asylum claimants (IRCC leaves them out of this table)."),
    htmltools::tags$p(htmltools::tags$strong("What is counted", .noWS = "after"),
      paste0(": work permits issued each month, including renewals. These are permits, not people. ",
             format(META$latest_month, "%Y"), " has data for January to ",
             month.name[as.integer(format(META$latest_month, "%m"))], " only.")))
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
    htmltools::tags$head(htmltools::tags$link(rel = "stylesheet", href = "styles.css"),
      # "More in About the data" links under the charts open that tab
      htmltools::tags$script(htmltools::HTML(
        "$(document).on('click', 'a.about-link', function(e) { e.preventDefault();",
        "  var t = document.querySelector('.navbar a[data-value=\"About the data\"]');",
        "  if (t) { t.click(); window.scrollTo(0, 0); } });"))),
    status_bar),
  bslib::nav_panel("Overview", overview_ui("overview")),
  bslib::nav_panel("Trends", trends_ui("trends")),
  bslib::nav_panel("Geography", geography_ui("geography")),
  bslib::nav_panel("Composition", composition_ui("composition")),
  bslib::nav_panel("Data", data_ui("data")),
  bslib::nav_panel("About the data", about_ui("about")),
  bslib::nav_panel("Help", help_ui("help"))
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
         programs = programs_from_choice(input$program),
         # January to the latest month in every year, or NULL (option off)
         last_moy = same_months_limit(input$same_months, input$resolution, META$latest_month))
  })

  # Quick period buttons: move the slider like a user would.
  set_period <- function(from, to) shinyWidgets::updateSliderTextInput(session, "range",
    selected = c(MONTH_LABELS[match(from, MONTHS)], MONTH_LABELS[match(to, MONTHS)]))
  last_m <- max(MONTHS)
  shiny::observeEvent(input$qp_full, set_period(min(MONTHS), last_m))
  shiny::observeEvent(input$qp_12m,  set_period(last_m %m-% months(11), last_m))
  shiny::observeEvent(input$qp_ytd,  set_period(lubridate::floor_date(last_m, "year"), last_m))
  shiny::observeEvent(input$qp_2019, set_period(max(min(MONTHS), as.Date("2019-01-01")), last_m))

  # Reset button: back to the full period, Canada, both programs, annual.
  shiny::observeEvent(input$reset, {
    shinyWidgets::updateSliderTextInput(session, "range",
      selected = c(MONTH_LABELS[1], tail(MONTH_LABELS, 1)))
    shiny::updateRadioButtons(session, "resolution", selected = "Annual")
    shiny::updateSelectInput(session, "geography", selected = "Canada")
    shiny::updateRadioButtons(session, "program", selected = "Both")
    shiny::updateCheckboxInput(session, "same_months", value = FALSE)
  })

  overview_server("overview", sel, DATA, META)   # also shows "What the data show"
  trends_server("trends", sel, DATA, META)
  geography_server("geography", sel, DATA, META)
  composition_server("composition", sel, DATA, META, bundle$detail)
  data_server("data", sel, DATA, META)
  about_server("about", DATA, META)
  help_server("help", sel, ASSISTANT_PROMPT, ASSISTANT_CONFIG)
}

shiny::shinyApp(ui, server)
