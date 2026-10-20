# =============================================================================
# R/mod_data.R: Data tab (filterable table of the current selection + CSV)
# =============================================================================
required_packages <- c("shiny", "bslib", "DT", "readr")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE, fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      htmltools::tags$h2(class = "chart-title", shiny::textOutput(ns("title"), inline = TRUE)),
      shiny::downloadButton(ns("download"), "Download CSV", class = "btn-primary btn-sm")
    ),
    bslib::card_body(DT::DTOutput(ns("table")), fillable = FALSE, fill = FALSE),
    bslib::card_footer(shiny::uiOutput(ns("source")))
  )
}

data_server <- function(id, sel, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {

    tbl <- shiny::reactive({
      s <- sel()
      table_for_download(filter_selection(data, s$start, s$end, s$geography, s$programs),
                         s$resolution, meta$latest_month, s$geography)
    })
    output$title <- shiny::renderText({
      chart_title("data", sel())
    })
    output$table <- DT::renderDT({
      t <- tbl()
      t$Period <- factor(t$Period, levels = unique(t$Period))   # drop-down filters
      t$Program <- factor(t$Program); t$Category <- factor(t$Category)
      t$Geography <- factor(t$Geography)
      # Show numbers with thousands separators as text; DataTables still
      # sorts them as numbers ("num-fmt"), and the column filter becomes a
      # simple text box. The CSV download keeps the raw numbers.
      num_cols <- c("Work permits issued", "Months included",
                    "Hidden small counts (counted as 0)")
      t[num_cols] <- lapply(t[num_cols], fmt_n)
      # Shorter column headers on screen so all eight columns fit at 1366 px
      # (the CSV download keeps the full names); the source line explains them.
      DT::datatable(t, rownames = FALSE, filter = "top", selection = "none",
                    colnames = c("Period", "Geography", "Program", "Category",
                                 "Work permits issued", "Months", "Hidden small counts",
                                 "Possible undercount (>10%)"),
                    options = list(pageLength = 15, scrollX = TRUE,
                                   columnDefs = list(list(type = "num-fmt", targets = 4:6),
                                                     list(className = "dt-right", targets = 4:6)),
                                   language = list(search = "Search table:")))
    }, server = FALSE)   # client-side: small table, correct numeric sorting
    output$download <- shiny::downloadHandler(
      filename = function() {
        s <- sel()
        sprintf("temporary_workers_%s_%s_%s_%s_to_%s.csv", tolower(s$resolution),
                gsub("[^A-Za-z]+", "_", s$geography), s$program_choice,
                format(s$start, "%Y%m"), format(s$end, "%Y%m"))
      },
      content = function(file) readr::write_csv(tbl(), file)
    )
    output$source <- shiny::renderUI(source_line(meta,
      extra = "'Hidden small counts' = how many of IRCC's counts in this figure are hidden for privacy (IRCC shows 0\u20135 as \"--\"); each is counted as 0. 'Possible undercount (>10%)' = Yes when they could add more than 10% to the figure (up to 5 each). The CSV download uses the full column names."))
  })
}
