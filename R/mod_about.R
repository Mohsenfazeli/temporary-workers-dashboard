# =============================================================================
# R/mod_about.R: About the data (plain-language explanation, assumptions,
# sources, how-to-use guide, IRCC distinct-people reference table)
# =============================================================================
required_packages <- c("shiny", "bslib", "htmltools")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Category mapping shown to users (matches map_category() in R/data.R).
CATEGORY_MAPPING <- data.frame(
  Program = c(rep("TFWP", 4), rep("IMP", 8)),
  `Dashboard category` = c(
    "Seasonal Agricultural Worker Program", "Other agricultural workers",
    "Caregivers", "Other TFWP workers with an LMIA",
    "Post-graduation work permits", "Spouses and family members",
    "International Experience Canada", "Significant benefit and PR-bridging permits",
    "Trade and other agreements", "Other Canadian interests",
    "Vulnerable workers", "Unknown or other IMP"),
  `IRCC program codes included` = c(
    "Agricultural Workers: Seasonal Agricultural Worker Program",
    "Agricultural Workers: Agricultural Stream; Other Agricultural Workers",
    "Caregivers; Live-In Caregivers (all sub-programs)",
    "Other Temporary Foreign Workers with LMIA (high- and low-wage positions are not split in the source)",
    "Post-grad employment",
    "Spouses of skilled workers; Spouses of students; Dependent of skilled worker; Family members of Economic PR applicants",
    "International Experience Canada (IEC): working holiday, young professionals, co-op",
    "Significant benefit: intra-company transfers, bridging open work permits, PR applicants in Canada, entrepreneurs, emergency repairs, other",
    "Agreements: CUSMA/NAFTA, CPTPP, other free trade agreements, Canada-provincial/territorial agreements, Atlantic pilot",
    "Other Canadian interests: post-doctoral fellows, reciprocal employment (other), exchange professors, charitable or religious work, research",
    "Vulnerable workers (open permits for workers experiencing abuse)",
    "Other IMP participants, almost entirely \"Unknown International Mobility Program\""),
  check.names = FALSE)

# Plain-language glossary, shown in About and on the Composition tab.
GLOSSARY <- data.frame(
  term = c("LMIA", "SAWP", "PGWP", "IEC", "Open work permit", "Significant benefit",
           "CUSMA", "Unknown IMP"),
  meaning = c(
    "Labour Market Impact Assessment: an Employment and Social Development Canada check that hiring a foreign worker will not hurt Canadian workers. Most TFWP jobs need one; IMP jobs do not.",
    "Seasonal Agricultural Worker Program: TFWP stream for farm workers from Mexico and the Caribbean for up to 8 months a year.",
    "Post-graduation work permit: an open permit for international students after they graduate from an eligible Canadian program.",
    "International Experience Canada: working-holiday and youth exchange permits under agreements with other countries.",
    "A permit not tied to one employer (e.g. post-graduation, spouses, bridging permits).",
    "IMP permits justified by a significant benefit to Canada, e.g. intra-company transfers and bridging permits for people applying for permanent residence.",
    "Canada\u2013United States\u2013Mexico Agreement (formerly NAFTA); trade agreements let some professionals work without an LMIA.",
    "IMP permits that IRCC records as \"Unknown International Mobility Program\"; their number rose sharply after 2021."),
  stringsAsFactors = FALSE
)

glossary_list <- function() {
  htmltools::tags$dl(class = "glossary",
    lapply(seq_len(nrow(GLOSSARY)), function(i) htmltools::tagList(
      htmltools::tags$dt(GLOSSARY$term[i]), htmltools::tags$dd(GLOSSARY$meaning[i]))))
}

about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("about"))
}

about_server <- function(id, data, meta, reference) {
  shiny::moduleServer(id, function(input, output, session) {
    output$about <- shiny::renderUI({
      tags <- htmltools::tags
      ref_tbl <- NULL
      if (!is.null(reference$table)) {
        csv <- data |> dplyr::filter(geography == "Canada") |>
          dplyr::group_by(program, year) |>
          dplyr::summarise(sum_months = sum(value), .groups = "drop")
        r <- merge(reference$table, csv, by = c("program", "year"))
        r <- r[order(r$program == "IMP", r$year), ]
        latest_y <- as.integer(format(meta$latest_month, "%Y"))
        r$Year <- ifelse(r$year == latest_y & as.integer(format(meta$latest_month, "%m")) < 12,
                         paste0(r$year, " YTD (Jan\u2013", month.abb[as.integer(format(meta$latest_month, "%m"))], ")"),
                         as.character(r$year))
        ref_tbl <- tags$table(class = "table table-sm ref-table",
          tags$caption("IRCC reference: Canada, permits that took effect (sum of months, used in this dashboard) vs IRCC's annual count of distinct people"),
          tags$thead(tags$tr(tags$th("Program"), tags$th("Year"),
                             tags$th(class = "text-end", "Sum of months (dashboard)"),
                             tags$th(class = "text-end", "IRCC distinct people"),
                             tags$th(class = "text-end", "Difference"))),
          tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(
            tags$td(r$program[i]), tags$td(r$Year[i]),
            tags$td(class = "text-end", fmt_n(r$sum_months[i])),
            tags$td(class = "text-end", fmt_n(r$people[i])),
            tags$td(class = "text-end", fmt_pct(pct_change(r$people[i], r$sum_months[i])))))))
      }
      latest_tx <- fmt_month(meta$latest_month)
      first_tx  <- fmt_month(meta$first_month)

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        bslib::card(
          bslib::card_header(tags$h2(class = "chart-title", "What this dashboard shows")),
          bslib::card_body(
            tags$p("This dashboard shows how many temporary foreign workers had a Canadian work permit take effect each month, by province or territory and by program."),
            tags$div(class = "alert alert-light border py-2 px-3 small",
              tags$strong("Data used: "), "every figure comes from the two IRCC open data files (CSV) for TFWP and IMP listed under Sources. ",
              "An optional IRCC reference (Canada's annual count of distinct people) can be switched on in the Trends tab; it is off by default and is not used in any calculation."),
            tags$h3("The two programs"),
            tags$ul(
              tags$li(tags$strong("Temporary Foreign Worker Program (TFWP): "),
                      "employers hire foreign workers to fill jobs when qualified Canadians or permanent residents are not available. The employer usually needs a Labour Market Impact Assessment (LMIA) from Employment and Social Development Canada. Includes seasonal agricultural workers and caregivers."),
              tags$li(tags$strong("International Mobility Program (IMP): "),
                      "work permits that do not need an LMIA because they serve broader economic, cultural or reciprocal interests, for example post-graduation work permits for international graduates, permits for spouses of skilled workers or students, International Experience Canada (working holiday) and permits under trade agreements such as CUSMA. ",
                      tags$strong("IRCC notes that the IMP table covers only a subset of IMP work permit holders: "),
                      "it does not include people in Canada primarily for reasons other than work, such as asylum claimants or people on humanitarian and compassionate grounds.")),
            tags$h3("What is counted"),
            tags$p("Each number is the count of ", tags$strong("work permits that took effect"),
                   " (were signed) in a month, for the province or territory the worker intended to go to. It is a flow of new and renewed permits, not the number of people holding a valid permit on a given day."),
            tags$p("Annual figures add up the 12 months, so they are ", tags$strong("work permits that took effect during the year"),
                   ", not people. A person whose permits took effect in two different months is counted twice. The latest year (",
                   format(meta$latest_month, "%Y"), ") is year-to-date and is only compared with the same months of earlier years."),
            tags$h3("Coverage and updates"),
            tags$ul(
              tags$li("Period: ", first_tx, " to ", latest_tx, " (latest month available)."),
              tags$li("Geography: Canada, the 10 provinces and 3 territories. Canada also includes permits with no province or territory stated; that group is not ranked."),
              tags$li("Data last retrieved: ", fmt_datetime(meta$retrieved_at),
                      " (", meta$source_label, "). IRCC updates the files monthly; the dashboard reads them live each time it starts."),
              tags$li("IRCC describes these data as preliminary estimates that are subject to change; past months can be revised in later releases."),
              if (identical(meta$source, "cache")) tags$li(tags$strong("Currently showing saved data: "), meta$warning)),
            tags$h3("Privacy masking and rounding"),
            tags$p("IRCC shows values between 0 and 5 as \"--\" and rounds all other values to the nearest 5 to protect privacy. ",
                   "In this dashboard a \"--\" cell is counted as 0, so totals can be slightly low (at most 5 per masked cell; about 0.5% of national monthly totals). Small places and categories, such as the territories, are affected most. Because of rounding, parts may not add up exactly to totals."),
            tags$h3("How to use"),
            tags$ol(
              tags$li("Use the ", tags$strong("Period"), " slider to choose the months you want (January 2015 to the latest month)."),
              tags$li("Choose ", tags$strong("Monthly"), " or ", tags$strong("Annual"), ". Part-years are labelled, and 2026 is year-to-date."),
              tags$li("Pick ", tags$strong("Canada"), " or one province or territory, and TFWP, IMP or both."),
              tags$li("Every tab updates. Hover over charts for exact values; use the expand icon for full screen; download the numbers from the ", tags$strong("Data"), " tab."),
              tags$li(tags$strong("Reset"), " returns all filters to the full period, Canada and both programs."))
          )
        ),
        bslib::card(
          bslib::card_header(tags$h2(class = "chart-title", "Assumptions and sources")),
          bslib::card_body(
            tags$h3("Assumptions"),
            tags$ol(
              tags$li("The data are a flow: permits that took effect in each month. Annual = sum of months (\"work permits that took effect during the year\"); the latest year is year-to-date and never compared with full years."),
              tags$li("Because people can have more than one permit in a year, summed figures are permits, not distinct people. IRCC's own annual count of distinct people for Canada is available as an optional reference (see table below)."),
              tags$li("Masked cells (\"--\", 0 to 5 in IRCC's footnote wording) count as 0 in all sums; tables show how many suppressed cells each figure includes and flag figures where they could add more than 10% (\"Possible undercount\")."),
              tags$li("A combination missing from IRCC's file means zero."),
              tags$li("Canada = the 13 provinces and territories plus \"province/territory not stated\"."),
              tags$li("TFWP and IMP are separate programs, so \"both\" = TFWP + IMP."),
              tags$li("Province/territory is the intended destination recorded on the permit, not where the person lives."),
              tags$li("Values are used as published by IRCC on the retrieval date; IRCC's data are preliminary estimates and may be revised."),
              tags$li("IMP figures cover the subset of IMP work permit holders in IRCC's table; people primarily in Canada for other reasons (e.g. asylum claimants, humanitarian and compassionate cases) are not included."),
              tags$li("TFWP streams and IMP sub-programs are grouped into 4 and 8 categories (mapping below). \"Unknown IMP\" is kept separate because IRCC's use of it rose after 2021."),
              tags$li("Growth over a period compares the last 12 months with the first 12 months of the selected range (or the last month with the first month if fewer than 24 months are selected)."),
              tags$li("IRCC's Excel tables are a supplementary source used only for the optional reference of Canada's annual distinct people (off by default); every dashboard figure and calculation comes from the two CSV files.")),
            tags$h3("Optional IRCC reference: distinct people per year (Canada)"),
            if (is.null(ref_tbl)) tags$p(reference$note %||% "The reference is not available right now.")
            else htmltools::tagList(
              tags$p("IRCC's Excel tables give an annual total that counts each person once per year. It is 1\u20133% below the sum of the months used here because some people had permits take effect in more than one month. IRCC's note: ",
                     tags$em("\u201cThe total unique count may not equal to the sum of permit holders in each program as an individual may hold more than one type of permit over a given period.\u201d")),
              ref_tbl),
            tags$h3("Sources"),
            tags$ul(
              tags$li("IRCC, Temporary Foreign Worker Program work permit holders by province/territory of intended destination, program and year in which permit(s) became effective (CSV): ",
                      tags$a(href = meta$urls[["TFWP"]], target = "_blank", meta$urls[["TFWP"]])),
              tags$li("IRCC, International Mobility Program work permit holders by province/territory of intended destination, program and year in which permit(s) became effective (CSV): ",
                      tags$a(href = meta$urls[["IMP"]], target = "_blank", meta$urls[["IMP"]])),
              tags$li("Supplementary reference (Canada annual distinct people only): IRCC Excel tables ",
                      tags$a(href = meta$reference_urls[["TFWP"]], target = "_blank", "TFWP (XLSX)"), " and ",
                      tags$a(href = meta$reference_urls[["IMP"]], target = "_blank", "IMP (XLSX)"),
                      if (!is.null(reference$source_line)) paste0(" (", reference$source_line, ").")),
              tags$li("Catalogue record and metadata: Open Government Portal, ",
                      tags$a(href = meta$metadata_url, target = "_blank",
                             "Temporary Residents: TFWP and IMP Work Permit Holders \u2013 Monthly IRCC Updates"),
                      ". Open Government Licence \u2013 Canada."),
              tags$li("Event notes on the Trends tab (context only): ",
                      tags$ul(lapply(seq_len(nrow(KEY_EVENTS)), function(i) tags$li(
                        KEY_EVENTS$label[i], ". Source: ",
                        tags$a(href = KEY_EVENTS$url[i], target = "_blank", KEY_EVENTS$source[i], .noWS = "after"), "."))))),
            tags$h3("Glossary"),
            glossary_list(),
            tags$h3("Category mapping"),
            tags$table(class = "table table-sm mapping-table",
              tags$thead(tags$tr(lapply(names(CATEGORY_MAPPING), tags$th))),
              tags$tbody(lapply(seq_len(nrow(CATEGORY_MAPPING)), function(i)
                tags$tr(lapply(CATEGORY_MAPPING[i, ], function(v) tags$td(v))))))
          )
        )
      )
    })
  })
}
