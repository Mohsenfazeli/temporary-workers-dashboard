# =============================================================================
# R/mod_about.R: About the data (plain-language explanation, assumptions,
# sources, how-to-use guide)
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
  term = c("Work permit issued", "TFWP", "IMP", "LMIA", "SAWP", "PGWP", "IEC", "Open work permit", "Significant benefit",
           "PR", "CUSMA", "Unknown IMP"),
  meaning = c(
    "A permit counted in the month IRCC signed it and gave it to the person (IRCC's wording: the month the permit \"became effective\"). New permits and renewals both count. It is a count of permits, not of people.",
    "Temporary Foreign Worker Program: an employer hires a foreign worker for a job, usually after an LMIA shows no Canadian worker is available.",
    "International Mobility Program: work permits that do not need an LMIA, for example for graduates of Canadian schools, spouses of workers or students, working holidays and trade agreements.",
    "Labour Market Impact Assessment: a check by Employment and Social Development Canada that hiring a foreign worker will not hurt Canadian workers, for example because no Canadian is available for the job. Most TFWP permits need one; IMP permits do not.",
    "Seasonal Agricultural Worker Program: TFWP stream for farm workers from Mexico and the Caribbean for up to 8 months a year.",
    "Post-graduation work permit: lets an international student who graduated from an eligible Canadian college or university work for any employer for a time after graduating.",
    "International Experience Canada: working-holiday and youth exchange permits under agreements with other countries.",
    "A permit not tied to one employer (e.g. post-graduation, spouses, bridging permits).",
    "IMP permits given because the work brings a significant benefit to Canada, for example staff transferred within a company, and \"bridging\" permits that let people keep working while they apply for permanent residence.",
    "Permanent residence (as in \"PR-bridging permits\").",
    "Canada\u2013United States\u2013Mexico Agreement (formerly NAFTA); trade agreements let some professionals work without an LMIA.",
    "IMP permits for which IRCC does not identify the program (IRCC's label: \"Unknown International Mobility Program\"). Shown as \"Unknown or other IMP\"."),
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

about_server <- function(id, data, meta) {
  shiny::moduleServer(id, function(input, output, session) {
    output$about <- shiny::renderUI(about_content(meta))
  })
}

# The About page content (also read, as plain text, by the Help assistant so
# its answers follow the same explanations; see R/assistant.R).
about_content <- function(meta) {
      tags <- htmltools::tags
      latest_tx <- fmt_month(meta$latest_month)
      first_tx  <- fmt_month(meta$first_month)

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        bslib::card(
          bslib::card_header(tags$h2(class = "chart-title", "What this dashboard shows")),
          bslib::card_body(
            tags$p("This dashboard shows how many work permits were issued each month under Canada's two programs for temporary foreign workers, by province or territory and by program."),
            tags$div(class = "alert alert-light border py-2 px-3 small",
              tags$strong("Data used: "), "every figure comes from the two IRCC open data files (CSV) for TFWP and IMP listed under Sources. ",
              if (identical(meta$source, "release")) htmltools::tagList(
                "The dashboard shows a fixed copy of the ", tags$strong(meta$release_phrase, .noWS = "after"),
                paste0(", downloaded ", fmt_date(meta$retrieved_at),
                       ", so every visitor sees the same, checked numbers. The release date is IRCC's own: ",
                       meta$release_evidence, ". ")),
              "No other data source is used: these two files are IRCC's official monthly series for both programs by province or territory, so every figure can be traced to one public, dated source and checked by anyone."),
            tags$h3("The two programs"),
            tags$ul(
              tags$li(tags$strong("Temporary Foreign Worker Program (TFWP): "),
                      "an employer hires a foreign worker for a job when no Canadian or permanent resident is available. The employer usually needs a Labour Market Impact Assessment (LMIA), a check by Employment and Social Development Canada. Includes seasonal farm workers and caregivers."),
              tags$li(tags$strong("International Mobility Program (IMP): "),
                      "work permits that do not need an LMIA because they serve Canada's wider economic, cultural or exchange interests, for example post-graduation work permits (PGWPs) for graduates of Canadian colleges and universities, permits for spouses of skilled workers or students, International Experience Canada (working holidays) and permits under trade agreements such as CUSMA. ",
                      tags$strong("Not everyone with an IMP permit is in these files: "),
                      "IRCC says its IMP table covers only a subset of IMP work permit holders. It leaves out people who are in Canada mainly for a reason other than work, such as asylum claimants waiting for a decision or people applying to stay on humanitarian grounds, even though they may hold a work permit. So the IMP numbers here are lower than the number of all IMP work permits.")),
            tags$h3("What is counted"),
            tags$p("Each number is the count of ", tags$strong("work permits issued"),
                   " in a month, for the province or territory where the person intended to work. IRCC's own wording is permits that \"became effective\": IRCC counts a permit in the month it was signed and given to the person. For someone coming from abroad that is usually at the border on arrival, so this is not the same as the number of applications approved."),
            tags$p("New permits and renewals (extensions) both count. It is not the number of people holding a valid permit on a given day."),
            tags$p("Yearly figures add up the months, so they count ", tags$strong("permits, not people"),
                   ": a person who got two permits in a year is counted twice. ",
                   format(meta$latest_month, "%Y"), " has data for January to ", month.name[as.integer(format(meta$latest_month, "%m"))],
                   " only, so it is only compared with the same months of earlier years."),
            tags$h3("Coverage and updates"),
            tags$ul(
              tags$li("Period: ", first_tx, " to ", latest_tx, " (latest month available)."),
              tags$li("Geography: Canada, the 10 provinces and 3 territories. Canada also includes permits with no province or territory stated; that group is not ranked."),
              if (identical(meta$source, "release"))
                tags$li("Data: ", meta$release_phrase, ", downloaded ", fmt_datetime(meta$retrieved_at),
                        ". IRCC updates the files monthly; this copy is refreshed when the dashboard is redeployed.")
              else tags$li("Data last retrieved: ", fmt_datetime(meta$retrieved_at),
                      " (", meta$source_label, "). IRCC updates the files monthly."),
              tags$li("IRCC describes these data as preliminary estimates that are subject to change; past months can be revised in later releases."),
              if (identical(meta$source, "cache")) tags$li(tags$strong("Currently showing saved data: "), meta$warning)),
            tags$h3("Privacy masking and rounding"),
            tags$p("To protect privacy, IRCC hides small counts (0 to 5 are shown as \"--\") and rounds all other counts to the nearest 5. ",
                   "Here a hidden count is counted as 0, so totals can be slightly low (by at most 5 for each hidden count). Small places and categories, such as the territories, are affected most; the Geography table marks a place with \u26a0 (many hidden counts) when its hidden counts could add more than 10% to its total. Because of rounding, parts may not add up exactly to totals."),
            tags$h3("Reading the charts"),
            tags$ul(
              tags$li(tags$strong("Permits, not people or arrivals. "), IN_CANADA_NOTE$text, " Source: ",
                      tags$a(href = IN_CANADA_NOTE$url, target = "_blank", IN_CANADA_NOTE$source, .noWS = "after"), "."),
              tags$li(tags$strong("Part years. "), "In the yearly view a year with fewer than 12 months of data is labelled with its months, such as \"2026 (Jan\u2013Jul)\", and drawn with lighter bars or open circles (not joined by a line to the full years). Its change from a year earlier compares the same months. Tick ", tags$em("Show only Jan\u2013Jul of each year"), " to compare January to July of every year."),
              tags$li(tags$strong("Hidden small counts. "), "IRCC shows 0 to 5 as \"--\"; these count as 0 (see Privacy masking and rounding)."),
              tags$li(tags$strong("Event lines. "), "Dotted lines on the Trends tab mark federal policy events for context only; they do not show cause. Full wording and sources are listed under Sources."),
              tags$li(tags$strong("Unknown IMP. "), "For some IMP permits IRCC does not identify the program (\"Unknown International Mobility Program\"). The Composition tab gives their share for the selected place and can leave them out of the IMP shares."),
              tags$li(tags$strong("Groupings. "), "TFWP streams and IMP sub-programs are grouped into categories for this dashboard; the Composition tab lists IRCC's own program names in each category.")),
            tags$h3("How to use"),
            tags$ol(
              tags$li("Use the ", tags$strong("Period"), " slider, or a quick-period button, to choose the months you want (January 2015 to the latest month)."),
              tags$li("Choose ", tags$strong("Monthly"), " or ", tags$strong("Annual"), ". Part years are labelled with their months, such as 2026 (Jan\u2013Jul)."),
              tags$li("Pick ", tags$strong("Canada"), " or one province or territory, and TFWP, IMP or both."),
              tags$li("Every tab updates. The Overview cards and the share-of-change chart (Geography) cover January to the last month selected. Hover over charts for exact values; use the expand icon for full screen; download the numbers from the ", tags$strong("Data"), " tab."),
              tags$li(tags$strong("Reset"), " returns all filters to the full period, Canada and both programs."))
          )
        ),
        bslib::card(
          bslib::card_header(tags$h2(class = "chart-title", "Assumptions and sources")),
          bslib::card_body(
            tags$h3("Assumptions"),
            tags$ol(
              tags$li("Each number counts permits issued in a month. Yearly figures add up the months; a part year (such as 2026, January to July) is only compared with the same months of other years, never with full years."),
              tags$li("Because people can have more than one permit in a year, summed figures are permits, not distinct people."),
              tags$li("Hidden small counts (\"--\", 0 to 5 in IRCC's wording) count as 0 in all sums; tables show how many each figure includes and flag figures where they could add more than 10% (\"Possible undercount\")."),
              tags$li("A combination missing from IRCC's file means zero."),
              tags$li("Canada = the 13 provinces and territories plus \"province/territory not stated\"."),
              tags$li("TFWP and IMP are separate programs, so \"both\" = TFWP + IMP."),
              tags$li("Province/territory is the intended destination recorded on the permit, not where the person lives."),
              tags$li("Values are used as published in the IRCC release shown in the header; IRCC's data are preliminary estimates and may be revised."),
              tags$li("IMP figures cover only the IMP work permit holders in IRCC's table; people in Canada mainly for other reasons (for example asylum claimants or people applying on humanitarian grounds) are not included."),
              tags$li("TFWP streams and IMP sub-programs are grouped into 4 and 8 categories (mapping below). \"Unknown IMP\" is kept separate because IRCC does not identify the program for these permits."),
              tags$li("Growth over a period compares the last 12 months with the first 12 months of the selected period (or the last month with the first month if fewer than 24 months are selected)."),
              tags$li("Every dashboard figure and calculation comes from the two IRCC CSV files listed below; no other data are used.")),
            tags$h3("Sources"),
            tags$ul(
              tags$li("IRCC, Temporary Foreign Worker Program work permit holders by province/territory of intended destination, program and year in which permit(s) became effective (CSV): ",
                      tags$a(href = meta$urls[["TFWP"]], target = "_blank", meta$urls[["TFWP"]])),
              tags$li("IRCC, International Mobility Program work permit holders by province/territory of intended destination, program and year in which permit(s) became effective (CSV): ",
                      tags$a(href = meta$urls[["IMP"]], target = "_blank", meta$urls[["IMP"]])),
              tags$li("Catalogue record and metadata: Open Government Portal, ",
                      tags$a(href = meta$metadata_url, target = "_blank",
                             "Temporary Residents: TFWP and IMP Work Permit Holders \u2013 Monthly IRCC Updates"),
                      ". Open Government Licence \u2013 Canada."),
              tags$li("Note on permits and arrivals: ",
                      tags$a(href = IN_CANADA_NOTE$url, target = "_blank", IN_CANADA_NOTE$source, .noWS = "after"), "."),
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
}
