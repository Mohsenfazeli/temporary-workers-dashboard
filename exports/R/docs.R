# =============================================================================
# exports/R/docs.R: the two dashboard documents on the Downloads tab,
# "How to use this dashboard" and "About the package and files", from the
# templates in exports/docs/ (R Markdown -> HTML -> PDF printed by headless
# Chrome, the same route as the submitted user guide). Every date, period and
# count in them comes from the data or the project files.
# =============================================================================

EXPORTS_APP_URL <- "https://mohsenfazeli.shinyapps.io/temporary-workers-dashboard/"

num_word <- function(n) {
  w <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
  if (n >= 1 && n <= 12) w[n] else as.character(n)
}

docs_vars <- function(bundle, project = ".") {
  meta <- bundle$meta; d <- bundle$data
  latest <- as.Date(meta$latest_month); first <- as.Date(meta$first_month)
  Y <- as.integer(format(latest, "%Y")); M <- as.integer(format(latest, "%m")); PY <- Y - 1L
  mon <- month.abb[M]; months_long <- if (M == 1) "January" else paste0("January\u2013", month.name[M])
  # Geography example: Canada's change in the same months and the largest contributor
  ytd <- function(geo, y) sum(d$value[d$geography == geo & d$year == y & as.integer(format(d$month, "%m")) <= M])
  ca_diff <- ytd("Canada", Y) - ytd("Canada", PY)
  pts <- PROVINCES_TERRITORIES
  diffs <- vapply(pts, function(g) ytd(g, Y) - ytd(g, PY), numeric(1))
  top <- if (ca_diff >= 0) pts[which.max(diffs)] else pts[which.min(diffs)]
  period <- if (M == 12) as.character(Y) else paste(months_long, Y)
  contrib_example <- sprintf("e.g. Canada issued %s %s permits in %s than a year earlier, and %s's %s of %s is %s of that",
                             fmt_n(abs(ca_diff)), if (ca_diff >= 0) "more" else "fewer", period, top,
                             if (diffs[[top]] >= 0) "increase" else "decrease", fmt_n(abs(diffs[[top]])),
                             sprintf("%d%%", floor(100 * diffs[[top]] / ca_diff + 0.5)))
  # Notes on the Overview tab (counts follow the dashboard's own functions)
  n_high <- length(release_highlights(d, latest))
  n_take <- length(make_takeaways(d, first, latest, "Canada", "Both"))
  # Packages: what shiny::runApp() installs (app.R's list and their dependencies)
  app_lines <- readLines(file.path(project, "app.R"), warn = FALSE)
  pk_txt <- paste(app_lines, collapse = "\n")
  pk_block <- regmatches(pk_txt, regexpr("required_packages <- c\\([^)]*\\)", pk_txt))
  pkgs <- regmatches(pk_block, gregexpr('"[A-Za-z0-9.]+"', pk_block))[[1]]
  pkgs <- gsub('"', "", pkgs)
  db <- utils::installed.packages()
  deps <- unique(c(pkgs, unlist(tools::package_dependencies(pkgs, db = db, recursive = TRUE,
                                                            which = c("Depends", "Imports", "LinkingTo")))))
  base_pk <- rownames(db)[db[, "Priority"] %in% "base"]
  n_pk <- length(setdiff(deps, base_pk))
  lock <- jsonlite::fromJSON(file.path(project, "renv.lock"))
  tabs <- regmatches(pk_txt, gregexpr('bslib::nav_panel\\("[^"]+"', pk_txt))[[1]]
  tab_mods <- sort(setdiff(basename(list.files(file.path(project, "R"), pattern = "^mod_.*\\.R$")),
                           c("mod_help.R", "mod_downloads.R")))
  mod_desc <- c(mod_data.R = "`mod_data.R` (table and CSV download)")
  mods_txt <- vapply(tab_mods, function(m) if (m %in% names(mod_desc)) mod_desc[[m]] else sprintf("`%s`", m), character(1))
  order_pref <- c("mod_overview.R", "mod_trends.R", "mod_geography.R", "mod_composition.R", "mod_data.R", "mod_about.R")
  mods_txt <- mods_txt[order(match(tab_mods, order_pref))]
  release_date <- meta$release_date %||% NA
  list(link = EXPORTS_APP_URL,
       app_title = sprintf("Temporary Workers in Canada, %s\u2013%d", format(first, "%Y"), Y),
       built_for = sprintf("Built for the %s", meta$release_phrase),
       release_phrase = meta$release_phrase,
       release_short = if (is.na(release_date)) meta$source_label else sprintf("IRCC release of %s", fmt_date(release_date)),
       release_date = if (is.na(release_date)) fmt_month(latest) else fmt_date(release_date),
       first_month = fmt_month(first), latest_month = fmt_month(latest),
       partyear_sentence = if (M < 12) sprintf(paste0("In Annual view %d covers %s only: it is drawn lighter, not joined to the full years, ",
                                                      "and compared with %s %d. Tick *Show only Jan\u2013%s of each year* to compare the same months every year."),
                                               Y, months_long, months_long, PY, mon) else "",
       reset_tail = if (M < 12) ", and unticks that box" else "",
       contrib_example = contrib_example, n_highlights = num_word(n_high), n_takeaways = num_word(n_take),
       n_packages = as.character(round(n_pk / 10) * 10), r_version = lock$R$Version, r_min = sub("^(\\d+\\.\\d+).*", "\\1", lock$R$Version),
       n_tabs_word = num_word(length(tabs)), tab_names = sub('.*"', "", tabs),
       n_tab_mods_word = num_word(length(tab_mods)), tab_mods = paste(mods_txt, collapse = ", "))
}

#' Render one template to PDF (HTML printed by headless Chrome). Returns the page count.
render_doc_pdf <- function(template, vars, out_pdf, images = character(), exports_dir = "exports") {
  work <- file.path(tempdir(), paste0("doc_", tools::file_path_sans_ext(basename(template)), "_", as.integer(stats::runif(1, 1, 1e6))))
  dir.create(work)
  file.copy(template, work); file.copy(file.path(exports_dir, "docs", "print.css"), work)
  for (im in images) file.copy(im, work)
  html <- rmarkdown::render(file.path(work, basename(template)), params = list(v = vars),
                            output_format = rmarkdown::html_document(theme = NULL, highlight = NULL, mathjax = NULL,
                                                                     self_contained = TRUE, css = file.path(work, "print.css")),
                            output_dir = work, quiet = TRUE, envir = new.env())
  chrome <- Sys.which(c("google-chrome", "chromium", "chromium-browser", "chrome"))
  chrome <- chrome[nzchar(chrome)][1]
  if (is.na(chrome)) stop("Chrome/Chromium not found (needed to print the PDFs).")
  out_pdf <- normalizePath(out_pdf, mustWork = FALSE)
  unlink(out_pdf)
  status <- system2(chrome, c("--headless=new", "--no-sandbox", "--disable-gpu", "--no-pdf-header-footer",
                              paste0("--print-to-pdf=", shQuote(out_pdf)), shQuote(paste0("file://", normalizePath(html)))),
                    stdout = FALSE, stderr = FALSE)
  if (status != 0 || !file.exists(out_pdf)) stop("PDF printing failed for ", basename(template))
  pdftools::pdf_info(out_pdf)$pages
}
