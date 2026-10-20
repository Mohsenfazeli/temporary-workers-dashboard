# =============================================================================
# R/mod_downloads.R: Downloads tab. Three PDF files built from the same IRCC
# data as the dashboard by tools/build_exports.R (at deploy time, no AI):
# the note on work permits for B.C., the user guide and the description of
# the code package. The files and www/exports/manifest.json (release, page
# counts, a fingerprint of the data) are read once at startup. If the data
# shown differ from the data the files were built from (for example in live
# mode after a new IRCC release), the tab says so.
# =============================================================================

EXPORTS_DIR <- file.path("www", "exports")

#' Short fingerprint of a data table: latest month, number of rows and total
#' permits. The build writes it to the manifest; the tab compares it with the
#' data the dashboard shows.
exports_fingerprint <- function(data, meta) {
  sprintf("%s|%d|%.0f", format(as.Date(meta$latest_month), "%Y-%m"), nrow(data), sum(data$value))
}

#' Read the manifest (NULL if the files have not been built).
read_exports_manifest <- function(dir = EXPORTS_DIR) {
  f <- file.path(dir, "manifest.json")
  if (!file.exists(f)) return(NULL)
  m <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(m) || is.null(m$files)) return(NULL)
  ok <- vapply(m$files, function(x) file.exists(file.path(dir, x$file)), logical(1))
  if (!all(ok)) return(NULL)
  m
}

#' The tab's content (a plain function of the manifest and the data shown, so
#' it can be tested without a browser).
downloads_content <- function(manifest, data, meta) {
  if (is.null(manifest)) {
    return(htmltools::div(class = "alert alert-secondary", role = "status",
      "The PDF files are not available in this copy of the dashboard. ",
      "They are made by tools/build_exports.R (see README.md)."))
  }
  same <- identical(manifest$fingerprint, exports_fingerprint(data, meta))
  notice <- if (!same) htmltools::div(class = "alert alert-warning downloads-mismatch", role = "status",
    htmltools::tags$strong("These files were built for an earlier version of the data. "),
    sprintf("They show the %s; the dashboard now shows the %s, so their numbers may differ from the charts.",
            manifest$release_phrase, meta$release_phrase %||% meta$source_label))
  item <- function(x) {
    htmltools::div(class = "downloads-item",
      htmltools::tags$h3(class = "downloads-title", x$title),
      htmltools::tags$p(x$description),
      if (!is.null(x$note)) htmltools::tags$p(class = "downloads-note", x$note),
      htmltools::tags$p(class = "downloads-meta",
        sprintf("PDF, %d %s, built for the IRCC release of %s", as.integer(x$pages),
                if (as.integer(x$pages) == 1) "page" else "pages", manifest$release_date_text)),
      htmltools::tags$a(class = "btn btn-outline-primary btn-sm", href = paste0("exports/", x$file),
                        download = x$file, target = "_blank", rel = "noopener",
                        bsicons::bs_icon("file-earmark-pdf"), paste(" Download", x$short)))
  }
  htmltools::tagList(
    notice,
    htmltools::tags$p(class = "downloads-intro",
      "Download a short note on what the data show for B.C., a guide to using this dashboard, and a description of its code. ",
      "Each file is built from the same IRCC data as the dashboard."),
    lapply(manifest$files, item))
}

downloads_ui <- function(id, data, meta, manifest = read_exports_manifest()) {
  ns <- shiny::NS(id)
  htmltools::div(id = ns("panel"), class = "downloads-tab",
    bslib::card(
      bslib::card_header("Downloads (PDF)"),
      bslib::card_body(downloads_content(manifest, data, meta))))
}
