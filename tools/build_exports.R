# =============================================================================
# tools/build_exports.R: build the three PDF files of the Downloads tab from the
# dashboard's data (data/baseline.rds), with no AI:
#   1. the note "work permits in Canada and B.C." (Question 2 note; numbers,
#      direction words and figures follow the data, see exports/R/note_q2_text.R)
#   2. "How to use this dashboard"   3. "About the package and files"
# Figures are pictures of this dashboard, taken from a temporary local copy in
# headless Chrome. The note is built as Word by exports/q2/build_note.py
# (python-docx) and converted to PDF by LibreOffice; the two guides are R
# Markdown printed to PDF by Chrome. The files and www/exports/manifest.json
# are replaced only if every step and check passes; otherwise this script stops
# with an error (tools/deploy.R then stops the deploy).
# Usage (from the project folder):  Rscript tools/build_exports.R
# Needs: Chrome or Chromium, python3 with python-docx, LibreOffice (soffice).
# =============================================================================
required_packages <- c("shinytest2", "chromote", "jsonlite", "rmarkdown", "pdftools", "magick")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
Sys.unsetenv("GEMINI_API_KEY")          # the build never calls the AI service
for (f in c("R/calc.R", "R/data.R", "R/mod_downloads.R")) source(f)
for (f in list.files("exports/R", pattern = "\\.R$", full.names = TRUE)) source(f)

build_exports <- function(project = ".", out_dir = EXPORTS_DIR, build_date = Sys.Date(),
                          bundle = load_release(file.path(project, "data", "baseline.rds")),
                          figures_from = NULL) {
  t0 <- Sys.time()
  work <- file.path(Sys.getenv("TW_EXPORTS_WORK", tempdir()), paste0("exports_build_", format(Sys.time(), "%H%M%S")))
  dir.create(work, recursive = TRUE)
  figs <- file.path(work, "figures")
  say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
  for (tool in c("python3", "soffice")) if (!nzchar(Sys.which(tool))) stop(tool, " not found.")

  # 1. Pictures of the dashboard (or copies given by a test)
  if (is.null(figures_from)) {
    say("Capturing the note's figures from a local copy of the dashboard...")
    capture_note_figures(project, figs, bundle)
    say("Capturing the Overview tab for the user guide...")
    capture_overview(project, file.path(work, "overview_annotated.jpg"))
  } else {
    dir.create(figs); file.copy(list.files(figures_from, full.names = TRUE), figs)
    file.copy(file.path(figs, "overview_annotated.jpg"), work)
  }

  # 2. The note: facts, text, Word file, PDF
  say("Building the note...")
  q2dir <- file.path(work, "q2")
  inp <- q2_write_inputs(bundle, q2dir, figs, build_date, exports_dir = file.path(project, "exports"))
  st <- system2("python3", c(shQuote(file.path(project, "exports", "q2", "build_note.py")), shQuote(file.path(q2dir, "spec.json"))),
                stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(st, "status")) && attr(st, "status") != 0) stop("Word builder failed:\n", paste(st, collapse = "\n"))
  lo_profile <- paste0("file://", file.path(work, "lo_profile"))
  lo_args <- c(paste0("-env:UserInstallation=", lo_profile), "--headless", "--convert-to", "pdf",
               "--outdir", shQuote(q2dir), shQuote(file.path(q2dir, "note.docx")))
  # R adds its own library folder to LD_LIBRARY_PATH, which stops LibreOffice from starting on Linux
  st <- if (.Platform$OS.type == "unix") system2("env", c("-u", "LD_LIBRARY_PATH", "soffice", lo_args), stdout = TRUE, stderr = TRUE) else
    system2("soffice", lo_args, stdout = TRUE, stderr = TRUE)
  note_pdf <- file.path(q2dir, "note.pdf")
  if (!file.exists(note_pdf)) stop("LibreOffice did not make the note's PDF:\n", paste(st, collapse = "\n"))
  check <- exports_check_note(note_pdf, inp, bundle)

  # 3. The two guides
  say("Rendering the user guide and the package description...")
  v <- docs_vars(bundle, project); v$overview_img <- "overview_annotated.jpg"
  guide_pdf <- file.path(work, "guide.pdf"); pkg_pdf <- file.path(work, "package.pdf")
  n_guide <- render_doc_pdf(file.path(project, "exports", "docs", "howto.Rmd"), v, guide_pdf,
                            file.path(work, "overview_annotated.jpg"), exports_dir = file.path(project, "exports"))
  n_pkg <- render_doc_pdf(file.path(project, "exports", "docs", "package.Rmd"), v, pkg_pdf, exports_dir = file.path(project, "exports"))
  for (p in c(guide_pdf, pkg_pdf)) exports_check_text(paste(pdftools::pdf_text(p), collapse = "\n"), basename(p))
  if (n_guide > 2 || n_pkg > 3) stop(sprintf("A guide is longer than expected (user guide %d pages, package %d pages).", n_guide, n_pkg))

  # 4. Manifest, then replace the published files in one step
  release_date <- bundle$meta$release_date
  files <- list(
    list(file = "work-permits-canada-bc-note.pdf", short = "the note", title = inp$tx$paras[1],
         description = "A short note for B.C. labour market planning: what the IRCC data show about recent work permit trends in Canada and B.C., with figures from this dashboard and appendices on method and exact values.",
         note = sprintf("Its numbers, figures and wording of trends update with each IRCC release; the context, causes and implications were written for the IRCC release of %s.",
                        fmt_date(Q2_CONTEXT_RELEASE)),
         pages = check$pages, body_pages = check$body_pages, src = note_pdf),
    list(file = "how-to-use-this-dashboard.pdf", short = "the guide", title = "How to use this dashboard",
         description = "A guide to the filters, tabs, headline cards and notes, with a labelled picture of the Overview tab.",
         pages = n_guide, src = guide_pdf),
    list(file = "about-the-package-and-files.pdf", short = "the description", title = "About the package and files",
         description = "What the dashboard's code package holds, how to run it on your own computer, how it is tested and how to rebuild the data.",
         pages = n_pkg, src = pkg_pdf))
  manifest <- list(
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"), build_date = format(build_date),
    release_phrase = bundle$meta$release_phrase, release_date = format(release_date),
    release_date_text = fmt_date(release_date), latest_month = format(bundle$meta$latest_month),
    fingerprint = exports_fingerprint(bundle$data, bundle$meta),
    context_release = format(Q2_CONTEXT_RELEASE),
    files = lapply(files, function(x) x[setdiff(names(x), "src")]),
    rules = inp$tx$rules)
  stage <- file.path(work, "exports")
  dir.create(stage)
  for (x in files) file.copy(x$src, file.path(stage, x$file))
  jsonlite::write_json(manifest, file.path(stage, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  dir.create(dirname(out_dir), showWarnings = FALSE, recursive = TRUE)
  old <- paste0(out_dir, "_old")
  unlink(old, recursive = TRUE)
  if (dir.exists(out_dir)) file.rename(out_dir, old)
  dir.create(out_dir)
  ok <- all(file.copy(list.files(stage, full.names = TRUE), out_dir))
  if (!ok) { unlink(out_dir, recursive = TRUE); if (dir.exists(old)) file.rename(old, out_dir); stop("Could not write ", out_dir) }
  unlink(old, recursive = TRUE)
  say(sprintf("Done in %.0f s: note %d pages (body %d), guide %d, package %d. Files in %s",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), check$pages, check$body_pages, n_guide, n_pkg, out_dir))
  invisible(list(manifest = manifest, work = work, inputs = inp))
}

#' No leftover {placeholders}, NA or template markers in a PDF's text.
exports_check_text <- function(txt, what) {
  bad <- c("\\{[a-z0-9_]+\\}", "\\bNA\\b", "\\bNaN\\b", "\\bInf\\b", "n/a", "`r ", "LINK_PLACEHOLDER")
  hit <- bad[vapply(bad, function(p) grepl(p, txt), logical(1))]
  if (length(hit)) stop(what, ": unexpected text in the PDF (", paste(hit, collapse = ", "), ")")
  invisible(TRUE)
}

#' The note: body on two pages (appendices start on page 3), every rounded
#' number of the body present, no stale latest month.
exports_check_note <- function(pdf, inp, bundle) {
  pages <- pdftools::pdf_text(pdf)
  txt <- paste(pages, collapse = "\n")
  exports_check_text(txt, "note")
  app_page <- which(grepl("APPENDICES", pages))[1]
  if (is.na(app_page) || app_page != 3) stop("The note's body is not two pages (appendices start on page ", app_page, ").")
  flat <- gsub("\\s+", " ", paste(pages[1:2], collapse = " "))
  shown <- unique(inp$tx$nums$shown[nzchar(inp$tx$nums$shown)])
  miss <- shown[!vapply(shown, function(s) grepl(s, flat, fixed = TRUE), logical(1))]
  if (length(miss)) stop("Numbers missing from the note's body: ", paste(miss, collapse = ", "))
  latest <- fmt_month(bundle$meta$latest_month)
  if (!grepl(latest, gsub("\\s+", " ", txt), fixed = TRUE)) stop("The note does not mention the latest month (", latest, ").")
  list(pages = length(pages), body_pages = app_page - 1L)
}

if (sys.nframe() == 0L) {
  res <- tryCatch(build_exports(), error = function(e) { message("BUILD FAILED: ", conditionMessage(e)); NULL })
  if (is.null(res)) quit(status = 1)
}
