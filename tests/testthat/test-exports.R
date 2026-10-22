# Downloads tab: the PDFs built by tools/build_exports.R.
#  1. Structural checks on note facts/text for the shipped release, plus tiny
#     synthetic fixtures (not derived from any written note).
#  2. On simulated newer releases (copies of the data with changed numbers;
#     test data only) the wording rules switch, the title carries the build date, and no
#     stale period ("July 2026", "Jan–Jul") is left.
#  3. The tab's content, the files and the mismatch notice; the running app.
for (f in list.files(file.path(PROJECT, "exports", "R"), pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
SYN <- file.path(PROJECT, "tests", "testthat", "fixtures", "exports_synthetic")
SEP15 <- load_release(file.path(PROJECT, "data", "baseline.rds"))
EXPORTS <- file.path(PROJECT, "exports")

# A copy of the release with extra months, built from the last month's rows
# (with an optional change of values); release date moved on. Test data only.
simulate_release <- function(bundle, months, release_date, scale = NULL) {
  d <- bundle$data
  last <- d[d$month == max(d$month), ]
  add <- do.call(rbind, lapply(seq_along(months), function(i) {
    x <- last; x$month <- as.Date(months[i]); x$year <- as.integer(format(x$month, "%Y")); x }))
  d <- rbind(d, add)
  if (!is.null(scale)) d$value <- scale(d)
  b <- bundle
  b$data <- d
  b$meta$latest_month <- max(d$month)
  b$meta$release_date <- as.Date(release_date)
  b$meta$source_label <- sprintf("IRCC open data release of %s", fmt_date(b$meta$release_date))
  b$meta$release_phrase <- sprintf("%s (data to %s)", b$meta$source_label, fmt_month(b$meta$latest_month))
  b
}

# ---- 1. Shipped release: structure + synthetic fixtures ---------------------------
test_that("q2_facts returns structured facts for the shipped release", {
  fx <- q2_facts(SEP15)
  expect_gte(nrow(fx$facts), 50)
  expect_true(all(c("id", "value", "display", "type") %in% names(fx$facts)))
  expect_false(anyDuplicated(fx$facts$id) > 0)
  expect_true(all(is.finite(fx$facts$value)))
  P <- fx$P
  expect_identical(c(P$Y, P$M, P$PY, P$FY0, P$LF, P$YMAX, P$PH), c(2026L, 7L, 2025L, 2015L, 2025L, 2023L, 2024L))
  expect_equal(nrow(fx$geo), 14L)   # 13 P/Ts + not stated, or ranked set from q2_facts
  expect_gte(nrow(fx$mix), 1L)
})

test_that("synthetic fixture facts round with the note's rounding rules", {
  syn <- utils::read.csv(file.path(SYN, "facts.csv"), stringsAsFactors = FALSE, encoding = "UTF-8",
                         colClasses = "character", na.strings = character())
  expect_equal(nrow(syn), 4L)
  expect_true(all(grepl("^synth_", syn$id)))
  for (i in seq_len(nrow(syn))) {
    expect_identical(q2_round(as.numeric(syn$value[i]), syn$rule[i]), syn$display[i],
                     label = syn$id[i])
  }
})

test_that("synthetic note_body.md is placeholder text only", {
  paras <- readLines(file.path(SYN, "note_body.md"), encoding = "UTF-8", warn = FALSE)
  paras <- paras[nzchar(trimws(paras))]
  expect_gte(length(paras), 3L)
  txt <- paste(paras, collapse = "\n")
  expect_match(txt, "placeholder", ignore.case = TRUE)
  expect_no_match(txt, "IRCC|TFWP|IMP|PGWP|British Columbia|rebounding|minister|assignment", ignore.case = TRUE)
})

test_that("note text for the shipped release is complete, dated, and has no holes", {
  fx <- q2_facts(SEP15); tx <- q2_note_text(fx, SEP15, as.Date("2026-10-20"))
  expect_match(tx$paras[1], "^Data Insight")
  expect_match(tx$paras[2], "context written for the release of September 15, 2026", fixed = TRUE)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", tx$paras)))
  expect_true("References" %in% tx$paras)
  expect_true(all(tx$rules$holds[tx$rules$rule != "new_12_month_high"]))
  expect_false(tx$rules$holds[tx$rules$rule == "new_12_month_high"])
  # Not compared to any frozen assignment prose
  expect_false(identical(tx$paras, readLines(file.path(SYN, "note_body.md"), warn = FALSE)))
})

test_that("every rounded body number equals its fact under its rounding rule", {
  fx <- q2_facts(SEP15); tx <- q2_note_text(fx, SEP15)
  expect_gte(nrow(tx$nums), 10L)
  expect_true(all(c("shown", "fact_id", "rule", "statement", "location") %in% names(tx$nums)))
  for (i in seq_len(nrow(tx$nums))) {
    if (tx$nums$rule[i] %in% c("pct0", "k", "h", "M2") && nzchar(tx$nums$shown[i])) {
      expect_identical(q2_round(fx$val(tx$nums$fact_id[i]), tx$nums$rule[i]), tx$nums$shown[i],
                       label = tx$nums$fact_id[i])
    }
  }
})

test_that("geography and mix outputs are internally consistent", {
  fx <- q2_facts(SEP15)
  g <- fx$geo
  expect_true("geography" %in% names(g))
  num_cols <- setdiff(names(g), "geography")
  expect_true(all(vapply(g[num_cols], is.numeric, logical(1))))
  expect_true(all(is.finite(as.matrix(g[num_cols]))))
  expect_true(all(c("geography", "program", "category", "value") %in% names(fx$mix)))
  expect_true(all(fx$mix$value >= 0))
})

test_that("appendix lines, captions and table headers follow the templates (dated lines aside)", {
  r <- q2_write_inputs(SEP15, file.path(tempdir(), "q2_sep15"), file.path(tempdir(), "figs"), as.Date("2026-10-20"),
                       exports_dir = EXPORTS)
  O <- r$original; S <- r$spec
  A0 <- unlist(O$appendix_a); A1 <- unlist(S$appendix_a)
  expect_identical(which(A0 != A1), 8L)                       # the figures' date only
  expect_identical(A1[8], sub("(plain-language version, October 19, 2026; Figure 1 October 21, 2026)",
                              "(plain-language version; figures captured on October 20, 2026, when this file was built)", A0[8], fixed = TRUE))
  for (k in 1:8) expect_identical(S$appendix_c[[k]][[3]], O$appendix_c[[k]][[3]])
  for (k in c("1", "2")) expect_identical(S$fig[[k]][[3]], O$fig[[k]][[3]])
  expect_identical(as.character(S$table_c1_headers), unlist(O$headers_c1))
  expect_identical(c(S$table_c1_caption, S$table_c2_caption, S$appendix_d_intro), unlist(O$P_calls))
  E0 <- unlist(O$appendix_e); E1 <- unlist(S$appendix_e)
  expect_identical(which(E0 != E1), 2L)                       # capture-date placeholder -> build date
  expect_identical(unlist(O$appendix_b), unlist(S$appendix_b))
  refs0 <- utils::read.csv(file.path(EXPORTS, "q2", "references_template.csv"), colClasses = "character", encoding = "UTF-8")
  refs1 <- utils::read.csv(file.path(tempdir(), "q2_sep15", "refs.csv"), colClasses = "character", encoding = "UTF-8")
  expect_identical(refs1, refs0)
  expect_no_match(paste(unlist(O$appendix_a), collapse = " "), "ministry manager|for the Minister|briefing note to", ignore.case = TRUE)
})

# ---- 2. Simulated newer releases ------------------------------------------------------
body_number_tokens <- function(paras) {
  body <- paras[seq_len(which(paras == "References") - 1)][-c(1, 2)]
  tok <- unlist(regmatches(body, gregexpr("[0-9][0-9,.]*(%| million)|[0-9]{1,3}(,[0-9]{3})+", body)))
  unique(tok)
}
stale <- c("July 2026", "Jul 2026", "Jan\u2013Jul", "January\u2013July")

test_that("a newer release (one more month, same story) updates every period", {
  b <- simulate_release(SEP15, "2026-08-01", "2026-10-15")
  out <- file.path(tempdir(), "q2_aug")
  r <- q2_write_inputs(b, out, file.path(tempdir(), "figs"), as.Date("2026-10-20"), exports_dir = EXPORTS)
  tx <- r$tx; S <- r$spec
  all_txt <- paste(c(tx$paras, unlist(S$appendix_a), vapply(S$appendix_c, `[[`, "", 3), S$fig$`1`[[3]], S$fig$`2`[[3]],
                     S$table_c1_headers, unlist(S$appendix_e), tx$nums$statement, tx$nums$location), collapse = "\n")
  for (s in stale) expect_false(grepl(s, all_txt, fixed = TRUE), label = s)
  expect_true(grepl("January\u2013August 2026", all_txt, fixed = TRUE))
  expect_match(tx$paras[2], "updated October 20, 2026 \u00b7 IRCC data to August 2026 \u00b7 Numbers: release of October 15, 2026; context written for the release of September 15, 2026", fixed = TRUE)
  expect_match(tx$paras[grep("^\\[1\\]", tx$paras)], "release of October 15, 2026", fixed = TRUE)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", tx$paras)))
  shown <- tx$nums$shown[nzchar(tx$nums$shown)]
  expect_setequal(setdiff(body_number_tokens(tx$paras), shown), character())   # every number has a fact
  refs <- utils::read.csv(file.path(out, "refs.csv"), colClasses = "character", encoding = "UTF-8")
  expect_match(refs$citation[1], "release of October 15, 2026 (data to August 2026)", fixed = TRUE)
})

test_that("when the story changes, the wording rules switch and the title carries the build date", {
  # 2026 months scaled down: permits fall from a year earlier, PGWPs fall
  b <- simulate_release(SEP15, "2026-08-01", "2026-10-15", scale = function(d)
    ifelse(d$year == 2026, round(d$value * ifelse(d$category == Q2_PGWP, 0.25, 0.55)), d$value))
  tx <- q2_note_text(q2_facts(b), b, as.Date("2026-10-20"))
  h <- setNames(tx$rules$holds, tx$rules$rule)
  expect_false(h[["rebound"]]); expect_false(h[["pgwp_drive"]]); expect_false(h[["pgwp_rose"]])
  expect_false(h[["bc_share_small"]]); expect_false(h[["bc_growth_all_graduates"]])
  expect_identical(tx$paras[1], "Data Insight \u2013 October 20, 2026")
  txt <- paste(tx$paras, collapse = "\n")
  for (s in c("climbing again", "Graduates drive", "rebound is small", "All of B.C.’s growth", "For B.C.:",
              "The rise is real", "may now be graduating", "despite rising permits", "more than all of B.C.’s growth",
              "grew only through graduates", "led by graduates", "highest January", "likely because new PGWPs", stale))
    expect_false(grepl(s, txt, fixed = TRUE), label = s)
  expect_match(txt, "Permits are falling.", fixed = TRUE)
  expect_match(txt, "fewer than in the same months of 2025", fixed = TRUE)
  expect_match(txt, "Table 1. Permits issued, B.C. and Canada, Jan\u2013Aug 2025 and Jan\u2013Aug 2026.", fixed = TRUE)
  shown <- tx$nums$shown[nzchar(tx$nums$shown)]
  expect_setequal(setdiff(body_number_tokens(tx$paras), shown), character())
})

test_that("a release in a later year drops the 2026 context (Statistics Canada) and handles January", {
  b <- simulate_release(SEP15, c("2026-08-01", "2026-09-01", "2026-10-01", "2026-11-01", "2026-12-01", "2027-01-01"), "2027-03-15")
  r <- q2_write_inputs(b, file.path(tempdir(), "q2_2027"), file.path(tempdir(), "figs"), as.Date("2027-03-20"), exports_dir = EXPORTS)
  tx <- r$tx
  h <- setNames(tx$rules$holds, tx$rules$rule)
  expect_false(h[["statcan_context"]])
  body <- tx$paras[seq_len(which(tx$paras == "References") - 1)]
  expect_false(any(grepl("[12]", body, fixed = TRUE)))
  expect_false(any(grepl("Statistics Canada estimates", body)))
  expect_true(any(grepl("In January 2027,", body, fixed = TRUE)))
  expect_false(grepl("non-permanent residents [12]", paste(unlist(r$spec$appendix_a), collapse = " "), fixed = TRUE))
  expect_false("context_statcan_npr" %in% tx$nums$fact_id)
  expect_identical(r$fx$P$LF, 2026L)
  for (s in c("July 2026", "Jan\u2013Jul")) expect_false(grepl(s, paste(tx$paras, collapse = " "), fixed = TRUE))
  expect_false(grepl("as they do in 2025\u20132026", paste(unlist(r$spec$appendix_a), collapse = " "), fixed = TRUE))
})

test_that("the note builds to a two-page body after the story changes (needs python-docx and LibreOffice)", {
  skip_if(!nzchar(Sys.which("python3")) || !nzchar(Sys.which("soffice")), "python3 or LibreOffice not available")
  skip_if(system2("python3", c("-c", shQuote("import docx")), stdout = FALSE, stderr = FALSE) != 0, "python-docx not installed")
  figs <- file.path(tempdir(), "placeholder_figs"); dir.create(figs, showWarnings = FALSE)
  dims <- c(f1 = "1864x862", f2 = "1864x1010", c1 = "1864x862", c2 = "2504x440", c3 = "1864x1062", c4 = "1864x1062",
            c5 = "1864x826", c6 = "1864x942", c7 = "1864x942", c8 = "1864x1244")
  for (k in names(dims)) { wh <- as.integer(strsplit(dims[[k]], "x")[[1]])
    grDevices::png(file.path(figs, q2_fig_files[[k]]), width = wh[1], height = wh[2]); graphics::plot.new(); grDevices::dev.off() }
  b <- simulate_release(SEP15, "2026-08-01", "2026-10-15", scale = function(d)
    ifelse(d$year == 2026, round(d$value * ifelse(d$category == Q2_PGWP, 0.25, 0.55)), d$value))
  out <- file.path(tempdir(), "q2_build_flip")
  q2_write_inputs(b, out, figs, as.Date("2026-10-20"), exports_dir = EXPORTS)
  st <- system2("python3", c(shQuote(file.path(EXPORTS, "q2", "build_note.py")), shQuote(file.path(out, "spec.json"))), stdout = TRUE, stderr = TRUE)
  expect_null(attr(st, "status"))
  system2("env", c("-u", "LD_LIBRARY_PATH", "soffice", paste0("-env:UserInstallation=file://", file.path(tempdir(), "lo_t")),
                   "--headless", "--convert-to", "pdf", "--outdir", shQuote(out), shQuote(file.path(out, "note.docx"))), stdout = FALSE, stderr = FALSE)
  pages <- pdftools::pdf_text(file.path(out, "note.pdf"))
  expect_identical(which(grepl("APPENDICES", pages))[1], 3L)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", pages)))
})

# ---- 3. Downloads tab -----------------------------------------------------------------
fake_manifest <- function(fp) list(release_phrase = "IRCC open data release of September 15, 2026 (data to July 2026)",
  release_date_text = "September 15, 2026", fingerprint = fp,
  files = list(list(file = "a.pdf", short = "the note", title = "Note title", description = "One line.", note = "Context note.", pages = 10),
               list(file = "b.pdf", short = "the guide", title = "How to use this dashboard", description = "Guide.", pages = 2),
               list(file = "c.pdf", short = "the description", title = "About the package and files", description = "Package.", pages = 1)))

test_that("the Downloads content lists each file with its pages and release, and warns on a data mismatch", {
  fp <- exports_fingerprint(SEP15$data, SEP15$meta)
  html <- as.character(downloads_content(fake_manifest(fp), SEP15$data, SEP15$meta))
  expect_match(html, "PDF, 10 pages, built for the IRCC release of September 15, 2026", fixed = TRUE)
  expect_match(html, "PDF, 1 page, built for", fixed = TRUE)
  expect_match(html, 'href="exports/a.pdf"', fixed = TRUE)
  expect_match(html, "Context note.", fixed = TRUE)
  expect_false(grepl("downloads-mismatch", html, fixed = TRUE))
  b <- simulate_release(SEP15, "2026-08-01", "2026-10-15")
  html2 <- as.character(downloads_content(fake_manifest(fp), b$data, b$meta))
  expect_match(html2, "downloads-mismatch", fixed = TRUE)
  expect_match(html2, "the dashboard now shows the IRCC open data release of October 15, 2026 (data to August 2026)", fixed = TRUE)
  expect_match(as.character(downloads_content(NULL, SEP15$data, SEP15$meta)), "not available", fixed = TRUE)
})

test_that("the built files match the release and the manifest", {
  dir <- file.path(PROJECT, "www", "exports")
  m <- read_exports_manifest(dir)
  skip_if(is.null(m), "PDFs not built (run tools/build_exports.R)")
  skip_if(!identical(m$fingerprint, exports_fingerprint(SEP15$data, SEP15$meta)), "PDFs built from other data")
  expect_identical(vapply(m$files, `[[`, "", "file"),
                   c("work-permits-canada-bc-note.pdf", "how-to-use-this-dashboard.pdf", "about-the-package-and-files.pdf"))
  for (x in m$files) expect_identical(pdftools::pdf_info(file.path(dir, x$file))$pages, as.integer(x$pages))
  expect_identical(as.integer(m$files[[1]]$pages), 11L); expect_identical(as.integer(m$files[[1]]$body_pages), 2L)
  expect_identical(as.integer(m$files[[2]]$pages), 2L)
  system2("pdftotext", c("-layout", shQuote(file.path(dir, m$files[[1]]$file)), shQuote(f <- tempfile(fileext = ".txt"))))
  skip_if(!file.exists(f) || !length(readLines(f, warn = FALSE)), "pdftotext not available")
  txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(txt, "Numbers update with each IRCC release; context written for the release of September 15, 2026", fixed = TRUE)
  expect_match(txt, "Data Insight", fixed = TRUE)
  expect_false(grepl("Mohsen_2|Mohsen_AI|analysis_q2\\.R|../q1/|ministry manager", txt))
})

test_that("the running app has a Downloads tab before Help, and its PDFs download", {
  skip_on_cran()
  dir <- file.path(PROJECT, "www", "exports")
  skip_if(is.null(read_exports_manifest(dir)), "PDFs not built")
  app <- shinytest2::AppDriver$new(PROJECT, name = "downloads", height = 900, width = 1300, load_timeout = 90000, timeout = 30000)
  on.exit(app$stop(), add = TRUE)
  tabs <- app$get_js("Array.from(document.querySelectorAll('.navbar-nav .nav-link')).map(function(a){return a.textContent.trim();})")
  tabs <- unlist(tabs)
  expect_identical(tail(tabs, 2), c("Downloads", "Help"))
  app$set_inputs(nav = "Downloads")
  txt <- app$get_text("#downloads-panel")
  expect_match(txt, "PDF, 11 pages, built for the IRCC release of September 15, 2026", fixed = TRUE)
  expect_match(txt, "written for the IRCC release of September 15, 2026", fixed = TRUE)
  expect_false(grepl("earlier version of the data", txt, fixed = TRUE))
  hrefs <- unlist(app$get_js("Array.from(document.querySelectorAll('#downloads-panel a[href^=\"exports/\"]')).map(function(a){return a.getAttribute('href');})"))
  expect_length(hrefs, 3)
  for (h in hrefs) {
    f <- tempfile(fileext = ".pdf")
    utils::download.file(paste0(sub("/$", "", app$get_url()), "/", h), f, mode = "wb", quiet = TRUE)
    expect_identical(readBin(f, "raw", 4), charToRaw("%PDF"))
    expect_identical(pdftools::pdf_info(f)$pages, pdftools::pdf_info(file.path(dir, basename(h)))$pages)
  }
})

test_that("deploy.R builds the PDFs first and stops the deploy if the build fails", {
  src <- paste(readLines(file.path(PROJECT, "tools", "deploy.R"), warn = FALSE), collapse = "\n")
  expect_match(src, "tools/build_exports.R", fixed = TRUE)
  expect_lt(regexpr("build_exports.R", src, fixed = TRUE), regexpr("deployApp(", src, fixed = TRUE))
  expect_match(src, "stop(\"Building the Downloads PDFs failed", fixed = TRUE)
  expect_match(src, "www/exports", fixed = TRUE)
})
