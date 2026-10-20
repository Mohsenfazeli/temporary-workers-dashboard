# Downloads tab: the PDFs built by tools/build_exports.R.
#  1. On the September 15, 2026 release the note equals the submitted note
#     (Mohsen_2): text, the 44 rounded body numbers and their exact values
#     (Appendix D), tables and appendix lines; the dated context line on the
#     byline and the figure date are the only differences.
#  2. On simulated newer releases (copies of the data with changed numbers;
#     test data only) the wording rules switch, the title falls back, and no
#     stale period ("July 2026", "Jan–Jul") is left.
#  3. The tab's content, the files and the mismatch notice; the running app.
for (f in list.files(file.path(PROJECT, "exports", "R"), pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
FIX <- file.path(PROJECT, "tests", "testthat", "fixtures", "exports_sep15")
SEP15 <- load_release(file.path(PROJECT, "data", "baseline.rds"))
EXPORTS <- file.path(PROJECT, "exports")
read_fix <- function(f) utils::read.csv(file.path(FIX, f), stringsAsFactors = FALSE, encoding = "UTF-8",
                                        colClasses = "character", na.strings = character())

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

# ---- 1. September 15, 2026: exact match --------------------------------------------
test_that("every fact equals the note's facts_q2.csv (ids, values, displays)", {
  fx <- q2_facts(SEP15)
  old <- read_fix("facts_q2.csv")
  common <- intersect(fx$facts$id, old$id)
  expect_gte(length(common), 110)
  o <- old[match(common, old$id), ]; n <- fx$facts[match(common, fx$facts$id), ]
  expect_equal(n$value, as.numeric(o$value), tolerance = 1e-12)
  expect_identical(n$display, o$display)
  expect_identical(n$type, o$type)
  P <- fx$P
  expect_identical(c(P$Y, P$M, P$PY, P$FY0, P$LF, P$YMAX, P$PH), c(2026L, 7L, 2025L, 2015L, 2025L, 2023L, 2024L))
})

test_that("Tables C1 and C2 equal the note's tables", {
  fx <- q2_facts(SEP15)
  g0 <- read_fix("geography_table.csv"); g1 <- fx$geo[seq_len(13), ]
  expect_identical(g1$geography, g0$geography)
  num <- c("p2015", "p2023", "p2025", "ytd2025", "ytd2026", "share_2025", "growth_2015_2023", "change_2023_2025",
           "ytd_change", "contrib_up", "contrib_ytd")
  expect_equal(unname(as.matrix(g1[, -1])), unname(apply(as.matrix(g0[, num]), 2, as.numeric)), tolerance = 1e-12)
  m0 <- read_fix("mix_2025.csv"); mm <- merge(m0, fx$mix, by = c("geography", "program", "category"))
  expect_equal(nrow(mm), nrow(m0)); expect_equal(nrow(fx$mix), nrow(m0))
  expect_equal(as.numeric(mm$value.x), mm$value.y)
})

test_that("the body is Mohsen's text plus the dated context line on the byline", {
  fx <- q2_facts(SEP15); tx <- q2_note_text(fx, SEP15, as.Date("2026-10-20"))
  orig <- readLines(file.path(FIX, "mohsen_text.md"), encoding = "UTF-8", warn = FALSE)
  orig <- orig[nzchar(trimws(orig))]
  want <- orig
  want[2] <- paste(orig[2], "\u00b7 Numbers update with each IRCC release; context written for the release of September 15, 2026")
  expect_identical(tx$paras, want)
  expect_true(all(tx$rules$holds[tx$rules$rule != "new_12_month_high"]))
  expect_false(tx$rules$holds[tx$rules$rule == "new_12_month_high"])
})

test_that("all 44 body numbers and Appendix D rows equal the note's", {
  fx <- q2_facts(SEP15); tx <- q2_note_text(fx, SEP15)
  bn <- read_fix("body_numbers.csv")
  expect_equal(nrow(tx$nums), 44L)
  for (col in c("shown", "fact_id", "rule", "statement", "location")) expect_identical(tx$nums[[col]], bn[[col]], label = col)
  # every rounded number equals its exact fact rounded (validate_q2.R rules)
  for (i in seq_len(nrow(bn))) if (bn$rule[i] %in% c("pct0", "k", "h", "M2"))
    expect_identical(q2_round(fx$val(bn$fact_id[i]), bn$rule[i]), bn$shown[i])
})

test_that("appendix lines, captions and table headers equal the note's (two dated lines aside)", {
  r <- q2_write_inputs(SEP15, file.path(tempdir(), "q2_sep15"), file.path(tempdir(), "figs"), as.Date("2026-10-20"),
                       exports_dir = EXPORTS)
  O <- r$original; S <- r$spec
  A0 <- unlist(O$appendix_a); A1 <- unlist(S$appendix_a)
  expect_identical(which(A0 != A1), 8L)                       # the figures' date only
  expect_identical(A1[8], sub("October 19, 2026", "October 20, 2026", A0[8], fixed = TRUE))
  for (k in 1:8) expect_identical(S$appendix_c[[k]][[3]], O$appendix_c[[k]][[3]])
  for (k in c("1", "2")) expect_identical(S$fig[[k]][[3]], O$fig[[k]][[3]])
  expect_identical(as.character(S$table_c1_headers), unlist(O$headers_c1))
  expect_identical(c(S$table_c1_caption, S$table_c2_caption, S$appendix_d_intro), unlist(O$P_calls))
  E0 <- unlist(O$appendix_e); E1 <- unlist(S$appendix_e)
  expect_identical(which(E0 != E1), 2L)                       # "(commit 3c6cae6)" -> the figures' version
  expect_identical(unlist(O$appendix_b), unlist(S$appendix_b))
  refs0 <- utils::read.csv(file.path(EXPORTS, "q2", "references_template.csv"), colClasses = "character", encoding = "UTF-8")
  refs1 <- utils::read.csv(file.path(tempdir(), "q2_sep15", "refs.csv"), colClasses = "character", encoding = "UTF-8")
  expect_identical(refs1, refs0)
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
  expect_match(tx$paras[2], "updated October 20, 2026 \u00b7 Numbers: release of October 15, 2026; context written for the release of September 15, 2026", fixed = TRUE)
  expect_match(tx$paras[grep("^\\[1\\]", tx$paras)], "release of October 15, 2026", fixed = TRUE)
  expect_false(any(grepl("\\{[a-z0-9_]+\\}", tx$paras)))
  shown <- tx$nums$shown[nzchar(tx$nums$shown)]
  expect_setequal(setdiff(body_number_tokens(tx$paras), shown), character())   # every number has a fact
  refs <- utils::read.csv(file.path(out, "refs.csv"), colClasses = "character", encoding = "UTF-8")
  expect_match(refs$citation[1], "release of October 15, 2026 (data to August 2026)", fixed = TRUE)
})

test_that("when the story changes, the wording rules switch and the title falls back", {
  # 2026 months scaled down: permits fall from a year earlier, PGWPs fall
  b <- simulate_release(SEP15, "2026-08-01", "2026-10-15", scale = function(d)
    ifelse(d$year == 2026, round(d$value * ifelse(d$category == Q2_PGWP, 0.25, 0.55)), d$value))
  tx <- q2_note_text(q2_facts(b), b, as.Date("2026-10-20"))
  h <- setNames(tx$rules$holds, tx$rules$rule)
  expect_false(h[["rebound"]]); expect_false(h[["pgwp_drive"]]); expect_false(h[["pgwp_rose"]])
  expect_false(h[["bc_share_small"]]); expect_false(h[["title_storyline"]]); expect_false(h[["bc_growth_all_graduates"]])
  expect_identical(tx$paras[1], "Work permits in Canada and B.C., January\u2013August 2026")
  txt <- paste(tx$paras, collapse = "\n")
  for (s in c("climbing again", "Graduates drive", "rebound is small", "All of B.C.’s growth", "For B.C.:",
              "The rise is real", "may now be graduating", "despite rising permits", stale))
    expect_false(grepl(s, txt, fixed = TRUE), label = s)
  expect_match(txt, "Permits are falling.", fixed = TRUE)
  expect_match(txt, "fewer than a year earlier", fixed = TRUE)
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

test_that("the built files match the release, the manifest and the submitted note", {
  dir <- file.path(PROJECT, "www", "exports")
  m <- read_exports_manifest(dir)
  skip_if(is.null(m), "PDFs not built (run tools/build_exports.R)")
  skip_if(!identical(m$fingerprint, exports_fingerprint(SEP15$data, SEP15$meta)), "PDFs built from other data")
  expect_identical(vapply(m$files, `[[`, "", "file"),
                   c("work-permits-canada-bc-note.pdf", "how-to-use-this-dashboard.pdf", "about-the-package-and-files.pdf"))
  for (x in m$files) expect_identical(pdftools::pdf_info(file.path(dir, x$file))$pages, as.integer(x$pages))
  expect_identical(as.integer(m$files[[1]]$pages), 10L); expect_identical(as.integer(m$files[[1]]$body_pages), 2L)
  expect_identical(as.integer(m$files[[2]]$pages), 2L)
  # the note's text equals Mohsen_2.pdf apart from the byline's context line and the two figure dates
  system2("pdftotext", c("-layout", shQuote(file.path(dir, m$files[[1]]$file)), shQuote(f <- tempfile(fileext = ".txt"))))
  new <- readLines(f, encoding = "UTF-8", warn = FALSE); old <- readLines(file.path(FIX, "mohsen_2_pdf.txt"), encoding = "UTF-8", warn = FALSE)
  norm <- function(x) { x <- gsub("\\s+", " ", trimws(x)); x[nzchar(x)] }
  dn <- setdiff(norm(new), norm(old)); do <- setdiff(norm(old), norm(new))
  allowed <- "Numbers update with each IRCC release|plain-language version, October|unchanged copy of the dashboard \\(|headless browser, sets the filters|^Mohsen Fazeli \u00b7 October 19, 2026$"
  expect_true(all(grepl(allowed, c(dn, do))),
              info = paste(c(dn, do), collapse = "\n"))
  expect_lte(length(dn), 4)
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
  expect_match(txt, "PDF, 10 pages, built for the IRCC release of September 15, 2026", fixed = TRUE)
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
