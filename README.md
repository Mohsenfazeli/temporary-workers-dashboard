# Temporary Workers in Canada: R Shiny dashboard

An interactive dashboard of temporary work permits by province/territory and program (TFWP and IMP), from January 2015 to the latest month IRCC has published.

**Live dashboard:** https://mohsenfazeli.shinyapps.io/temporary-workers-dashboard/ (the first visit can take up to a minute while the server wakes up).

![Overview tab](docs/overview.png)
![Trends tab](docs/trends.png)

## Run it locally
This repository holds the app code, the processed data and the package list. Internet is needed only once, to install R packages.

1. Install R from https://cloud.r-project.org (R 4.5 or newer recommended; built and tested with R 4.5.0). RStudio is optional.
2. Clone or download this repository. Set R's working folder to it (RStudio: *Session > Set Working Directory > Choose Directory…*, or `setwd("C:/path/to/temporary-workers-dashboard")`); `getwd()` should show it.
3. **Quick run (recommended)**, in the R console: `install.packages("shiny")`, then `shiny::runApp()`. The first run installs about 90 R packages (ready-made versions from CRAN, so usually no extra tools are needed) and can take several minutes; then the dashboard opens in your browser.
4. **Exact versions (optional)**: `install.packages("renv")`, `renv::restore()`, then `shiny::runApp()` (slower; on Windows a package may need Rtools).

The **Help** tab (optional AI assistant) needs a `GEMINI_API_KEY`, which is not included; without it, everything else works as usual (see *Help assistant* below).

By default the app shows a fixed IRCC release: the two IRCC files as published on September 15, 2026 (January 2015 to July 2026), saved in `data/baseline.rds`. The header says "Data: IRCC open data release of September 15, 2026 (data to July 2026)". The date is IRCC's own: the files' Last-Modified date on the IRCC server and the "date modified" in the Open Government catalogue, both checked on October 18, 2026. Every visitor sees the same numbers, so the figures match the written notes.

Live refresh is an option for maintainers: start the app with `TW_DATA_SOURCE=live` and it downloads the IRCC files at startup. If the IRCC website can't be reached, IRCC changes its file layout, or a download does not match the verified copy for months older than 12 months, the app uses the saved copy in `data/cache.rds` and says so in the header. The cache is only overwritten by a download that passes every check.

## Data
Main data (Open Government Licence – Canada; saved as the September 15, 2026 release, or read live with `TW_DATA_SOURCE=live`):
- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv
- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv

No other data source is used: every figure in the dashboard comes from these two IRCC CSV files.

How the data are handled:
- The date range, year list and latest month are taken from the data, so a new IRCC month appears without code changes.
- Values IRCC suppresses for privacy ("--", 0–5) count as 0 and are flagged; figures where they could add more than 10% are marked "Possible undercount".
- Counts are work permits issued (IRCC's wording: permits that "became effective", i.e. signed and given to the person), not people or new arrivals; they include permits for people already in Canada (extensions and changes of status), so they can rise while arrivals fall (IRCC, Supplementary Information for the 2026–2028 Immigration Levels Plan).
- Dotted lines on the Trends charts mark six dated policy events, each linked to its official Government of Canada source. They are context only and do not imply cause.
- "Same months each year" (Annual view) compares January to July of every year with 2026.

## Help assistant (optional)
The **Help** tab has a small "Ask about this dashboard" assistant. It answers questions about how to use the dashboard, what the terms mean and what the data show, using only the dashboard's own text and a table of key figures computed from the same IRCC data (`R/assistant.R`). Answers are written by Google's Gemini API, called from the server, so the key never reaches the browser.
- It needs an API key in the environment variable `GEMINI_API_KEY` (free key from Google AI Studio). Without it the Help tab says the assistant is not available and everything else works as usual.
- Optional settings: `TW_GEMINI_MODEL` (default `gemini-3.5-flash-lite`, which the free tier allows many more questions a day than Flash), `TW_GEMINI_FALLBACK` (default `gemini-3.5-flash`, tried once if the first model is busy or over its limit), `TW_GEMINI_TIMEOUT` (seconds, default 30), `TW_ASSISTANT_SESSION_MAX` (questions per visit, default 20), `TW_ASSISTANT_DAILY_MAX` (questions per day, default 300).
- Questions are limited to 500 characters. AI answers can be wrong; the charts and About the data are the reference.
- `tools/deploy.R` uploads the key in a temporary `.Renviron` that is deleted after the upload. Never commit a key: `.Renviron` is in `.gitignore`.

## Downloads tab (PDFs)
The **Downloads** tab offers three PDF files built from the same data as the dashboard, with no AI: a two-page note on work permits for B.C. (with appendices), *How to use this dashboard* and *About the package and files*.
- `tools/build_exports.R` builds them into `www/exports/` (with `manifest.json`: release, page counts and a fingerprint of the data). It takes pictures of the dashboard from a temporary local copy in headless Chrome, writes the note's Word file with `exports/q2/build_note.py` (python-docx) and converts it with LibreOffice; the two guides are R Markdown printed by Chrome. Needs Chrome or Chromium, python3 with python-docx, and LibreOffice. The files are replaced only if every check passes.
- `tools/deploy.R` runs it before every upload and stops the deploy if it fails (the free shinyapps.io servers cannot run Chrome or LibreOffice, so the files are built before the upload, not on the server).
- The note's numbers, figures and words for direction ("rose", "more than tripled", "falling faster in B.C.") follow the data through tested rules (`exports/R/note_q2_text.R`); its context, causes and implications were written for the IRCC release of September 15, 2026, and the note says so on its byline. With that release the note equals the submitted note (checked by `tests/testthat/test-exports.R`).
- If the dashboard shows other data than the files were built from (for example in live mode after a new IRCC release), the tab says so. `www/exports/` is not in git; without it the tab says the files are not available.

## Structure
- `app.R`: page layout, global sidebar controls, header status bar; loads the data once at startup.
- `R/data.R`: `load_release()` (default: the dated IRCC release) and `load_data()` (live read, checks, tidy table, cache fallback).
- `R/calc.R`: calculation and formatting functions, all tested.
- `R/theme.R`: colours (colour-blind safe), chart styling, source lines.
- `R/mod_*.R`: one module per tab (Overview, Trends, Geography, Composition, Data, About, Downloads, Help).
- `R/assistant.R`: the Help assistant (context, limits, Gemini call, fallbacks).
- `www/styles.css`: government-style visual tweaks.
- `data/cache.rds`: offline fallback (IRCC data as downloaded on October 16, 2026; replaced on the server only by a download that passes every check).
- `data/baseline.rds`: the IRCC release of September 15, 2026 that the app shows by default (date in `RELEASE_INFO`, `R/data.R`); never overwritten. Each new download is compared with it for settled months (at least 12 months before its latest month); if more than 2% of cells or any full-year Canada program total (more than 1%) differ, the download is treated as damaged. Refresh it with `tools/build_cache_and_profile.R` when you redeploy with a newer verified release, and update `RELEASE_INFO` to IRCC's date for that release.
- `data_profile.md`: data profile, checks and assumptions.
- `tests/`: testthat and shinytest2 tests.
- `tools/get_testdata.R`: downloads the two IRCC CSV files into `testdata/` for the tests.
- `tools/build_cache_and_profile.R`: rebuilds `data/cache.rds` from local copies of the IRCC files in `raw_profile/` (not included; download them first).
- `exports/`: templates and code for the Downloads PDFs (note facts, text rules, Word builder, guides).
- `tools/build_exports.R`: builds the Downloads PDFs.
- `tools/deploy.R`: builds the Downloads PDFs, then publishes to shinyapps.io using the `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` environment variables, or an account already set up on the computer (no secrets stored).
- `renv.lock`: exact package versions.

## Tests
```
Rscript tools/get_testdata.R          # one-off: local copies of the IRCC files
Rscript tests/testthat.R               # sets NOT_CRAN itself
```
In the R console instead: `source("tools/get_testdata.R")`, then `system2(file.path(R.home("bin"), "Rscript"), "tests/testthat.R")`. The tests need internet.
- The browser tests need Google Chrome or Chromium.
- The live test reads IRCC directly and skips with a message if IRCC can't be reached.
- The performance tests time the app starting from the saved copy (`data/cache.rds`; target under 10 s) and filter changes (median under 1 s, none over 2 s). A live start also downloads about 17 MB from IRCC, so it takes longer on a slow connection.
- The Downloads tests (`test-exports.R`) check that the note built from the September 15, 2026 release equals the submitted note (frozen copies in `tests/testthat/fixtures/exports_sep15/`) and that simulated newer releases switch the wording rules; the build test skips without python-docx or LibreOffice, and the file tests skip until `tools/build_exports.R` has been run.
- `tests/testthat.R` exits with status 1 if any test fails or stops with an error; `Rscript tools/windows_check.R` runs everything and prints ALL CHECKS PASSED only when there are 0 failures and 0 errors.

## Optional settings (environment variables)
- `TW_DATA_SOURCE`: `live` downloads the IRCC files at startup instead of showing the saved release (default).
- `TW_LOCAL_DIR`: read local copies of the two IRCC CSV files instead of the URLs (same code path as live).
- `TW_TIMEOUT`: download timeout in seconds per file once the server has answered (default 60). Before downloading, the app checks in parallel that each IRCC server answers (6-second connect limit), so an unreachable site falls back to the cache in about 6 seconds.
- `TW_TRIES`: download attempts (default 2).
