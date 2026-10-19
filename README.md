# Temporary Workers in Canada: R Shiny dashboard (ECON 24R, Question 1)

An interactive dashboard of temporary work permits by province/territory and program (TFWP and IMP), from January 2015 to the latest month IRCC has published.

**Live dashboard:** https://mohsenfazeli.shinyapps.io/temporary-workers-dashboard/ (the first visit can take up to a minute while the server wakes up).

![Overview tab](docs/overview.png)
![Trends tab](docs/trends.png)

## Run it locally
1. Install R (4.3 or newer; built with 4.5.0) from https://cloud.r-project.org. RStudio is optional.
2. Clone or download this repository and open R in its folder (in RStudio: open the folder, or `setwd()` to it).
3. Start the app: `shiny::runApp()` (if the shiny package isn't installed yet, run `install.packages("shiny")` first). A browser tab opens. On first run every script installs any missing packages itself from CRAN as ready-made binaries, so no compiler (e.g. Rtools on Windows) is needed.
4. Optional, only if you want the exact package versions used to build it: `install.packages("renv"); renv::restore()`. This can take much longer, and on Windows it may need Rtools if an exact version is only available as source code.

The IRCC data download automatically at startup. If the IRCC website can't be reached, IRCC changes its file layout, or a download does not match the verified copy (`data/baseline.rds`) for months older than 12 months, the app uses the saved copy in `data/cache.rds` and says so in the header. The cache is only overwritten by a download that passes every check.

## Data
Main data, read live at startup (Open Government Licence – Canada):
- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv
- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv

Supplementary reference only: IRCC Excel tables (`EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx` and the IMP equivalent, on www.ircc.canada.ca). They give Canada's annual counts of distinct people. They are off by default: shown on the Trends tab only if the reader ticks "Show IRCC's own annual count", and in a table in About. Every figure in the dashboard comes from the two CSVs.

How the data are handled:
- The date range, year list and latest month are taken from the data, so a new IRCC month appears without code changes.
- Values IRCC suppresses for privacy ("--", 0–5) count as 0 and are flagged; figures where they could add more than 10% are marked "Possible undercount".

## Structure
- `app.R`: page layout, global sidebar controls, header status bar; loads the data once at startup.
- `R/data.R`: `load_data()` (live read, checks, tidy table, cache fallback, Excel reference).
- `R/calc.R`: calculation and formatting functions, all tested.
- `R/theme.R`: colours (colour-blind safe), chart styling, source lines.
- `R/mod_*.R`: one module per tab (Overview, Trends, Geography, Composition, Data, About).
- `www/styles.css`: government-style visual tweaks.
- `data/cache.rds`: offline fallback (IRCC data as downloaded on October 16, 2026; replaced on the server only by a download that passes every check).
- `data/baseline.rds`: verified copy of the same data, never overwritten. Each new download is compared with it for settled months (at least 12 months before its latest month); if more than 2% of cells or any full-year Canada program total (more than 1%) differ, the download is treated as damaged. Refresh it with `tools/build_cache_and_profile.R` when you redeploy with newer verified files.
- `data_profile.md`: data profile, checks and assumptions.
- `tests/`: testthat and shinytest2 tests.
- `tools/get_testdata.R`: downloads the four IRCC files into `testdata/` for the tests.
- `tools/build_cache_and_profile.R`: rebuilds `data/cache.rds` from local copies of the IRCC files in `raw_profile/` (not included; download them first).
- `tools/deploy.R`: publishes to shinyapps.io using the `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` environment variables.
- `renv.lock`: exact package versions.

## Tests
```
Rscript tools/get_testdata.R          # one-off: local copies of the IRCC files
NOT_CRAN=true Rscript tests/testthat.R
```
- The browser tests need Google Chrome or Chromium.
- The live test reads IRCC directly and skips with a message if IRCC can't be reached.
- The performance tests time the app starting from the saved copy (`data/cache.rds`; target under 10 s) and filter changes (median under 1 s, none over 2 s). A live start also downloads about 17 MB from IRCC, so it takes longer on a slow connection.
- `tests/testthat.R` exits with status 1 if any test fails or stops with an error; `Rscript tools/windows_check.R` runs everything and prints ALL CHECKS PASSED only when there are 0 failures and 0 errors.

## Optional settings (environment variables)
- `TW_LOCAL_DIR`: read local copies of the four IRCC files instead of the URLs (same code path).
- `TW_TIMEOUT`: download timeout in seconds per file once the server has answered (default 60). Before downloading, the app checks in parallel that each IRCC server answers (6-second connect limit), so an unreachable site falls back to the cache in about 6 seconds.
- `TW_TRIES`: download attempts (default 2).
