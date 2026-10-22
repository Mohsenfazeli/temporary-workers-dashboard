# Temporary Workers in Canada

An R Shiny dashboard of temporary work permits in Canada (TFWP and IMP) by province/territory and program, from January 2015 to the latest month published by Immigration, Refugees and Citizenship Canada (IRCC).

**Live app:** https://mohsenfazeli.shinyapps.io/temporary-workers-dashboard/ (the first visit can take up to a minute while the server wakes up).

![Overview tab](docs/overview.png)
![Trends tab](docs/trends.png)

## Data sources and licence

Main data under the [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada), from IRCC’s open data dataset *[Temporary Residents: Temporary Foreign Worker Program (TFWP) and International Mobility Program (IMP) Work Permit Holders – Monthly IRCC Updates](https://open.canada.ca/data/en/dataset/360024f2-17e9-4558-bfc1-3616485d65b9)*:

- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv
- https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv

No other data source is used for the figures. Policy-event markers cite official Government of Canada news releases as dated context only.

## Definitions

- Each value is a **flow**: work permits that took effect (IRCC: “became effective”) in that month, by intended province or territory—not the stock of people holding a valid permit.
- Values IRCC suppresses for privacy (`"--"`, 0–5) count as 0 and are flagged; figures where they could add more than 10% are marked “Possible undercount”.
- Counts include renewals and changes of status, so they can rise while arrivals fall.
- “Same months each year” (Annual view) compares January through the latest published month of every year with the current partial year.

## Views

| Tab | What it shows |
|-----|----------------|
| **Overview** | Headline totals, year-over-year change, and a national chart |
| **Trends** | Time series with optional policy-event markers |
| **Geography** | Provinces and territories ranked; share of Canada; contribution to national change |
| **Composition** | TFWP and IMP sub-programs |
| **Data** | Downloadable filtered table |
| **About** | Sources, measure definition, masking, and assumptions |
| **Downloads** | PDFs built from the same data (B.C. note, how-to guide, package description) |
| **Help** | Optional AI assistant (needs `GEMINI_API_KEY`; otherwise unavailable) |

By default the app shows a fixed IRCC release: the two files as published on September 15, 2026 (January 2015 to July 2026), in `data/baseline.rds`. Live refresh is optional (`TW_DATA_SOURCE=live`).

## Run locally

This repository holds the app code, the processed data and the package list.

**Windows, nothing to install:** download the [offline Windows package](https://github.com/Mohsenfazeli/temporary-workers-dashboard/releases/tag/v1.0-offline-windows) (about 200 MB; built with `tools/build_windows_offline.R`, not stored in this repository), unzip it, open `temporary-workers-dashboard` and double-click `run_dashboard.bat`. It includes R for Windows and the packages in `renv.lock`. Nothing is installed on the machine; close the console window to stop the app.

**Any computer with R** (Windows, macOS, Linux):

1. Install R from https://cloud.r-project.org (R 4.5 or newer recommended; built and tested with R 4.5.0). RStudio is optional.
2. Clone or download this repository and set R’s working directory to it.
3. Once, with internet: `install.packages("renv")`, then `renv::restore()`. Quicker alternative: `install.packages("shiny")`; the first `shiny::runApp()` installs the rest from CRAN.
4. Start: `Rscript tools/run_local.R`, `shiny::runApp()`, or on Windows double-click `run_dashboard.bat`.

**Live data:** `run_dashboard.bat live`, `Rscript tools/run_local.R live`, or `Sys.setenv(TW_DATA_SOURCE = "live"); shiny::runApp()`.

If a live download fails or fails its checks, the app falls back to `data/baseline.rds` (or to `data/cache.rds` if a previous successful live read wrote one) and says so in the header.

## Structure

- `app.R` — layout, sidebar filters, header status; loads data once at startup
- `R/data.R` — `load_release()` (default) and `load_data()` (live read, checks, cache)
- `R/calc.R` — calculations and formatting
- `R/theme.R` — colours, chart styling, source lines, policy events
- `R/mod_*.R` — one module per tab
- `R/assistant.R` — Help assistant
- `data/baseline.rds` — verified September 15, 2026 release shown by default
- `data_profile.md` — data profile, checks and assumptions
- `exports/` — templates and builders for the Downloads PDFs
- `tools/` — local run, rebuild, exports, deploy, offline Windows package, tests helpers
- `tests/` — testthat and shinytest2
- `renv.lock` — exact package versions

## Tests

```
Rscript tools/get_testdata.R          # one-off: local copies of the IRCC files
Rscript tests/testthat.R               # sets NOT_CRAN itself
```

- Browser tests need Google Chrome or Chromium.
- The live test reads IRCC directly and skips if IRCC can’t be reached.
- Performance tests time cold start from the saved copy (target under 10 s) and filter changes (median under 1 s).
- Downloads tests (`test-exports.R`) check note facts, wording rules and the Downloads tab; the full PDF build skips without python-docx or LibreOffice.

`Rscript tools/windows_check.R` runs the suite and prints ALL CHECKS PASSED only when there are 0 failures and 0 errors.

## Optional settings

| Variable | Role |
|----------|------|
| `TW_DATA_SOURCE` | `live` downloads IRCC at startup (default: shipped release) |
| `TW_LOCAL_DIR` | Local copies of the two IRCC CSV files |
| `TW_TIMEOUT` | Download timeout in seconds (default 60) |
| `TW_TRIES` | Download attempts (default 2) |
| `GEMINI_API_KEY` | Enables the Help assistant |
| `TW_GEMINI_MODEL` / `TW_GEMINI_FALLBACK` / `TW_GEMINI_TIMEOUT` | Assistant model and timeout |
| `TW_ASSISTANT_SESSION_MAX` / `TW_ASSISTANT_DAILY_MAX` | Assistant rate limits |
