# =============================================================================
# tools/build_cache_and_profile.R
# Rebuilds the dashboard data from source: downloads the two IRCC CSV files
# itself (URLs in R/data.R), saves them to raw_profile/, rebuilds the tidy data
# with the same code the app uses, prints the checks used in data_profile.md,
# and compares the result with the release the app ships (data/baseline.rds).
#
# Run from the project folder (no manual download needed):
#   Rscript tools/build_cache_and_profile.R            download, rebuild, check
#   Rscript tools/build_cache_and_profile.R --local    use the files already in raw_profile/
#   Rscript tools/build_cache_and_profile.R --install  also replace data/cache.rds and
#                                                      data/baseline.rds (maintainers only;
#                                                      then set RELEASE_INFO in R/data.R)
# Without --install nothing in data/ is changed: the rebuilt data go to
# raw_profile/rebuilt.rds, so the dashboard keeps showing the shipped release.
# IRCC replaces the files at the same URLs with each release, so a download
# made after September 15, 2026 is a newer release than the shipped one; the
# script says so and reports how the two differ. It never invents data.
# (Kept outside R/ so Shiny does not run it automatically at app start.)
# =============================================================================
required_packages <- c("dplyr", "tidyr", "readr", "lubridate", "tibble", "curl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
source("R/calc.R")   # formatting helpers used by data.R (fmt_datetime)
source("R/data.R")

args       <- commandArgs(trailingOnly = TRUE)
use_local  <- "--local" %in% args
do_install <- "--install" %in% args
dir.create("raw_profile", showWarnings = FALSE)
local_files <- c(TFWP = "raw_profile/ODP-TR-Work-TFWP-PT_program.csv",
                 IMP  = "raw_profile/ODP-TR-Work-IMP-PT_program.csv")

if (!use_local) {
  # Download both files (browser user agent, retries; same code as the app).
  cat("Downloading the two IRCC files...\n")
  got <- fetch_many(IRCC_URLS, timeout = 180, tries = 3)
  bad <- vapply(got, is.character, logical(1))
  if (any(bad)) stop(paste(unlist(got[bad]), collapse = "; "),
                     "\nCheck the internet connection, or use --local with files in raw_profile/.", call. = FALSE)
  # Keep any older copies that differ (never silently overwrite a download).
  stamp <- format(Sys.time(), "%Y%m%d-%H%M")
  for (p in names(local_files)) {
    f <- local_files[[p]]
    if (file.exists(f) && !identical(readBin(f, "raw", file.info(f)$size), got[[p]])) {
      keep <- file.path("raw_profile", paste0("replaced_", stamp))
      dir.create(keep, showWarnings = FALSE)
      file.rename(f, file.path(keep, basename(f)))
      cat("Older copy kept in", keep, "\n")
    }
    writeBin(got[[p]], f)
  }
  retrieved <- Sys.time()
  # IRCC's release date for these files: the HTTP Last-Modified header.
  last_mod <- vapply(IRCC_URLS, function(u) {
    h <- tryCatch(curl::curl_fetch_memory(u, handle = curl::new_handle(nobody = TRUE,
           followlocation = TRUE, useragent = BROWSER_UA))$headers, error = function(e) NULL)
    if (is.null(h)) return(NA_character_)
    m <- regmatches(rawToChar(h), regexpr("(?i)last-modified:[^\r\n]*", rawToChar(h), perl = TRUE))
    if (length(m)) trimws(sub("(?i)last-modified:", "", m, perl = TRUE)) else NA_character_
  }, character(1))
  cat("IRCC Last-Modified:", paste(names(last_mod), last_mod, sep = " = ", collapse = "; "), "\n")
  label <- sprintf("downloaded %s", format(retrieved, "%Y-%m-%d %H:%M %Z"))
} else {
  if (!all(file.exists(local_files))) stop("--local: put both IRCC files in raw_profile/ first.", call. = FALSE)
  retrieved <- file.info(local_files[["TFWP"]])$mtime
  label <- sprintf("local files in raw_profile/ (dated %s)", format(retrieved, "%Y-%m-%d %H:%M"))
}
cat("MD5:", tools::md5sum(local_files), "\n")

# Same code path as the live read; only the source differs. Only the two
# IRCC CSV files are used (no other data source).
out_path <- file.path("raw_profile", "rebuilt.rds")
d <- load_data(sources = local_files,
               cache_path = out_path, write_cache = TRUE,
               source_label = label, retrieved_at = retrieved)
x <- d$data; det <- d$detail

cat("\nCoverage:", format(d$meta$first_month), "to", format(d$meta$latest_month),
    "| tidy rows:", nrow(x), "| detail rows:", nrow(det), "\n")
print(det |> count(program, name = "raw_rows"))
print(det |> group_by(program) |> summarise(share_masked = round(mean(masked), 3),
      masked = sum(masked), max_value = max(value)))

# Check 1: tidy totals equal the raw-cell totals (nothing lost or doubled).
raw_tot  <- det |> group_by(program) |> summarise(raw = sum(value))
tidy_tot <- x |> filter(geography == "Canada") |> group_by(program) |>
  summarise(tidy = sum(value))
print(left_join(raw_tot, tidy_tot, by = "program"))

# Check 2: Canada = sum of 13 provinces/territories + not stated (exact).
chk <- x |> group_by(month, program, category) |>
  summarise(canada = sum(value[geography == "Canada"]),
            parts = sum(value[geography != "Canada"]), .groups = "drop")
cat("Canada vs parts, max abs diff:", max(abs(chk$canada - chk$parts)), "\n")

# Check 3: one row per key, every month present.
cat("Duplicate keys:", anyDuplicated(x[c("month","geography","program","category")]), "\n")
cat("Months:", length(unique(x$month)), "expected:",
    length(seq(as.Date("2015-01-01"), d$meta$latest_month, by = "month")), "\n")

# Spot-checks: national monthly totals for three months.
spot <- c("2016-06-01", "2020-04-01", "2026-07-01")
sp <- x |> filter(geography == "Canada", month %in% as.Date(spot)) |>
  group_by(month, program) |> summarise(total = sum(value),
  masked_cells = sum(masked_cells), .groups = "drop") |>
  tidyr::pivot_wider(names_from = program, values_from = c(total, masked_cells))
print(as.data.frame(sp))

# Same three months recomputed independently, straight from the raw text.
for (p in names(local_files)) {
  r <- readr::read_tsv(local_files[[p]], col_types = readr::cols(.default = "c"),
                       na = character(), progress = FALSE)
  r$v <- suppressWarnings(as.numeric(r$TOTAL)); r$v[is.na(r$v)] <- 0
  r$m <- as.Date(sprintf("%s-%02d-01", r$EN_YEAR, match(r$EN_MONTH, month.abb)))
  print(tapply(r$v[r$m %in% as.Date(spot)], r$m[r$m %in% as.Date(spot)], sum))
}

# Annual national totals (sum of months; 2026 = Jan-latest month YTD).
ann <- x |> filter(geography == "Canada") |> group_by(year, program) |>
  summarise(total = sum(value), months = n_distinct(month), .groups = "drop") |>
  tidyr::pivot_wider(names_from = program, values_from = total)
print(as.data.frame(ann))

# Category totals 2015-latest, for the mapping table.
print(as.data.frame(x |> filter(geography == "Canada") |>
  group_by(program, category) |> summarise(total = sum(value), .groups = "drop")))

# Not-stated geography totals.
print(as.data.frame(x |> filter(geography == "Province/territory not stated") |>
  group_by(program) |> summarise(total = sum(value))))

# Hard checks: stop if the rebuilt data are not internally consistent.
stopifnot("tidy totals differ from raw totals" =
            isTRUE(all.equal(raw_tot$raw[order(raw_tot$program)], tidy_tot$tidy[order(tidy_tot$program)])),
          "Canada is not the sum of its parts" = max(abs(chk$canada - chk$parts)) == 0,
          "duplicate keys" = anyDuplicated(x[c("month","geography","program","category")]) == 0,
          "months missing" = length(unique(x$month)) ==
            length(seq(as.Date("2015-01-01"), d$meta$latest_month, by = "month")))
cat("\nRebuilt data written:", out_path, file.info(out_path)$size, "bytes (all checks passed)\n")

# Compare with the release the app shows by default (data/baseline.rds).
if (file.exists("data/baseline.rds")) {
  b <- readRDS("data/baseline.rds")$data
  key <- c("month", "geography", "program", "category")
  same <- nrow(b) == nrow(x) && isTRUE(all.equal(
    as.data.frame(dplyr::arrange(b[c(key, "value")], dplyr::across(dplyr::all_of(key)))),
    as.data.frame(dplyr::arrange(x[c(key, "value")], dplyr::across(dplyr::all_of(key)))),
    check.attributes = FALSE))
  cat(sprintf("\nShipped release: %s (data to %s). Rebuilt: data to %s.\n",
              fmt_date(RELEASE_INFO$date), fmt_month(max(b$month)), fmt_month(d$meta$latest_month)))
  if (same) {
    cat("RESULT: the rebuilt data are identical to data/baseline.rds (every cell).\n")
  } else {
    cat("RESULT: these files are a different IRCC release than the shipped one; the dashboard",
        "keeps showing the shipped release.\n")
    v <- tryCatch({ check_against_baseline(x, b); "passed" }, error = function(e) conditionMessage(e))
    cat("Baseline check of settled months:", v, "\n")
    yr <- function(z) stats::aggregate(value ~ program + year, sum,
            data = transform(z[z$geography == "Canada", ], year = lubridate::year(month)))
    cmp <- merge(yr(b), yr(x), by = c("program", "year"), all = TRUE, suffixes = c("_shipped", "_rebuilt"))
    cmp$change_pct <- round(100 * (cmp$value_rebuilt / cmp$value_shipped - 1), 2)
    print(cmp[order(cmp$program, cmp$year), ], row.names = FALSE)
  }
}

if (do_install) {
  # The same verified data become the baseline that every later live download
  # is checked against (R/data.R, check_against_baseline). Never written by the app.
  # It is also the dated release the app shows by default (load_release).
  file.copy(out_path, "data/cache.rds", overwrite = TRUE)
  file.copy(out_path, "data/baseline.rds", overwrite = TRUE)
  cat("\nInstalled as data/cache.rds and data/baseline.rds.",
      "NOW set RELEASE_INFO in R/data.R to IRCC's date for this release",
      "(the Last-Modified date above), then run Rscript tools/build_exports.R.\n")
} else {
  cat("\ndata/ was not changed. To adopt these data (maintainers): add --install.\n")
}
