# =============================================================================
# tools/build_cache_and_profile.R
# Builds data/cache.rds from the two IRCC files that were downloaded by hand
# (Oct 16, 2026, 4:49-4:50 PM PT) and prints the checks used in
# data_profile.md. Run from the project folder: Rscript tools/build_cache_and_profile.R
# (Kept outside R/ so Shiny does not run it automatically at app start.)
# =============================================================================
required_packages <- c("dplyr", "tidyr", "readr", "lubridate", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
source("R/data.R")
suppressWarnings(file.remove("data/cache.rds"))

local_files <- c(TFWP = "raw_profile/ODP-TR-Work-TFWP-PT_program.csv",
                 IMP  = "raw_profile/ODP-TR-Work-IMP-PT_program.csv")
cat("MD5:", tools::md5sum(local_files), "\n")

# Same code path as the live read; only the source differs.
ref_files <- c(TFWP = "raw_profile/EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx",
               IMP  = "raw_profile/EN_ODP-TR-Work-IMP_PT_program_sign.xlsx")
d <- load_data(sources = local_files,
               reference_sources = ref_files,
               cache_path = "data/cache.rds", write_cache = TRUE,
               source_label = "downloaded 2026-10-16 4:50 PM PT",
               retrieved_at = as.POSIXct("2026-10-16 16:50:00",
                                         tz = "America/Vancouver"))
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

# Plausibility check against IRCC's "Understanding student and temporary
# worker numbers" page (new arrivals only, which is a narrower measure, so
# our counts must be at least as large every month).
arr <- data.frame(
  month = seq(as.Date("2023-12-01"), as.Date("2026-07-01"), by = "month"),
  tfw = c(5440,6520,6555,7545,10940,10525,10205,8455,7095,7960,6900,6020,4720,
          3880,3310,4775,7730,7435,6440,5390,4155,3995,3100,2535,2090,
          2650,2065,3475,6405,4585,3975,3580),
  imp = c(24875,26615,36350,56080,23745,24125,25775,21100,18935,21620,18315,
          15260,11815,10975,11040,13705,15835,16150,17480,12935,12545,13380,
          11360,10745,7580,9090,8735,10350,15310,15000,16320,11815))
ours <- x |> filter(geography == "Canada") |> group_by(month, program) |>
  summarise(v = sum(value), .groups = "drop") |>
  tidyr::pivot_wider(names_from = program, values_from = v)
cmp <- dplyr::inner_join(arr, ours, by = "month")
cat("Months compared:", nrow(cmp), "| TFWP >= arrivals every month:",
    all(cmp$TFWP >= cmp$tfw), "| IMP >= arrivals every month:",
    all(cmp$IMP >= cmp$imp), "\n")
print(d$reference$table); cat(substr(d$reference$notes,1,300),"\n")
cat("Cache written:", file.exists("data/cache.rds"),
    file.info("data/cache.rds")$size, "bytes\n")
