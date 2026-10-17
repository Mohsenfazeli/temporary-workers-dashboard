# =============================================================================
# R/data.R: live download, cleaning and validation of the two IRCC files
# -----------------------------------------------------------------------------
# Question 1, Temporary Workers dashboard (BC ECON 24R, REQ 125819)
#
# What this file does, in order:
#   1. Downloads both IRCC open-data CSVs straight from their URLs (live).
#   2. Works out the text encoding (UTF-8 or Latin-1), the delimiter (tab or
#      comma), where the header row is, and drops any footnote lines.
#   3. Checks that every expected column is present. If IRCC changes the
#      layout, it stops with a clear message instead of showing wrong numbers.
#   4. Tidies the data to ONE ROW per month x geography x program x category.
#   5. Treats masked cells ("--", 0 to 5 in IRCC's footnote wording) as 0 in sums, but
#      keeps a count of them so the dashboard can flag them.
#   6. Builds a "Canada" total as the sum of all provinces, territories and
#      "Province/territory not stated" (the files have no Canada row).
#   7. Returns the data plus metadata (when retrieved, latest month, source).
#   8. Saves data/cache.rds after every successful live read; if the live
#      read fails OR the file layout has changed, it loads that cache instead
#      and records a clear warning naming the problem.
#   9. Also reads IRCC's two Excel tables, used ONLY as a reference for
#      Canada's annual count of distinct people (IRCC de-duplicates people
#      within a year, so these are 1-3% below the sum of the months). If the
#      Excel read fails, the dashboard still works and hides the reference.
# =============================================================================

# ---- 0. Packages: install any that are missing, then load them -------------
required_packages <- c("dplyr", "tidyr", "readr", "lubridate", "tibble", "curl",
                       "readxl")
missing_packages  <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(tibble)
  library(readxl)
})

# ---- 1. Constants -----------------------------------------------------------

# The two official data sources named in the assignment (and nothing else).
IRCC_URLS <- c(
  TFWP = "https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv",
  IMP  = "https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv"
)

# Supplementary reference (Excel versions of the same tables). Used ONLY for
# IRCC's distinct-people annual totals for Canada; never for the core data.
IRCC_REFERENCE_URLS <- c(
  TFWP = "https://www.ircc.canada.ca/opendata-donneesouvertes/data/EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx",
  IMP  = "https://www.ircc.canada.ca/opendata-donneesouvertes/data/EN_ODP-TR-Work-IMP_PT_program_sign.xlsx"
)

# The open-data catalogue page that documents both files (cited in the app).
IRCC_METADATA_URL <- "https://open.canada.ca/data/en/dataset/360024f2-17e9-4558-bfc1-3616485d65b9"

# Columns we rely on. If any of these disappear or are renamed, we stop.
EXPECTED_COLUMNS <- c(
  "EN_YEAR", "EN_QUARTER", "EN_MONTH", "EN_PROVINCE_TERRITORY",
  "EN_PROGRAM_LEVEL_2", "EN_PROGRAM_LEVEL_3",
  "EN_PROGRAM_LEVEL_4", "EN_PROGRAM_LEVEL_5", "TOTAL"
)

# The 13 provinces and territories, in the usual west-to-east-then-north
# order Statistics Canada uses, plus IRCC's "not stated" group.
PROVINCES_TERRITORIES <- c(
  "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia",
  "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan",
  "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
)
NOT_STATED_GEO <- "Province/territory not stated"

# Browser-like user agent: the IRCC server has refused some scripted requests.
BROWSER_UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/129.0 Safari/537.36"
)

# Display order of the dashboard categories (used for consistent colours).
CATEGORY_LEVELS <- c(
  # TFWP
  "Seasonal Agricultural Worker Program",
  "Other agricultural workers",
  "Caregivers",
  "Other TFWP workers with an LMIA",
  "Other TFWP",
  # IMP
  "Post-graduation work permits",
  "Spouses and family members",
  "International Experience Canada",
  "Significant benefit and PR-bridging permits",
  "Trade and other agreements",
  "Other Canadian interests",
  "Vulnerable workers",
  "Unknown or other IMP"
)

# ---- 2. Category mapping ----------------------------------------------------
# IRCC's program hierarchy has up to 4 levels (LEVEL_2 broadest, LEVEL_5 most
# detailed); the IMP file has 107 detailed categories, too many for a chart.
# map_category() groups them into a few plain-language categories. Each rule
# is written out so it can be explained and checked against the mapping
# table in data_profile.md. Anything new that IRCC adds and no rule matches
# lands in "Other TFWP" or "Unknown or other IMP", and is reported in the metadata.
map_category <- function(program, l2, l3, l4) {
  dplyr::case_when(
    # --- TFWP (Temporary Foreign Worker Program) ---
    program == "TFWP" & l3 == "Seasonal Agricultural Worker Program"
      ~ "Seasonal Agricultural Worker Program",
    program == "TFWP" & l2 == "Agricultural Workers"
      ~ "Other agricultural workers",          # Agricultural Stream + other
    program == "TFWP" & l2 %in% c("Caregivers", "Live-In Caregivers")
      ~ "Caregivers",
    program == "TFWP" & l2 == "Other Temporary Foreign Workers with LMIA"
      ~ "Other TFWP workers with an LMIA",
    program == "TFWP"
      ~ "Other TFWP",                           # safety net for new codes
    # --- IMP (International Mobility Program) ---
    program == "IMP" & l4 == "Post-grad employment"
      ~ "Post-graduation work permits",
    program == "IMP" & l4 %in% c("Spouses of skilled workers",
                                 "Spouses of students",
                                 "Dependent of skilled worker",
                                 "Family members of Economic PR applicants")
      ~ "Spouses and family members",
    program == "IMP" & l4 == "International Experience Canada (IEC)"
      ~ "International Experience Canada",
    program == "IMP" & l3 == "Significant benefit"
      ~ "Significant benefit and PR-bridging permits",
    program == "IMP" & l2 == "Agreements"
      ~ "Trade and other agreements",
    program == "IMP" & l2 == "Canadian Interests"
      ~ "Other Canadian interests",
    program == "IMP" & l2 == "Vulnerable workers"
      ~ "Vulnerable workers",
    program == "IMP"
      ~ "Unknown or other IMP"                  # incl. "Unknown IMP"
  )
}

# ---- 3. Helper: fetch files as raw bytes -----------------------------------
# `srcs` are URLs (live read) or local file paths (used for testing, because
# some networks cannot reach the IRCC server). URLs are downloaded at the
# same time (libcurl can fetch several files in parallel), which keeps the
# app's start-up fast. Returns a list of raw vectors, or an error string for
# each file that could not be fetched.
fetch_many <- function(srcs, timeout = 60, tries = 2) {
  out <- vector("list", length(srcs))
  names(out) <- names(srcs)
  is_url <- grepl("^https?://", srcs)

  # Local files: just read the bytes.
  for (i in which(!is_url)) {
    out[[i]] <- if (file.exists(srcs[[i]])) {
      readBin(srcs[[i]], what = "raw", n = file.info(srcs[[i]])$size)
    } else {
      paste("Local data file not found:", srcs[[i]])
    }
  }
  if (!any(is_url)) return(out)

  # Quick reachability check first: one small request (headers only) per web
  # host, giving up after 8 seconds. If a host cannot be reached, its files
  # are marked as failed straight away, so the app falls back to the saved
  # copy in seconds instead of waiting for full download timeouts.
  host_of <- function(u) sub("^https?://([^/]+).*$", "\\1", u)
  url_list <- unname(srcs[is_url])
  first_per_host <- url_list[!duplicated(host_of(url_list))]   # one URL per host
  reachable <- stats::setNames(hosts_reachable(first_per_host), host_of(first_per_host))
  todo <- which(is_url)
  for (i in todo) {
    if (!reachable[[host_of(srcs[[i]])]]) {
      out[[i]] <- paste("Download failed (IRCC server not reachable):", srcs[[i]])
    }
  }
  todo <- todo[vapply(out[todo], is.null, logical(1))]
  if (length(todo) == 0) return(out)

  # URLs: download in parallel, retrying the ones that failed.
  old <- options(timeout = timeout, HTTPUserAgent = BROWSER_UA)
  on.exit(options(old), add = TRUE)            # restore the user's options
  for (attempt in seq_len(tries)) {
    dest <- vapply(todo, function(i) tempfile(fileext = ".dat"), character(1))
    try(suppressWarnings(utils::download.file(
      unname(srcs[todo]), dest, mode = "wb", quiet = TRUE, method = "libcurl",
      headers = c("User-Agent" = BROWSER_UA, "Accept" = "*/*"))),
      silent = TRUE)
    for (k in seq_along(todo)) {
      i <- todo[k]
      if (file.exists(dest[k]) && file.info(dest[k])$size > 0) {
        out[[i]] <- readBin(dest[k], what = "raw", n = file.info(dest[k])$size)
      } else {
        out[[i]] <- paste("Download failed:", srcs[[i]])
      }
      unlink(dest[k])
    }
    todo <- todo[vapply(out[todo], is.character, logical(1))]
    if (length(todo) == 0) break
    if (attempt < tries) Sys.sleep(1)          # short pause, then retry
  }
  out
}

# Can these URLs' servers be reached? Sends one request per URL for the
# headers only (no download), all at the same time, with a 6-second
# connection limit and 8 seconds overall. Returns TRUE/FALSE per URL.
hosts_reachable <- function(urls, connect_timeout = 6, total_timeout = 8) {
  ok <- stats::setNames(rep(FALSE, length(urls)), urls)
  pool <- curl::new_pool()
  for (u in urls) {
    h <- curl::new_handle(nobody = TRUE, connecttimeout = connect_timeout,
                          timeout = total_timeout, useragent = BROWSER_UA,
                          followlocation = TRUE)
    # local() keeps the right URL in each callback
    local({
      url <- u
      curl::curl_fetch_multi(url, handle = h, pool = pool,
        # Any HTTP answer (even "method not allowed") means the server is up;
        # the real download then reports any file-level problem.
        done = function(res) ok[[url]] <<- res$status_code > 0,
        fail = function(msg) ok[[url]] <<- FALSE)
    })
  }
  curl::multi_run(pool = pool, timeout = total_timeout + 2)
  unname(ok)
}

# Single-file wrapper (stops with a clear message if the file cannot be read).
fetch_raw <- function(src, timeout = 60, tries = 2) {
  res <- fetch_many(c(file = src), timeout = timeout, tries = tries)[[1]]
  if (is.character(res)) stop(res, call. = FALSE)
  res
}

# ---- 4. Helper: raw bytes -> data frame of character columns ----------------
parse_ircc_file <- function(bytes, program) {
  # 4a. Strip a UTF-8 byte-order mark if there is one.
  if (length(bytes) >= 3 && all(bytes[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]
  }
  # 4b. Encoding: if the bytes are valid UTF-8 use that, otherwise Latin-1.
  text <- rawToChar(bytes)
  encoding <- if (validUTF8(text)) "UTF-8" else "Latin-1"
  if (encoding == "Latin-1") text <- iconv(text, from = "latin1", to = "UTF-8")
  Encoding(text) <- "UTF-8"

  # 4c. Split into lines (the files use Windows CRLF line endings).
  lines <- strsplit(text, "\r?\n")[[1]]

  # 4d. Header row = first line containing EN_YEAR (allows title rows above).
  header_at <- grep("EN_YEAR", lines, fixed = TRUE)[1]
  if (is.na(header_at)) {
    stop(sprintf("[%s] Layout changed: no header row with 'EN_YEAR' found. ",
                 program), "Check the IRCC file before using it.",
         call. = FALSE)
  }
  # 4e. Delimiter: tab if the header contains tabs, otherwise comma.
  delim <- if (grepl("\t", lines[header_at], fixed = TRUE)) "\t" else ","

  # 4f. Data lines = lines after the header whose first field is a 4-digit
  #     year. Anything else (blank lines, notes, footnotes) is set aside.
  body     <- lines[(header_at + 1):length(lines)]
  is_data  <- grepl("^\"?\\d{4}\"?[\t,]", body)
  footnotes <- trimws(body[!is_data & nzchar(trimws(body))])

  df <- readr::read_delim(
    I(paste(c(lines[header_at], body[is_data]), collapse = "\n")),
    delim = delim, col_types = readr::cols(.default = "c"),
    na = character(), trim_ws = TRUE, progress = FALSE
  )

  # 4g. Validate the layout: every expected column must be present.
  missing_cols <- setdiff(EXPECTED_COLUMNS, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("[%s] Layout changed: missing column(s) %s. Found: %s",
                 program, paste(missing_cols, collapse = ", "),
                 paste(names(df), collapse = ", ")), call. = FALSE)
  }

  list(df = df, encoding = encoding,
       delimiter = if (delim == "\t") "tab" else "comma",
       header_row = header_at, footnotes = footnotes,
       raw_rows = nrow(df), raw_columns = names(df))
}

# ---- 5. Helper: clean one parsed file into tidy detail rows -----------------
clean_ircc_file <- function(df, program) {
  # Tidy whitespace, including non-breaking spaces IRCC uses in some labels.
  squish <- function(x) trimws(gsub("[\u00A0[:space:]]+", " ", x))

  out <- df |>
    transmute(
      program    = program,
      year       = as.integer(EN_YEAR),
      month_num  = match(squish(EN_MONTH), month.abb),  # "Jan" -> 1, etc.
      geography  = squish(EN_PROVINCE_TERRITORY),
      level_2    = squish(EN_PROGRAM_LEVEL_2),
      level_3    = squish(EN_PROGRAM_LEVEL_3),
      level_4    = squish(EN_PROGRAM_LEVEL_4),
      level_5    = squish(EN_PROGRAM_LEVEL_5),
      value_raw  = squish(TOTAL)
    )

  # Months must be recognisable English abbreviations.
  if (anyNA(out$month_num) || anyNA(out$year)) {
    stop(sprintf("[%s] Unrecognised year or month values in the file.",
                 program), call. = FALSE)
  }
  # Values must be whole numbers or IRCC's mask "--". Anything else = stop.
  bad <- setdiff(unique(out$value_raw[!grepl("^[0-9,]+$", out$value_raw)]), "--")
  if (length(bad) > 0) {
    stop(sprintf("[%s] Unexpected value(s) in TOTAL column: %s", program,
                 paste(head(bad, 5), collapse = ", ")), call. = FALSE)
  }

  out |>
    mutate(
      month  = lubridate::make_date(year, month_num, 1L),  # 1st of the month
      masked = value_raw == "--",                          # 0 to 5 per IRCC's footnote
      value  = as.numeric(ifelse(masked, "0", gsub(",", "", value_raw))),
      # Built-in totals would be labelled "Total"/"Canada"; flag them so they
      # can be removed (none exist in the October 2026 files).
      is_builtin_total = geography %in% c("Canada", "Total") |
        grepl("^total", level_2, ignore.case = TRUE),
      category = map_category(program, level_2, level_3, level_4)
    )
}

# ---- 5b. Reference: IRCC's distinct-people annual totals for Canada --------
# Reads one IRCC Excel table and returns Canada's annual total of distinct
# people ("<year> Total" columns in the national "Total" row). The Excel
# layout is: title rows, then a header row with "2015 Total", "2016 Total"...
# above the month row, then province/program rows, then a row labelled
# exactly "Total" (Canada), then notes.
parse_ircc_reference <- function(bytes, program) {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  writeBin(bytes, tmp)
  sheet <- readxl::read_excel(tmp, sheet = 1, col_names = FALSE,
                              col_types = "text", .name_repair = "minimal")
  m <- as.matrix(sheet)
  m[is.na(m)] <- ""

  # Columns whose header says "<year> Total" hold the annual figures.
  hits <- which(matrix(grepl("^\\s*\\d{4} Total\\s*$", m), nrow(m)), arr.ind = TRUE)
  if (nrow(hits) == 0) stop("[", program, " reference] no '<year> Total' columns found")
  year_cols  <- hits[, "col"]
  year_label <- trimws(m[hits])

  # The national row is labelled exactly "Total" in one of the label columns
  # (the columns to the left of the first data column).
  first_data <- min(which(apply(m, 2, function(x) any(grepl("^(Jan|Q1)$", x)))))
  label_cols <- seq_len(first_data - 1)
  total_row <- which(apply(m[, label_cols, drop = FALSE], 1,
                           function(x) any(trimws(x) == "Total")))
  if (length(total_row) != 1) stop("[", program, " reference] national 'Total' row not found")

  raw_vals <- trimws(m[total_row, year_cols])
  # Notes cell: the long text starting with "Notes".
  notes <- m[grepl("^Notes", m)]
  source_line <- m[grepl("^Source: IRCC", m)]

  tibble(
    program = program,
    year    = as.integer(sub(" Total", "", year_label)),
    people  = suppressWarnings(as.numeric(gsub(",", "", raw_vals))),
    people_raw = raw_vals
  ) |>
    filter(!is.na(year)) |>
    arrange(year) |>
    structure(notes = if (length(notes)) notes[1] else NA_character_,
              source_line = if (length(source_line)) source_line[1] else NA_character_)
}

# Combine the two programs' references; returns NULL (with a reason) on failure.
build_reference <- function(raw_list) {
  out <- tryCatch({
    parts <- lapply(names(raw_list), function(p) {
      if (is.character(raw_list[[p]])) stop(raw_list[[p]])
      parse_ircc_reference(raw_list[[p]], p)
    })
    tbl <- bind_rows(parts)
    list(table = mutate(tbl, geography = "Canada"),
         notes = attr(parts[[1]], "notes"),
         source_line = attr(parts[[1]], "source_line"),
         urls = IRCC_REFERENCE_URLS, note = NULL)
  }, error = function(e) {
    list(table = NULL, notes = NULL, source_line = NULL,
         urls = IRCC_REFERENCE_URLS,
         note = paste("IRCC's reference count of distinct people is not",
                      "available right now:", conditionMessage(e)))
  })
  out
}

# ---- 6. Main function -------------------------------------------------------
#' Load the TFWP and IMP data (live first, cache as fallback)
#'
#' @param sources     Named vector c(TFWP = ..., IMP = ...) of URLs or local
#'                    paths for the two core CSV files.
#' @param reference_sources Named vector c(TFWP = ..., IMP = ...) of URLs or
#'                    local paths for IRCC's Excel tables (reference only).
#'                    Use NULL to skip the reference.
#' @param cache_path  Where to save / read the offline copy.
#' @param timeout     Seconds to wait for the downloads.
#' @param tries       Download attempts before giving up.
#' @param source_label Optional text describing where the data came from.
#' @param retrieved_at Optional POSIXct for when files were downloaded
#'                    (for local files downloaded earlier); defaults to now.
#' @param write_cache Save the result to cache_path. Defaults to TRUE for a
#'                    live read and FALSE for local test files.
#' @return list(data = tidy tibble, detail = cell-level tibble,
#'              reference = IRCC distinct-people reference (or NULL table),
#'              meta = metadata list)
load_data <- function(sources = IRCC_URLS,
                      reference_sources = IRCC_REFERENCE_URLS,
                      cache_path = file.path("data", "cache.rds"),
                      timeout = 60,
                      tries = 2,
                      source_label = NULL,
                      retrieved_at = NULL,
                      write_cache = NULL) {

  is_url <- all(grepl("^https?://", sources))
  if (is.null(write_cache)) write_cache <- is_url

  # 6a. Fetch everything at once (core CSVs + reference Excel files).
  all_srcs <- c(sources, if (!is.null(reference_sources))
    setNames(reference_sources, paste0("ref_", names(reference_sources))))
  raw <- fetch_many(all_srcs, timeout = timeout, tries = tries)

  result <- tryCatch({
    core_raw <- raw[names(sources)]
    failed <- vapply(core_raw, is.character, logical(1))
    if (any(failed)) stop(paste(unlist(core_raw[failed]), collapse = "; "),
                          call. = FALSE)

    # 6b. Parse each program's file through the same code path.
    parsed <- lapply(names(sources), function(p) parse_ircc_file(core_raw[[p]], p))
    names(parsed) <- names(sources)

    # 6c. Clean and stack the two files.
    detail <- bind_rows(lapply(names(parsed), function(p) {
      clean_ircc_file(parsed[[p]]$df, p)
    }))

    # 6d. Drop built-in totals (if IRCC ever adds them) to avoid double counts.
    n_builtin_totals <- sum(detail$is_builtin_total)
    detail <- filter(detail, !is_builtin_total)

    # 6e. Every geography must be a known province/territory or "not stated".
    unknown_geo <- setdiff(unique(detail$geography),
                           c(PROVINCES_TERRITORIES, NOT_STATED_GEO))
    if (length(unknown_geo) > 0) {
      stop("Layout changed: unexpected geography value(s): ",
           paste(unknown_geo, collapse = ", "), call. = FALSE)
    }

    # 6f. No duplicate cells (same month, place and full program path).
    dup <- detail |>
      count(program, month, geography, level_2, level_3, level_4, level_5) |>
      filter(n > 1)
    if (nrow(dup) > 0) stop("Layout changed: duplicate rows found in the IRCC file(s).",
                            call. = FALSE)

    # 6g. Aggregate detailed cells into dashboard categories. Missing
    #     combinations (IRCC omits zero cells) are filled with 0 so that
    #     every month x geography x program x category exists exactly once.
    all_months  <- seq(min(detail$month), max(detail$month), by = "month")
    cat_by_prog <- distinct(detail, program, category)

    by_pt <- detail |>
      group_by(month, geography, program, category) |>
      summarise(value = sum(value), cells = n(),
                masked_cells = sum(masked), .groups = "drop") |>
      right_join(
        tidyr::crossing(month = all_months,
                        geography = c(PROVINCES_TERRITORIES, NOT_STATED_GEO),
                        cat_by_prog),
        by = c("month", "geography", "program", "category")) |>
      mutate(across(c(value, cells, masked_cells), ~ coalesce(.x, 0)))

    # 6h. Canada = provinces + territories + "not stated" (no Canada row in
    #     the files). Never add Canada to the other geographies again.
    canada <- by_pt |>
      group_by(month, program, category) |>
      summarise(value = sum(value), cells = sum(cells),
                masked_cells = sum(masked_cells), .groups = "drop") |>
      mutate(geography = "Canada")

    data <- bind_rows(canada, by_pt) |>
      mutate(
        geo_type = case_when(geography == "Canada" ~ "country",
                             geography == NOT_STATED_GEO ~ "not stated",
                             TRUE ~ "province/territory"),
        geography = factor(geography,
                           levels = c("Canada", PROVINCES_TERRITORIES,
                                      NOT_STATED_GEO)),
        category  = factor(category, levels = CATEGORY_LEVELS),
        year      = as.integer(lubridate::year(month)),
        any_masked = masked_cells > 0
      ) |>
      select(month, year, geography, geo_type, program, category,
             value, cells, masked_cells, any_masked) |>
      arrange(month, geography, program, category)

    # 6i. Reference (optional): failure here never stops the dashboard.
    reference <- if (is.null(reference_sources)) {
      list(table = NULL, note = "Reference not requested.")
    } else {
      ref_raw <- raw[paste0("ref_", names(reference_sources))]
      names(ref_raw) <- names(reference_sources)
      build_reference(ref_raw)
    }
    # Keep the reference to years covered by the core data (Canada only).
    if (!is.null(reference$table)) {
      reference$table <- filter(reference$table,
                                year %in% unique(data$year), geography == "Canada")
    }

    # 6j. Metadata describing what was read.
    now_pt <- lubridate::with_tz(retrieved_at %||% Sys.time(), "America/Vancouver")
    meta <- list(
      source         = if (is_url) "live" else "local file",
      source_label   = source_label %||%
        if (is_url) "live IRCC download" else "local file",
      retrieved_at   = now_pt,
      first_month    = min(data$month),
      latest_month   = max(data$month),
      urls           = IRCC_URLS,
      reference_urls = IRCC_REFERENCE_URLS,
      sources_read   = sources,
      metadata_url   = IRCC_METADATA_URL,
      files = lapply(parsed, function(x) x[c("encoding", "delimiter",
                     "header_row", "raw_rows", "raw_columns", "footnotes")]),
      masked_cells   = tapply(detail$masked, detail$program, sum),
      builtin_totals_removed = n_builtin_totals,
      unmapped_categories = unique(detail$category[
        detail$category %in% c("Other TFWP", "Unknown or other IMP") &
          detail$level_2 != "Other IMP Participants"]),
      warning        = NULL,         # set when the cache is used
      cache_note     = NULL
    )
    list(data = data, detail = detail, reference = reference, meta = meta)
  }, error = function(e) e)

  # 6k. Success: save the cache (live reads only, by default) and return.
  if (!inherits(result, "error")) {
    if (write_cache) {
      dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
      saveRDS(result, cache_path)
    }
    return(result)
  }

  # 6l. Failure (download problem OR changed layout): fall back to the cache
  #     and record a clear warning that names the problem.
  msg <- conditionMessage(result)
  layout_problem <- grepl("Layout changed|Unexpected value|Unrecognised", msg)
  problem <- if (layout_problem) {
    paste("IRCC's file layout has changed, so the new file was not used.", msg)
  } else {
    paste("The live IRCC download failed.", msg)
  }
  if (!file.exists(cache_path)) {
    stop(problem, " No saved copy exists at ", cache_path, ".", call. = FALSE)
  }
  cached <- readRDS(cache_path)
  cached$meta$source_when_cached <- cached$meta$source
  cached$meta$source  <- "cache"
  cached$meta$warning <- problem
  cached$meta$problem_type <- if (layout_problem) "layout" else "download"
  cached$meta$cache_note <- sprintf(
    "Showing the saved copy of the data retrieved %s.",
    format(cached$meta$retrieved_at, "%B %-d, %Y, %-I:%M %p PT"))
  warning(problem, call. = FALSE)       # also visible in the R console/logs
  cached
}

# `%||%` (use the right-hand value when the left is NULL); base R >= 4.4 has
# it, defined here for older R versions.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
