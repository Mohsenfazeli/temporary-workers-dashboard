# Data profile: Question 1, Temporary Workers dashboard

Prepared Fri Oct 16, 2026 (PT). Built by `tools/build_cache_and_profile.R`; full output in `raw_profile/profile_output.txt`.

## 1. Sources
| Program | URL | Downloaded | Size | MD5 |
|---|---|---|---|---|
| TFWP | https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-TFWP-PT_program.csv | Oct 16, 2026, 4:49–4:50 PM PT, by hand on Mohsen's Windows PC | 1,711,315 bytes | 75c4e517e70bf93368750cd24d6d27a4 |
| IMP | https://www.cic.gc.ca/opendata-donneesouvertes/data/ODP-TR-Work-IMP-PT_program.csv | same | 14,887,463 bytes | 0ce1f6f26702a107ecd1d1c2701eaefb |

- Catalogue record: Open Government Portal, *Temporary Residents: Temporary Foreign Worker Program (TFWP) and International Mobility Program (IMP) Work Permit Holders – Monthly IRCC Updates*, IRCC, record 360024f2-17e9-4558-bfc1-3616485d65b9 (https://open.canada.ca/data/en/dataset/360024f2-17e9-4558-bfc1-3616485d65b9). Updated monthly; record modified Sep 15, 2026; temporal coverage Jan 1, 2015 to Jul 31, 2026. Licence: Open Government Licence – Canada.
- The files were downloaded by hand because the server where I'm building this can't reach the IRCC website: every attempt was refused during the secure-connection (TLS) setup, including retries with a browser user-agent and longer timeouts. The app itself still reads the live URLs first.

## 2. Encoding and structure (both files)
- Encoding is UTF-8 with no byte-order mark. The files are **tab-delimited**, despite the .csv name, with Windows (CRLF) line endings.
- There is one header row (row 1). There are **no title rows and no footnote rows**. The rows are in no particular order.
- There are 17 columns, bilingual in pairs: `EN_YEAR, EN_QUARTER, EN_MONTH, FR_ANNEÉ, FR_TRIMESTRE, FR_MOIS, EN_PROVINCE_TERRITORY, FR_PROVINCE_TERRITOIRE, EN_PROGRAM_LEVEL_2, FR_NIVEAU_DE_PROGRAMME_2, EN_PROGRAM_LEVEL_3, FR_…_3, EN_PROGRAM_LEVEL_4, FR_…_4, EN_PROGRAM_LEVEL_5, FR_…_5, TOTAL`.
- The layout is long: one row per year × month × province/territory × program path (levels 2–5), and `TOTAL` holds the count. Months are English abbreviations (Jan to Dec).
- `load_data()` still checks the encoding (UTF-8, else Latin-1), the delimiter (tab or comma), the header row (the first line containing `EN_YEAR`) and any footnotes. A test variant with Latin-1 encoding, commas, a title row and a footnote parsed to identical results.

## 3. What is counted (measure definition)
- **Plain words:** each number is how many people had a TFWP (or IMP) work permit **take effect** (be signed or issued) in that month, by the province or territory they intended to work in. It counts new permits starting, including renewals and extensions. It is **not** the number of people holding a valid permit at a point in time.
- **Type: flow.** So a period total is the sum of its months. For 2026 that is a year-to-date total for January to July.
- **Evidence:**
  - IRCC's titles for both files: "work permit holders by province/territory of intended destination, program and **year in which permit(s) became effective**".
  - The catalogue publishes separate "**on December 31st**" files for the stock (people holding a valid permit). These two files are not those.
  - The catalogue description: "Datasets include … work permit holders by year in which permit(s) became effective or with a valid permit on December 31st."
- **De-duplication:** the CSVs have no annual or total columns, so annual figures are sums of the months. A person whose permits took effect in two different months is counted twice, so a summed annual figure is described as "work permits that took effect", not people.

## 4. Coverage
- Both files run from **January 2015 to July 2026**, the latest month. That's 139 months, with no gaps: 12 months for each year from 2015 to 2025 and 7 months for 2026. This matches the catalogue's coverage dates.

## 5. Row counts
| | Raw rows | Masked "--" cells | Share masked | Largest cell |
|---|---|---|---|---|
| TFWP | 6,462 | 1,674 | 25.9% | 5,865 |
| IMP | 49,646 | 20,716 | 41.7% | 29,990 |

- There are no duplicate keys. Every non-masked value is a multiple of 5, and there are no zeros: IRCC leaves out empty cells.
- The tidy table has **25,020 rows**, one per month × geography × program × category: 139 months × 15 geographies (Canada, 13 provinces/territories, not stated) × 12 categories. Combinations missing from the file are filled with 0.

## 6. Geographies
- All 13 provinces and territories plus **"Province/territory not stated"** appear in both files.
- **There is no Canada row.** Canada = the 13 provinces/territories + not stated, and this matches exactly in every month and category.
- Not stated totals 2015–Jul 2026: TFWP 14,225 (1.0%) and IMP 392,510 (7.9%). For IMP that's 68,265 in 2024 alone.

## 7. Program categories and proposed mapping
**TFWP**: 4 values at level 2 and 7 at level 3. Levels 4 and 5 are always "Program not stated".

| Dashboard category | IRCC codes (level 2 / level 3) | 2015–Jul 2026 total |
|---|---|---|
| Seasonal Agricultural Worker Program | Agricultural Workers / Seasonal Agricultural Worker Program | 414,935 |
| Other agricultural workers | Agricultural Workers / Agricultural Stream; Other Agricultural Workers | 282,820 |
| Caregivers | Caregivers / (not stated); Live-In Caregivers / Live-In Caregiver Occupations; Live-In Caregiver Program; FDM validation exemption | 59,905 |
| Other TFWP workers with an LMIA | Other Temporary Foreign Workers with LMIA / (not stated) | 660,465 |

The file does not split the TFWP into high-wage and low-wage streams.

**IMP**: 4 values at level 2, 15 at level 3, 41 at level 4 and 107 at level 5.

| Dashboard category | IRCC codes | 2015–Jul 2026 total |
|---|---|---|
| Post-graduation work permits | Level 4 = Post-grad employment | 1,555,245 |
| Spouses and family members | Level 4 = Spouses of skilled workers; Spouses of students; Dependent of skilled worker; Family members of Economic PR applicants | 973,565 |
| International Experience Canada | Level 4 = International Experience Canada (IEC) | 610,840 |
| Significant benefit and PR-bridging permits | Level 3 = Significant benefit (intra-company transfers, bridging open work permits, PR applicants in Canada, general, entrepreneurs, emergency repairs, CSQ holders, rural and northern pilot) | 579,750 |
| Trade and other agreements | Level 2 = Agreements (NAFTA/CUSMA, CPTPP, other FTAs, Canada–provincial/territorial, Atlantic pilot, non-trade) | 306,020 |
| Other Canadian interests | the rest of Canadian Interests (post-docs, reciprocal employment – other, exchange professors, charitable/religious, research) | 214,065 |
| Vulnerable workers | Level 2 = Vulnerable workers | 24,815 |
| Unknown or other IMP | Level 2 = Other IMP Participants (almost entirely "Unknown International Mobility Program") | 734,880 |

- Watch out: **"Unknown IMP" is close to 0 before 2021**, then 28,610 in 2021, 105,655 in 2022, 198,785 in 2023 and 96,970 in Jan–Jul 2026. IRCC does not identify the program for these permits, so shares of the other IMP categories are lower in those years; the dashboard says so and can show shares of permits with a known program only.
- The raw levels 2–5 are kept in the `detail` table, so a finer breakdown is possible.
- Any new IRCC code that matches no rule goes to "Other TFWP" or "Unknown or other IMP" and is listed in the metadata.

## 8. Masking and rounding
- From IRCC: values from 0 to 5 are shown as "--", all other values are rounded to the nearest 5, and the sums may not equal the totals.
- **Proposed treatment:** a "--" cell counts as **0** in all sums. The tidy table keeps `masked_cells` and `any_masked` so charts and tables can flag results that include masked cells, and the About tab will disclose this.
- **Size of the effect:** total counts may be understated by up to 5 per masked cell. Over the whole period that's at most about 8,370 for TFWP (0.6%) and about 103,580 for IMP (2.1%). The effect matters most for territories and small categories. For a single month at the national level it's at most 6 × 5 = 30 for TFWP and 219 × 5 = 1,095 for IMP (July 2026).
- Rounding: our totals are sums of cells that were already rounded, so they can differ from any total IRCC publishes elsewhere.

## 9. Built-in totals and double counting
- The files have **no Canada row, no "Total" rows, no program subtotals, and no annual or quarterly columns**. Each row is the most detailed cell, so adding up rows never counts anything twice.
- `load_data()` would remove any row labelled Canada or Total if IRCC added one, and it reports the count, which is currently 0.
- Our own Canada rows are tagged `geo_type = "country"` and must never be added to the province rows.
- "Province/territory not stated" is kept as its own geography. It's included in Canada but not ranked with the provinces.
- TFWP and IMP are separate programs, so "both" = TFWP + IMP. The tidy data checked out: Canada equals the sum of its parts with a difference of 0, and the tidy totals equal the raw totals exactly (TFWP 1,418,125 and IMP 4,999,180).

## 10. Spot-checks of national totals
The files publish no totals of their own, so I recomputed three months independently straight from the raw text and compared them with the `load_data()` output:

| Month | TFWP | IMP | Both | Masked cells (TFWP / IMP) | Independent recompute |
|---|---|---|---|---|---|
| June 2016 | 11,260 | 26,005 | 37,265 | 18 / 106 | matches |
| April 2020 | 13,045 | 16,490 | 29,535 | 9 / 101 | matches |
| July 2026 | 13,255 | 59,240 | 72,495 | 6 / 219 | matches |


Annual national totals (sums of months):

| Year | TFWP | IMP |
|---|---|---|
| 2015 | 75,465 | 189,515 |
| 2016 | 81,590 | 231,025 |
| 2017 | 79,920 | 247,025 |
| 2018 | 85,660 | 272,520 |
| 2019 | 99,925 | 330,220 |
| 2020 | 86,680 | 252,060 |
| 2021 | 106,090 | 339,840 |
| 2022 | 138,195 | 479,380 |
| 2023 | 187,800 | 781,965 |
| 2024 | 194,680 | 729,555 |
| 2025 | 169,330 | 585,585 |
| 2026 YTD (Jan–Jul) | 112,790 | 560,490 |

## 11. What load_data() returns
- `data`: the tidy table, with columns month, year, geography, geo_type, program, category, value, cells, masked_cells and any_masked.
- `detail`: the cleaned cell-level rows, with all IRCC levels.
- `meta`: source (`release` by default, or `live`, `local file` or `cache`), source_label, retrieved_at (PT), first_month and latest_month (both taken from the data), the URLs, per-file encoding, delimiter, header row, row count, columns and footnotes, the masked counts, the number of built-in totals removed, any unmapped categories, and cache_note.
- How it behaves:
  - It tries the live URLs first, with a browser user-agent, a 60-second timeout and 3 tries.
  - A local-path override is available for testing and goes through the same parsing code.
  - `data/cache.rds` is written after every successful read.
  - A failed download falls back to the cache and adds a notice. This was tested by simulating a failed download.
  - A **changed column layout** (approved behaviour) shows a clear warning naming the problem and falls back to the cache. The app stops only if there is no cache. This was tested.
- The current cache was built from the files above, with source_label "downloaded 2026-10-16 4:50 PM PT" and retrieved_at 2026-10-16 16:50 PT.

## 12. Assumptions
1. **[RESOLVED: option C, approved by Mohsen on Oct 16, 2026]** The measure is a **flow**: permits that took effect in the month. Annual = the sum of the monthly counts from the two CSVs. It is labelled "work permits that took effect during the year" (not people), and 2026 = year to date (Jan–Jul), never compared with full years.
2. Because people can get more than one permit in a year, summed figures are described as "work permits that took effect" rather than strictly "distinct people".
3. A masked "--" cell (0–5) counts as 0 in sums and is flagged in the data, and this is disclosed. Totals may be understated by at most 5 per masked cell.
4. A combination missing from the file means 0. IRCC publishes no zero cells.
5. Canada = the 13 provinces/territories + "Province/territory not stated". Not stated is shown separately but never ranked as a province.
6. TFWP and IMP don't overlap, so "both" = TFWP + IMP.
7. Province/territory means the **intended** destination recorded on the permit, not where the person actually lives.
8. Values are taken as published. Sums of rounded cells may differ slightly from totals IRCC publishes elsewhere.
9. Program categories are grouped as in the tables in section 7. "Unknown IMP" is shown as its own category, because IRCC does not identify the program for these permits.
10. By default the dashboard shows the IRCC release of September 15, 2026 (IRCC files' Last-Modified date and the Open Government catalogue's date modified, checked October 18, 2026). IRCC may revise past months in later releases.
11. The month is the month the permit took effect, not the month the application was made or the person arrived.
12. **Only the two IRCC CSV files are used** (decision by Mohsen, October 18, 2026). No other table, including IRCC's Excel versions of these tables, and no non-IRCC data (for example population) are read, stored or shown. Event lines and notes cite official Government of Canada news releases and notices as dated context only, with no figures from them.

## 13. Files
- `R/data.R`: `load_data()` and helpers.
- `data/cache.rds`: offline fallback (the two CSVs, processed).
- `tools/build_cache_and_profile.R`: downloads the two IRCC files into `raw_profile/` itself, rebuilds the data and reruns these checks (`--local` reuses the files already there).
- `raw_profile/`: the downloaded CSVs (not for submission), `ckan.json` (catalogue metadata), `profile_output.txt`.

## 14. Data sources used (updated October 18, 2026)
Only the two IRCC CSV files listed in section 1 are used. An earlier version of this profile also compared the CSVs with IRCC's Excel versions of the same tables and with IRCC's new-arrivals web page, and the dashboard had an optional, off-by-default reference of IRCC's annual count of distinct people taken from the Excel tables. Following the decision to use only these two IRCC data files, that comparison, the reference layer and its stored copy were removed from the dashboard, the tests, the cached data and this profile.

The caveats repeated in the dashboard text are IRCC's own notes for these tables: values of 0 to 5 are shown as "--" and other values are rounded to the nearest 5 (catalogue description); the data are preliminary estimates subject to change; and the IMP table covers a subset of IMP work permit holders, excluding people in Canada mainly for reasons other than work (for example asylum claimants).
