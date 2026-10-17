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
- **De-duplication (superseded; see section 14):** the CSVs have no footnotes and **no annual or total columns**, so there is no IRCC annual figure to compare against the sum of the months. IRCC's companion tables (the XLSX versions) carry notes saying a person can hold more than one permit in a period, so unique-person totals can differ from sums. I've only seen that note quoted by a third party, not in these files. So a summed annual figure is best described as "work permits that took effect", which can be slightly more than the number of distinct people.
- **Plausibility check:** every month from Dec 2023 to Jul 2026 in these files is at least as large as IRCC's published "new worker arrivals" for that month (canada.ca *Understanding student and temporary worker numbers in Canada*, data as of Jul 31, 2026). That's what you'd expect, because arrivals are a narrower flow that leaves out extensions and agricultural workers: 32 of 32 months for TFWP and 32 of 32 for IMP.

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

- Watch out: **"Unknown IMP" is close to 0 before 2021**, then 28,610 in 2021, 105,655 in 2022, 198,785 in 2023 and 96,970 in Jan–Jul 2026. Part of the composition shift is about recording, not real change, and the dashboard must say so.
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

- IRCC publishes official totals for this table only in its XLSX versions (`EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx` and `EN_ODP-TR-Work-IMP_PT_program_sign.xlsx`), and those are also on the IRCC server this machine can't reach. **Update:** this comparison was done later with copies Mohsen downloaded (see section 14). The monthly totals agree within the masking bound. The arrivals-page check in section 3 is consistent with these numbers.

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

## 10b. Check against IRCC's own Excel tables (added Oct 16, 2026, 5:10 PM PT)
See section 14. In short, the monthly totals agree with IRCC's Excel totals once masking is allowed for, but **IRCC's annual totals are de-duplicated counts of persons, 1–4% below the sum of the months**.

## 11. What load_data() returns
- `data`: the tidy table, with columns month, year, geography, geo_type, program, category, value, cells, masked_cells and any_masked.
- `detail`: the cleaned cell-level rows, with all IRCC levels.
- `meta`: source (`live`, `local file` or `cache`), source_label, retrieved_at (PT), first_month and latest_month (both taken from the data), the URLs, per-file encoding, delimiter, header row, row count, columns and footnotes, the masked counts, the number of built-in totals removed, any unmapped categories, and cache_note.
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
9. Program categories are grouped as in the tables in section 7. "Unknown IMP" is shown as its own category, with a note that its rise after 2021 reflects recording practice.
10. Data are as published on the retrieval date. IRCC may revise past months in later releases.
11. The month is the month the permit took effect, not the month the application was made or the person arrived.
12. **Reference only:** IRCC's Excel tables (`EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx` and `EN_ODP-TR-Work-IMP_PT_program_sign.xlsx`) are read live and kept in the cache, and they are used **only** for Canada's annual counts of distinct people (TFWP and IMP). These appear as labelled reference markers on the annual trend chart (Canada, Annual view) and in a comparison table in About. No other figure in the dashboard comes from the Excel tables. If they can't be read, the dashboard still works without the reference and shows a short note saying so.

## 13. Files
- `R/data.R`: `load_data()` and helpers.
- `data/cache.rds`: offline fallback, including the Excel reference (about 774,500 bytes).
- `tools/build_cache_and_profile.R`: rebuilds the cache and the checks.
- `raw_profile/`: the downloaded CSVs (not for submission), `ckan.json` (catalogue metadata), `profile_output.txt`.

## 14. Check against IRCC's published Excel tables
Files: `EN_ODP-TR-Work-TFWP_PT_program_sign.xlsx` and `EN_ODP-TR-Work-IMP_PT_program_sign.xlsx`. Mohsen downloaded both on his PC on Oct 16, 2026 and they were copied to `raw_profile/`. I used them only to verify the CSV figures; they are not dashboard data. Script: `raw_profile/xlsx_check.py`. Both tables are stamped "Source: IRCC, July 31, 2026" and cover January 2015 to July 2026, the same as the CSVs.

**Structure:** one sheet per program. Rows are province/territory × program with a "<P/T> Total" row for each and a national "Total" row. Columns are months, quarter totals and year totals. Explicit zeros are shown as 0, and 1–5 as "--".

**IRCC's footnotes (verbatim):**
- "all values between 0 and 5 are shown as "--" … All other values are rounded to the closest multiple of 5 …; as a result of rounding, data may not sum to the totals indicated."
- "Data are preliminary estimates and are subject to change."
- "The total unique count may not equal to the sum of permit holders in each program as an individual may hold more than one type of permit over a given period."
- IMP only: "This table includes information on a subset of IMP work permit holders. It does not include IMP work permit holders who may be in Canada for reasons other than primarily working (e.g. for asylum claimants or Humanitarian and Compassionate reasons)."
- "… count TRs … based on the type of permit held by a TR (effective from the date that the permit was signed, or a valid permit at the end of a given year)."

**Measure definition: confirmed as a flow.** Counts are of people whose permit took effect (was signed) in the period.

**Monthly totals: consistent.**
- National, all 278 program-months: the Excel total is 0 to 420 above our sum of the CSV cells. The average gap is +268 a month for IMP (about 0.5%) and +21 for TFWP (about 0.2%).
- Every gap is within 5 × the number of masked cells plus rounding (278 of 278). That is what counting "--" as 0 is expected to do: IRCC totals the unrounded figures, while we add up cells that were already rounded or masked.
- Provinces and territories: 3,613 of 3,619 program-months fall within the same bound. The other 6 are off by 5–10, which is rounding.

| Month (Canada) | Excel TFWP | CSV TFWP | Excel IMP | CSV IMP |
|---|---|---|---|---|
| June 2016 | 11,295 | 11,260 | 26,235 | 26,005 |
| April 2020 | 13,065 | 13,045 | 16,710 | 16,490 |
| July 2026 | 13,260 | 13,255 | 59,645 | 59,240 |

British Columbia, same months (TFWP / IMP), Excel vs CSV:
- June 2016: 2,115 vs 2,120 / 4,650 vs 4,630
- April 2020: 1,465 vs 1,465 / 3,360 vs 3,325
- July 2026: 3,420 vs 3,420 / 9,925 vs 9,925

**Annual totals: not a sum of months.** IRCC's "Year Total" column is a unique count of persons for the year. It is always below the sum of IRCC's own monthly totals, because someone with permits in two different months is counted once.

| Year | TFWP sum of Excel months | TFWP Excel year total | Gap | IMP sum of Excel months | IMP Excel year total | Gap |
|---|---|---|---|---|---|---|
| 2015 | 75,705 | 72,955 | −3.6% | 191,730 | 186,195 | −2.9% |
| 2019 | 100,260 | 98,025 | −2.2% | 333,450 | 323,475 | −3.0% |
| 2023 | 187,980 | 183,180 | −2.6% | 785,920 | 761,200 | −3.1% |
| 2024 | 194,830 | 190,790 | −2.1% | 733,510 | 713,945 | −2.7% |
| 2025 | 169,530 | 166,470 | −1.8% | 589,715 | 578,335 | −1.9% |
| 2026 (Jan–Jul) | 112,920 | 111,900 | −0.9% | 562,830 | 557,735 | −0.9% |

- Compared with our CSV sums, IRCC's annual national totals are 1.2–2.7% lower in 2019–2025.
- Quarter totals are also unique counts.
- For small provinces, territories and categories the gap can be larger; up to −17% was seen in one TFWP province-year.
- **The two CSVs cannot reproduce IRCC's annual person counts.** They only have monthly cells.

**Effect on the assumptions (resolved):** Mohsen chose option C. Annual figures in the dashboard stay as the sum of the months, labelled "work permits that took effect during the year". IRCC's annual distinct-people totals for Canada are shown alongside them as a clearly labelled reference, with the footnote quoted and the 1–3% gap explained (assumptions 1 and 12).
