# =============================================================================
# exports/R/note_q2_text.R: the Question 2 note's text for any IRCC release.
# Mohsen's text (q2 word/mohsen_text.md, October 19, 2026) is the template:
#   (a) numbers are filled in from the facts (exports/R/note_q2_facts.R), rounded
#       as in the note (validate_q2.R rules: pct0, k, h, M2);
#   (b) words that state a direction or a comparison ("rose", "more than tripled",
#       "falling faster in B.C.") are chosen by tested rules, with a plain
#       fallback when a rule no longer holds;
#   (c) context, causes and implications written for the September 15, 2026
#       release are kept only while the facts they rely on still hold, and a
#       dated line at the end of the Data notes says when they were written.
# On the September 15, 2026 release the output is Mohsen's text plus that line.
# =============================================================================

Q2_CONTEXT_RELEASE <- as.Date("2026-09-15")     # the release the context was written for
Q2_NOTE_WRITTEN    <- "October 19, 2026"        # date of Mohsen's text
Q2_FIGS_ORIGINAL   <- "dashboard shots of October 19, 2026"

q2_r0 <- function(x) floor(x + 0.5)              # round half up (as validate_q2.R)
q2_round <- function(v, rule) {
  if (rule == "k" && abs(v) < 10000) rule <- "h"  # small counts: nearest hundred
  switch(rule,
         pct0 = sprintf("%d%%", q2_r0(abs(100 * v))),
         k = formatC(q2_r0(abs(v) / 1000) * 1000, format = "d", big.mark = ","),
         h = formatC(q2_r0(abs(v) / 100) * 100, format = "d", big.mark = ","),
         M2 = sprintf("%.2f million", v / 1e6),
         stop("Unknown rounding rule: ", rule))
}
q2_multiple_word <- function(ratio) {
  if (ratio > 4) "more than quadrupled" else if (ratio > 3) "more than tripled" else
    if (ratio > 2) "more than doubled" else if (ratio > 1) "rose" else "fell"
}

#' Build the note's text, numbers table and rule log.
#' @param fx output of q2_facts(); @param bundle the release; @param build_date Date
q2_note_text <- function(fx, bundle, build_date = Sys.Date()) {
  v <- fx$val; P <- fx$P; Y <- P$Y; PY <- P$PY; FY0 <- P$FY0; LF <- P$LF; YMAX <- P$YMAX; yy <- P$yy
  N <- function(x) fmt_n(x)
  YS <- function(y) q2_ytd_short(P, y); YL <- function(y = NULL) q2_ytd_long(P, y)
  release_date <- as.Date(bundle$meta$release_date %||% NA)
  context_release <- isTRUE(release_date == Q2_CONTEXT_RELEASE)
  id <- list(ytd_prev = paste0("ca_ytd", yy(PY)), ytd_cur = paste0("ca_ytd", yy(Y)),
             growth = sprintf("ca_growth_%d_%d", FY0, YMAX), bc_share_prev = paste0("bc_share_ytd", yy(PY)),
             unk_share_ymax = paste0("unk_share_", YMAX), unk_share_cur = paste0("unk_share_ytd", yy(Y)),
             bc_tfwp_share = paste0("bc_tfwp_share_", LF), ca_tfwp_share = paste0("ca_tfwp_share_", LF))
  ytdv <- function(pre, which) v(paste0(pre, "_ytd", yy(if (which == "prev") PY else Y)))

  # ---- Rules ------------------------------------------------------------------
  rules <- list()
  rule <- function(name, value, meaning) {
    rules[[name]] <<- data.frame(rule = name, holds = isTRUE(value), meaning = meaning, stringsAsFactors = FALSE)
    isTRUE(value)
  }
  near0 <- function(x) abs(x) < 0.01
  ca_chg <- v("ca_ytd_change")
  rebound <- rule("rebound", ca_chg > 0, sprintf("Permits in %s are above the same months of %d", YS(Y), PY))
  statcan <- rule("statcan_context", Y == 2026,
                  "The latest year is 2026, so Statistics Canada's estimate to July 1, 2026 [12] still applies")
  pgwp_up <- rule("pgwp_rose", v("pgwp_ytd_increase") > 0, "PGWPs rose over the same months")
  pgwp_drive <- rule("pgwp_drive", rebound && v("pgwp_share_of_increase") > 0.5,
                     "PGWPs supplied more than half of a rise in permits")
  bc_tfwp <- v("bc_tfwp_ytd_change"); ca_tfwp <- v("tfwp_ytd_change")
  tfwp_faster <- rule("bc_tfwp_falling_faster", bc_tfwp < 0 && ca_tfwp < 0 && bc_tfwp < ca_tfwp,
                      "TFWP permits fall in B.C. and Canada, faster in B.C.")
  bc_contrib <- v("bc_contrib_ytd"); bc_share_prev <- v(id$bc_share_prev)
  bc_small <- rule("bc_share_small", rebound && bc_contrib < bc_share_prev,
                   "B.C.'s share of the rise is below its share of permits a year earlier")
  ratio <- v(paste0("ca_", YMAX)) / v(paste0("ca_", FY0))
  fell_after_peak <- rule("fell_after_peak_year", v(paste0("ca_", LF)) < v(paste0("ca_", YMAX)),
                          sprintf("Permits in %d are below the peak year %d", LF, YMAX))
  ann <- fx$ann
  dip2020 <- rule("dip_only_2020",
                  all(c(2019, 2020) %in% names(ann)) && YMAX > 2020 && all(diff(ann[as.character(FY0:2019)]) > 0) &&
                    ann[["2020"]] < ann[["2019"]] && all(diff(ann[as.character(2020:YMAX)]) > 0),
                  sprintf("Permits rose every year from %d to %d except 2020", FY0, YMAX))
  dip_any2020 <- all(c(2019, 2020) %in% names(ann)) && ann[["2020"]] < ann[["2019"]]
  pk_end <- fx$peak$end
  peak_then_fell <- rule("peak_before_last_full_year", pk_end < as.Date(sprintf("%d-12-01", LF)) &&
                           v(paste0("ca_", LF)) < v("ca_r12_peak"),
                         sprintf("The highest 12-month total ended before December %d, and %d was lower", LF, LF))
  policy_decline <- rule("decline_in_policy_period", fell_after_peak && peak_then_fell &&
                           pk_end >= as.Date("2023-01-01") && pk_end <= as.Date("2025-12-01"),
                         "The fall after the peak happened in 2023-2025, the period of the cited policy changes [5-8]")
  new_high12 <- rule("new_12_month_high", v("ca_last12") >= v("ca_r12_peak"),
                     "The latest 12-month total is the highest on record")
  above_ph <- rule("above_previous_ytd_high", v(id$ytd_cur) > max(P$ytd_prior),
                   sprintf("%s is above every earlier year's same months", YS(Y)))
  unk_up <- rule("unknown_imp_rose", v("unk_ytd_increase") > 0, "Unknown IMP rose too (so the PGWP rise is not a recoding)")
  lmia_down <- rule("lmia_falling", v("lmia_ytd_change") < 0, "Non-agricultural LMIA permits fell over the same months")
  lmia_fell25 <- rule("lmia_fell_2025", v("lmia_2025") < v("lmia_2024"), "Non-agricultural LMIA permits fell from 2024 to 2025")
  pg_bc <- v("bc_pgwp_ytd_change"); pg_ca <- v("pgwp_ytd_change")
  bc_pgwp_slower <- rule("bc_pgwp_slower", pg_bc > 0 && pg_ca > 0 && pg_bc < pg_ca, "B.C.'s PGWPs grew more slowly than Canada's")
  bc_other_down <- rule("bc_other_down_canada_up", v("bc_other_ytd_change") < 0 && v("ca_other_ytd_change") > 0,
                        "B.C.'s permits other than PGWPs fell while Canada's rose")
  bc_all_grads <- rule("bc_growth_all_graduates", v("bc_ytd_increase") > 0 && v("bc_pgwp_ytd_increase") > v("bc_ytd_increase"),
                       "B.C.'s PGWP gain is larger than its total gain")
  on_flat <- rule("ontario_tfwp_near0", near0(v("on_tfwp_ytd_change")), "Ontario TFWP change is under 1% either way")
  agri_flat <- rule("bc_agri_near0", near0(v("bc_agri_ytd_change")), "B.C. agricultural TFWP change is under 1% either way")
  tfwp_heavier <- rule("bc_tfwp_share_higher", v(id$bc_tfwp_share) > v(id$ca_tfwp_share),
                       sprintf("TFWP is a larger share of B.C.'s permits than of Canada's (%d)", LF))
  ab_unk <- rule("alberta_unknown_imp", v("ab_ytd_increase") > 0 && v("ab_unk_ytd_increase") > 0 &&
                   v("ab_unk_share_of_increase") >= 0.25, "Unknown IMP is at least a quarter of Alberta's rise")
  bc_lmia <- v("bc_lmia_ytd_change"); bc_lmia_down <- bc_lmia < 0
  title_rule <- rule("title_storyline", pgwp_drive && bc_small,
                     "Graduates drive a rebound and B.C.'s share is small (the note's title)")
  geo13 <- fx$geo[fx$geo$geography %in% names(Q2_GEO_CODE), ]
  top <- geo13$geography[which.max(geo13$contrib_ytd)]
  top_k <- Q2_GEO_CODE[[top]]
  rule("top_contributor_ontario", top == "Ontario", sprintf("Province supplying most of the change: %s", top))
  tfwp_mix_note <- rule("bc_tfwp_share_highest_of_four", {
    mx <- fx$mix; tf <- tapply(mx$value[mx$program == "TFWP"], mx$geography[mx$program == "TFWP"], sum) /
      tapply(mx$value, mx$geography, sum)
    tf[["British Columbia"]] > max(tf[c("Canada", "Ontario", "Alberta")]) },
    sprintf("B.C. relies more on TFWP than Canada, Ontario or Alberta (%d; Table C2)", LF))
  pt_same <- rule("robustness_same_direction",
                  (v("bc_contrib_ytd_pt") < v(sprintf("bc_share_ytd%s_pt", yy(PY)))) == (bc_contrib < bc_share_prev),
                  "B.C.'s share finding is the same without 'province/territory not stated'")

  # ---- Numbers in the body (Appendix D rows) ---------------------------------------
  rows <- list()
  reg <- function(order, fid, rule_, statement, location, shown = NULL, phrase = NULL) {
    if (is.null(shown)) shown <- if (fid == "context_statcan_npr") "155,000" else q2_round(v(fid), rule_)
    rows[[length(rows) + 1]] <<- data.frame(order = order, phrase = phrase %||% shown, shown = shown, fact_id = fid,
                                            rule = rule_, statement = statement, location = location,
                                            stringsAsFactors = FALSE)
    shown
  }
  ud <- function(x, up = "rose", down = "fell") if (x >= 0) up else down
  ys <- YS(Y); ysp <- YS(PY)
  ytd_pair <- sprintf("%s vs %d", ys, PY)              # "Jan–Jul 2026 vs 2025"
  inc_word <- if (rebound) "increase" else "decrease"
  fy <- function(y) v(paste0("ca_", y))
  loc_c5 <- sprintf("(Geography comparison, TFWP only, B.C.): %s to %s", N(ytdv("bc_tfwp", "prev")), N(ytdv("bc_tfwp", "cur")))
  l12_loc <- sprintf("Calculated from the IRCC files: %s (%s\u2013%s) / %s (%s\u2013%s)", N(v("ca_last12")),
                     fmt_month_short(fx$peak$l12_start), fmt_month_short(P$latest), N(v("ca_r12_peak")),
                     fmt_month_short(fx$peak$start), fmt_month_short(pk_end))
  pk_long <- sprintf("%s to %s", fmt_month(fx$peak$start), fmt_month(pk_end))

  # Summary
  s_b1_lead <- if (rebound && statcan) "Permits are rebounding, but the temporary population is not." else
    if (rebound) "Permits are rebounding." else "Permits are falling."
  s_b1 <- paste0("- ", s_b1_lead, " Permits ", ud(ca_chg), " ",
                 reg(2, "ca_ytd_change", "pct0", sprintf("Summary: permits %s %s, %s vs %s", ud(ca_chg), q2_round(ca_chg, "pct0"), ys, ysp),
                     sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
                 " in ", YL(Y), " from a year earlier and, over 12 months, ",
                 if (new_high12) "are at a new high" else paste0(if (rebound) "are back to " else "are at ",
                   reg(3, "ca_last12_share_of_peak", "pct0", "Summary: latest 12-month total as a share of the 12-month peak", l12_loc),
                   " of their peak"),
                 if (statcan && rebound) ", yet Statistics Canada estimates temporary residents fell [12]." else
                   if (statcan) "; Statistics Canada estimates temporary residents fell too [12]." else ".")
  pgwp_share_loc <- sprintf("Overview What the data show panel: %s of %s (%s)", N(v("pgwp_ytd_increase")),
                            N(v("ca_ytd_increase")), fmt_share(v("pgwp_share_of_increase")))
  s_b2 <- if (rebound && pgwp_up) {
    paste0("- ", if (pgwp_drive) "Graduates drive the rebound." else "Graduates add to the rebound.",
           " Post-graduation work permits (PGWPs) supplied ",
           reg(4, "pgwp_share_of_increase", "pct0", sprintf("Summary: PGWPs' share of the %s increase", ys), pgwp_share_loc),
           " of the increase.")
  } else {
    paste0("- ", if (pgwp_up) "Graduate permits rose." else "Graduate permits fell.",
           " Post-graduation work permits (PGWPs) ", ud(pg_ca), " ",
           reg(4, "pgwp_ytd_change", "pct0", sprintf("Summary: PGWPs, Canada, %s", ytd_pair),
               sprintf("Figure C3: %s to %s", N(ytdv("pgwp", "prev")), N(ytdv("pgwp", "cur")))),
           " over the same months.")
  }
  tfwp_lead <- if (tfwp_faster) "Employer-sponsored permits are falling faster in B.C." else
    if (bc_tfwp < 0 && ca_tfwp < 0) "Employer-sponsored permits are falling, though more slowly in B.C." else
      if (bc_tfwp < 0) "Employer-sponsored permits are falling in B.C." else
        if (ca_tfwp < 0) "Employer-sponsored permits are rising in B.C. but falling nationally." else
          "Employer-sponsored permits are rising."
  s_b3 <- paste0("- ", tfwp_lead, " Temporary Foreign Worker Program (TFWP) permits ", ud(bc_tfwp), " ",
                 reg(5, "bc_tfwp_ytd_change", "pct0", sprintf("Summary: TFWP permits, B.C., %s", ytd_pair), paste("Figure C5", loc_c5)),
                 " in B.C. over the same months, against ",
                 if ((bc_tfwp < 0) == (ca_tfwp < 0)) "" else "a ",
                 reg(6, "tfwp_ytd_change", "pct0", sprintf("Summary: TFWP permits, Canada, %s", ytd_pair),
                     sprintf("Trends tab, change from a year earlier: TFWP %s %s (%s to %s)", if (ca_tfwp < 0) "down" else "up",
                             sprintf("%.1f%%", abs(100 * ca_tfwp)), N(ytdv("tfwp", "prev")), N(ytdv("tfwp", "cur")))),
                 if ((bc_tfwp < 0) == (ca_tfwp < 0)) "" else paste0(" ", ud(ca_tfwp, "rise", "fall")),
                 " nationally.")
  bc_contrib_loc <- sprintf("Figure 2: %s (%s of %s)", fmt_share(bc_contrib), N(v("bc_ytd_increase")), N(v("ca_ytd_increase")))
  bc_share_loc <- sprintf("Figure C8 (Share column): %s (%s of %s)", fmt_share(bc_share_prev), N(ytdv("bc", "prev")), N(ytdv("ca", "prev")))
  s_b4 <- if (rebound && v("bc_ytd_increase") > 0) {
    paste0("- ", if (bc_small) "B.C.’s share of the rebound is small." else "B.C. keeps pace with the rebound.",
           " It supplied ", reg(7, "bc_contrib_ytd", "pct0", sprintf("Summary: B.C. share of Canada's %s increase", ys), bc_contrib_loc),
           " of the increase, ", if (bc_small) "though it held " else "and held ",
           reg(8, id$bc_share_prev, "pct0", sprintf("Summary: B.C. share of Canada's permits, %s", ysp), bc_share_loc),
           " of permits a year earlier.")
  } else {
    bcc <- v("bc_ytd_change")
    paste0("- ", if (rebound) "B.C. misses the rebound." else if (bcc < 0) "B.C. follows the national decline." else
      "B.C. bucks the national decline.", " B.C. permits ", ud(bcc), " ",
      reg(7, "bc_ytd_change", "pct0", sprintf("Summary: permits, B.C., %s", ytd_pair),
          sprintf("Figure C8 / Geography tab: %s to %s", N(ytdv("bc", "prev")), N(ytdv("bc", "cur")))),
      " over the same months.")
  }
  for_bc <- if (bc_all_grads) "For B.C.: growth now rests on graduates, whose routes to permanent residence have narrowed [10]. How many stay is the key planning question." else NULL

  # Background
  bg2 <- sprintf("The data, from IRCC’s open data release of %s, count permits that took effect each month, by intended province, from %s to %s [1, 2]. %s Figures are rounded; exact values are in Appendix D.",
                 if (is.na(release_date)) fmt_date(Sys.Date()) else fmt_date(release_date), fmt_month(P$first), fmt_month(P$latest),
                 if (P$M == 12) sprintf("%d is compared with %d.", Y, PY) else sprintf("%s is compared with the same months of %d.", YL(Y), PY))

  # Findings 1
  gw <- q2_multiple_word(ratio)
  h1 <- paste0("Permits ", if (fell_after_peak && rebound) paste0(gw, ", fell, and are climbing again") else
    if (fell_after_peak) paste0(gw, ", fell, and are still falling") else
      if (rebound) paste0(gw, " and are still rising") else paste0(gw, " and are now falling"))
  reg(1, id$growth, "tripled", sprintf("Heading: permits %s, %d to %d", gw, FY0, YMAX),
      sprintf("Figure C1 (Overview chart; %s in %d and %s in %d)", N(fy(FY0)), FY0, N(fy(YMAX)), YMAX), shown = gw)
  c1 <- "Figure C1 (Overview chart)"
  f1a <- paste0("Permits ", if (ratio >= 1) "rose" else "fell", " from ", reg(9, paste0("ca_", FY0), "k", sprintf("Permits, Canada, %d", FY0), c1),
                " in ", FY0, " to ", reg(10, paste0("ca_", YMAX), "k", sprintf("Permits, Canada, %d", YMAX), c1), " in ", YMAX,
                if (dip2020) { reg(11, "ca_2020", "dip2020", sprintf("Permits fell only in 2020 (%s in 2019 to %s)", N(ann[["2019"]]), N(ann[["2020"]])), c1,
                                   shown = "", phrase = "dipping only in 2020")
                  ", dipping only in 2020, when COVID-19 travel restrictions took effect [4]." } else
                  if (dip_any2020) ", with a dip in 2020, when COVID-19 travel restrictions took effect [4]." else ".")
  peak_txt <- reg(12, "ca_r12_peak", "M2", sprintf("Highest 12-month total (%s), Canada", pk_long), "Calculated from the IRCC files")
  lf_txt <- reg(13, paste0("ca_", LF), "k", sprintf("Permits, Canada, %d", LF), c1)
  f1b <- if (peak_then_fell) sprintf("Over 12-month periods, they peaked at %s in the year to %s, then fell to %s in %d.", peak_txt, fmt_month(pk_end), lf_txt, LF) else
    sprintf("Over 12-month periods, they peaked at %s in the year to %s; %d totalled %s.", peak_txt, fmt_month(pk_end), LF, lf_txt)
  f1c <- if (policy_decline) "The decline followed a federal target to lower temporary residents’ share of the population [5] and tighter study permit, PGWP and TFWP rules [6–8], although the data show timing, not cause." else NULL
  f1 <- paste(c(f1a, f1b, f1c), collapse = " ")
  f2a <- paste0("In ", YL(Y), ", ", reg(14, id$ytd_cur, "k", sprintf("Permits, Canada, %s", ys), sprintf("Overview card 1 (Figure C2): %s", N(v(id$ytd_cur)))),
                " permits took effect, ",
                reg(15, "ca_ytd_change", "pct0", sprintf("Permits, Canada, %s vs %s", ys, ysp), sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
                if (rebound) " more" else " fewer", " than a year earlier",
                if (above_ph) { reg(16, paste0("ca_ytd", yy(P$PH)), "above2024",
                                    sprintf("%s above the previous high for these months (%s: %s)", ys, YS(P$PH), N(max(P$ytd_prior))), "Figure 1",
                                    shown = "", phrase = sprintf("above the %d high", P$PH))
                  sprintf(" and above the %d high for these months", P$PH) } else
                  if (rebound) sprintf(" but below the %d high for these months", P$PH) else "",
                " (Figure 1).")
  f2b <- if (statcan) { reg(17, "context_statcan_npr", "statcan", "Statistics Canada: non-permanent residents, July 1, 2025 to July 1, 2026",
                            "Statistics Canada, The Daily, September 23, 2026 [12]: \u2212154,614 (to 2,779,774); not IRCC data")
    paste0(if (rebound) "Yet permits are not people: " else "Permits are not people: ",
           "Statistics Canada estimates that non-permanent residents fell by 155,000 in the year to July 1, 2026, led by study permit holders [12].") } else NULL
  f2 <- paste(c(f2a, f2b), collapse = " ")

  # Findings 2
  h2 <- if (pgwp_up && lmia_down) "Graduates lift permits; employer-sponsored permits fall" else
    if (pgwp_up) "Graduates lift permits; employer-sponsored permits also rise" else
      if (lmia_down) "Graduate and employer-sponsored permits both fall" else "Graduate permits fall; employer-sponsored permits rise"
  c3 <- function(y) sprintf("Figure C3 (Composition tab; hover on the %d bar)", y)
  pg_prev <- reg(18, paste0("pgwp_ytd", yy(PY)), "k", sprintf("PGWPs, Canada, %s", ysp), c3(PY))
  pg_cur <- reg(19, paste0("pgwp_ytd", yy(Y)), "k", sprintf("PGWPs, Canada, %s", ys), c3(Y))
  inc_txt <- function(o) reg(o, "ca_ytd_increase", "k", sprintf("Total %s, Canada, %s to %d", inc_word, ysp, Y),
                             sprintf("Overview card 2 (%s \u2212 %s)", N(v(id$ytd_cur)), N(v(id$ytd_prev))))
  f3a <- if (pgwp_up && rebound) {
    sprintf("In %s, PGWPs rose from %s to %s, supplying %s of the %s additional permits (Figure C3).", YL(), pg_prev, pg_cur,
            reg(20, "pgwp_ytd_increase", "k", sprintf("PGWP increase, Canada, %s to %d", ysp, Y),
                sprintf("Calculated from Figure C3 values (%s \u2212 %s)", N(ytdv("pgwp", "cur")), N(ytdv("pgwp", "prev")))), inc_txt(21))
  } else if (pgwp_up) {
    sprintf("In %s, PGWPs rose from %s to %s, while total permits fell by %s (Figure C3).", YL(), pg_prev, pg_cur, inc_txt(21))
  } else sprintf("In %s, PGWPs fell from %s to %s (Figure C3).", YL(), pg_prev, pg_cur)
  f3b <- if (pgwp_up) "The cause is untested; larger student cohorts admitted before the 2024 study permit cap [6] may now be graduating." else NULL
  unk_loc <- sprintf("Figure C3 (Unknown or other IMP; %s to %s)", N(ytdv("unk", "prev")), N(ytdv("unk", "cur")))
  f3c <- if (pgwp_up && unk_up) paste0("The rise is real, not a recoding: permits with no identified program (“Unknown IMP”) also rose, by ",
                                      reg(22, "unk_ytd_increase", "k", sprintf("Unknown IMP increase, Canada, %s to %d", ysp, Y), unk_loc), ".") else
    if (pgwp_up) paste0("Part of the rise may be a recoding: permits with no identified program (“Unknown IMP”) fell by ",
                        reg(22, "unk_ytd_increase", "k", sprintf("Unknown IMP decrease, Canada, %s to %d", ysp, Y), unk_loc), ".") else NULL
  f3 <- paste(c(f3a, f3b, f3c), collapse = " ")
  comp_tfwp <- "Composition tab (TFWP chart; Other TFWP workers with an LMIA)"
  lmia_lead <- if (lmia_down == pgwp_up) "Employer-sponsored permits moved the other way." else "Employer-sponsored permits moved the same way."
  lmia_w <- q2_multiple_word(v("lmia_2024") / v("lmia_2021"))
  lm21 <- reg(23, "lmia_growth_2021_2024", "tripled", "Non-agricultural LMIA permits, Canada, 2021 to 2024", comp_tfwp, shown = lmia_w)
  lm25 <- reg(24, "lmia_fall_2024_2025", "pct0", "Non-agricultural LMIA permits, Canada, 2024 to 2025", comp_tfwp)
  lmy <- reg(25, "lmia_ytd_change", "pct0", sprintf("Non-agricultural LMIA permits, Canada, %s", ytd_pair), comp_tfwp)
  f4 <- paste0(lmia_lead, " Non-agricultural LMIA permits ", lm21, " from 2021 to 2024, after low-wage caps were raised in 2022 [9]. They then ",
               if (lmia_fell25) paste0("fell ", lm25, " in 2025") else paste0("rose ", lm25, " in 2025"),
               if (lmia_fell25 && lmia_down) paste0(" and another ", lmy, " in ", YL(Y)) else
                 paste0(if (lmia_down) " and fell " else ", and rose ", lmy, " in ", YL(Y)),
               if (lmia_fell25) ", after the 2024 low-wage limits [7]." else ".")

  # Findings 3
  h3 <- if (bc_small) "B.C.’s share of the rebound shrinks" else if (rebound) "B.C.’s share of the rebound" else "B.C. in the national decline"
  top_name <- if (top == "British Columbia") "B.C." else top
  top_loc <- sprintf("Figure 2: %s (%s of %s)", fmt_share(v(paste0(top_k, "_contrib_ytd"))), N(v(paste0(top_k, "_ytd_increase"))), N(v("ca_ytd_increase")))
  f5a <- if (rebound && v("bc_ytd_increase") > 0) {
    paste0("B.C. supplied ", reg(26, "bc_contrib_ytd", "pct0", sprintf("B.C. share of Canada's %s increase", ys), sprintf("Figure 2: %s", fmt_share(bc_contrib))),
           " of the national increase, ", if (bc_contrib < bc_share_prev) "below" else "above", " its ",
           reg(27, id$bc_share_prev, "pct0", sprintf("B.C. share of Canada's permits, %s", ysp), sprintf("Figure C8 (Share column): %s", fmt_share(bc_share_prev))),
           " share of permits a year earlier (Figure C8)",
           if (top == "British Columbia") "; no province supplied more (Figure 2)." else
             paste0("; ", top_name, " supplied ", reg(28, paste0(top_k, "_contrib_ytd"), "pct0", sprintf("%s share of Canada's %s increase", top_name, ys), top_loc), " (Figure 2)."))
  } else if (rebound) {
    paste0("B.C. permits fell by ", reg(26, "bc_ytd_increase", "k", sprintf("Change in B.C. permits, %s to %d", ysp, Y), "Figure 2"),
           " while Canada’s rose (Figure 2); ", top_name, " supplied ",
           reg(28, paste0(top_k, "_contrib_ytd"), "pct0", sprintf("%s share of Canada's %s increase", top_name, ys), top_loc), " of the national increase.")
  } else {
    bcc <- v("bc_ytd_change")
    paste0("B.C. permits ", ud(bcc), " ", reg(26, "bc_ytd_change", "pct0", sprintf("Permits, B.C., %s", ytd_pair), "Figure 2"), " in ", YL(Y),
           ", against a ", reg(27, "ca_ytd_change", "pct0", sprintf("Permits, Canada, %s", ytd_pair), sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
           " fall nationally (Figure 2).")
  }
  reasons <- c(
    if (bc_pgwp_slower) sprintf("B.C.’s PGWPs grew more slowly than Canada’s (%s against %s)",
                                reg(29, "bc_pgwp_ytd_change", "pct0", sprintf("PGWP growth, B.C., %s", ytd_pair),
                                    sprintf("Composition tab, B.C. (Figure C4): %s to %s", N(ytdv("bc_pgwp", "prev")), N(ytdv("bc_pgwp", "cur")))),
                                reg(30, "pgwp_ytd_change", "pct0", sprintf("PGWP growth, Canada, %s", ytd_pair),
                                    sprintf("Figure C3: %s to %s", N(ytdv("pgwp", "prev")), N(ytdv("pgwp", "cur"))))),
    if (bc_other_down) sprintf("its other permits fell by %s while Canada’s rose by %s",
                               reg(31, "bc_other_ytd_change", "h", sprintf("Change in B.C. permits other than PGWPs, %s to %d", ysp, Y),
                                   sprintf("Calculated: B.C. increase %s \u2212 PGWP increase %s", N(v("bc_ytd_increase")), N(v("bc_pgwp_ytd_increase")))),
                               reg(32, "ca_other_ytd_change", "k", sprintf("Change in Canada's permits other than PGWPs, %s to %d", ysp, Y),
                                   sprintf("Calculated: %s \u2212 %s", N(v("ca_ytd_increase")), N(v("pgwp_ytd_increase"))))))
  f5b <- if (!bc_small || !length(reasons)) NULL else if (length(reasons) == 2)
    paste0("Two things explain the gap. ", reasons[1], ", and ", reasons[2], ".") else
      paste0("One thing explains part of the gap: ", reasons[1], ".")
  if (!is.null(f5b) && length(reasons) == 1 && !bc_pgwp_slower) f5b <- sub(": its other", ": B.C.’s other", f5b, fixed = TRUE)
  f5c <- if (bc_all_grads) { reg(33, "bc_pgwp_ytd_increase", "pgwp_gt_total",
                                 sprintf("B.C. PGWP gain (%s) exceeds B.C.'s total gain (%s)", N(v("bc_pgwp_ytd_increase")), N(v("bc_ytd_increase"))),
                                 "Calculated from the IRCC files", shown = "", phrase = "All of B.C.’s growth")
    paste0("All of B.C.’s growth ", if (bc_other_down) "therefore " else "", "came from graduates.") } else NULL
  f5 <- paste(c(f5a, f5b, f5c), collapse = " ")
  fig2_cap <- sprintf("Figure 2. Share of Canada’s %s %s, by province and territory.", YL(Y), inc_word)

  f6_lead <- if (tfwp_faster) "Employer-sponsored permits are also falling faster in B.C." else tfwp_lead
  ab_t <- v("ab_tfwp_ytd_change")
  bc_part <- if (near0(bc_tfwp)) "were almost unchanged in B.C." else paste0(ud(bc_tfwp), " ",
    reg(34, "bc_tfwp_ytd_change", "pct0", sprintf("TFWP permits, B.C., %s", ytd_pair), "Figure C5 (B.C. bar for 2026)"), " in B.C.")
  ab_num <- function() reg(35, "ab_tfwp_ytd_change", "pct0", sprintf("TFWP permits, Alberta, %s", ytd_pair),
                           sprintf("Geography comparison chart, Alberta, TFWP only: %s to %s", N(ytdv("ab_tfwp", "prev")), N(ytdv("ab_tfwp", "cur"))))
  on_t <- v("on_tfwp_ytd_change")
  ab_same <- !near0(bc_tfwp) && (ab_t < 0) == (bc_tfwp < 0)
  on_same <- ab_same && !on_flat && (on_t < 0) == (bc_tfwp < 0)     # all three the same way: one list
  ab_part <- if (on_same) paste0(", ", ab_num(), " in Alberta") else if (ab_same) paste0(" and ", ab_num(), " in Alberta") else
    paste0(", ", ud(ab_t), " ", ab_num(), " in Alberta")
  on_loc <- sprintf("Geography comparison chart, Ontario, TFWP only: %s to %s", N(ytdv("on_tfwp", "prev")), N(ytdv("on_tfwp", "cur")))
  on_part <- if (on_flat) { reg(36, "on_tfwp_ytd_change", "near0", sprintf("TFWP permits, Ontario, %s", ytd_pair), on_loc,
                                shown = "", phrase = "almost unchanged in Ontario")
    ", but were almost unchanged in Ontario" } else
      paste0(if (on_same) " and " else paste0(", and ", ud(on_t), " "),
             reg(36, "on_tfwp_ytd_change", "pct0", sprintf("TFWP permits, Ontario, %s", ytd_pair), on_loc), " in Ontario")
  f6b <- paste0("In ", YL(), ", TFWP permits ", bc_part, ab_part, on_part, " (Figure C5).")
  f6b <- sub("Figure C5 (B.C. bar for 2026)", sprintf("Figure C5 (B.C. bar for %d)", Y), f6b, fixed = TRUE)
  c6 <- function() sprintf("Figure C6: %s to %s", N(ytdv("bc_lmia", "prev")), N(ytdv("bc_lmia", "cur")))
  agri_loc <- sprintf("Figure C6: %s to %s", N(ytdv("bc_agri", "prev")), N(ytdv("bc_agri", "cur")))
  lmia_num <- reg(37, "bc_lmia_ytd_change", "pct0", sprintf("Non-agricultural LMIA permits, B.C., %s", ytd_pair), c6())
  agri_txt <- if (agri_flat) { reg(38, "bc_agri_ytd_change", "near0", sprintf("Agricultural TFWP permits, B.C., %s", ytd_pair), agri_loc,
                                   shown = "", phrase = "agricultural permits held steady")
    "agricultural permits held steady" } else
      paste0("agricultural permits ", ud(v("bc_agri_ytd_change")), " ",
             reg(38, "bc_agri_ytd_change", "pct0", sprintf("Agricultural TFWP permits, B.C., %s", ytd_pair), agri_loc))
  f6c <- if (bc_tfwp < 0 && bc_lmia_down) sprintf("B.C.’s decline is in non-agricultural LMIA permits (down %s); %s (Figure C6).", lmia_num, agri_txt) else
    sprintf("In B.C., non-agricultural LMIA permits were %s %s; %s (Figure C6).", if (bc_lmia_down) "down" else "up", lmia_num, agri_txt)
  bts <- reg(39, id$bc_tfwp_share, "pct0", sprintf("TFWP share of all permits, B.C., %d", LF),
             sprintf("Table C2; %s of %s", N(v(paste0("bc_tfwp_", LF))), N(v(paste0("bc_", LF)))))
  cts <- reg(40, id$ca_tfwp_share, "pct0", sprintf("TFWP share of all permits, Canada, %d", LF),
             sprintf("Table C2; %s of %s", N(v(paste0("tfwp_", LF))), N(v(paste0("ca_", LF)))))
  f6d <- paste0("The TFWP is ", bts, " of B.C.’s permits against ", cts, " nationally (", LF, ")",
                if (tfwp_heavier && bc_tfwp < 0) ", so this decline weighs more on B.C." else if (tfwp_heavier) ", so these permits matter more for B.C." else ".")
  if (!grepl("\\.$", f6d)) f6d <- f6d
  f6 <- paste(f6_lead, f6b, f6c, f6d)

  # Implications
  i1 <- paste0("- Track temporary labour supply monthly. The Outlook expects temporary residents to flow out of B.C. over 2025–2035 [3]",
               if (statcan && rebound) "; the falling temporary population fits this, despite rising permits." else
                 if (statcan) "; the falling temporary population fits this." else ".",
               " Reading monthly IRCC permits with Statistics Canada population estimates would show early if supply strays from that path.")
  i2 <- "- Find out how many graduates stay. The study permit cap may shrink future graduating classes, and routes to permanent residence have narrowed: the 2025 BC Provincial Nominee Program allocation was cut and its international post-graduate stream closed [10]. An IRCC data request for PGWPs by B.C. economic region and institution would show how many graduates stay, and where, helping test whether their skills match high-opportunity occupations."
  i3 <- paste0("- Identify sectors ", if (bc_lmia_down) "losing" else "gaining", " employer-sponsored workers. IRCC’s TFWP tables by occupation would show which B.C. occupations account for the ",
               reg(41, "bc_lmia_ytd_change", "pct0", sprintf("Non-agricultural LMIA permits, B.C., %s", ytd_pair), "Figure C6"),
               if (bc_lmia_down) " fall" else " rise", " in non-agricultural LMIA permits.")

  # Data notes
  calc <- "Calculated from the IRCC files"
  u1 <- reg(42, id$unk_share_ymax, "pct0", sprintf("Unknown IMP share of all permits, Canada, %d", YMAX), calc)
  u2 <- reg(43, id$unk_share_cur, "pct0", sprintf("Unknown IMP share of all permits, Canada, %s", ys), calc)
  dn1 <- paste0("A person can hold several permits, and a permit does not show employment. IRCC suppresses values of 0–5 (“--”, counted here as 0) and rounds others to the nearest 5; data are preliminary [1, 2]. “Unknown IMP” made up ",
                u1, " of permits in ", YMAX,
                if (ab_unk) paste0(", ", u2, " in ", YL(Y), " and ",
                                   reg(44, "ab_unk_share_of_increase", "pct0", sprintf("Unknown IMP share of Alberta's %s increase", ys), calc),
                                   " of Alberta’s ", Y, " increase, so stream comparisons are approximate, especially for Alberta.") else
                  paste0(" and ", u2, " in ", YL(Y), ", so stream comparisons are approximate."))
  # (c) The dated context line sits on the byline, so the two-page body keeps its layout.
  context_line <- if (context_release)
    sprintf("Numbers update with each IRCC release; context written for the release of %s", fmt_date(Q2_CONTEXT_RELEASE)) else
      sprintf("Numbers: %s; context written for the release of %s",
              if (is.na(release_date)) "the data shown" else paste("release of", fmt_date(release_date)), fmt_date(Q2_CONTEXT_RELEASE))
  title <- if (title_rule) sprintf("Graduates drive the %d work permit rebound, but B.C.’s share shrinks", Y) else
    sprintf("Work permits in Canada and B.C., %s", YL(Y))
  byline <- paste("Mohsen Fazeli \u00b7", if (context_release) Q2_NOTE_WRITTEN else paste("updated", fmt_date(build_date)), "\u00b7", context_line)
  rel_ref <- if (is.na(release_date)) "data shown" else paste("release of", fmt_date(release_date))
  refs_lines <- c(
    sprintf("[1] IRCC, TFWP work permit holders, %s.", rel_ref), sprintf("[2] IRCC, IMP work permit holders, %s.", rel_ref),
    "[3] PSFS, B.C. Labour Market Outlook: 2025 Edition, October 2025, pp. 9–13, 26.",
    "[4] PMO, COVID-19 response news release, March 2020.",
    "[5] IRCC, Minister’s announcement on temporary residents, March 2024.",
    "[6] IRCC, Cap on new international student permits, January 2024.",
    "[7] ESDC, Reducing the number of temporary foreign workers, August 2024.",
    "[8] IRCC, Strengthening temporary residence programs, September 2024.",
    "[9] ESDC, TFW Program Workforce Solutions Road Map, April 2022.",
    "[10] BC Gov News, B.C. nominee program and workforce priorities, April 2025.",
    "[11] IRCC, PGWP Program and labour market impact, November 2024.",
    "[12] Statistics Canada, Canada’s population estimates, The Daily, September 23, 2026.",
    "[13] M. Fazeli, Temporary Workers in Canada dashboard, October 2026.")
  paras <- c(title, byline, "Summary",
             "This note uses Immigration, Refugees and Citizenship Canada (IRCC) work permit data [1, 2], analysed in the accompanying dashboard [13], to inform B.C. labour market and skills planning, including the B.C. Labour Market Outlook [3].",
             s_b1, s_b2, s_b3, s_b4, for_bc, "Background",
             "The TFWP lets employers hire foreign workers, usually after a Labour Market Impact Assessment (LMIA) shows no Canadian is available; this note calls these permits employer-sponsored. The International Mobility Program (IMP) needs no LMIA; it covers PGWPs for graduates of Canadian institutions [11], spouses and youth exchanges.",
             bg2, "Findings", h1, f1, f2,
             "[FIGURE 1: annual stacked bars, TFWP and IMP, same months each year]",
             if (P$M == 12) "Figure 1. Permits that took effect, by year, Canada." else sprintf("Figure 1. Permits that took effect, %s of each year, Canada.", YL()),
             h2, f3, f4, h3, f5,
             "[FIGURE 2: horizontal bars, each province’s share of Canada’s increase, B.C. highlighted]", fig2_cap,
             f6, "Implications and next steps for B.C.", i1, i2, i3, "Data notes", dn1, "References", refs_lines)
  nums <- do.call(rbind, rows)
  nums <- nums[order(nums$order), ]
  # every registered phrase must be in the text (a number that a fallback dropped is not registered)
  flat <- paste(paras, collapse = " ")
  miss <- nums$phrase[!vapply(nums$phrase, function(p) grepl(p, flat, fixed = TRUE), logical(1))]
  if (length(miss)) stop("Numbers registered but not in the text: ", paste(miss, collapse = "; "))
  list(paras = paras, nums = nums, rules = do.call(rbind, unname(rules)),
       flags = list(rebound = rebound, statcan = statcan, pgwp_up = pgwp_up, unk_up = unk_up, pt_same = pt_same,
                    tfwp_mix_note = tfwp_mix_note, context_release = context_release, inc_word = inc_word))
}
