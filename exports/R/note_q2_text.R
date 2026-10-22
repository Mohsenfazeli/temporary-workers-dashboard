# =============================================================================
# exports/R/note_q2_text.R: the Downloads note's text for any IRCC release.
#   (a) numbers are filled in from the facts (exports/R/note_q2_facts.R), rounded
#       as in the note (rules: pct0, k, h, M2);
#   (b) words that state a direction or a comparison ("rose", "more than tripled",
#       "falling faster in B.C.") are chosen by tested rules, with a plain
#       fallback when a rule no longer holds;
#   (c) context, causes and implications written for the September 15, 2026
#       release are kept only while the facts they rely on still hold, and a
#       dated line at the end of the Data notes says when they were written.
# =============================================================================

Q2_CONTEXT_RELEASE <- as.Date("2026-09-15")     # the release the context was written for
Q2_NOTE_WRITTEN    <- "October 19, 2026"        # date of the original note text
Q2_FIGS_ORIGINAL   <- "dashboard shots of October 19, 2026"

q2_r0 <- function(x) floor(x + 0.5)              # round half up
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

  # Oct 21, 2026 (reviewer feedback on the note): one time frame in the body
  # (January–July, plus full years), a two-sentence bottom line, the summary in
  # the order of the findings, Table 1 (B.C. and Canada), captions that state the
  # takeaway, one label per concept ("issued", "employer-sponsored", "non-farm",
  # "farm"), and the implications by priority, graduate retention first.
  sg <- function(x, s) paste0(if (x >= 0) "+" else "\u2212", s)   # signed rounded percentage (Table 1)
  bc_inc <- v("bc_ytd_increase")
  bc_contrib_loc <- sprintf("Figure 2: %s (%s of %s)", fmt_share(bc_contrib), N(bc_inc), N(v("ca_ytd_increase")))
  bc_share_loc <- sprintf("Figure C8 (Share column): %s (%s of %s)", fmt_share(bc_share_prev), N(ytdv("bc", "prev")), N(ytdv("ca", "prev")))
  ab_t <- v("ab_tfwp_ytd_change"); on_t <- v("on_tfwp_ytd_change")

  # ---- Summary: bottom line, then the bullets in the order of the findings ----
  bl <- paste0(if (rebound) "Work permits are rebounding" else "Work permits are falling",
               if (rebound && pgwp_drive) ", led by graduates" else "",
               if (tfwp_faster) ", while employer-sponsored permits fall faster in B.C. than nationally." else
                 if (bc_tfwp < 0) ", while employer-sponsored permits fall in B.C." else ".",
               if (bc_all_grads) " B.C.’s growth now rests on graduates, whose routes to permanent residence have narrowed [10]; how many stay is an open question." else
                 " Routes to permanent residence for graduates have narrowed [10]; how many stay is an open question.")
  s_b1_lead <- if (rebound && statcan) "Permits are rebounding, but the temporary population is not." else
    if (rebound) "Permits are rebounding." else "Permits are falling."
  s_b1 <- paste0("- ", s_b1_lead, " Permits ", ud(ca_chg), " ",
                 reg(2, "ca_ytd_change", "pct0", sprintf("Summary: permits %s %s, %s vs %s", ud(ca_chg), q2_round(ca_chg, "pct0"), ys, ysp),
                     sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
                 " in ", YL(Y), " from the same months of ", PY,
                 if (above_ph) { reg(3, paste0("ca_ytd", yy(P$PH)), "highest_ytd", sprintf("Summary: %s is the highest %s in the data", ys, YL()),
                                     "Figure 1; calculated from the IRCC files", shown = "", phrase = "in the data")
                   sprintf(", the highest %s in the data", YL()) } else "",
                 if (statcan && rebound) ", yet Statistics Canada estimates temporary residents fell [12]." else
                   if (statcan) "; Statistics Canada estimates temporary residents fell too [12]." else ".")
  pgwp_share_loc <- sprintf("Overview What the data show panel: %s of %s (%s)", N(v("pgwp_ytd_increase")),
                            N(v("ca_ytd_increase")), fmt_share(v("pgwp_share_of_increase")))
  s_b2 <- if (rebound && pgwp_up) {
    paste0("- ", if (pgwp_drive) "Graduates drive the rebound." else "Graduates add to the rebound.",
           " Post-graduation work permits (PGWPs) supplied ",
           reg(4, "pgwp_share_of_increase", "pct0", sprintf("Summary: PGWPs' share of the %s increase", ys), pgwp_share_loc),
           " of the national increase.")
  } else {
    paste0("- ", if (pgwp_up) "Graduate permits rose." else "Graduate permits fell.",
           " Post-graduation work permits (PGWPs) ", ud(pg_ca), " ",
           reg(4, "pgwp_ytd_change", "pct0", sprintf("Summary: PGWPs, Canada, %s", ytd_pair),
               sprintf("Figure C3: %s to %s", N(ytdv("pgwp", "prev")), N(ytdv("pgwp", "cur")))),
           " over the same months.")
  }
  s_b3 <- if (rebound && bc_inc > 0) {
    paste0("- ", if (bc_small) "B.C.’s share of the rebound is small." else "B.C. keeps pace with the rebound.",
           " B.C. held ", reg(5, id$bc_share_prev, "pct0", sprintf("Summary: B.C. share of Canada's permits, %s", ysp), bc_share_loc),
           " of Canada’s permits a year earlier ", if (bc_small) "but" else "and", " supplied ",
           reg(6, "bc_contrib_ytd", "pct0", sprintf("Summary: B.C. share of Canada's %s increase", ys), bc_contrib_loc), " of the increase.")
  } else {
    bcc <- v("bc_ytd_change")
    paste0("- ", if (rebound) "B.C. misses the rebound." else if (bcc < 0) "B.C. follows the national decline." else
      "B.C. bucks the national decline.", " B.C. permits ", ud(bcc), " ",
      reg(5, "bc_ytd_change", "pct0", sprintf("Summary: permits, B.C., %s", ytd_pair),
          sprintf("Figure C8 / Geography tab: %s to %s", N(ytdv("bc", "prev")), N(ytdv("bc", "cur")))),
      " over the same months.")
  }
  tfwp_lead <- if (tfwp_faster) "Employer-sponsored permits are falling faster in B.C." else
    if (bc_tfwp < 0 && ca_tfwp < 0) "Employer-sponsored permits are falling, though more slowly in B.C." else
      if (bc_tfwp < 0) "Employer-sponsored permits are falling in B.C." else
        if (ca_tfwp < 0) "Employer-sponsored permits are rising in B.C. but falling nationally." else
          "Employer-sponsored permits are rising."
  same_dir <- (bc_tfwp < 0) == (ca_tfwp < 0)
  s_b4 <- paste0("- ", tfwp_lead, " Temporary Foreign Worker Program (TFWP) permits ", ud(bc_tfwp), " ",
                 reg(7, "bc_tfwp_ytd_change", "pct0", sprintf("Summary: TFWP permits, B.C., %s", ytd_pair), paste("Figure C5", loc_c5)),
                 " in B.C. over the same months, against ", if (same_dir) "" else "a ",
                 reg(8, "tfwp_ytd_change", "pct0", sprintf("Summary: TFWP permits, Canada, %s", ytd_pair),
                     sprintf("Trends tab, change from a year earlier: TFWP %s %s (%s to %s)", if (ca_tfwp < 0) "down" else "up",
                             sprintf("%.1f%%", abs(100 * ca_tfwp)), N(ytdv("tfwp", "prev")), N(ytdv("tfwp", "cur")))),
                 if (same_dir) "" else paste0(" ", ud(ca_tfwp, "rise", "fall")), " nationally.")

  # ---- Background -------------------------------------------------------------
  bg1 <- sprintf("This note uses Immigration, Refugees and Citizenship Canada (IRCC) work permit data [1, 2], analysed in the accompanying dashboard [13], with B.C. as the focus geography, alongside the B.C. Labour Market Outlook [3]. The data, from IRCC’s open data release of %s, count permits issued each month, by intended province, from %s to %s [1, 2]. %s Policy events are context: the data show timing, not cause. Figures are rounded; exact values are in Appendix D.",
                 if (is.na(release_date)) fmt_date(Sys.Date()) else fmt_date(release_date), fmt_month(P$first), fmt_month(P$latest),
                 if (P$M == 12) sprintf("%d is compared with %d.", Y, PY) else sprintf("%s is compared with the same months of %d.", YL(Y), PY))
  bg2 <- "The TFWP lets employers hire foreign workers, usually after a Labour Market Impact Assessment (LMIA) shows no Canadian is available; this note calls these permits employer-sponsored, split into farm permits (agricultural streams) and non-farm permits (IRCC’s “other workers with an LMIA”). The International Mobility Program (IMP) needs no LMIA; it covers PGWPs for graduates of Canadian institutions [11], spouses and youth exchanges."

  # ---- Findings 1: full years, then the same months (no 12-month totals) ----------
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
  lf_txt <- reg(13, paste0("ca_", LF), "k", sprintf("Permits, Canada, %d", LF), c1)
  f1b <- if (policy_decline) sprintf("They fell to %s in %d, after a federal target to lower temporary residents’ share of the population [5] and tighter study permit, PGWP and TFWP rules [6–8].", lf_txt, LF) else
    if (fell_after_peak) sprintf("They fell to %s in %d.", lf_txt, LF) else sprintf("They reached %s in %d.", lf_txt, LF)
  f1 <- paste(f1a, f1b)
  f2a <- paste0("In ", YL(Y), ", ", reg(14, id$ytd_cur, "k", sprintf("Permits, Canada, %s", ys), sprintf("Overview card 1 (Figure C2): %s", N(v(id$ytd_cur)))),
                " permits were issued, ",
                reg(15, "ca_ytd_change", "pct0", sprintf("Permits, Canada, %s vs %s", ys, ysp), sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
                if (rebound) " more" else " fewer", " than in the same months of ", PY,
                if (above_ph) { reg(16, paste0("ca_ytd", yy(P$PH)), "highest_ytd",
                                    sprintf("%s is the highest %s in the data (previous high %s: %s)", ys, YL(), YS(P$PH), N(max(P$ytd_prior))),
                                    "Figure 1 (tallest bar); calculated from the IRCC files", shown = "", phrase = "the highest")
                  sprintf(" and the highest %s in the data", YL()) } else
                  if (rebound) sprintf(", but below the %d high for these months", P$PH) else "",
                " (Figure 1).")
  f2b <- if (statcan) { reg(17, "context_statcan_npr", "statcan", "Statistics Canada: non-permanent residents, July 1, 2025 to July 1, 2026",
                            "Statistics Canada, The Daily, September 23, 2026 [12]: \u2212154,614 (to 2,779,774); not IRCC data")
    paste0(if (rebound) "Yet permits are not people: " else "Permits are not people: ",
           "Statistics Canada estimates that non-permanent residents fell by 155,000 in the year to July 1, 2026, led by study permit holders [12].",
           if (rebound && pgwp_up) " Permits can rise while people fall, likely because new PGWPs go to graduates of Canadian institutions [11], who were already in Canada as students." else "") } else NULL
  f2 <- paste(c(f2a, f2b), collapse = " ")
  fig1_cap <- if (above_ph) sprintf("Figure 1. %s has the most permits of any %s in the data.", YL(Y), if (P$M == 12) "year" else YL()) else
    if (P$M == 12) "Figure 1. Permits issued, by year, Canada." else sprintf("Figure 1. Permits issued, %s of each year, Canada.", YL())

  # ---- Findings 2 -------------------------------------------------------------
  h2 <- if (pgwp_up && lmia_down) "Graduates lift permits; employer-sponsored permits fall" else
    if (pgwp_up) "Graduates lift permits; employer-sponsored permits also rise" else
      if (lmia_down) "Graduate and employer-sponsored permits both fall" else "Graduate permits fall; employer-sponsored permits rise"
  c3 <- function(y) sprintf("Figure C3 (Composition tab; hover on the %d bar)", y)
  pg_prev <- reg(18, paste0("pgwp_ytd", yy(PY)), "k", sprintf("PGWPs, Canada, %s", ysp), c3(PY))
  pg_cur <- reg(19, paste0("pgwp_ytd", yy(Y)), "k", sprintf("PGWPs, Canada, %s", ys), c3(Y))
  inc_txt <- function(o) reg(o, "ca_ytd_increase", "k", sprintf("Total %s, Canada, %s to %d", inc_word, ysp, Y),
                             sprintf("Overview card 2 (%s \u2212 %s)", N(v(id$ytd_cur)), N(v(id$ytd_prev))))
  f3a <- if (pgwp_up && rebound) {
    paste0(sprintf("In %s, PGWPs rose from %s to %s. ", YL(), pg_prev, pg_cur),
           sprintf("They supplied %s of the %s additional permits.",
                   reg(20, "pgwp_ytd_increase", "k", sprintf("PGWP increase, Canada, %s to %d", ysp, Y),
                       sprintf("Calculated from Figure C3 values (%s \u2212 %s)", N(ytdv("pgwp", "cur")), N(ytdv("pgwp", "prev")))), inc_txt(21)))
  } else if (pgwp_up) {
    sprintf("In %s, PGWPs rose from %s to %s, while total permits fell by %s.", YL(), pg_prev, pg_cur, inc_txt(21))
  } else sprintf("In %s, PGWPs fell from %s to %s.", YL(), pg_prev, pg_cur)
  f3b <- if (pgwp_up) "The cause is untested; larger student cohorts admitted before the 2024 study permit cap [6] may now be graduating." else NULL
  f3 <- paste(c(f3a, f3b), collapse = " ")
  comp_tfwp <- "Composition tab (TFWP chart; Other TFWP workers with an LMIA)"
  lmia_lead <- if (lmia_down == pgwp_up) "Employer-sponsored permits moved the other way." else "Employer-sponsored permits moved the same way."
  lmia_w <- q2_multiple_word(v("lmia_2024") / v("lmia_2021"))
  lm21 <- reg(23, "lmia_growth_2021_2024", "tripled", "Non-farm employer-sponsored permits (IRCC: other workers with an LMIA), Canada, 2021 to 2024", comp_tfwp, shown = lmia_w)
  lm25 <- reg(24, "lmia_fall_2024_2025", "pct0", "Non-farm employer-sponsored permits, Canada, 2024 to 2025", comp_tfwp)
  lmy <- reg(25, "lmia_ytd_change", "pct0", sprintf("Non-farm employer-sponsored permits, Canada, %s", ytd_pair), comp_tfwp)
  f4 <- paste0(lmia_lead, " Nationally, non-farm permits ", lm21, " from 2021 to 2024, after low-wage caps were raised in 2022 [9]. They then ",
               if (lmia_fell25) "fell " else "rose ", lm25, " from 2024 to 2025",
               if (lmia_fell25 && lmia_down) " and another " else if (lmia_down) " and fell " else ", and rose ", lmy, " in ", YL(Y), " from a year earlier",
               if (lmia_fell25) ", after the 2024 low-wage limits [7]." else ".")

  # ---- Findings 3: one sentence on shares; Table 1 holds the detail -------------
  h3 <- if (bc_small) "B.C.’s share of the rebound shrinks" else if (rebound) "B.C.’s share of the rebound" else "B.C. in the national decline"
  top_name <- if (top == "British Columbia") "B.C." else top
  top_loc <- sprintf("Figure 2: %s (%s of %s)", fmt_share(v(paste0(top_k, "_contrib_ytd"))), N(v(paste0(top_k, "_ytd_increase"))), N(v("ca_ytd_increase")))
  f5a <- if (rebound && bc_inc > 0) {
    paste0("B.C. held ", reg(26, id$bc_share_prev, "pct0", sprintf("B.C. share of Canada's permits, %s", ysp), bc_share_loc),
           " of Canada’s permits in ", YL(PY), if (bc_small) " but" else " and", " supplied ",
           reg(27, "bc_contrib_ytd", "pct0", sprintf("B.C. share of Canada's %s increase", ys), bc_contrib_loc), " of the increase to ", Y, ". ",
           if (top == "British Columbia") "No province supplied more (Figure 2)." else
             paste0(top_name, " supplied ", reg(28, paste0(top_k, "_contrib_ytd"), "pct0", sprintf("%s share of Canada's %s increase", top_name, ys), top_loc), " (Figure 2)."))
  } else if (rebound) {
    paste0("B.C. permits fell by ", reg(26, "bc_ytd_increase", "k", sprintf("Change in B.C. permits, %s to %d", ysp, Y), "Figure 2"),
           " while Canada’s rose (Figure 2). ", top_name, " supplied ",
           reg(28, paste0(top_k, "_contrib_ytd"), "pct0", sprintf("%s share of Canada's %s increase", top_name, ys), top_loc), " of the national increase.")
  } else {
    bcc <- v("bc_ytd_change")
    paste0("B.C. permits ", ud(bcc), " ", reg(26, "bc_ytd_change", "pct0", sprintf("Permits, B.C., %s", ytd_pair), "Figure 2"), " in ", YL(Y),
           ", against a ", reg(27, "ca_ytd_change", "pct0", sprintf("Permits, Canada, %s", ytd_pair), sprintf("Overview card 2 (Figure C2): %s", fmt_pct(ca_chg))),
           " fall nationally (Figure 2).")
  }
  f5c <- if (bc_all_grads) { reg(29, "bc_pgwp_ytd_increase", "pgwp_gt_total",
                                 sprintf("B.C. PGWP gain (%s) exceeds B.C.'s total gain (%s); other permits fell by %s", N(v("bc_pgwp_ytd_increase")), N(bc_inc),
                                         N(abs(v("bc_other_ytd_change")))),
                                 "Calculated from the IRCC files", shown = "", phrase = "more than all of B.C.’s growth")
    paste0("Graduates account for more than all of B.C.’s growth", if (bc_other_down) "; other permits fell" else "", " (Table 1).") } else
      "Table 1 compares B.C. with Canada."
  f5 <- paste(f5a, f5c)

  # Table 1: four rows, B.C. and Canada, the same months of PY and Y
  t1 <- list(all = list("All permits", c("bc", "ca"), c("bc_ytd_change", "ca_ytd_change"), "all permits", "Overview cards"),
             pgwp = list("PGWPs", c("bc_pgwp", "pgwp"), c("bc_pgwp_ytd_change", "pgwp_ytd_change"), "PGWPs", "Composition tab (Figures C3, C4)"),
             other = list("All other permits (all minus PGWPs)", c("bc_other", "ca_other"), c("bc_other_ytd_pct", "ca_other_ytd_pct"),
                          "permits other than PGWPs", "Calculated from the IRCC files (all permits minus PGWPs)"),
             tfwp = list("of which employer-sponsored (TFWP)", c("bc_tfwp", "tfwp"), c("bc_tfwp_ytd_change", "tfwp_ytd_change"),
                         "TFWP permits", "Geography comparison, TFWP only (Figure C5)"))
  t1_rows <- character(); o <- 30
  for (r in t1) {
    cells <- character()
    for (g in 1:2) {
      gn <- c("B.C.", "Canada")[g]; ids <- paste0(r[[2]][g], "_ytd", yy(c(PY, Y)))
      cells <- c(cells,
                 reg(o + 0.1, ids[1], "k", sprintf("Table 1: %s, %s, %s", r[[4]], gn, ysp), r[[5]]),
                 reg(o + 0.2, ids[2], "k", sprintf("Table 1: %s, %s, %s", r[[4]], gn, ys), r[[5]]),
                 sg(v(r[[3]][g]), reg(o + 0.3, r[[3]][g], "pct0", sprintf("Table 1: %s, %s, %s", r[[4]], gn, ytd_pair), r[[5]])))
      o <- o + 1
    }
    t1_rows <- c(t1_rows, paste0("| ", paste(c(r[[1]], cells), collapse = " | "), " |"))
  }
  t1_head <- sprintf("| Permits issued, %s | B.C. %d | B.C. %d | B.C. change | Canada %d | Canada %d | Canada change |",
                     if (P$M == 12) "full year" else YL(), PY, Y, PY, Y)
  t1_cap <- if (bc_all_grads && bc_other_down) "Table 1. B.C. grew only through graduates; its other permits fell, while Canada’s rose." else
    sprintf("Table 1. Permits issued, B.C. and Canada, %s and %s.", ysp, ys)
  fig2_cap <- sprintf("Figure 2. %s supplied %s of Canada’s %s %s%s", top_name,
                      if (v(paste0(top_k, "_contrib_ytd")) > 0.5) "most" else "the largest share", YL(Y), inc_word,
                      if (bc_small && top != "British Columbia") "; B.C. less than its share." else ".")

  # Employer-sponsored permits in B.C., with Canada (Table 1), Alberta and Ontario
  f6_lead <- if (tfwp_faster) "Employer-sponsored permits are also falling faster in B.C." else tfwp_lead
  bc_part <- if (near0(bc_tfwp)) "were almost unchanged in B.C." else paste0(ud(bc_tfwp), " ",
    reg(38, "bc_tfwp_ytd_change", "pct0", sprintf("TFWP permits, B.C., %s", ytd_pair), sprintf("Table 1; Figure C5 (B.C. bar for %d)", Y)), " in B.C.")
  ca_part <- paste0(", against ", if (same_dir) "" else "a ",
                    reg(39, "tfwp_ytd_change", "pct0", sprintf("TFWP permits, Canada, %s", ytd_pair), "Table 1; Trends tab, TFWP only"),
                    if (same_dir) "" else paste0(" ", ud(ca_tfwp, "rise", "fall")), " in Canada (Table 1).")
  ab_txt <- paste0("They ", ud(ab_t), " ", reg(40, "ab_tfwp_ytd_change", "pct0", sprintf("TFWP permits, Alberta, %s", ytd_pair),
                                               sprintf("Geography comparison chart, Alberta, TFWP only: %s to %s", N(ytdv("ab_tfwp", "prev")), N(ytdv("ab_tfwp", "cur")))),
                   " in Alberta")
  on_loc <- sprintf("Geography comparison chart, Ontario, TFWP only: %s to %s", N(ytdv("on_tfwp", "prev")), N(ytdv("on_tfwp", "cur")))
  on_txt <- if (on_flat) { reg(41, "on_tfwp_ytd_change", "near0", sprintf("TFWP permits, Ontario, %s", ytd_pair), on_loc,
                               shown = "", phrase = "almost unchanged in Ontario")
    paste0(if (ab_t < 0) " but" else " and", " were almost unchanged in Ontario.") } else
      paste0(if ((on_t < 0) == (ab_t < 0)) " and " else " but ", ud(on_t), " ",
             reg(41, "on_tfwp_ytd_change", "pct0", sprintf("TFWP permits, Ontario, %s", ytd_pair), on_loc), " in Ontario.")
  f6b <- paste0("In ", YL(), ", TFWP permits ", bc_part, ca_part, " ", ab_txt, on_txt)
  c6 <- function() sprintf("Figure C6: %s to %s", N(ytdv("bc_lmia", "prev")), N(ytdv("bc_lmia", "cur")))
  agri_loc <- sprintf("Figure C6: %s to %s", N(ytdv("bc_agri", "prev")), N(ytdv("bc_agri", "cur")))
  lmia_num <- reg(42, "bc_lmia_ytd_change", "pct0", sprintf("Non-farm employer-sponsored permits, B.C., %s", ytd_pair), c6())
  agri_txt <- if (agri_flat) { reg(43, "bc_agri_ytd_change", "near0", sprintf("Farm (agricultural) TFWP permits, B.C., %s", ytd_pair), agri_loc,
                                   shown = "", phrase = "farm permits held steady")
    "farm permits held steady" } else
      paste0("farm permits ", ud(v("bc_agri_ytd_change")), " ",
             reg(43, "bc_agri_ytd_change", "pct0", sprintf("Farm (agricultural) TFWP permits, B.C., %s", ytd_pair), agri_loc))
  f6c <- if (bc_tfwp < 0 && bc_lmia_down) sprintf("B.C.’s decline is in non-farm permits (down %s); %s.", lmia_num, agri_txt) else
    sprintf("In B.C., non-farm permits were %s %s; %s.", if (bc_lmia_down) "down" else "up", lmia_num, agri_txt)
  bts <- reg(44, id$bc_tfwp_share, "pct0", sprintf("TFWP share of all permits, B.C., %d", LF),
             sprintf("Table C2; %s of %s", N(v(paste0("bc_tfwp_", LF))), N(v(paste0("bc_", LF)))))
  cts <- reg(45, id$ca_tfwp_share, "pct0", sprintf("TFWP share of all permits, Canada, %d", LF),
             sprintf("Table C2; %s of %s", N(v(paste0("tfwp_", LF))), N(v(paste0("ca_", LF)))))
  f6d <- paste0("The TFWP was ", bts, " of B.C.’s ", LF, " permits, against ", cts, " nationally",
                if (tfwp_heavier && bc_tfwp < 0) ", so this decline weighs more on B.C." else if (tfwp_heavier) ", so these permits matter more for B.C." else ".")
  f6 <- paste(f6_lead, f6b, f6c, f6d)

  # ---- Open questions the release leaves unanswered ------------------------------
  i1 <- paste0("- How many graduates stay? ",
               if (bc_all_grads) sprintf("Graduates account for more than all of B.C.’s growth in %d (Table 1). ", Y) else "",
               "The study permit cap may shrink future graduating classes, and routes to permanent residence have narrowed: the 2025 BC Provincial Nominee Program allocation was cut and its international post-graduate stream closed [10]. PGWP counts by B.C. economic region and institution are not in these open files, so retention and skill match cannot be read from this release alone.")
  i2 <- paste0("- Which sectors account for the ", if (bc_lmia_down) "fall" else "rise", " in employer-sponsored workers? IRCC’s TFWP tables by occupation would show which B.C. occupations account for the ",
               reg(46, "bc_lmia_ytd_change", "pct0", sprintf("Non-farm employer-sponsored permits, B.C., %s", ytd_pair), "Figure C6"),
               if (bc_lmia_down) " fall" else " rise", " in non-farm permits.")
  i3 <- paste0("- How do monthly permits line up with the temporary population? The Outlook expects temporary residents to flow out of B.C. over 2025–2035 [3]",
               if (statcan) "; the falling temporary population [12] fits this." else ".",
               " Comparing monthly IRCC permits with Statistics Canada population estimates would show whether supply is moving with that path.")

  # ---- Data notes (the Unknown IMP check is here) --------------------------------
  calc <- "Calculated from the IRCC files"
  unk_loc <- sprintf("Figure C3 (Unknown or other IMP; %s to %s)", N(ytdv("unk", "prev")), N(ytdv("unk", "cur")))
  u1 <- reg(47, id$unk_share_ymax, "pct0", sprintf("Unknown IMP share of all permits, Canada, %d", YMAX), calc)
  u2 <- reg(48, id$unk_share_cur, "pct0", sprintf("Unknown IMP share of all permits, Canada, %s", ys), calc)
  dn1 <- paste0("A person can hold several permits, and a permit does not show employment. IRCC suppresses values of 0–5 (“--”, counted here as 0) and rounds others to the nearest 5; data are preliminary [1, 2]. Permits with no identified program (“Unknown IMP”) made up ",
                u1, " of permits in ", YMAX, " and ", u2, " in ", YL(Y), ".",
                if (ab_unk) paste0(" They supplied ", reg(49, "ab_unk_share_of_increase", "pct0", sprintf("Unknown IMP share of Alberta's %s increase", ys), calc),
                                   " of Alberta’s ", Y, " increase, so stream comparisons are approximate, especially for Alberta.") else
                  " Stream comparisons are therefore approximate.",
                if (pgwp_up && unk_up) paste0(" The PGWP rise is not a recoding out of Unknown IMP, which also rose, by ",
                                              reg(50, "unk_ytd_increase", "k", sprintf("Unknown IMP increase, Canada, %s to %d", ysp, Y), unk_loc), ".") else
                  if (pgwp_up) paste0(" Part of the PGWP rise may be a recoding: Unknown IMP fell by ",
                                      reg(50, "unk_ytd_increase", "k", sprintf("Unknown IMP decrease, Canada, %s to %d", ysp, Y), unk_loc), ".") else "")
  # (c) The dated context line sits on the byline, so the two-page body keeps its layout.
  context_line <- if (context_release)
    sprintf("Numbers update with each IRCC release; context written for the release of %s", fmt_date(Q2_CONTEXT_RELEASE)) else
      sprintf("Numbers: %s; context written for the release of %s",
              if (is.na(release_date)) "the data shown" else paste("release of", fmt_date(release_date)), fmt_date(Q2_CONTEXT_RELEASE))
  # Title rule: "Data Insight – <the note's date>" (en dash, no subtitle), the
  # same date as the byline: the original note date for the release it was
  # written for, otherwise the build date. The byline also names the data period.
  note_date <- if (context_release) Q2_NOTE_WRITTEN else fmt_date(build_date)
  title <- sprintf("Data Insight \u2013 %s", note_date)
  byline <- paste("Mohsen Fazeli \u00b7", if (context_release) note_date else paste("updated", note_date), "\u00b7 IRCC data to",
                  fmt_month(P$latest), "\u00b7", context_line)
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
  paras <- c(title, byline, "Summary", bl, s_b1, s_b2, s_b3, s_b4, "Background", bg1, bg2,
             "Findings", h1, f1, f2,
             "[FIGURE 1: annual stacked bars, TFWP and IMP, same months each year]", fig1_cap,
             h2, f3, f4, h3, f5,
             "[TABLE 1]", t1_cap, t1_head, t1_rows,
             "[FIGURE 2: horizontal bars, each province’s share of Canada’s increase, B.C. highlighted]", fig2_cap,
             f6, "Open questions", i1, i2, i3, "Data notes", dn1, "References", refs_lines)
  nums <- do.call(rbind, rows)
  nums <- nums[order(nums$order), ]
  # every registered phrase must be in the text (table rows without their "|"; "−" read as "-")
  flat <- paste(ifelse(startsWith(paras, "|"), gsub(" *\\| *", " ", paras), paras), collapse = " ")
  miss <- nums$phrase[!vapply(nums$phrase, function(p) grepl(p, flat, fixed = TRUE), logical(1))]
  if (length(miss)) stop("Numbers registered but not in the text: ", paste(miss, collapse = "; "))
  list(paras = paras, nums = nums, rules = do.call(rbind, unname(rules)),
       flags = list(rebound = rebound, statcan = statcan, pgwp_up = pgwp_up, unk_up = unk_up, pt_same = pt_same,
                    tfwp_mix_note = tfwp_mix_note, context_release = context_release, inc_word = inc_word))
}
