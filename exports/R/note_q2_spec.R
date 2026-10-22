# =============================================================================
# exports/R/note_q2_spec.R: everything the Word builder (exports/q2/build_note.py)
# needs, written to one folder: facts.csv, nums.csv, refs.csv, geo.csv, mix.csv
# and spec.json (text, captions, appendix lines). Appendix lines that do not
# depend on the data are read unchanged from exports/q2/original_strings.json
# (static appendix wording).
# =============================================================================

q2_fig_files <- c(f1 = "dash_fig1_overview_headline_samemonths.png", f2 = "dash_fig2_geography_contribution_bc.png",
                  c1 = "dash_c1_overview_headline_annual.png", c2 = "dash_c2_overview_cards_last12.png",
                  c3 = "dash_c3_composition_imp_canada_samemonths.png", c4 = "dash_c4_composition_imp_bc_samemonths.png",
                  c5 = "dash_c5_geography_compare_bc_tfwp.png", c6 = "dash_c6_composition_tfwp_bc_samemonths.png",
                  c7 = "dash_c7_composition_tfwp_bc_monthly.png", c8 = "dash_c8_geography_table_ytdprev.png")

#' Periods for the dashboard filters used by each figure (also used by the capture script).
q2_fig_ranges <- function(P) {
  ms <- function(d) fmt_month_short(d)
  L <- P$latest
  list(full = c(ms(P$first), ms(L)), last12 = c(ms(seq(L, by = "-11 months", length.out = 2)[2]), ms(L)),
       y21 = c("Jan 2021", ms(L)), y19 = c("Jan 2019", ms(L)), y22 = c("Jan 2022", ms(L)),
       ytd_prev = c(sprintf("Jan %d", P$PY), sprintf("%s %d", P$mon, P$PY)), same = P$M < 12)
}

q2_replace_once <- function(s, old, new) {
  if (lengths(regmatches(s, gregexpr(old, s, fixed = TRUE))) != 1) stop("Template text not found once: ", old)
  sub(old, new, s, fixed = TRUE)
}

q2_write_inputs <- function(bundle, out_dir, figures_dir, build_date = Sys.Date(),
                            exports_dir = file.path("exports")) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fx <- q2_facts(bundle)
  tx <- q2_note_text(fx, bundle, build_date)
  P <- fx$P; Y <- P$Y; PY <- P$PY; FY0 <- P$FY0; LF <- P$LF; YMAX <- P$YMAX; yy <- P$yy; v <- fx$val
  fl <- tx$flags; R <- q2_fig_ranges(P)
  O <- jsonlite::fromJSON(file.path(exports_dir, "q2", "original_strings.json"), simplifyVector = FALSE)
  YL <- function(y = NULL) q2_ytd_long(P, y); YS <- function(y) q2_ytd_short(P, y)
  phrase <- bundle$meta$release_phrase
  retrieved <- fmt_date(bundle$meta$retrieved_at)
  bdate <- fmt_date(build_date)
  pk_long <- sprintf("%s to %s", fmt_month(fx$peak$start), fmt_month(fx$peak$end))
  same_txt <- if (R$same) sprintf("show only Jan\u2013%s of each year (every bar is %s), ", P$mon, YL()) else ""

  # Appendix A: data-dependent lines rebuilt, the rest unchanged
  A <- unlist(O$appendix_a)
  at <- function(prefix) { i <- which(startsWith(A, prefix)); stopifnot(length(i) == 1); i }
  i <- at("**Data:**")
  A[i] <- q2_replace_once(A[i], "IRCC open data release of September 15, 2026 (data to July 2026)", phrase)
  A[i] <- q2_replace_once(A[i], "(files retrieved October 16, 2026)", sprintf("(files retrieved %s)", retrieved))
  A[i] <- q2_replace_once(A[i], "Latest month: July 2026.", sprintf("Latest month: %s.", fmt_month(P$latest)))
  if (!fl$statcan) A[i] <- q2_replace_once(A[i], "; the one outside number is Statistics Canada's estimate of the fall in non-permanent residents [12], a count of people, not permits.", ".")
  A[at("**Part-year:**")] <- if (P$M < 12)
    sprintf("**Part-year:** %d covers January to %s and is only compared with January to %s of earlier years.", Y, P$month, P$month) else
      sprintf("**Part-year:** none; %d is a full calendar year.", Y)
  rng <- function(r) paste(r[1], "to", r[2])
  # Figure filters (named here, not under the figures) and their periods
  fig_filters <- sprintf(paste0("Figure 1 (Overview tab): Canada, both programs, Annual view, %s%s; ",
                                "Figure 2 (Geography tab): British Columbia (highlighted), both programs, period ending %s."),
                         if (R$same) sprintf("show only Jan\u2013%s of each year, ", P$mon) else "", rng(R$full), fmt_month(P$latest))
  i <- at("**Figures**")
  # The figures are captured when this file is built: that date is named as such, so it is not read as the note's date.
  A[i] <- q2_replace_once(A[i], "(plain-language version, October 19, 2026; Figure 1 October 21, 2026)",
                          sprintf("(plain-language version; figures captured on %s, when this file was built)", bdate))
  A[i] <- q2_replace_once(A[i], O$fig_filters[[1]], fig_filters)
  A[i] <- q2_replace_once(A[i], "Figure 1's bars are labelled with plain years; each covers January\u2013July of that year, as the chart title says.",
                          if (R$same) sprintf("Figure 1's bars are labelled with plain years; each covers %s of that year, as the chart title says.", YL()) else "")
  A[i] <- sub(" +$", "", A[i])
  A[at("**One time frame:**")] <- sprintf(paste0("**One time frame:** the body compares %s with %s of earlier years (and full calendar ",
                                                 "years up to %d); \"highest %s in the data\" means above %s of every year from %d to %d."),
                                          YL(Y), YL(), LF, YL(), YL(), FY0, PY)
  bcv <- v("bc_ytd_change")
  A[at("**Percentages**")] <- sprintf(paste0("**Percentages** are recalculated from the raw counts and rounded once, from the exact value ",
                                             "(B.C.'s %s%.2f%% is shown as %s); the appendix tables and Appendix D show them to one decimal."),
                                      if (bcv >= 0) "+" else "\u2212", abs(100 * bcv), q2_round(bcv, "pct0"))
  if (!fl$context_release) { i <- at("**Streams**")
    A[i] <- q2_replace_once(A[i], "; in this release it is entirely IRCC's \"Unknown\" program, as its other codes are suppressed cells counted as 0.", ".") }
  A[at("**Robustness check:**")] <- sprintf(paste0(
    "**Robustness check:** leaving permits with no province or territory stated out of Canada (so Canada = the 13 ",
    "provinces and territories), B.C. supplied {bc_contrib_ytd_pt} of the %s %s, against its ",
    "{bc_share_ytd%s_pt} share of permits a year earlier ({bc_contrib_ytd} and {bc_share_ytd%s} with them included), so ",
    "the finding on B.C.'s share %s on that group."), YL(Y), fl$inc_word, yy(PY), yy(PY), if (fl$pt_same) "does not depend" else "depends")
  if (!(fl$statcan && fl$rebound)) { i <- at("**Permits vs. people:**")
    A[i] <- q2_replace_once(A[i], " in opposite directions, as they do in 2025–2026.", " in opposite directions.") }
  i <- at("**Unknown IMP check:**")
  A[i] <- q2_replace_once(A[i], "would have fallen in 2026; it rose",
                          sprintf("would have fallen in %d; it %s", Y, if (fl$unk_up) "rose" else "fell, so part of the PGWP change may be re-labelling"))
  i <- at("**Policy events**")
  A[i] <- q2_replace_once(A[i], paste0(" The cohort explanation for the 2026 PGWP rise is labelled as untested, and the explanation for ",
                                       "permits rising while people fall (new PGWPs go to graduates already in Canada [11]) as likely."),
                          if (!fl$pgwp_up) "" else paste0(sprintf(" The cohort explanation for the %d PGWP rise is labelled as untested", Y),
                            if (fl$statcan && fl$rebound) paste0(", and the explanation for permits rising while people fall (new PGWPs go to ",
                                                                 "graduates already in Canada [11]) as likely.") else "."))

  # Appendix C captions
  cap <- list(
    list(q2_fig_files[["c1"]], "Figure C1", sprintf("Full calendar years. Dashboard Overview tab; filters: Canada, both programs, Annual view, full period%s.",
                                                    if (R$same) sprintf(" (lighter bar: %d, %s only)", Y, YL()) else "")),
    list(q2_fig_files[["c2"]], "Figure C2", sprintf(paste0("The four headline cards. Dashboard Overview tab; filters: Canada, both programs, Last 12 months button (%s). ",
                                                           "The cards follow the last month of the period, so they show %s, its change from the same months of %d, ",
                                                           "the IMP / TFWP split and the latest month."), rng(R$last12), YL(Y), PY)),
    list(q2_fig_files[["c3"]], "Figure C3", sprintf("IMP permits by sub-program, Canada. Dashboard Composition tab, IMP chart; filters: Canada, Annual view, %s%s, Number of permits.",
                                                    sub("^show only", "show only", same_txt), rng(R$y21))),
    list(q2_fig_files[["c4"]], "Figure C4", sprintf("B.C. IMP permits by sub-program. Dashboard Composition tab, IMP chart; filters: British Columbia, Annual view, %s%s, Number of permits.",
                                                    same_txt, rng(R$y21))),
    list(q2_fig_files[["c5"]], "Figure C5", sprintf("TFWP permits, B.C. and Canada, change from a year earlier%s. Dashboard Geography tab, comparison chart; filters: British Columbia, TFWP only, Annual view, %s.",
                                                    if (R$same) sprintf(" (%d: %s compared with the same months of %d)", Y, YL(), PY) else "", rng(R$y19))),
    list(q2_fig_files[["c6"]], "Figure C6", sprintf("B.C. TFWP permits by stream. Dashboard Composition tab, TFWP chart; filters: British Columbia, Annual view, %s%s, Number of permits.",
                                                    same_txt, rng(R$y21))),
    list(q2_fig_files[["c7"]], "Figure C7", sprintf("B.C. TFWP permits by month, showing the seasonal peaks of agricultural streams. Dashboard Composition tab, TFWP chart; filters: British Columbia, Monthly view, %s, Number of permits.",
                                                    rng(R$y22))),
    list(q2_fig_files[["c8"]], "Figure C8", sprintf("Share of Canada's permits, %s (Share column). Dashboard Geography tab, table; filters: British Columbia, both programs, %s. Over a period this short the Growth column compares single months.",
                                                    YL(PY), rng(R$ytd_prev))))
  widths <- vapply(O$appendix_c, function(x) as.numeric(x[[4]]), numeric(1))
  appendix_c <- lapply(seq_along(cap), function(k) list(cap[[k]][[1]], cap[[k]][[2]], cap[[k]][[3]], widths[k]))
  fig <- list(`1` = list(q2_fig_files[["f1"]], as.numeric(O$fig$`1`[[2]]), O$fig$`1`[[3]]),     # caption note: the tab (filters in Appendix A)
              `2` = list(q2_fig_files[["f2"]], as.numeric(O$fig$`2`[[2]]), O$fig$`2`[[3]]))
  E <- unlist(O$appendix_e)
  E[1] <- q2_replace_once(E[1], "IRCC open data release of September 15, 2026 (data to July 2026)", phrase)
  E[2] <- q2_replace_once(E[2], "(commit 7d2fad4; Figure 1 commit 5cc71ef)", sprintf("(the version of %s, when this file was built)", bdate))

  # References: the release, retrieval date and dashboard title follow the data
  refs <- utils::read.csv(file.path(exports_dir, "q2", "references_template.csv"), stringsAsFactors = FALSE,
                          encoding = "UTF-8", check.names = FALSE, colClasses = "character")
  if (!fl$context_release) {
    for (k in 1:2) {
      refs$citation[k] <- q2_replace_once(refs$citation[k], "IRCC open data release of September 15, 2026 (data to July 2026)", phrase)
      refs$accessed[k] <- sprintf("%s (files retrieved)", retrieved)
    }
    refs$citation[13] <- q2_replace_once(refs$citation[13], "Shows the IRCC open data release of September 15, 2026.",
                                         sprintf("Shows the %s.", bundle$meta$source_label))
  }
  refs$citation[13] <- q2_replace_once(refs$citation[13], "2015–2026", sprintf("%d\u2013%d", as.integer(format(P$first, "%Y")), Y))

  wcsv <- function(x, f) utils::write.csv(x, file.path(out_dir, f), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  facts <- fx$facts; facts$value <- format(facts$value, digits = 17, scientific = FALSE, trim = TRUE)
  wcsv(facts, "facts.csv")
  wcsv(tx$nums[, c("phrase", "shown", "fact_id", "rule", "statement", "location")], "nums.csv")
  wcsv(refs, "refs.csv")
  geo <- fx$geo; for (cc in names(geo)[-1]) geo[[cc]] <- format(geo[[cc]], digits = 17, scientific = FALSE, trim = TRUE)
  wcsv(geo, "geo.csv"); wcsv(fx$mix, "mix.csv")
  spec <- list(
    facts_csv = file.path(out_dir, "facts.csv"), refs_csv = file.path(out_dir, "refs.csv"), nums_csv = file.path(out_dir, "nums.csv"),
    geo_csv = file.path(out_dir, "geo.csv"), mix_csv = file.path(out_dir, "mix.csv"), figures_dir = figures_dir,
    out_docx = file.path(out_dir, "note.docx"), used_ids_txt = file.path(out_dir, "used_ids.txt"),
    footer = "Mohsen Fazeli \u00b7 Work permit insights for B.C.", paras = tx$paras, fig = fig,
    appendix_a = as.list(A), appendix_b = O$appendix_b, appendix_c = appendix_c,
    table_c1_caption = sprintf("**Table C1. Permits by province and territory** (ranked by %d; growth figures for small territories rest on few permits and suppressed cells)", LF),
    table_c1_headers = c("Province / territory", FY0, YMAX, LF, paste("Share", LF), sprintf("%d\u2013%d", FY0, YMAX), sprintf("%d\u2013%d", YMAX, LF),
                         sprintf("%s vs %d", YS(Y), PY), sprintf("Share of %d\u2013%d growth", FY0, YMAX), sprintf("Share of %d %s", Y, fl$inc_word)),
    table_c2_caption = sprintf("**Table C2. Permits by stream, share of each geography's %d total**%s", LF,
                               if (fl$tfwp_mix_note) " (B.C. relies more on employer-sponsored TFWP permits than Canada, Ontario or Alberta)" else ""),
    bc_tfwp_share_id = paste0("bc_tfwp_share_", LF), ca_tfwp_share_id = paste0("ca_tfwp_share_", LF),
    appendix_d_intro = O$P_calls[[3]], appendix_e = as.list(E))
  jsonlite::write_json(spec, file.path(out_dir, "spec.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(list(spec = spec, fx = fx, tx = tx, original = O))
}
