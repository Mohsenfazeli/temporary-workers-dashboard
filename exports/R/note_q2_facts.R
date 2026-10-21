# =============================================================================
# exports/R/note_q2_facts.R: the numbers behind the Question 2 note, for any
# IRCC release. Same definitions as the note's own analysis (q2 analysis_q2.R),
# but every period is relative to the data instead of written as a year:
#   Y   = year of the latest month, M = its month number, PY = Y - 1
#   FY0 = first full calendar year (2015), LF = last full year before Y (2025)
#   YMAX = full year with the most permits (2023 in the Sep 15, 2026 release)
#   PH  = earlier year with the most permits in the same months (Jan-M; 2024)
# Fact ids keep the note's naming with the actual years (ca_2015, ca_ytd26...),
# so on the September 15, 2026 release they are the same ids, values and
# displays as the note's facts_q2.csv (checked by tests/testthat/test-exports.R).
# Needs R/calc.R and R/data.R (sourced by the caller).
# =============================================================================

Q2_PGWP <- "Post-graduation work permits"
Q2_LMIA <- "Other TFWP workers with an LMIA"
Q2_AGRI <- c("Seasonal Agricultural Worker Program", "Other agricultural workers")
Q2_UNK  <- "Unknown or other IMP"
Q2_GEO_CODE <- c("Newfoundland and Labrador" = "nl", "Prince Edward Island" = "pe", "Nova Scotia" = "nsc",
                 "New Brunswick" = "nb", "Quebec" = "qc", "Ontario" = "on", "Manitoba" = "mb",
                 "Saskatchewan" = "sk", "Alberta" = "ab", "British Columbia" = "bc", "Yukon" = "yt",
                 "Northwest Territories" = "nt", "Nunavut" = "nu")

#' Periods of a release (all relative to its latest month).
q2_periods <- function(bundle) {
  d <- bundle$data
  latest <- as.Date(bundle$meta$latest_month)
  first  <- as.Date(bundle$meta$first_month)
  Y <- as.integer(format(latest, "%Y")); M <- as.integer(format(latest, "%m"))
  FY0 <- as.integer(format(first, "%Y")) + if (as.integer(format(first, "%m")) == 1) 0L else 1L
  LF <- Y - 1L
  if (LF <= FY0) stop("The data need at least two full calendar years before the latest year.")
  ann <- vapply(FY0:LF, function(y) sum(d$value[d$geography == "Canada" & d$year == y]), numeric(1))
  YMAX <- (FY0:LF)[which.max(ann)]
  ytd_prior <- vapply(FY0:(Y - 1L), function(y)
    sum(d$value[d$geography == "Canada" & d$year == y & as.integer(format(d$month, "%m")) <= M]), numeric(1))
  PH <- (FY0:(Y - 1L))[which.max(ytd_prior)]
  yy <- function(y) sprintf("%02d", y %% 100)
  list(latest = latest, first = first, Y = Y, M = M, PY = Y - 1L, FY0 = FY0, LF = LF, YMAX = YMAX, PH = PH,
       yy = yy, ytd_prior = setNames(ytd_prior, FY0:(Y - 1L)),
       mon = month.abb[M], month = month.name[M])
}

# Period labels: "Jan–Jul 2026" / "January–July 2026" (a full year: "2026").
q2_ytd_short <- function(P, y) if (P$M == 12) as.character(y) else if (P$M == 1) paste("Jan", y) else
  sprintf("Jan\u2013%s %d", P$mon, y)
q2_ytd_long <- function(P, y = NULL) {
  s <- if (P$M == 12) "the full year" else if (P$M == 1) "January" else paste0("January\u2013", P$month)
  if (is.null(y)) s else if (P$M == 12) as.character(y) else paste(s, y)
}

#' All facts used by the note. Returns list(facts = data frame, val = function(id),
#' P = periods, geo = Table C1 rows, mix = Table C2 rows, peak = 12-month peak info).
q2_facts <- function(bundle) {
  d <- bundle$data
  P <- q2_periods(bundle)
  Y <- P$Y; PY <- P$PY; FY0 <- P$FY0; LF <- P$LF; YMAX <- P$YMAX; M <- P$M; yy <- P$yy
  facts <- list()
  add_fact <- function(id, value, display, type, period, geography) {
    facts[[id]] <<- data.frame(id = id, value = value, display = display, type = type,
                               period = period, geography = geography, stringsAsFactors = FALSE)
    invisible(value)
  }
  geo_v <- as.character(d$geography); cat_v <- as.character(d$category)
  cnt <- function(id, geo = "Canada", progs = c("TFWP", "IMP"), cats = NULL, start, end, period) {
    start <- as.Date(start); end <- as.Date(end)
    keep <- geo_v == geo & d$program %in% progs & d$month >= start & d$month <= end
    if (!is.null(cats)) keep <- keep & cat_v %in% cats
    v <- sum(d$value[keep])
    add_fact(id, v, fmt_n(v), "count", period, geo)
  }
  cnt_y <- function(id, y, ...) cnt(id, start = sprintf("%d-01-01", y), end = sprintf("%d-12-01", y), period = as.character(y), ...)
  cnt_t <- function(id, y, ...) cnt(id, start = sprintf("%d-01-01", y), end = sprintf("%d-%02d-01", y, M),
                                    period = q2_ytd_short(P, y), ...)
  val <- function(id) { if (is.null(facts[[id]])) stop("Unknown fact: ", id); facts[[id]]$value }
  pct <- function(id, new_id, old_id) {
    v <- pct_change(val(new_id), val(old_id))
    add_fact(id, v, sprintf("%.1f%%", abs(100 * v)), "pct",
             paste(facts[[old_id]]$period, "to", facts[[new_id]]$period), facts[[new_id]]$geography)
  }
  shr <- function(id, part_id, total_id) {
    v <- val(part_id) / val(total_id)
    add_fact(id, v, sprintf("%.1f%%", 100 * v), "share", facts[[part_id]]$period, facts[[part_id]]$geography)
  }
  dif <- function(id, a_id, b_id) {
    v <- val(a_id) - val(b_id)
    add_fact(id, v, fmt_n(abs(v)), "difference", paste(facts[[b_id]]$period, "to", facts[[a_id]]$period),
             facts[[a_id]]$geography)
  }
  ytd <- function(pre) paste0(pre, "_ytd", yy(c(PY, Y)))   # ids of the two YTD counts (previous, current)

  # Canada, annual and same months
  for (y in FY0:LF) cnt_y(paste0("ca_", y), y)
  pct(sprintf("ca_growth_%d_%d", FY0, YMAX), paste0("ca_", YMAX), paste0("ca_", FY0))
  for (y in unique(c(PY, Y, P$PH))) cnt_t(paste0("ca_ytd", yy(y)), y)
  pct("ca_ytd_change", ytd("ca")[2], ytd("ca")[1])
  dif("ca_ytd_increase", ytd("ca")[2], ytd("ca")[1])

  # 12-month totals: peak and latest
  m <- stats::aggregate(value ~ month, data = d[geo_v == "Canada", ], FUN = sum)
  m <- m[order(m$month), ]
  m$roll12 <- as.numeric(stats::filter(m$value, rep(1, 12), sides = 1))
  pk <- m$month[which.max(m$roll12)]
  pk_start <- seq(pk, by = "-11 months", length.out = 2)[2]
  l12_start <- seq(P$latest, by = "-11 months", length.out = 2)[2]
  cnt("ca_r12_peak", start = pk_start, end = pk, period = paste("12 months to", fmt_month_short(pk)))
  cnt("ca_last12", start = l12_start, end = P$latest, period = paste("12 months to", fmt_month_short(P$latest)))
  shr("ca_last12_share_of_peak", "ca_last12", "ca_r12_peak")

  # Programs and streams, Canada
  cnt_y(paste0("tfwp_", LF), LF, progs = "TFWP")
  for (y in c(PY, Y)) cnt_t(paste0("tfwp_ytd", yy(y)), y, progs = "TFWP")
  pct("tfwp_ytd_change", ytd("tfwp")[2], ytd("tfwp")[1])
  for (y in c(PY, Y)) cnt_t(paste0("pgwp_ytd", yy(y)), y, progs = "IMP", cats = Q2_PGWP)
  pct("pgwp_ytd_change", ytd("pgwp")[2], ytd("pgwp")[1])
  dif("pgwp_ytd_increase", ytd("pgwp")[2], ytd("pgwp")[1])
  shr("pgwp_share_of_increase", "pgwp_ytd_increase", "ca_ytd_increase")
  for (y in c(2021, 2024, 2025)) cnt_y(paste0("lmia_", y), y, progs = "TFWP", cats = Q2_LMIA)
  for (y in c(PY, Y)) cnt_t(paste0("lmia_ytd", yy(y)), y, progs = "TFWP", cats = Q2_LMIA)
  pct("lmia_growth_2021_2024", "lmia_2024", "lmia_2021")
  pct("lmia_fall_2024_2025", "lmia_2025", "lmia_2024")
  pct("lmia_ytd_change", ytd("lmia")[2], ytd("lmia")[1])
  cnt_y(paste0("unk_", YMAX), YMAX, progs = "IMP", cats = Q2_UNK)
  shr(paste0("unk_share_", YMAX), paste0("unk_", YMAX), paste0("ca_", YMAX))
  for (y in c(PY, Y)) cnt_t(paste0("unk_ytd", yy(y)), y, progs = "IMP", cats = Q2_UNK)
  dif("unk_ytd_increase", ytd("unk")[2], ytd("unk")[1])
  shr(paste0("unk_share_ytd", yy(Y)), ytd("unk")[2], ytd("ca")[2])

  # Provinces and territories
  for (g in names(Q2_GEO_CODE)) {
    k <- Q2_GEO_CODE[[g]]
    for (y in unique(c(FY0, YMAX, LF))) cnt_y(sprintf("%s_%d", k, y), y, geo = g)
    for (y in c(PY, Y)) cnt_t(paste0(k, "_ytd", yy(y)), y, geo = g)
    pct(paste0(k, "_ytd_change"), ytd(k)[2], ytd(k)[1])
    shr(sprintf("%s_share_%d", k, LF), sprintf("%s_%d", k, LF), paste0("ca_", LF))
    dif(paste0(k, "_ytd_increase"), ytd(k)[2], ytd(k)[1])
    shr(paste0(k, "_contrib_ytd"), paste0(k, "_ytd_increase"), "ca_ytd_increase")
  }
  shr(paste0("bc_share_ytd", yy(PY)), ytd("bc")[1], ytd("ca")[1])
  for (k in c("bc", "on", "ab")) {
    g <- names(Q2_GEO_CODE)[Q2_GEO_CODE == k]
    for (y in c(PY, Y)) cnt_t(paste0(k, "_tfwp_ytd", yy(y)), y, geo = g, progs = "TFWP")
    pct(paste0(k, "_tfwp_ytd_change"), ytd(paste0(k, "_tfwp"))[2], ytd(paste0(k, "_tfwp"))[1])
  }
  for (y in c(PY, Y)) cnt_t(paste0("ab_unk_ytd", yy(y)), y, geo = "Alberta", progs = "IMP", cats = Q2_UNK)
  dif("ab_unk_ytd_increase", ytd("ab_unk")[2], ytd("ab_unk")[1])
  shr("ab_unk_share_of_increase", "ab_unk_ytd_increase", "ab_ytd_increase")
  cnt_y(paste0("bc_tfwp_", LF), LF, geo = "British Columbia", progs = "TFWP")
  shr(paste0("bc_tfwp_share_", LF), paste0("bc_tfwp_", LF), paste0("bc_", LF))
  shr(paste0("ca_tfwp_share_", LF), paste0("tfwp_", LF), paste0("ca_", LF))
  for (y in c(PY, Y)) cnt_t(paste0("bc_pgwp_ytd", yy(y)), y, geo = "British Columbia", progs = "IMP", cats = Q2_PGWP)
  pct("bc_pgwp_ytd_change", ytd("bc_pgwp")[2], ytd("bc_pgwp")[1])
  dif("bc_pgwp_ytd_increase", ytd("bc_pgwp")[2], ytd("bc_pgwp")[1])
  dif("bc_other_ytd_change", "bc_ytd_increase", "bc_pgwp_ytd_increase")
  dif("ca_other_ytd_change", "ca_ytd_increase", "pgwp_ytd_increase")
  # Table 1 (Oct 21, 2026): permits other than PGWPs in each period, and their change
  for (k in c("bc", "ca")) {
    pg <- if (k == "bc") "bc_pgwp" else "pgwp"
    for (j in 1:2) dif(ytd(paste0(k, "_other"))[j], ytd(k)[j], ytd(pg)[j])
    pct(paste0(k, "_other_ytd_pct"), ytd(paste0(k, "_other"))[2], ytd(paste0(k, "_other"))[1])
  }
  for (y in c(PY, Y)) cnt_t(paste0("bc_lmia_ytd", yy(y)), y, geo = "British Columbia", progs = "TFWP", cats = Q2_LMIA)
  pct("bc_lmia_ytd_change", ytd("bc_lmia")[2], ytd("bc_lmia")[1])
  for (y in c(PY, Y)) cnt_t(paste0("bc_agri_ytd", yy(y)), y, geo = "British Columbia", progs = "TFWP", cats = Q2_AGRI)
  pct("bc_agri_ytd_change", ytd("bc_agri")[2], ytd("bc_agri")[1])
  # Robustness: Canada without "province/territory not stated"
  for (y in c(PY, Y)) cnt_t(paste0("ns_ytd", yy(y)), y, geo = NOT_STATED_GEO)
  dif(paste0("pt_ytd", yy(PY)), ytd("ca")[1], ytd("ns")[1])
  dif(paste0("pt_ytd", yy(Y)), ytd("ca")[2], ytd("ns")[2])
  dif("pt_ytd_increase", ytd("pt")[2], ytd("pt")[1])
  shr("bc_contrib_ytd_pt", "bc_ytd_increase", "pt_ytd_increase")
  shr(sprintf("bc_share_ytd%s_pt", yy(PY)), ytd("bc")[1], ytd("pt")[1])

  # Table C1: provinces and territories (FY0, YMAX, LF, same months)
  pts <- names(Q2_GEO_CODE)
  ca_diff_up <- val(paste0("ca_", YMAX)) - val(paste0("ca_", FY0))
  geo <- do.call(rbind, lapply(pts, function(g) {
    k <- Q2_GEO_CODE[[g]]
    a <- val(sprintf("%s_%d", k, FY0)); b <- val(sprintf("%s_%d", k, YMAX)); c <- val(sprintf("%s_%d", k, LF))
    yp <- val(ytd(k)[1]); yc <- val(ytd(k)[2])
    data.frame(geography = g, p_a = a, p_b = b, p_c = c, ytd_prev = yp, ytd_cur = yc,
               share_c = c / val(paste0("ca_", LF)), growth_a_b = pct_change(b, a), change_b_c = pct_change(c, b),
               ytd_change = pct_change(yc, yp), contrib_up = (b - a) / ca_diff_up,
               contrib_ytd = (yc - yp) / val("ca_ytd_increase"), stringsAsFactors = FALSE)
  }))
  geo <- geo[order(-geo$p_c), ]
  ca <- function(y) val(paste0("ca_", y))
  geo <- rbind(geo, data.frame(geography = "**Canada (incl. not stated)**", p_a = ca(FY0), p_b = ca(YMAX), p_c = ca(LF),
                               ytd_prev = val(ytd("ca")[1]), ytd_cur = val(ytd("ca")[2]), share_c = 1,
                               growth_a_b = ca(YMAX) / ca(FY0) - 1, change_b_c = ca(LF) / ca(YMAX) - 1,
                               ytd_change = val(ytd("ca")[2]) / val(ytd("ca")[1]) - 1, contrib_up = 1, contrib_ytd = 1))
  # Table C2: stream mix in the last full year
  x <- d[geo_v %in% c("Canada", "British Columbia", "Ontario", "Alberta") & d$year == LF, ]
  mix <- stats::aggregate(value ~ geography + program + category,
                          data = data.frame(geography = as.character(x$geography), program = x$program,
                                            category = as.character(x$category), value = x$value), FUN = sum)
  mix$share <- mix$value / stats::ave(mix$value, mix$geography, FUN = sum)
  list(facts = do.call(rbind, unname(facts)), val = val, P = P, geo = geo, mix = mix,
       peak = list(end = pk, start = pk_start, l12_start = l12_start),
       ann = setNames(vapply(FY0:LF, ca, numeric(1)), FY0:LF))
}
