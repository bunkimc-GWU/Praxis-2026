# =============================================================================
# 06_rq2_pairs.R — RQ2 / RH2: does HoQ weighting separate alternatives more
#                  than equal weighting?
# =============================================================================
# Workbook equivalents: 'Pairwise Comparison Results', 'RQ2 Findings T2-T2'
#   (primary), 'RQ2 Findings T1-T2', 'RQ2 Findings T1-T1', 'RQ2 Findings All
#   Pairs', 'Gap Distribution', 'RQ2 Spread 40-Alt'.
# Design: all C(40,2) = 780 pairs; each pair is solved as a 3-alternative TOPSIS
# set {A, B, anchor}, anchor = Comp22's score profile, under HoQ base weights
# and equal weights (1/35).

cat("\n== 06 RQ2 pairwise separation ==\n")

ids <- rownames(SCORES)
.ord <- match(ids, sort(ids, method = "radix"))   # C-locale string order ("Comp1" < "Comp10" < "Comp2")
pairs <- do.call(rbind, lapply(seq_along(ids), function(i)
  do.call(rbind, lapply(seq_along(ids), function(j)
    if (.ord[i] < .ord[j]) data.frame(A = ids[i], B = ids[j], stringsAsFactors = FALSE)))))

tier_of <- setNames(sub("Tier ", "", companies$tier), companies$id)

pair_ccs <- function(anchor, weights) {
  t(vapply(seq_len(nrow(pairs)), function(k) {
    M <- SCORES[c(pairs$A[k], pairs$B[k], anchor), ]
    rownames(M) <- c("A", "B", "anchor")
    topsis(M, weights)$cc[1:2]
  }, numeric(2)))
}

build_pairs <- function(anchor) {
  h <- pair_ccs(anchor, HOQ$base); e <- pair_ccs(anchor, HOQ$equal)
  P <- data.frame(pairs, hoq_cc_A = h[, 1], hoq_cc_B = h[, 2],
                  eq_cc_A = e[, 1], eq_cc_B = e[, 2], stringsAsFactors = FALSE)
  P$hoq_gap <- P$hoq_cc_A - P$hoq_cc_B
  P$eq_gap  <- P$eq_cc_A - P$eq_cc_B
  P$pct_change <- ifelse(P$eq_gap == 0, NA, (P$hoq_gap - P$eq_gap) / P$eq_gap)
  P$concordance <- ifelse(P$hoq_gap == 0 | P$eq_gap == 0, "tied",
                   ifelse(sign(P$hoq_gap) == sign(P$eq_gap), "concordant", "discordant"))
  P$pct_change_mag <- ifelse(P$eq_gap == 0, NA, (abs(P$hoq_gap) - abs(P$eq_gap)) / abs(P$eq_gap))
  ta <- tier_of[P$A]; tb <- tier_of[P$B]
  P$tier_group <- paste0("T", pmin(ta, tb), "\u2013T", pmax(ta, tb))
  P$delta_abs_gap <- abs(P$hoq_gap) - abs(P$eq_gap)
  P
}

# rh2_stats(): the RQ2 Findings panels for one pair group
rh2_stats <- function(P, group) {
  sel <- if (group == "All pairs") rep(TRUE, nrow(P)) else P$tier_group == group
  g <- abs(P$hoq_gap[sel]); e <- abs(P$eq_gap[sel])
  k <- P$pct_change_mag[sel]; kk <- k[!is.na(k)]
  n <- length(kk); d <- g - e
  inc <- sum(kk > 0); dec <- sum(kk < 0)
  md <- mean(d); sdd <- sd(d); se <- sdd / sqrt(n); tq <- qt(0.975, n - 1)
  out <- list(
    group = group, n = n,
    mean_spread = c(hoq = mean(g), equal = mean(e)),
    median_spread = c(hoq = median(g), equal = median(e)),
    sd_spread = c(hoq = sd(g), equal = sd(e)),
    max_spread = c(hoq = max(g), equal = max(e)),
    mean_change = mean(g) / mean(e) - 1, median_change = median(g) / median(e) - 1,
    n_increase = inc, n_decrease = dec, n_over_10pct = sum(kk > 0.1),
    n_discordant = sum(P$concordance[sel] == "discordant"),
    median_pair_change = median(kk),
    mean_diff = md, sd_diff = sdd, se = se, t = md / se,
    p_t = 2 * pt(abs(md / se), n - 1, lower.tail = FALSE),
    p_sign = 2 * pbinom(min(inc, dec), n, 0.5),
    cohen_d = md / sdd, ci = c(md - tq * se, md + tq * se))
  out$ci_pct <- out$ci / mean(e)
  tests <- c(mean = out$mean_change, median = out$median_change,
             median_pair = out$median_pair_change,
             ci_low = out$ci_pct[1], ci_high = out$ci_pct[2])
  out$tests <- tests; out$tests_passed <- sum(tests > CFG$rh2_threshold)
  out$verdict <- if (out$ci_pct[1] > CFG$rh2_threshold) "RH2 SUPPORTED" else
                 if (out$ci_pct[2] < CFG$rh2_threshold) "RH2 REJECTED" else
                 "RH2 NOT ESTABLISHED (95% CI straddles threshold)"
  out
}

PAIRS <- build_pairs(CFG$anchor)
SENS  <- lapply(setNames(CFG$anchor_sens, CFG$anchor_sens), build_pairs)

groups <- c("T2\u2013T2", "T1\u2013T2", "T1\u2013T1", "All pairs")
RQ2 <- lapply(setNames(groups, groups), function(g) rh2_stats(PAIRS, g))
RQ2_SENS <- lapply(SENS, function(P) rh2_stats(P, "T2\u2013T2"))
for (g in groups) cat(sprintf("   %-9s n=%3d  mean %+.1f%%  median %+.1f%%  95%% CI %+.1f%% to %+.1f%%  -> %s\n",
  g, RQ2[[g]]$n, 100 * RQ2[[g]]$mean_change, 100 * RQ2[[g]]$median_change,
  100 * RQ2[[g]]$ci_pct[1], 100 * RQ2[[g]]$ci_pct[2], RQ2[[g]]$verdict))
for (a in names(RQ2_SENS)) cat(sprintf("   anchor %-6s T2-T2 mean change %+.1f%%\n", a,
                                       100 * RQ2_SENS[[a]]$mean_change))

# ---- Gap distribution (|HoQ gap|, signed gap) -----------------------------
gap_summary <- with(PAIRS, list(
  abs_mean = mean(abs(hoq_gap)), abs_median = median(abs(hoq_gap)),
  abs_sd_pop = sqrt(mean((abs(hoq_gap) - mean(abs(hoq_gap)))^2)),
  abs_max = max(abs(hoq_gap)), n_below_0.01 = sum(abs(hoq_gap) < 0.01),
  signed = c(mean = mean(hoq_gap), median = median(hoq_gap), sd = sd(hoq_gap),
             skew = excel_skew(hoq_gap), kurt = excel_kurt(hoq_gap),
             min = min(hoq_gap), max = max(hoq_gap)),
  wider = sum(abs(hoq_gap) > abs(eq_gap)), narrower = sum(abs(hoq_gap) < abs(eq_gap)),
  equal = sum(abs(hoq_gap) == abs(eq_gap)),
  histogram = table(cut(abs(hoq_gap), breaks = seq(0, ceiling(max(abs(hoq_gap)) / 0.025) * 0.025, 0.025),
                        right = FALSE, include.lowest = TRUE))))

# ---- Spread across all 40 alternatives in one TOPSIS run -------------------
spread40 <- function(w) {
  cc <- topsis(SCORES, w)$cc
  c(range = max(cc) - min(cc), sd = sd(cc), iqr = pinc(cc, 0.75) - pinc(cc, 0.25),
    avedev = avedev(cc), cv = sd(cc) / mean(cc),
    gini_md = sum(abs(outer(cc, cc, "-"))) / 2 / choose(length(cc), 2))
}
SPREAD40 <- rbind(hoq = spread40(HOQ$base), equal = spread40(HOQ$equal))

# ---- reconciliation --------------------------------------------------------
ref <- xl_raw("Pairwise Comparison Results", "A2:AU781")
check("06", "Pair list vs 'Pairwise Comparison Results'!A:B",
      paste(PAIRS$A, PAIRS$B), paste(ref[[1]], ref[[2]]))
check("06", "HoQ & equal CCs (anchor Comp22) vs C:F",
      as.matrix(PAIRS[, 3:6]), sapply(ref[3:6], as.numeric), 1e-12)
check("06", "Anchor sensitivity Comp36 vs AM:AP",
      as.matrix(SENS$Comp36[, 3:6]), sapply(ref[39:42], as.numeric), 1e-12)
check("06", "Anchor sensitivity Comp6 vs AR:AU",
      as.matrix(SENS$Comp6[, 3:6]), sapply(ref[44:47], as.numeric), 1e-12)
check("06", "Concordance labels vs J", PAIRS$concordance, as.character(ref[[10]]))
tabs <- c("T2\u2013T2" = "RQ2 Findings T2-T2", "T1\u2013T2" = "RQ2 Findings T1-T2",
          "T1\u2013T1" = "RQ2 Findings T1-T1", "All pairs" = "RQ2 Findings All Pairs")
for (g in names(tabs)) {
  s <- RQ2[[g]]; tab <- tabs[[g]]
  check("06", paste0(g, ": mean/median/SD/max spread vs B5:C8"),
        rbind(s$mean_spread, s$median_spread, s$sd_spread, s$max_spread),
        xl_num(tab, "B5:C8"), 1e-12)
  check("06", paste0(g, ": per-pair counts vs B11:B15"),
        c(s$n_increase, s$n_decrease, s$n_over_10pct, s$n_discordant, s$n),
        xl_vec_num(tab, "B11:B15"), 0)
  check("06", paste0(g, ": paired tests vs B19:B27"),
        c(s$mean_diff, s$sd_diff, s$se, s$t, s$p_t, s$p_sign, s$cohen_d, s$ci),
        xl_vec_num(tab, "B19:B27"), 1e-9)
  check("06", paste0(g, ": five RH2 operationalisations vs B31:B35"),
        s$tests, xl_vec_num(tab, "B31:B35"), 1e-9)
}

save_csv(PAIRS, "06_rq2_pairs_anchor_Comp22.csv")
save_csv(do.call(rbind, lapply(RQ2, function(s) data.frame(
  group = s$group, n = s$n, mean_hoq = s$mean_spread[1], mean_equal = s$mean_spread[2],
  mean_change = s$mean_change, median_change = s$median_change,
  median_pair_change = s$median_pair_change, n_discordant = s$n_discordant,
  t = s$t, p_t = s$p_t, p_sign = s$p_sign, cohen_d = s$cohen_d,
  ci_low_pct = s$ci_pct[1], ci_high_pct = s$ci_pct[2],
  tests_passed = s$tests_passed, verdict = s$verdict))), "06_rq2_summary.csv")