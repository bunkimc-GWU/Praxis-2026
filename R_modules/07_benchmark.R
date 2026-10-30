# =============================================================================
# 07_BENCHMARK.R — EA2: HOQ-WEIGHTED vs EQUAL-WEIGHTED BENCHMARK
# =============================================================================
# Runs TOPSIS with equal weights (1/35) and compares against HoQ-weighted
# results using Kendall's tau and CC spread metrics.
#
# Dependencies: 03_topsis_engine.R
# Output:       BENCHMARK (comparison data.frame)
#               PAIRWISE_GAPS (per-pair CC gap comparison)
# =============================================================================

if (!exists("BASE_TOPSIS")) source("03_topsis_engine.R")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 07 — EA2: BENCHMARK COMPARISON\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# ── EQUAL-WEIGHT TOPSIS ─────────────────────────────────────────────────────

wj_equal <- rep(1 / PROJECT$n_hows, PROJECT$n_hows)
names(wj_equal) <- colnames(ALTERNATIVES_MATRIX)

EQUAL_TOPSIS <- run_topsis(ALTERNATIVES_MATRIX, wj_equal)


# ── PURE HOQ-WEIGHTED TOPSIS (no CTQ scaling) ──────────────────────────────
# EA2 compares HoQ technical priorities vs naive equal-weighting.
# We use WJ_BASE (pure HoQ, before CTQ scaling) to isolate the
# discriminative contribution of the HoQ methodology itself.
# BASE_TOPSIS uses WJ_CTQ (HoQ + CTQ), which belongs to H3, not EA2.

wj_hoq_pure <- WJ_BASE[colnames(ALTERNATIVES_MATRIX)]
HOQ_PURE_TOPSIS <- run_topsis(ALTERNATIVES_MATRIX, wj_hoq_pure)


# ── KENDALL'S TAU ────────────────────────────────────────────────────────────

kendalls_tau <- function(ranks_a, ranks_b) {
  n <- length(ranks_a)
  concordant <- 0L
  discordant <- 0L
  pairs <- combn(n, 2)
  for (i in 1:ncol(pairs)) {
    a1 <- ranks_a[pairs[1, i]]; a2 <- ranks_a[pairs[2, i]]
    b1 <- ranks_b[pairs[1, i]]; b2 <- ranks_b[pairs[2, i]]
    s <- sign(a1 - a2) * sign(b1 - b2)
    if (s > 0) concordant <- concordant + 1L
    if (s < 0) discordant <- discordant + 1L
  }
  tau <- (concordant - discordant) / ncol(pairs)
  list(tau = tau, concordant = concordant, discordant = discordant)
}

hoq_ranks   <- HOQ_PURE_TOPSIS$topsis_rank
equal_ranks <- EQUAL_TOPSIS$topsis_rank[match(HOQ_PURE_TOPSIS$manufacturer, EQUAL_TOPSIS$manufacturer)]
tau_result  <- kendalls_tau(hoq_ranks, equal_ranks)

#bunkim's additional calculation for total pairs
n <- length(hoq_ranks)
total_pairs <- n*(n - 1) / 2


# ── CC SPREAD COMPARISON ────────────────────────────────────────────────────

hoq_spread   <- HOQ_PURE_TOPSIS$cc_spread[1]
equal_spread <- EQUAL_TOPSIS$cc_spread[1]
spread_improvement <- (hoq_spread - equal_spread) / equal_spread * 100


# ── PAIRWISE CC GAP ANALYSIS ────────────────────────────────────────────────

PAIRWISE_GAPS <- data.frame()
for (i in 1:(length(MANUFACTURERS) - 1)) {
  for (j in (i + 1):length(MANUFACTURERS)) {
    m1 <- MANUFACTURERS[i]; m2 <- MANUFACTURERS[j]
    hoq_gap   <- abs(HOQ_PURE_TOPSIS$cc[HOQ_PURE_TOPSIS$manufacturer == m1] -
                     HOQ_PURE_TOPSIS$cc[HOQ_PURE_TOPSIS$manufacturer == m2])
    equal_gap <- abs(EQUAL_TOPSIS$cc[EQUAL_TOPSIS$manufacturer == m1] -
                     EQUAL_TOPSIS$cc[EQUAL_TOPSIS$manufacturer == m2])
    pct_change <- if (equal_gap > 0) (hoq_gap - equal_gap) / equal_gap * 100 else 0

    PAIRWISE_GAPS <- rbind(PAIRWISE_GAPS, data.frame(
      pair = paste0(m1, " vs ", m2),
      hoq_gap = hoq_gap, equal_gap = equal_gap,
      pct_change = pct_change,
      stringsAsFactors = FALSE
    ))
  }
}


# ── BENCHMARK SUMMARY ───────────────────────────────────────────────────────

BENCHMARK <- list(
  hoq_topsis   = HOQ_PURE_TOPSIS,
  equal_topsis = EQUAL_TOPSIS,
  kendall_tau  = tau_result$tau,
  hoq_spread   = hoq_spread,
  equal_spread = equal_spread,
  spread_improvement_pct = spread_improvement,
  pairwise_gaps = PAIRWISE_GAPS
)

cat("EA2 Benchmark: Pure HoQ weights vs Equal weights\n")
cat("  (CTQ scaling is H3 concern — isolated from EA2)\n\n")
cat(sprintf("  %-6s  %10s  %8s  %10s  %8s\n",
            "Mfr", "HoQ CC", "HoQ Rank", "Equal CC", "Eq Rank"))
cat("  ", strrep("-", 48), "\n")
for (m in MANUFACTURERS) {
  h <- HOQ_PURE_TOPSIS[HOQ_PURE_TOPSIS$manufacturer == m, ]
  e <- EQUAL_TOPSIS[EQUAL_TOPSIS$manufacturer == m, ]
  cat(sprintf("  %-6s  %10.6f  %8d  %10.6f  %8d\n",
              m, h$cc, h$topsis_rank, e$cc, e$topsis_rank))
}

# ── PRINT RESULTS ───────────────────────────────────────────────────────

cat("\n\nEA2 Benchmark Results\n")

cat(sprintf("\n Kendall's tau       :%.3f\n", tau_result$tau))

#bunkim's additional calculation for total pairs
cat(sprintf("  Concordant           :%.0f\n", tau_result$concordant))
cat(sprintf("  Discordant           :%.0f\n", tau_result$discordant))
cat(sprintf("  Total pairs          :%.0f\n", total_pairs))
#bunkim's additional calculation for total pairs



cat(sprintf("  HoQ CC Spread        :%.6f\n", hoq_spread))
cat(sprintf("  Equal CC Spread      :%.6f\n", equal_spread))
cat(sprintf("  Spread improvement   :%+.1f%%\n\n", spread_improvement))

cat("Pairwise CC gap analysis:\n")
cat(sprintf("  %-15s  %10s  %10s  %10s\n",
            "Pair", "HoQ Gap", "Equal Gap", "HoQ Impact"))
cat("  ", strrep("-", 48), "\n")
for (i in 1:nrow(PAIRWISE_GAPS)) {
  row <- PAIRWISE_GAPS[i, ]
  cat(sprintf("  %-15s  %10.6f  %10.6f  %+9.0f%%\n",
              row$pair, row$hoq_gap, row$equal_gap, row$pct_change))
}

cat(sprintf("\n  EA2 RESULT: %s\n",
            ifelse(spread_improvement > 0,
                   "SUPPORTED — HoQ increases discriminative power",
                   "NOT SUPPORTED — Equal weighting produces equal or larger spread")))



cat("\nMODULE 07 COMPLETE — BENCHMARK ready.\n\n")
