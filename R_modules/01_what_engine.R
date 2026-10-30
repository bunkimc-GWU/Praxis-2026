# =============================================================================
# 01_WHAT_ENGINE.R — WHAT IMPORTANCE WEIGHTS BC04112026
# =============================================================================
# WHAT weights are the raw importance values from FDA 483 frequency data.
# This module calculates the relative weights of each WHAT category based
# on the total WHATs.
# =============================================================================


cat("\n═════════════════════════════════════════════════════════════════════\n")
cat("MODULE 01 — WHAT WEIGHT ENGINE EXECUTION *aligned*\n")
cat("═════════════════════════════════════════════════════════════════════\n\n")

# ── 1. COMPUTE FDA-DERIVED IMPORTANCE (W01–W17) ─────────────────────────────

fda_total <- sum(FDA_CATEGORIES$count)
stopifnot(fda_total == PROJECT$fda_total)

fda_importance <- FDA_CATEGORIES$count / fda_total
names(fda_importance) <- FDA_CATEGORIES$what_id

cat("FDA category counts (17 categories, ", fda_total, " total records):\n")
for (i in 1:nrow(FDA_CATEGORIES)) {
  cat(sprintf("  %s  %-45s  %5d  (%.4f)\n",
              FDA_CATEGORIES$what_id[i],
              FDA_CATEGORIES$category[i],
              FDA_CATEGORIES$count[i],
              fda_importance[i]))
}
cat(sprintf("\n  Importance sum: %.8f (should be 1.0)\n\n", sum(fda_importance)))


# ── 2. WHAT WEIGHTS = RAW IMPORTANCE (NO CTQ MULTIPLICATION) ─────────────────
# Quality WHATs (W01-W17): FDA violation frequency (sums to 1.0)
# Time WHATs (W18-W21): equal within-family importance (0.25 each, sums to 1.0)
# Cost WHATs (W22-W25): equal within-family importance (0.25 each, sums to 1.0)
#
# All 25 WHATs sum to 3.0.
# The HoQ Step 2 normalization (Pj_raw / sum(Pj_raw))
# eliminates the absolute scale. Only relative weights within and
# across families matter.

WHAT_WEIGHTS <- WHAT_DEFS$importance
names(WHAT_WEIGHTS) <- WHAT_DEFS$what_id

cat("WHAT importance weights :\n")
for (i in 1:length(WHAT_WEIGHTS)) {
  cat(sprintf("  %s  %-8s  %.10f\n",
              names(WHAT_WEIGHTS)[i],
              WHAT_DEFS$ctq_group[i],
              WHAT_WEIGHTS[i]))
}
cat(sprintf("\n  Weight sum: %.10f (3 families × 1.0 each)\n", sum(WHAT_WEIGHTS)))
cat("  (Normalization handled by HoQ Step 2)\n\n")


cat("MODULE 01 COMPLETE — WHAT_WEIGHTS ready (25 importance values)\n\n")
