# =============================================================================
# 02_HOQ_ENGINE.R — HOUSE OF QUALITY WEIGHT COMPUTATION BC04112026
# =============================================================================
# Implements the complete HoQ basement calculation:
#   Step 1: Pj_raw  = Σ(Wi × Rij)        — weighted relationship sum
#   Step 2: Pj_norm = Pj_raw / Σ(Pj_raw) — normalize to unit sum
#   Step 3: Pj_diff = Pj_norm × Dj        — difficulty adjustment
#   Step 4: Pj_roof = Pj_diff %*% Roof    — roof correlation effect
#   Step 5: Pj_hoq  = Pj_diff + Pj_roof   — combined priority
#   Step 6: wj      = Pj_hoq / Σ(Pj_hoq)  — final normalized weights
#
# Dependencies: 01_what_engine.R
# Output:       WJ (named vector of 35 criterion weights)
#               HOQ_RESULTS (full calculation chain for reporting)
#               compute_hoq_wj() — reusable function for CTQ scenarios
# =============================================================================

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 02 — HOUSE OF QUALITY HOW PRIORITY ENGINE EXECUTION *aligned*\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# ── CORE HoQ FUNCTION ────────────────────────────────────────────────────────
# Reusable: called once for base case, then again for each CTQ scenario.

compute_hoq_wj <- function(what_wts,
                            rel_mat   = REL_MATRIX,
                            diff_vec  = HOW_DEFS$difficulty,
                            roof_mat  = ROOF_MATRIX,
                            how_ids   = HOW_DEFS$how_id) {

  n_how <- length(how_ids)

  # Step 1: Technical Priority (raw)
  pj_raw <- as.vector(what_wts %*% rel_mat)
  names(pj_raw) <- how_ids

  # Step 2: Normalized Priority
  pj_norm <- pj_raw / sum(pj_raw)

  # Step 3: Difficulty Adjustment
  names(diff_vec) <- how_ids
  pj_diff <- pj_norm * diff_vec

  # Step 4: Roof Correlation Effect
  pj_roof <- as.vector(pj_diff %*% roof_mat)
  names(pj_roof) <- how_ids

  # Step 5: HoQ Adjusted Priority
  pj_hoq <- pj_diff + pj_roof

  # Step 6: Final normalized wj
  wj <- pj_hoq / sum(pj_hoq)
  names(wj) <- how_ids

  list(
    pj_raw  = pj_raw,
    pj_norm = pj_norm,
    pj_diff = pj_diff,
    pj_roof = pj_roof,
    pj_hoq  = pj_hoq,
    wj      = wj,
    wj_rank = rank(-wj, ties.method = "min")
  )
}


# ── RUN BASE CASE ────────────────────────────────────────────────────────────

HOQ_RESULTS <- compute_hoq_wj(WHAT_WEIGHTS)
WJ <- HOQ_RESULTS$wj

cat("HoQ calculation chain (base case):\n")
cat(sprintf("  Step 1  Pj_raw sum  : %.6f\n", sum(HOQ_RESULTS$pj_raw)))
cat(sprintf("  Step 2  Pj_norm sum : %.8f\n", sum(HOQ_RESULTS$pj_norm)))
cat(sprintf("  Step 3  Pj_diff sum : %.6f\n", sum(HOQ_RESULTS$pj_diff)))
cat(sprintf("  Step 5  Pj_hoq sum  : %.6f\n", sum(HOQ_RESULTS$pj_hoq)))
cat(sprintf("  Step 6  wj sum      : %.8f\n\n", sum(WJ)))

cat("Top 10 HOW criteria by wj:\n")
cat(sprintf("  %-5s  %-50s  %8s  %6s\n", "ID", "Description", "wj", "Rank"))
cat("  ", strrep("-", 75), "\n")
top_idx <- order(-WJ)[1:10]
for (i in top_idx) {
  cat(sprintf("  %-5s  %-50s  %.6f  %4d\n",
              HOW_DEFS$how_id[i],
              substr(HOW_DEFS$description[i], 1, 50),
              WJ[i],
              HOQ_RESULTS$wj_rank[i]))
}


cat("\nMODULE 02 COMPLETE — WJ ready (35 criterion weights)\n")
cat("  Export: compute_hoq_wj(what_wts) available for CTQ scenarios.\n\n")
