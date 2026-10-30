# =============================================================================
# 04_CTQ_SCENARIOS.R — FULL CTQ PARAMETERIZATION
# =============================================================================
# For each CTQ scenario, rebuilds the ENTIRE pipeline:
#   WHAT weights → HoQ → wj → TOPSIS → CC → Rank
# This is the "full recalculation" that makes the CTQ scenario chart
# lines actually move (unlike the base-wj approximation).
#
# Dependencies: 03_topsis_engine.R
# Output:       CTQ_RESULTS (list of TOPSIS results per scenario)
#               CTQ_SUMMARY (data.frame for charts)
# =============================================================================

if (!exists("BASE_TOPSIS")) source("03_topsis_engine.R")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 04 — FULL CTQ PARAMETERIZATION\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# ── RUN FULL PIPELINE PER SCENARIO ──────────────────────────────────────────

CTQ_RESULTS <- list()
CTQ_SUMMARY <- data.frame()

for (s in 1:nrow(CTQ_SCENARIOS)) {
  scen <- CTQ_SCENARIOS[s, ]

  # Post-HoQ CTQ scaling: apply CTQ ratio to base wj weights
  # This preserves HoQ technical priorities while enabling CTQ-driven differentiation
  wj_s <- apply_ctq_scaling(WJ, scen$Q, scen$T, scen$C)
  names(wj_s) <- names(WJ)
  wj_s <- wj_s[colnames(ALTERNATIVES_MATRIX)]

  # TOPSIS with CTQ-scaled wj
  topsis_s <- run_topsis(ALTERNATIVES_MATRIX, wj_s)
  topsis_s$scenario <- scen$scenario_name
  topsis_s$scenario_id <- scen$scenario_id
  topsis_s$Q <- scen$Q
  topsis_s$T <- scen$T
  topsis_s$C <- scen$C

  CTQ_RESULTS[[s]] <- list(
    scenario   = scen,
    wj_scaled  = wj_s,
    topsis     = topsis_s
  )

  CTQ_SUMMARY <- rbind(CTQ_SUMMARY, topsis_s)

  # Print scenario results
  marker <- if (s == 4) " ★ BASE CASE" else ""
  cat(sprintf("Scenario %d: %s%s\n", s, scen$scenario_name, marker))
  cat(sprintf("  CTQ: Q=%.0f%% T=%.0f%% C=%.0f%%\n",
              scen$Q*100, scen$T*100, scen$C*100))
  for (i in order(topsis_s$topsis_rank)) {
    cat(sprintf("  %-6s  CC=%.6f  Rank %d\n",
                topsis_s$manufacturer[i],
                topsis_s$cc[i],
                topsis_s$topsis_rank[i]))
  }
  cat("\n")
}

cat("MODULE 04 COMPLETE — CTQ_SUMMARY ready (", nrow(CTQ_SUMMARY), " rows).\n\n")
