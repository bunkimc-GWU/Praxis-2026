# =============================================================================
# 05_SENSITIVITY.R — PER-CRITERION WEIGHT PERTURBATION
# =============================================================================
# For each HOW criterion, perturbs its wj by ±10%, ±25%, ±50% and
# reruns TOPSIS, recording:
#   - CC for each manufacturer at each perturbation level
#   - Whether the ranking changes (rank stability)
#   - CC range (for tornado chart)
#
# Dependencies: 03_topsis_engine.R (via 04_ctq_scenarios.R)
# Output:       SENSITIVITY_RESULTS (full perturbation data)
#               TORNADO_DATA (formatted for tornado chart)
# =============================================================================

if (!exists("WJ_CTQ")) source("03_topsis_engine.R")

library(dplyr)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 05 — SENSITIVITY ANALYSIS (WEIGHT PERTURBATION)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

levels <- SIM_PARAMS$sensitivity_levels
base_wj <- WJ_CTQ[colnames(ALTERNATIVES_MATRIX)]

SENSITIVITY_RESULTS <- data.frame()

for (h_idx in 1:length(base_wj)) {
  hid <- names(base_wj)[h_idx]

  for (lvl in levels) {
    # Perturb one criterion's weight
    wj_perturbed <- base_wj
    wj_perturbed[h_idx] <- base_wj[h_idx] * (1 + lvl)

    # Renormalize so weights still sum to 1
    wj_perturbed <- wj_perturbed / sum(wj_perturbed)

    # Run TOPSIS
    res <- run_topsis(ALTERNATIVES_MATRIX, wj_perturbed)
    res$how_id <- hid
    res$perturbation <- lvl
    res$base_rank_1 <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank == 1]

    SENSITIVITY_RESULTS <- rbind(SENSITIVITY_RESULTS, res)
  }
}

# ── BUILD TORNADO DATA ──────────────────────────────────────────────────────
# For each HOW criterion: what is the CC range of the #1 manufacturer
# across all perturbation levels? Wider range = more sensitive.

base_winner <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank == 1]
base_cc     <- BASE_TOPSIS$cc[BASE_TOPSIS$manufacturer == base_winner]

TORNADO_DATA <- SENSITIVITY_RESULTS %>%
  filter(manufacturer == base_winner) %>%
  group_by(how_id) %>%
  summarise(
    cc_min     = min(cc),
    cc_max     = max(cc),
    cc_range   = max(cc) - min(cc),
    rank_stable = all(topsis_rank == 1),
    .groups = "drop"
  ) %>%
  left_join(HOW_DEFS, by = "how_id") %>%
  arrange(desc(cc_range))

# For unstable criteria: identify WHAT flipped to Rank 1 and at which perturbation
flip_details <- SENSITIVITY_RESULTS %>%
  filter(topsis_rank == 1 & manufacturer != base_winner) %>%
  group_by(how_id) %>%
  summarise(
    flipped_to = paste(unique(manufacturer), collapse = "/"),
    flip_at_perturbation = paste(unique(paste0(ifelse(perturbation > 0, "+", ""),
                                               perturbation * 100, "%")), collapse = ", "),
    .groups = "drop"
  )

TORNADO_DATA <- TORNADO_DATA %>%
  left_join(flip_details, by = "how_id") %>%
  mutate(
    flipped_to = ifelse(is.na(flipped_to), "—", flipped_to),
    flip_at_perturbation = ifelse(is.na(flip_at_perturbation), "—", flip_at_perturbation)
  )

# Print top 10 most sensitive criteria
cat(sprintf("Sensitivity analysis complete: %d criteria × %d levels = %d TOPSIS runs\n\n",
            length(base_wj), length(levels), nrow(SENSITIVITY_RESULTS)))

cat(sprintf("Top 10 most sensitive criteria (for %s CC):\n", base_winner))
cat(sprintf("  %-5s  %-45s  %8s  %8s  %8s  %s\n",
            "ID", "Description", "CC Min", "CC Max", "Range", "Rank Stable"))
cat("  ", strrep("-", 85), "\n")
for (i in 1:min(10, nrow(TORNADO_DATA))) {
  row <- TORNADO_DATA[i, ]
  cat(sprintf("  %-5s  %-45s  %.6f  %.6f  %.6f  %s\n",
              row$how_id,
              substr(row$description, 1, 45),
              row$cc_min, row$cc_max, row$cc_range,
              ifelse(row$rank_stable, "YES", "NO ***")))
}

# Check: does any perturbation change the #1 ranking?
rank_changes <- SENSITIVITY_RESULTS %>%
  filter(topsis_rank == 1 & manufacturer != base_winner)

cat(sprintf("\n  Rank-1 changes under perturbation: %d / %d runs\n",
            nrow(rank_changes), nrow(SENSITIVITY_RESULTS) / 3))
if (nrow(rank_changes) == 0) {
  cat(sprintf("  %s holds Rank 1 across ALL sensitivity scenarios.\n", base_winner))
}

cat("\nMODULE 05 COMPLETE — TORNADO_DATA ready (", nrow(TORNADO_DATA), " criteria).\n\n")
