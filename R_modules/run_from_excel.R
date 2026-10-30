# =============================================================================
# MCDM PIPELINE USING EXCEL TEMPLATE INPUTS
# =============================================================================
# Reads ALL inputs from: ../data/MCDM_Input_Template.xlsx
# Outputs:              ../outputs/MCDM_Results.xlsx  (15 sheets)
#                       ../outputs/charts/                (13 charts)
# =============================================================================

cat("\n")
cat(strrep("=", 72), "\n")
cat("BCHOKSHI GWID G39538196 GWU D. ENGR HYBRID MCDM TOOL\n")
cat("Advisor: Dr. Brandon D. Shaw, DE\n")
cat(strrep("=", 72), "\n\n")

t_start <- Sys.time()


# ── STEP 1: LOAD EXCEL INPUTS ─────────────────────────────────────────────────
# This sources 00_config_from_excel.R which:
#   - Imports all matrices, scores, parameters from the Excel template

source("00_config_from_excel.R")

cat("Manufacturer names loaded from Excel:\n")
for (i in seq_along(MANUFACTURERS)) {
  cat(sprintf("  [%d] %s\n", i, MANUFACTURERS[i]))
}
cat("\n")


# ── STEP 2: RUN THE FULL COMPUTATION CHAIN ────────────────────────────────────
# Modules are written with `if (!exists("X")) source("Y")` guards. 

cat(strrep("-", 72), "\n")
cat("  Running computation pipeline...\n")
cat(strrep("-", 72), "\n\n")

source("01_what_engine.R")      # FDA-derived WHAT weights
source("02_hoq_engine.R")       # HOQ  -> raw criterion weights (WJ)
source("03_topsis_engine.R")    # TOPSIS base case  (BASE_TOPSIS, WJ_CTQ)
source("04_ctq_scenarios.R")    # 5 CTQ scenario sweeps  (CTQ_SUMMARY)
source("05_sensitivity.R")      # Per-criterion weight perturbation
source("06_bootstrap.R")        # EA1: bootstrap rank stability
source("07_benchmark.R")        # EA2: HOQ vs equal-weight benchmark
source("08_monte_carlo.R")      # EA3: Dirichlet Monte Carlo simulation
source("09_report_tables.R")    # Assemble structured report tables
source("10_excel_export.R")     # Write MCDM_Results.xlsx (15 sheets)
source("11_visualizations.R")   # Generate charts


# ── STEP 3: ELAPSED TIME ─────────────────────────────────────────────────────

elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))


# ── STEP 4: RESULTS SUMMARY ──────────────────────────────────────────────────

cat("\n")
cat(strrep("=", 72), "\n")
cat("  PIPELINE COMPLETE\n")
cat(strrep("=", 72), "\n\n")
cat(sprintf("  Total time: %.1f minutes\n", elapsed))
cat("  Outputs:\n")
cat("    ../outputs/MCDM_Results.xlsx  (15 sheets)\n")
cat("    ../outputs/charts/                (13 charts)\n\n")

# Recommended manufacturer (Rank 1 by TOPSIS closeness coefficient)
winner    <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank == 1]
winner_cc <- BASE_TOPSIS$cc[BASE_TOPSIS$topsis_rank == 1]
cat(sprintf("  Recommended: %s (CC = %.4f)\n", winner, winner_cc))

# EA1: bootstrap rank stability
ea1_top_id  <- BOOTSTRAP_RESULTS$how_id[which.min(BOOTSTRAP_RESULTS$mean_rank)]
ea1_pct_r1  <- max(BOOTSTRAP_RESULTS$pct_rank1)
cat(sprintf("  EA1: %s holds Rank 1 in %.1f%% of bootstrap resamples\n",
            ea1_top_id, ea1_pct_r1))

# EA2: HoQ vs equal-weight spread improvement
cat(sprintf("  EA2: CC spread improvement = %+.1f%%\n",
            BENCHMARK$spread_improvement_pct))

# EA3: Monte Carlo robustness
mc_rf        <- MC_SUMMARY$rank_freq
mc_winner    <- mc_rf$manufacturer[which.max(mc_rf$pct_rank1)]
mc_pct       <- max(mc_rf$pct_rank1)
mc_n         <- format(SIM_PARAMS$monte_carlo_n, big.mark = ",")
cat(sprintf("  EA3: %s holds Rank 1 in %.1f%% of %s MC scenarios\n\n",
            mc_winner, mc_pct, mc_n))

cat("  Excel input file used:\n")
cat(sprintf("    %s\n\n",
            normalizePath(EXCEL_INPUT_PATH, mustWork = FALSE)))

