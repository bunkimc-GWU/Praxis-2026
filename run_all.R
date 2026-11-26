# =============================================================================
# run_all.R — runs the full HoQ–TOPSIS pipeline and reconciles every stage
#             with the workbook
# =============================================================================
# Usage (from this folder, with the workbook copied next to it):
#   Rscript run_all.R
# or, to point at a workbook elsewhere:
#   HOQ_WORKBOOK="C:/path/HOQ and TOPSIS 17.xlsm" Rscript run_all.R
#
# Set SKIP_BOOTSTRAP=1 to skip the 10,000-replicate RQ1 bootstrap during
# development (it is the only slow step).

t_start <- Sys.time()
source("R/00_utils.R")
source("R/01_data_prep.R")      # FDA records  -> WHAT counts
source("R/02_hoq.R")            # WHAT counts  -> HOW weights (HoQ chain)
source("R/03_topsis.R")         # TOPSIS engine + active-triplet check
if (Sys.getenv("SKIP_BOOTSTRAP") != "1") {
  source("R/04_rq1_bootstrap.R")  # RQ1 / RH1
} else cat("\n== 04 RQ1 bootstrap skipped (SKIP_BOOTSTRAP=1) ==\n")
source("R/05_scores.R")         # synthetic supplier scores + fit check
source("R/06_rq2_pairs.R")      # RQ2 / RH2
source("R/07_rq3_ctq.R")        # RQ3 / RH3
source("R/08_cv.R")             # cross-validation

# ---- one-page results summary ---------------------------------------------
sink(file.path(CFG$out_dir, "results_summary.txt"))
cat("HoQ–TOPSIS pipeline — results summary\n", format(Sys.time()), "\n\n")
if (exists("RQ1")) cat(sprintf("RQ1: top HOW %s, rank-1 in %s of %d replicates, %d of 35 rank-stable -> %s\n",
  RQ1$top$how_id, fmt_pct(RQ1$top$rank1_freq), CFG$boot_B, 35 - RQ1$n_rank_unstable, RQ1$verdict))
for (g in names(RQ2)) with(RQ2[[g]], cat(sprintf(
  "RQ2 %-9s n=%3d mean %+.1f%% median %+.1f%% 95%% CI %+.1f%% to %+.1f%% (%d of 5 tests) -> %s\n",
  g, n, 100 * mean_change, 100 * median_change, 100 * ci_pct[1], 100 * ci_pct[2],
  tests_passed, verdict)))
for (k in names(RQ3)) with(RQ3[[k]]$panels, cat(sprintf(
  "RQ3 %-10s mean stability %s (95%% CI %s to %s) -> %s\n",
  k, fmt_pct(mean), fmt_pct(ci[1]), fmt_pct(ci[2]), verdict)))
cat(sprintf("CV: mean Spearman %.4f; all-data spread change %+.1f%%\n",
            mean(CV$panelA$spearman), 100 * CV$panelC["all_delta"]))
sink()

check_summary()
cat("\nTotal run time:", format(round(Sys.time() - t_start, 1)), "\n")
cat("Outputs written to", normalizePath(CFG$out_dir), "\n")