# =============================================================================
# 06_BOOTSTRAP.R — RANK STABILITY VIA BOOTSTRAP RESAMPLING (EA1)
# =============================================================================
# Resamples FDA 483 category counts (multinomial), rebuilds WHAT weights,
# reruns full HoQ pipeline, and records rank positions for all 35 criteria.
#
# Outputs:
#   BOOTSTRAP_RESULTS    — per-criterion rank statistics
#   BOOTSTRAP_WJ_RESULTS — per-criterion wj statistics
#   BOOTSTRAP_HOW_RANKS  — raw rank matrix (n_bootstrap × 35)
#   BOOTSTRAP_HOW_WJ     — raw wj matrix (n_bootstrap × 35)
# =============================================================================

if (!exists("WJ")) source("03_topsis_engine.R")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 06 — BOOTSTRAP RANK STABILITY\n")
cat("══════════════════════════════════════════════════════════════\n\n")

set.seed(SIM_PARAMS$random_seed)
n_boot <- SIM_PARAMS$bootstrap_n
ci_alpha <- (1 - SIM_PARAMS$ci_level) / 2

cat(sprintf("Running %s bootstrap resamples of %s FDA records...\n",
            format(n_boot, big.mark = ","),
            format(PROJECT$fda_total, big.mark = ",")))

fda_counts <- FDA_CATEGORIES$count
fda_probs  <- fda_counts / sum(fda_counts)
n_total    <- sum(fda_counts)

# Pre-allocate rank matrix: rows = bootstrap iterations, cols = 35 HOWs
BOOTSTRAP_HOW_RANKS <- matrix(0L, nrow = n_boot, ncol = PROJECT$n_hows)
colnames(BOOTSTRAP_HOW_RANKS) <- HOW_DEFS$how_id

# Pre-allocate wj matrix: rows = bootstrap iterations, cols = 35 HOWs
BOOTSTRAP_HOW_WJ <- matrix(NA_real_, nrow = n_boot, ncol = PROJECT$n_hows)
colnames(BOOTSTRAP_HOW_WJ) <- HOW_DEFS$how_id

t_start <- Sys.time()

# Record first N sample iterations for transparency/audit
N_SAMPLES <- min(100L, n_boot)
BOOTSTRAP_SAMPLES <- list()

for (b in 1:n_boot) {
  # Step 1: Resample FDA category counts (multinomial)
  resampled_counts <- rmultinom(1, size = n_total, prob = fda_probs)[, 1]
  resampled_props  <- resampled_counts / sum(resampled_counts)

  # Step 2: Rebuild WHAT weights with resampled Quality importance (no CTQ)
  what_wts_boot <- WHAT_DEFS$importance
  what_wts_boot[1:17] <- resampled_props
  names(what_wts_boot) <- WHAT_DEFS$what_id

  # Step 3: Full HoQ pipeline
  hoq_boot <- compute_hoq_wj(what_wts_boot)

  # Step 4: Record ranks and wj
  BOOTSTRAP_HOW_RANKS[b, ] <- hoq_boot$wj_rank
  BOOTSTRAP_HOW_WJ[b, ]    <- hoq_boot$wj
  
  # Save full detail for first N iterations
  if (b <= N_SAMPLES) {
    BOOTSTRAP_SAMPLES[[b]] <- list(
      iteration        = b,
      resampled_counts = resampled_counts,
      resampled_props  = resampled_props,
      what_weights     = what_wts_boot,
      wj               = hoq_boot$wj,
      wj_rank          = hoq_boot$wj_rank,
      rank1_criterion = HOW_DEFS$how_id[which.min(hoq_boot$wj_rank)]
    )
  }

  if (b %% 2000 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    cat(sprintf("  %s / %s complete (%.1f sec)\n",
                format(b, big.mark = ","),
                format(n_boot, big.mark = ","),
                elapsed))
  }
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
cat(sprintf("  Done in %.1f seconds (%.0f iterations/sec)\n\n",
            elapsed_total, n_boot / elapsed_total))

#── COMPUTE BOOTSTRAP wj STATISTICS ─────────────────────────────────────────

BOOTSTRAP_WJ_RESULTS <- data.frame(
  how_id      = HOW_DEFS$how_id,
  mean_wj     = colMeans(BOOTSTRAP_HOW_WJ, na.rm = TRUE),
  median_wj   = apply(BOOTSTRAP_HOW_WJ, 2, median, na.rm = TRUE),
  wj_ci_lower = apply(BOOTSTRAP_HOW_WJ, 2, quantile, probs = ci_alpha, na.rm = TRUE),
  wj_ci_upper = apply(BOOTSTRAP_HOW_WJ, 2, quantile, probs = 1 - ci_alpha, na.rm = TRUE),
  stringsAsFactors = FALSE
)

cat("\nVerification checks:\n")
cat("BOOTSTRAP_WJ_RESULTS exists: ", exists("BOOTSTRAP_WJ_RESULTS"), "\n", sep = "")
cat("BOOTSTRAP_HOW_WJ dimensions: ", paste(dim(BOOTSTRAP_HOW_WJ), collapse = " x "), "\n", sep = "")
print(head(BOOTSTRAP_WJ_RESULTS))

# ── COMPUTE BOOTSTRAP STATISTICS ────────────────────────────────────────────

BOOTSTRAP_RESULTS <- data.frame(
  how_id      = HOW_DEFS$how_id,
  description = HOW_DEFS$description,
  base_rank   = HOQ_RESULTS$wj_rank,
  mean_rank   = colMeans(BOOTSTRAP_HOW_RANKS),
  median_rank = apply(BOOTSTRAP_HOW_RANKS, 2, median),
  ci_lower    = apply(BOOTSTRAP_HOW_RANKS, 2, quantile, probs = ci_alpha),
  ci_upper    = apply(BOOTSTRAP_HOW_RANKS, 2, quantile, probs = 1 - ci_alpha),
  pct_rank1   = colMeans(BOOTSTRAP_HOW_RANKS == 1) * 100,
  pct_top5    = colMeans(BOOTSTRAP_HOW_RANKS <= 5) * 100,
  stringsAsFactors = FALSE
)

BOOTSTRAP_RESULTS <- BOOTSTRAP_RESULTS[order(BOOTSTRAP_RESULTS$mean_rank), ]

# ── LONG-FORM BOOTSTRAP OUTPUT FOR MINITAB ─────────────────────────────────

BOOTSTRAP_LONG <- data.frame(
  iteration   = rep(seq_len(n_boot), each = PROJECT$n_hows),
  how_id      = rep(HOW_DEFS$how_id, times = n_boot),
  description = rep(HOW_DEFS$description, times = n_boot),
  wj          = as.vector(t(BOOTSTRAP_HOW_WJ)),
  rank        = as.vector(t(BOOTSTRAP_HOW_RANKS)),
  stringsAsFactors = FALSE
)

dir.create("../outputs", showWarnings = FALSE)

write.csv(
  BOOTSTRAP_LONG,
  "../outputs/BOOTSTRAP_LONG_FORM.csv",
  row.names = FALSE
)
cat("  Saved long-form bootstrap file: ../outputs/BOOTSTRAP_LONG_FORM.csv\n")


cat("Bootstrap Rank Stability (top 10 criteria):\n")
cat(sprintf("  %-5s  %-42s  %6s  %6s  %10s  %8s  %8s\n",
            "ID", "Description", "Base", "Mean", "95% CI", "P(#1)", "P(Top5)"))
cat("  ", strrep("-", 90), "\n")
for (i in 1:min(10, nrow(BOOTSTRAP_RESULTS))) {
  row <- BOOTSTRAP_RESULTS[i, ]
  cat(sprintf("  %-5s  %-42s  %4d    %5.1f  [%2.0f, %2.0f]    %5.1f%%   %5.1f%%\n",
              row$how_id,
              substr(row$description, 1, 42),
              row$base_rank, row$mean_rank,
              row$ci_lower, row$ci_upper,
              row$pct_rank1, row$pct_top5))
  }

cat("\nMODULE 06 COMPLETE — BOOTSTRAP_RESULTS and BOOTSTRAP_wj_RESULTS ready.\n\n")
