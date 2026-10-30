# =============================================================================
# 08_MONTE_CARLO.R — EA3: DIRICHLET MONTE CARLO SIMULATION
# =============================================================================
# Draws CTQ weight vectors from Dirichlet(alpha), rebuilds full pipeline
# for each draw, and records manufacturer rankings to quantify decision
# robustness.
#
# Dependencies: 03_topsis_engine.R (via earlier modules)
# Output:       MC_RESULTS       — per-iteration rank + CC data
#               MC_SUMMARY       — rank frequency + dominance probabilities
#               MC_TIPPING_POINT — quality weight vs ranking data
# =============================================================================

if (!exists("WJ")) source("03_topsis_engine.R")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 08 — MONTE CARLO SIMULATION (EA3)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

set.seed(SIM_PARAMS$random_seed)
n_mc    <- SIM_PARAMS$monte_carlo_n
alpha   <- SIM_PARAMS$dirichlet_alpha

cat(sprintf("Dirichlet alpha: (%.1f, %.1f, %.1f)\n", alpha[1], alpha[2], alpha[3]))
cat(sprintf("Running %s Monte Carlo iterations...\n\n",
            format(n_mc, big.mark = ",")))


# ── GENERATE DIRICHLET DRAWS ────────────────────────────────────────────────
# Using gamma-ratio method (no external package dependency)

rdirichlet_manual <- function(n, alpha) {
  k <- length(alpha)
  draws <- matrix(0, nrow = n, ncol = k)
  for (j in 1:k) {
    draws[, j] <- rgamma(n, shape = alpha[j], rate = 1)
  }
  draws / rowSums(draws)
}

ctq_draws <- rdirichlet_manual(n_mc, alpha)
colnames(ctq_draws) <- c("Q", "T", "C")


# ── RUN FULL PIPELINE PER DRAW ──────────────────────────────────────────────

MC_RESULTS <- data.frame(
  iteration    = integer(n_mc * 3),
  manufacturer = character(n_mc * 3),
  cc           = numeric(n_mc * 3),
  topsis_rank  = integer(n_mc * 3),
  Q_weight     = numeric(n_mc * 3),
  T_weight     = numeric(n_mc * 3),
  C_weight     = numeric(n_mc * 3),
  stringsAsFactors = FALSE
)

t_start <- Sys.time()
row_idx <- 1L

for (i in 1:n_mc) {
  q_w <- ctq_draws[i, 1]
  t_w <- ctq_draws[i, 2]
  c_w <- ctq_draws[i, 3]

  # Post-HoQ CTQ scaling: apply CTQ to base wj (bypasses HoQ dampening)
  wj_i <- apply_ctq_scaling(WJ, q_w, t_w, c_w)
  names(wj_i) <- names(WJ)
  wj_i <- wj_i[colnames(ALTERNATIVES_MATRIX)]
  topsis_i <- run_topsis(ALTERNATIVES_MATRIX, wj_i)

  for (m in 1:3) {
    MC_RESULTS$iteration[row_idx]    <- i
    MC_RESULTS$manufacturer[row_idx] <- topsis_i$manufacturer[m]
    MC_RESULTS$cc[row_idx]           <- topsis_i$cc[m]
    MC_RESULTS$topsis_rank[row_idx]  <- topsis_i$topsis_rank[m]
    MC_RESULTS$Q_weight[row_idx]     <- q_w
    MC_RESULTS$T_weight[row_idx]     <- t_w
    MC_RESULTS$C_weight[row_idx]     <- c_w
    row_idx <- row_idx + 1L
  }

  if (i %% 2000 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    cat(sprintf("  %s / %s (%.1f sec, %.0f iter/sec)\n",
                format(i, big.mark = ","),
                format(n_mc, big.mark = ","),
                elapsed, i / elapsed))
  }
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
cat(sprintf("  Done in %.1f seconds\n\n", elapsed_total))


# ── RANK FREQUENCY TABLE ────────────────────────────────────────────────────

MC_RANK_FREQ <- data.frame()
for (mfr in MANUFACTURERS) {
  mfr_data <- MC_RESULTS[MC_RESULTS$manufacturer == mfr, ]
  MC_RANK_FREQ <- rbind(MC_RANK_FREQ, data.frame(
    manufacturer = mfr,
    pct_rank1    = mean(mfr_data$topsis_rank == 1) * 100,
    pct_rank2    = mean(mfr_data$topsis_rank == 2) * 100,
    pct_rank3    = mean(mfr_data$topsis_rank == 3) * 100,
    mean_rank    = mean(mfr_data$topsis_rank),
    cc_mean      = mean(mfr_data$cc),
    cc_ci_lower  = quantile(mfr_data$cc, 0.025),
    cc_ci_upper  = quantile(mfr_data$cc, 0.975),
    stringsAsFactors = FALSE
  ))
}


# ── DOMINANCE PROBABILITIES ─────────────────────────────────────────────────

MC_DOMINANCE <- data.frame()
for (i in 1:(length(MANUFACTURERS) - 1)) {
  for (j in (i + 1):length(MANUFACTURERS)) {
    m1 <- MANUFACTURERS[i]; m2 <- MANUFACTURERS[j]
    cc1 <- MC_RESULTS$cc[MC_RESULTS$manufacturer == m1]
    cc2 <- MC_RESULTS$cc[MC_RESULTS$manufacturer == m2]
    p_dom <- mean(cc1 > cc2) * 100
    MC_DOMINANCE <- rbind(MC_DOMINANCE, data.frame(
      comparison = paste0("P(", m1, " > ", m2, ")"),
      probability = p_dom,
      stringsAsFactors = FALSE
    ))
  }
}


# ── QUALITY WEIGHT TIPPING POINT ────────────────────────────────────────────
# Bin quality weights and compute rank frequencies per bin

q_bins <- seq(0, 1, by = 0.10)
MC_TIPPING_POINT <- data.frame()
for (b in 1:(length(q_bins) - 1)) {
  lo <- q_bins[b]; hi <- q_bins[b + 1]
  bin_label <- sprintf("%.0f-%.0f%%", lo * 100, hi * 100)

  for (mfr in MANUFACTURERS) {
    subset_idx <- MC_RESULTS$manufacturer == mfr &
                  MC_RESULTS$Q_weight >= lo &
                  MC_RESULTS$Q_weight < hi
    n_in_bin <- sum(subset_idx)
    if (n_in_bin > 0) {
      pct_r1 <- mean(MC_RESULTS$topsis_rank[subset_idx] == 1) * 100
    } else {
      pct_r1 <- NA
    }
    MC_TIPPING_POINT <- rbind(MC_TIPPING_POINT, data.frame(
      q_bin = bin_label, q_lo = lo, q_hi = hi,
      q_mid = (lo + hi) / 2,
      manufacturer = mfr, pct_rank1 = pct_r1, n = n_in_bin,
      stringsAsFactors = FALSE
    ))
  }
}


# ── SUMMARY OUTPUT ──────────────────────────────────────────────────────────

MC_SUMMARY <- list(
  rank_freq      = MC_RANK_FREQ,
  dominance      = MC_DOMINANCE,
  tipping_point  = MC_TIPPING_POINT,
  ctq_draws      = ctq_draws,
  iteration_detail = MC_RESULTS
)

cat("Rank Frequency Table:\n")
cat(sprintf("  %-6s  %8s  %8s  %8s  %8s  %10s  %18s\n",
            "Mfr", "P(#1)", "P(#2)", "P(#3)", "Mean Rk", "Mean CC", "95% CI"))
cat("  ", strrep("-", 76), "\n")
for (i in 1:nrow(MC_RANK_FREQ)) {
  row <- MC_RANK_FREQ[i, ]
  cat(sprintf("  %-6s  %7.1f%%  %7.1f%%  %7.1f%%  %8.2f  %10.6f  [%.4f, %.4f]\n",
              row$manufacturer,
              row$pct_rank1, row$pct_rank2, row$pct_rank3,
              row$mean_rank, row$cc_mean,
              row$cc_ci_lower, row$cc_ci_upper))
}

cat("\nDominance Probabilities:\n")
for (i in 1:nrow(MC_DOMINANCE)) {
  cat(sprintf("  %s = %.1f%%\n", MC_DOMINANCE$comparison[i], MC_DOMINANCE$probability[i]))
}

# Robustness index: % of iterations where base case ranking holds
base_ranking <- paste(MANUFACTURERS[order(BASE_TOPSIS$topsis_rank)], collapse = ">")
mc_rankings <- tapply(1:nrow(MC_RESULTS), MC_RESULTS$iteration, function(idx) {
  d <- MC_RESULTS[idx, ]
  paste(d$manufacturer[order(d$topsis_rank)], collapse = ">")
})
robustness_index <- mean(mc_rankings == base_ranking) * 100

cat(sprintf("\nRobustness Index: %.1f%% of iterations preserve base ranking (%s)\n",
            robustness_index, base_ranking))

cat("\nMODULE 08 COMPLETE — MC_SUMMARY ready.\n\n")
