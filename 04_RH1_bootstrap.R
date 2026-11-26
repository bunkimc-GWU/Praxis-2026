# =============================================================================
# 04_rq1_bootstrap.R — RQ1 / RH1: stability of the HoQ weight ranking
# =============================================================================
# Workbook equivalents: 'Bootstrap HoQ' (static 10,000-replicate run),
#   'RQ1 Findings' (RH1 verdict).
# Method: nonparametric bootstrap. Each replicate redraws the 38,400 FDA records
# with replacement (multinomial over W01–W17), recomputes the WHAT importances
# and runs the full HoQ chain (Steps 1–14, equal CTQ shares). Difficulty scores
# and the fixed Time/Cost importances are held constant.

cat("\n== 04 RQ1 bootstrap (B = ", CFG$boot_B, ") ==\n", sep = "")

run_bootstrap <- function(counts, B = CFG$boot_B, seed = CFG$boot_seed) {
  N <- sum(counts)
  cum <- Reduce(`+`, counts / N, accumulate = TRUE)   # sequential double sums
  if (.HAS_CPP) {
    cnt <- mb32_boot_counts(seed, B, N, cum)
  } else {
    rng <- new_rng(seed); K <- length(counts)
    cnt <- matrix(0L, B, K)
    for (b in seq_len(B)) {
      u <- rng$un(N)
      j <- pmin(findInterval(u, cum[-K]) + 1L, K)     # first j with u < cum[j]
      cnt[b, ] <- tabulate(j, K)
      if (b %% 100 == 0) cat("   replicate", b, "\n")
    }
  }
  W <- t(apply(cnt, 1, function(cn) hoq_chain(cn)$ctq_final))
  colnames(W) <- HOQ$how_id
  list(counts = cnt, W = W)
}

t0 <- Sys.time()
BOOT <- run_bootstrap(DATA$what_counts)
cat("   bootstrap time:", format(round(Sys.time() - t0, 1)), "\n")

W  <- BOOT$W
RK <- t(apply(W, 1, rank_eq))                       # rank of each HOW per replicate
baseline <- hoq_chain(DATA$what_counts)             # equal CTQ shares

boot_tab <- data.frame(
  how_id        = HOQ$how_id,
  how_name      = HOQ$how_name,
  baseline_wj   = baseline$ctq_final,
  baseline_rank = baseline$rank_final,
  boot_mean     = colMeans(W),
  boot_sd       = apply(W, 2, sd),
  ci_low        = apply(W, 2, pinc, 0.025),
  ci_high       = apply(W, 2, pinc, 0.975),
  rank1_freq    = colMeans(RK == 1),
  top5_freq     = colMeans(RK <= 5),
  modal_rank    = apply(RK, 2, modal_rank),
  mean_rank     = colMeans(RK),
  rank_ci_low   = apply(RK, 2, pinc, 0.025),
  rank_ci_high  = apply(RK, 2, pinc, 0.975),
  stringsAsFactors = FALSE)
boot_tab$rank_ci <- ifelse(boot_tab$rank_ci_low == boot_tab$rank_ci_high,
                           as.character(boot_tab$rank_ci_low),
                           paste0(boot_tab$rank_ci_low, "\u2013", boot_tab$rank_ci_high))
boot_tab <- boot_tab[order(boot_tab$baseline_rank, boot_tab$how_id), ]

# ---- RH1 verdict ('RQ1 Findings' Panel D) ----------------------------------
top        <- boot_tab[boot_tab$baseline_rank == 1, ][1, ]
n_ever_r1  <- sum(boot_tab$rank1_freq > 0)
rh1_tests  <- c(rank1_freq_ge_threshold = top$rank1_freq >= CFG$rh1_threshold,
                modal_rank_is_1         = top$modal_rank == 1,
                no_competitor_rank1     = n_ever_r1 == 1)
RQ1 <- list(table = boot_tab, top = top, n_ever_rank1 = n_ever_r1,
            n_rank_unstable = sum(boot_tab$rank_ci_low != boot_tab$rank_ci_high),
            tests = rh1_tests,
            verdict = if (all(rh1_tests)) "RH1 SUPPORTED" else "RH1 NOT SUPPORTED")
cat(sprintf("   Top HOW %s holds rank 1 in %s of replicates; %d of 35 fully rank-stable; %s\n",
            top$how_id, fmt_pct(top$rank1_freq), 35 - RQ1$n_rank_unstable, RQ1$verdict))

# ---- reconciliation with 'Bootstrap HoQ'!A5:Q39 ---------------------------
ref <- xl_raw("Bootstrap HoQ", "A5:Q39")
ref_id <- as.character(ref[[1]])
m <- match(ref_id, boot_tab$how_id)
num <- function(k) as.numeric(ref[[k]])
check("04", "HOW order (sorted by baseline rank)", boot_tab$how_id[m], ref_id)
check("04", "Baseline wj vs 'Bootstrap HoQ'!C", boot_tab$baseline_wj[m], num(3), CFG$tol_loose)
check("04", "Boot mean wj vs 'Bootstrap HoQ'!E",  boot_tab$boot_mean[m], num(5), 1e-12)
check("04", "Boot SD vs 'Bootstrap HoQ'!F",       boot_tab$boot_sd[m],   num(6), 1e-12)
check("04", "95% CI low vs 'Bootstrap HoQ'!G",    boot_tab$ci_low[m],    num(7), 1e-12)
check("04", "95% CI high vs 'Bootstrap HoQ'!H",   boot_tab$ci_high[m],   num(8), 1e-12)
check("04", "Rank-1 freq vs 'Bootstrap HoQ'!I",   boot_tab$rank1_freq[m], num(9), 0)
check("04", "Top-5 freq vs 'Bootstrap HoQ'!J",    boot_tab$top5_freq[m], num(10), 0)
check("04", "Modal rank vs 'Bootstrap HoQ'!K",    boot_tab$modal_rank[m], num(11), 0)
check("04", "Mean rank vs 'Bootstrap HoQ'!O",     boot_tab$mean_rank[m], num(15), 1e-3)
check("04", "Rank CI low/high vs 'Bootstrap HoQ'!P:Q",
      c(boot_tab$rank_ci_low[m], boot_tab$rank_ci_high[m]), c(num(16), num(17)), 0)

save_csv(boot_tab, "04_rq1_bootstrap_table.csv")