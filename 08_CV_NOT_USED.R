# =============================================================================
# 08_cv.R — cross-validation: repeated random holdout of the FDA records
# =============================================================================
# Workbook equivalents: 'FDA DATA'!L:P (Train/Test labels), 'CV Inputs',
#   'CV Weights', 'CV TOPSIS', 'CV Summary'.
# Design: 5 independent reps; each draws 2,260 Test records by simple random
# sampling without replacement (seeded backward Fisher–Yates, first 2,260 of
# the shuffle), the remaining 36,140 are Train. HoQ base weights (Steps 1–4)
# are re-derived for All / Train / Test, then the 780 pairs are re-solved with
# a sector-average anchor (mean score per criterion across the 40 companies).

cat("\n== 08 Cross-validation ==\n")

cv_labels <- function(seed, N = DATA$N, n_test = CFG$cv_test_n) {
  idx <- if (.HAS_CPP) mb32_fisher_yates(seed, N) else {
    rng <- new_rng(seed); x <- 0:(N - 1); u <- rng$un(N - 1)
    for (k in seq_len(N - 1)) { i <- N - k; j <- floor(u[k] * (i + 1))
      tmp <- x[i + 1]; x[i + 1] <- x[j + 1]; x[j + 1] <- tmp }
    x
  }
  lab <- rep("Train", N); lab[idx[seq_len(n_test)] + 1] <- "Test"; lab
}
CV_LAB <- sapply(CFG$cv_seeds, cv_labels)
colnames(CV_LAB) <- paste0("rep", 1:5)

# Scenarios: All, Train 1–5, Test 1–5
scen <- c(list(All = rep(TRUE, DATA$N)),
          setNames(lapply(1:5, function(r) CV_LAB[, r] == "Train"), paste("Train", 1:5)),
          setNames(lapply(1:5, function(r) CV_LAB[, r] == "Test"),  paste("Test", 1:5)))
CV_COUNTS <- sapply(scen, function(s) count_what(DATA$fda$category[s]))
CV_W <- apply(CV_COUNTS, 2, function(cn) hoq_chain(cn)$base)   # 35 x 11
rownames(CV_W) <- HOQ$how_id
CV_RANK <- apply(CV_W, 2, rank_eq)

# TOPSIS over the 780 pairs with the sector-average anchor
anchor_avg <- colMeans(SCORES)
cv_pair_cc <- function(w) t(vapply(seq_len(nrow(PAIRS)), function(k) {
  M <- rbind(SCORES[PAIRS$A[k], ], SCORES[PAIRS$B[k], ], anchor_avg)
  topsis(M, w)$cc[1:2]
}, numeric(2)))
CV_CC <- c(list(Equal = cv_pair_cc(HOQ$equal)),
           lapply(setNames(colnames(CV_W), colnames(CV_W)), function(s) cv_pair_cc(CV_W[, s])))
CV_SPREAD <- sapply(CV_CC, function(cc) abs(cc[, 1] - cc[, 2]))   # 780 x 12

# Panel A — weight stability, Train vs Test
panelA <- do.call(rbind, lapply(1:5, function(r) {
  x <- CV_W[, paste("Train", r)]; y <- CV_W[, paste("Test", r)]
  rx <- CV_RANK[, paste("Train", r)]; ry <- CV_RANK[, paste("Test", r)]
  data.frame(rep = r, max_abs_dw = max(abs(x - y)), mean_abs_dw = mean(abs(x - y)),
             spearman = cor(rx, ry), kendall = kendall_tau_a(x, y),
             max_rank_shift = max(abs(rx - ry)),
             top_train = HOQ$how_id[match(1, rx)], top_test = HOQ$how_id[match(1, ry)])
}))
# Panel B — CC spread vs equal weights
eq_mean <- mean(CV_SPREAD[, "Equal"]); eq_med <- median(CV_SPREAD[, "Equal"])
panelB <- do.call(rbind, lapply(1:5, function(r) {
  tr <- CV_SPREAD[, paste("Train", r)]; te <- CV_SPREAD[, paste("Test", r)]
  gt <- CV_CC[[paste("Train", r)]]; gs <- CV_CC[[paste("Test", r)]]
  d_tr <- mean(tr) / eq_mean - 1; d_te <- mean(te) / eq_mean - 1
  data.frame(rep = r, train_mean = mean(tr), test_mean = mean(te),
             train_delta = d_tr, test_delta = d_te,
             train_median_delta = median(tr) / eq_med - 1,
             test_median_delta = median(te) / eq_med - 1,
             train_gt_threshold = ifelse(d_tr > CFG$rh2_threshold, "YES", "NO"),
             test_gt_threshold = ifelse(d_te > CFG$rh2_threshold, "YES", "NO"),
             abs_train_test = abs(d_tr - d_te),
             pair_order_agreement = mean(sign(gt[, 1] - gt[, 2]) == sign(gs[, 1] - gs[, 2])))
}))
panelC <- c(equal_mean = eq_mean, all_mean = mean(CV_SPREAD[, "All"]),
            all_delta = mean(CV_SPREAD[, "All"]) / eq_mean - 1,
            all_median_delta = median(CV_SPREAD[, "All"]) / eq_med - 1)
CV <- list(labels = CV_LAB, counts = CV_COUNTS, weights = CV_W, panelA = panelA,
           panelB = panelB, panelC = panelC)
cat(sprintf("   mean Spearman rho %.4f; Train mean delta %+.1f%%, Test %+.1f%% (all data %+.1f%%)\n",
            mean(panelA$spearman), 100 * mean(panelB$train_delta),
            100 * mean(panelB$test_delta), 100 * panelC["all_delta"]))

# ---- reconciliation --------------------------------------------------------
check("08", "Train/Test labels vs 'FDA DATA'!L:P (5 x 38,400)", CV_LAB,
      xl_chr("FDA DATA", sprintf("L2:P%d", DATA$N + 1)))
check("08", "Scenario counts vs 'CV Inputs'!D5:N21", CV_COUNTS,
      xl_num("CV Inputs", "D5:N21"), 0)
check("08", "Scenario base weights vs 'CV Weights'!D118:N152", CV_W,
      xl_num("CV Weights", "D118:N152"), 1e-12)
check("08", "Pair CCs (12 scenarios) vs 'CV TOPSIS'!D8:AA787",
      do.call(cbind, CV_CC), xl_num("CV TOPSIS", "D8:AA787"), 1e-12)
check("08", "Panel A vs 'CV Summary'!B6:F10", as.matrix(panelA[, 2:6]),
      xl_num("CV Summary", "B6:F10"), 1e-9)
check("08", "Panel A top HOW vs 'CV Summary'!G6:H10",
      as.matrix(panelA[, 7:8]), xl_chr("CV Summary", "G6:H10"))
check("08", "Panel B vs 'CV Summary'!B19:G23", as.matrix(panelB[, 2:7]),
      xl_num("CV Summary", "B19:G23"), 1e-9)
check("08", "Panel C vs 'CV Summary'!B31:B34", panelC,
      xl_vec_num("CV Summary", "B31:B34"), 1e-9)

save_csv(panelA, "08_cv_panelA_weights.csv")
save_csv(panelB, "08_cv_panelB_spread.csv")