# =============================================================================
# 03_TOPSIS_ENGINE.R — TOPSIS RANKING OF ALTERNATIVES BC04112026
# =============================================================================
# Implements standard TOPSIS (Hwang & Yoon 1981):
#   1. Vector normalization
#   2. Weighted normalized matrix
#   3. PIS / NIS identification
#   4. Euclidean distances S+, S-
#   5. Closeness Coefficient CC = S- / (S+ + S-)
#   6. Ranking by CC descending
#
# Dependencies: 02_hoq_engine.R
# Output:       BASE_TOPSIS (base case results)
#               run_topsis() — reusable for any weight vector
# =============================================================================

if (!exists("WJ")) source("02_hoq_engine.R")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("MODULE 03 — TOPSIS RANKING OF ALTERNATIVES *aligned*\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# ── CORE TOPSIS FUNCTION ────────────────────────────────────────────────────
# Pure function: takes alternatives matrix + weight vector, returns full results.
# No side effects, no global state — safe for parallel MC calls.

run_topsis <- function(dec_mat, wj_vec) {

  mfr_names <- rownames(dec_mat)
  how_ids   <- colnames(dec_mat)
  n_mfr     <- nrow(dec_mat)
  n_how     <- ncol(dec_mat)

  # Step 1: Vector normalization
  col_norms   <- sqrt(colSums(dec_mat^2))
  norm_matrix <- sweep(dec_mat, 2, col_norms, "/")

  # Step 2: Weighted normalized matrix
  weighted_matrix <- sweep(norm_matrix, 2, wj_vec, "*")

  # Step 3: PIS (best per criterion) and NIS (worst per criterion)
  pis <- apply(weighted_matrix, 2, max)
  nis <- apply(weighted_matrix, 2, min)

  # Step 4: Euclidean distances
  s_plus  <- sqrt(rowSums(sweep(weighted_matrix, 2, pis, "-")^2))
  s_minus <- sqrt(rowSums(sweep(weighted_matrix, 2, nis, "-")^2))

  # Step 5: Closeness Coefficient
  cc <- s_minus / (s_plus + s_minus)

  # Step 6: Ranking
  topsis_rank <- rank(-cc, ties.method = "min")

  # CC Spread (discriminative power metric)
  cc_spread <- max(cc) - min(cc)

  data.frame(
    manufacturer = mfr_names,
    s_plus       = s_plus,
    s_minus      = s_minus,
    cc           = cc,
    topsis_rank  = topsis_rank,
    cc_spread    = cc_spread,
    row.names    = NULL,
    stringsAsFactors = FALSE
  )
}


# ── RUN BASE CASE ────────────────────────────────────────────────────────────
# Apply post-HoQ CTQ scaling to base wj, then run TOPSIS

WJ_BASE <- WJ  # Store unscaled HoQ weights for reference
WJ_CTQ <- apply_ctq_scaling(WJ, CTQ_BASE$Q, CTQ_BASE$T, CTQ_BASE$C)
names(WJ_CTQ) <- names(WJ)

wj_aligned <- WJ_CTQ[colnames(ALTERNATIVES_MATRIX)]
BASE_TOPSIS <- run_topsis(ALTERNATIVES_MATRIX, wj_aligned)

cat("Base Case TOPSIS Results (CTQ: Q=", CTQ_BASE$Q*100, "%, T=",
    CTQ_BASE$T*100, "%, C=", CTQ_BASE$C*100, "%):\n\n")
cat(sprintf("  %-6s  %10s  %10s  %12s  %6s\n",
            "Mfr", "S+", "S-", "CC", "Rank"))
cat("  ", strrep("-", 50), "\n")
for (i in order(BASE_TOPSIS$topsis_rank)) {
  cat(sprintf("  %-6s  %10.8f  %10.8f  %12.10f  %6d\n",
              BASE_TOPSIS$manufacturer[i],
              BASE_TOPSIS$s_plus[i],
              BASE_TOPSIS$s_minus[i],
              BASE_TOPSIS$cc[i],
              BASE_TOPSIS$topsis_rank[i]))
}
cat(sprintf("\n  CC Spread: %.6f\n", BASE_TOPSIS$cc_spread[1]))


cat("\nMODULE 03 COMPLETE — BASE_TOPSIS ready.\n")
cat("  Export: run_topsis(dec_mat, wj_vec) available for all modules.\n\n")
