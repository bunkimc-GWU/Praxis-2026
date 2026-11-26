# =============================================================================
# 03_topsis.R — TOPSIS (vector normalisation, all criteria benefit-type) and
#               rank-agreement helpers
# =============================================================================
# Workbook equivalents: 'TOPSIS_HoQ', 'TOPSIS_Equal Weight', 'TOPSIS_CTQ Weight',
#   'Kendall'. The active triplet check uses the scores currently on Config.

cat("\n== 03 TOPSIS ==\n")

# topsis(): M = alternatives x criteria score matrix, w = criterion weights.
# Returns S+, S- and the closeness coefficient CC = S- / (S+ + S-).
topsis <- function(M, w) {
  M <- as.matrix(M)
  norm <- sqrt(colSums(M^2))                        # Equation 17 divisor
  V <- sweep(sweep(M, 2, norm, "/"), 2, w, "*")     # Equation 18
  pis <- apply(V, 2, max); nis <- apply(V, 2, min)
  s_plus  <- sqrt(rowSums(sweep(V, 2, pis)^2))
  s_minus <- sqrt(rowSums(sweep(V, 2, nis)^2))
  alt <- if (is.null(rownames(M))) as.character(seq_len(nrow(M))) else rownames(M)
  cc <- s_minus / (s_plus + s_minus)
  data.frame(alt = alt, s_plus = unname(s_plus), s_minus = unname(s_minus),
             cc = unname(cc), rank = unname(rank_eq(cc)), row.names = NULL,
             stringsAsFactors = FALSE)
}

# Kendall tau-a between two score vectors (ΣΣ sign·sign / n(n−1))
kendall_tau_a <- function(x, y) {
  n <- length(x)
  sum(sign(outer(x, x, "-")) * sign(outer(y, y, "-"))) / (n * (n - 1))
}
spearman_excel <- function(x, y) cor(rank_eq(x), rank_eq(y))  # CORREL of RANK()

# ---- reconciliation on the active triplet (Overview Tool J3:L3) -------------
.cfg_scores <- xl_num("Config", "F8:AN47")
dimnames(.cfg_scores) <- list(xl_vec_chr("Config", "A8:A47"), HOQ$how_id)
active <- xl_vec_chr("Overview Tool", "J3:L3")
.M <- .cfg_scores[active, , drop = FALSE]
check("03", "TOPSIS_HoQ CC (active triplet)", topsis(.M, HOQ$base)$cc,
      xl_vec_num("TOPSIS_HoQ", "D28:D30"))
check("03", "TOPSIS_Equal Weight CC (active triplet)", topsis(.M, HOQ$equal)$cc,
      xl_vec_num("TOPSIS_Equal Weight", "D28:D30"))
check("03", "TOPSIS_CTQ Weight CC (active triplet)", topsis(.M, HOQ_live$ctq_final)$cc,
      xl_vec_num("TOPSIS_CTQ Weight", "D28:D30"))