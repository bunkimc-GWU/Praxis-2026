# =============================================================================
# 02_hoq.R — House of Quality: WHAT importances → HOW (technical characteristic)
#            weights, CTQ composition and CTQ-scaled weights
# =============================================================================
# Workbook equivalents: 'What Weights', 'HOQ' (rows 9–60),
#   'How HoQ Calculation Chain', 'CTQ Fractions & Scaled Weights'
# Output: object HOQ (list) with base weights, equal weights, CTQ fractions.

cat("\n== 02 House of Quality ==\n")

how_id   <- xl_vec_chr("HOQ", "I7:AQ7")            # H01–H35
how_name <- xl_vec_chr("HOQ", "I8:AQ8")
REL      <- xl_num("Config", "F53:AN77")             # 25 WHAT x 35 HOW (9/3/1/0)
REL[is.na(REL)] <- 0
DIFF     <- xl_vec_num("HOQ", "I38:AQ38")            # difficulty score (input)
dimnames(REL) <- list(DATA$what$id, how_id)

# hoq_chain(): the 14-step calculation chain for one set of WHAT counts.
#   counts : FDA counts for W01–W17 (importance = count / sum(counts))
#   ctq    : stakeholder CTQ shares (Quality, Time, Cost) used in Step 12
hoq_chain <- function(counts, ctq = CFG$ctq_equal,
                      rel = REL, diff = DIFF, fixed = DATA$fixed_importance) {
  h  <- c(counts / sum(counts), fixed)               # WHAT importance (HOQ!H9:H33)
  grp <- DATA$what$ctq
  P_raw  <- colSums(rel * h)                         # Step 1  Pj = Σ wt × rel
  P_norm <- P_raw / sum(P_raw)                       # Step 2
  P_diff <- P_norm * diff                            # Step 3  × difficulty
  base   <- P_diff / sum(P_diff)                     # Step 4  HoQ base weight
  contQ  <- colSums(rel[grp == "Q", ] * h[grp == "Q"])   # Step 5
  contT  <- colSums(rel[grp == "T", ] * h[grp == "T"])   # Step 6
  contC  <- colSums(rel[grp == "C", ] * h[grp == "C"])   # Step 7
  tot    <- contQ + contT + contC                    # Step 8
  dominant <- ifelse(contQ >= contT, ifelse(contQ >= contC, "Quality", "Cost"),
                     ifelse(contT >= contC, "Time", "Cost"))
  fQ <- contQ / tot; fT <- contT / tot; fC <- contC / tot   # Steps 9–11
  mult  <- ctq[1] * fQ + ctq[2] * fT + ctq[3] * fC   # Step 12
  ctq_b <- base * mult                               # Step 13
  final <- ctq_b / sum(ctq_b)                        # Step 14
  list(importance = h, P_raw = P_raw, P_norm = P_norm, P_diff = P_diff,
       base = base, equal = rep(1 / length(base), length(base)),
       contQ = contQ, contT = contT, contC = contC, dominant = dominant,
       fQ = fQ, fT = fT, fC = fC, mult = mult, ctq_final = final,
       rank_final = rank_eq(final), rank_base = rank_eq(base))
}

# CTQ-scaled weights for any (q, t, c) share vector — used by RQ3
ctq_weights <- function(q, t, c, base = HOQ$base, fQ = HOQ$fQ, fT = HOQ$fT,
                        fC = HOQ$fC) {
  w <- base * (q * fQ + t * fT + c * fC)
  w / sum(w)
}

HOQ <- hoq_chain(DATA$what_counts)
HOQ$how_id <- how_id; HOQ$how_name <- how_name

# ---- reconciliation --------------------------------------------------------
ctq_live <- xl_vec_num("Overview Tool", "B2:B4")    # shares currently on the HOQ tab
HOQ_live <- hoq_chain(DATA$what_counts, ctq = ctq_live)
check("02", "WHAT importance vs HOQ!H9:H33", HOQ$importance, xl_vec_num("HOQ", "H9:H33"))
check("02", "Step 1 raw priority vs HOQ!I36:AQ36", HOQ$P_raw, xl_vec_num("HOQ", "I36:AQ36"))
check("02", "Step 4 HoQ base weights vs HOQ!I40:AQ40", HOQ$base, xl_vec_num("HOQ", "I40:AQ40"))
check("02", "Dominant CTQ dimension vs HOQ!I48:AQ48", HOQ$dominant, xl_vec_chr("HOQ", "I48:AQ48"))
check("02", "Q fraction vs HOQ!I51:AQ51", HOQ$fQ, xl_vec_num("HOQ", "I51:AQ51"))
check("02", "T fraction vs HOQ!I52:AQ52", HOQ$fT, xl_vec_num("HOQ", "I52:AQ52"))
check("02", "C fraction vs HOQ!I53:AQ53", HOQ$fC, xl_vec_num("HOQ", "I53:AQ53"))
check("02", "Step 14 CTQ final weights vs HOQ!I58:AQ58", HOQ_live$ctq_final,
      xl_vec_num("HOQ", "I58:AQ58"))
check("02", "Final weight ranks vs HOQ!I59:AQ59", HOQ_live$rank_final,
      xl_vec_num("HOQ", "I59:AQ59"), 0)

save_csv(data.frame(how_id, how_name, P_raw = HOQ$P_raw, P_norm = HOQ$P_norm,
                    difficulty = DIFF, base_weight = HOQ$base,
                    base_rank = HOQ$rank_base, dominant = HOQ$dominant,
                    Q_frac = HOQ$fQ, T_frac = HOQ$fT, C_frac = HOQ$fC,
                    ctq_final = HOQ$ctq_final, ctq_rank = HOQ$rank_final),
         "02_hoq_weights.csv")