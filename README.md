# A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty

**Author:** B. Chokshi
**Degree / Institution: DE, Engineering Management, George Washington University, 2026
**Status:** Completed

## Overview

This repository contains the R code used in the praxis analysis. The praxis develops a hybrid decision model for selecting sterile injectable drug contract manufacturing organizations (CMOs):

- **House of Quality (HoQ):** translates stakeholder requirements into weighted evaluation criteria.
- **TOPSIS** (Technique for Order of Preference by Similarity to Ideal Solution): ranks candidate CMOs by their relative closeness to an ideal solution.
- **Uncertainty treatment:** Uses a Monte-Carlo Simulation to address changes in CTQ prefernces.



# HoQ–TOPSIS pipeline in R

R port of the analysis in **HOQ and TOPSIS 17.xlsm**: FDA inspection records → House of Quality weights → TOPSIS, plus the RQ1–RQ3 studies and the cross-validation (not used in praxis). Each module recomputes its stage and then compares its results with the values saved in the workbook; `outputs/reconciliation_checks.csv` lists every comparison with PASS/FAIL.

## Folder layout
hoq_topsis_R/
  run_all.R
  README.md
  R/    00_utils.R … 08_cv.R
  src/  mulberry32.cpp
  outputs/   (created on first run)

## Requirements
- R ≥ 4.1
- install.packages(c("readxl", "Rcpp"))
- A C++ compiler for Rcpp: Rtools (Windows), Xcode command-line tools (macOS),  build-essential (Linux). Without one, an exact pure-R generator is used; that  is fine for everything except the RQ1 bootstrap, which then takes hours.

## Run
1. Put "HOQ and TOPSIS 17.xlsm" in this folder (or set HOQ_WORKBOOK to its path).
   Open it in Excel and save once first, so the saved formula values are current.
2. In R: setwd("path/to/hoq_topsis_R"); source("run_all.R")
   or from a terminal: Rscript run_all.R
   With Rcpp this takes a few minutes; the bootstrap is the slow step.
   Set SKIP_BOOTSTRAP=1 to skip it while testing.
3. Results go to outputs/: one CSV per stage, results_summary.txt and
   reconciliation_checks.csv.

## Modules
| File | Stage | Workbook tabs reproduced |
|---|---|---|
| R/00_utils.R | Settings, workbook reader, mulberry32, Box–Muller / Gamma / Beta / Dirichlet samplers, Excel-equivalent statistics, check log | Simulation Parameters | src/mulberry32.cpp | Fast generator, bootstrap counts, Fisher–Yates shuffle | — |
| R/01_data_prep.R | FDA records → W01–W17 counts | FDA DATA, FDA Record Summary |
| R/02_hoq.R | 14-step HoQ chain: base weights, CTQ fractions, CTQ-scaled weights | What Weights, HOQ, How HoQ Calculation Chain, CTQ Fractions & Scaled Weights |
| R/03_topsis.R | TOPSIS engine, Kendall τ-a, Spearman | TOPSIS_HoQ, TOPSIS_Equal Weight, TOPSIS_CTQ Weight, Kendall |
| R/04_rq1_bootstrap.R | RQ1/RH1 bootstrap (B = 10,000) and verdict | Bootstrap HoQ, RQ1 Findings |
| R/05_scores.R | Two-stage Beta scores, χ² fit, firm-level check | Config, RH2 Score Distribution Fit |
| R/06_rq2_pairs.R | RQ2/RH2: 780 pairs, paired tests, tier groups, anchor sensitivity, gap and 40-alternative spread | Pairwise Comparison Results, RQ2 Findings (4 tabs), Gap Distribution, RQ2 Spread 40-Alt |
| R/07_rq3_ctq.R | RQ3/RH3: Dirichlet draws, triple sampling, stability, worked examples | RQ3 Tier 2 Sample, RQ3 Population Sample, five RH3 example tabs |
| R/08_cv.R | Cross-validation: Train/Test split, weights, TOPSIS, summary panels | FDA DATA L:P, CV Inputs, CV Weights, CV TOPSIS, CV Summary |
| run_all.R | Runs everything, writes the summary and the check log | — |

## Reproducing the workbook's random numbers
The workbook's static simulation results came from a seeded mulberry32 generator. The R code uses the same generator, samplers and draw order:
- Scores: one stream per tier (Tier 1 seed 20261201, Tier 2 seed 20261202).
  Per company: m_i ~ Beta(30μ, 30(1−μ)), then 35 × Beta(7m_i, 7(1−m_i)),  score = min(5, 1 + floor(5x)). Beta = X/(X+Y) from Marsaglia–Tsang Gamma draws; normals from Box–Muller with the sine value cached for the next call; for a < 1, Gamma(a) = Gamma(a+1)·U^(1/a) with U drawn first.
- RQ1 bootstrap: seed 20261124; each record is assigned to the first category whose running share exceeds the uniform draw.
- RQ3 CTQ draws: Dirichlet(2,2,2) with Gamma(2) = −ln(U1·U2); seed 777001 (1,000 common draws) and 20260917 (each worked example, 10,000 draws).
- RQ3 triples: draw companies until 3 distinct, reject repeated triples; the main sample also excludes pilot triples. Seeds: Tier 2 555012 / 555013; population 555002 / 555003.
- Cross-validation: backward Fisher–Yates; first 2,260 records = Test. Seeds 20261121–20261125.

Integer outputs (scores, Train/Test labels, triples, bootstrap counts) should match exactly. Continuous values are compared to 1e-12 … 1e-6, because Excel stores 15 significant digits.

## Scope notes
- Charts and text-only report tabs are not reproduced; every figure they show comes from objects in the modules.
- 'Bootstrap Live' (the 1,000-replicate live version) is not ported; the 10,000-replicate run in module 04 is the reported analysis.
- The RH2 worked examples (Comp1-2-3, Comp5-6-8) are one call each, e.g. topsis(SCORES[c("Comp1","Comp2","Comp3"), ], HOQ$base)


## Repository structure
00_utils.R: mulberry32, the Beta and Gamma samplers, Dirichlet, file I/O
01_data_prep.R: FDA data and CFR categories, then citation counts
02_hoq.R: What weights, relationship matrix, roof and basement, How weights, CTQ fractions and scaled weights
03_topsis.R: TOPSIS and Kendall tau
04_rq1_bootstrap.R
05_scores.R: two-stage Beta scores and the χ² fit check
06_rq2_pairs.R: 780 pairs, the statistical tests, tier groups and anchor sensitivity
07_rq3_ctq.R: Dirichlet draws, the Tier 2 and population samples, and the worked examples
08_cv.R: cross-validation
run_all.R and a README (dependencies, run order)


### File 1 OF 11: r/00_Utils.r

# =============================================================================
# 00_utils.R — shared settings, workbook reader, random-number generators,
#              small statistics helpers and the reconciliation (check) log
# =============================================================================
# Every other module sources this file first (run_all.R does it for you).

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required: install.packages('readxl')")
})

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
CFG <- list(
  workbook   = Sys.getenv("HOQ_WORKBOOK", "HOQ and TOPSIS 17.xlsm"),
  out_dir    = "outputs",
  tol        = 1e-9,     # default tolerance when comparing with the workbook
  tol_loose  = 1e-6,     # for values Excel stores to 15 significant digits
  # Simulation parameters (mirrors 'Simulation Parameters'!A4:D19)
  boot_B          = 10000L, boot_seed  = 20261124,
  seed_tier1      = 20261201, seed_tier2 = 20261202,
  kappa           = 30,       ab_within  = 7,
  rq3_draws       = 1000L,    rq3_alpha  = 2, rq3_draw_seed = 777001,
  rq3_t2_seeds    = c(pilot = 555012, main = 555013),
  rq3_pop_seeds   = c(pilot = 555002, main = 555003),
  rq3_pilot_n     = 30L,      rq3_main_n = 200L,
  rq3_threshold   = 0.70,     near_tie   = 0.01,
  worked_draws    = 10000L,   worked_seed = 20260917,
  cv_seeds        = 20261121:20261125, cv_test_n = 2260L,
  rh1_threshold   = 0.95,     rh2_threshold = 0.10,
  anchor          = "Comp22", anchor_sens = c("Comp36", "Comp6"),
  ctq_equal       = c(Q = 1/3, T = 1/3, C = 1/3)
)
dir.create(CFG$out_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Workbook reader (uses the cached values Excel saved with the file)
# -----------------------------------------------------------------------------
xl_raw <- function(sheet, range) {
  # col_types = "list" keeps each cell's native type, so numbers are never
  # round-tripped through text (which would cost precision)
  d <- readxl::read_excel(CFG$workbook, sheet = sheet, range = range,
                          col_names = FALSE, col_types = "list",
                          .name_repair = "minimal")
  d <- lapply(d, function(col) {
    v <- lapply(col, function(x) if (is.null(x) || length(x) == 0) NA else x)
    isnum <- vapply(v, function(x) is.numeric(x) || is.na(x), TRUE)
    if (all(isnum)) as.numeric(unlist(v)) else
      vapply(v, function(x) if (is.na(x)[1]) "" else
        if (is.numeric(x)) format(x, digits = 15) else as.character(x), "")
  })
  as.data.frame(d, stringsAsFactors = FALSE, col.names = seq_along(d))
}
xl_num <- function(sheet, range) {
  d <- xl_raw(sheet, range)
  m <- vapply(d, function(col) suppressWarnings(as.numeric(col)), numeric(nrow(d)))
  matrix(m, nrow = nrow(d))
}
xl_chr <- function(sheet, range) {
  d <- xl_raw(sheet, range)
  m <- vapply(d, function(col) { x <- as.character(col); x[is.na(x)] <- ""; x },
              character(nrow(d)))
  matrix(m, nrow = nrow(d))
}
xl_vec_num <- function(sheet, range) as.vector(xl_num(sheet, range))
xl_vec_chr <- function(sheet, range) as.vector(xl_chr(sheet, range))

# -----------------------------------------------------------------------------
# mulberry32 random-number stream
# -----------------------------------------------------------------------------
# Uses the compiled version in src/mulberry32.cpp when Rcpp and a compiler are
# available (needed for the 384-million-draw bootstrap). Falls back to an exact
# pure-R implementation otherwise (fine for everything except the bootstrap).
.HAS_CPP <- FALSE
if (requireNamespace("Rcpp", quietly = TRUE)) {
  ok <- try(Rcpp::sourceCpp(file.path("src", "mulberry32.cpp"),
                            env = globalenv()), silent = TRUE)
  .HAS_CPP <- !inherits(ok, "try-error")
}
if (!.HAS_CPP) message("Rcpp not available: using the pure-R mulberry32 ",
                       "(exact but slow; the RQ1 bootstrap will take hours).")

# --- pure-R exact 32-bit arithmetic (doubles hold unsigned 32-bit integers) --
.U32 <- 4294967296
.to_s32 <- function(x) as.integer(ifelse(x >= 2^31, x - .U32, x))
.to_u32 <- function(i) { i <- as.numeric(i); ifelse(i < 0, i + .U32, i) }
.xor  <- function(x, y) .to_u32(bitwXor(.to_s32(x), .to_s32(y)))
.or   <- function(x, y) .to_u32(bitwOr(.to_s32(x), .to_s32(y)))
.shr  <- function(x, k) floor(x / 2^k)
.imul <- function(a, b) {             # (a * b) mod 2^32, exact in doubles
  ah <- floor(a / 65536); al <- a %% 65536
  bh <- floor(b / 65536); bl <- b %% 65536
  (al * bl + ((ah * bl + al * bh) %% 65536) * 65536) %% .U32
}
.mb32_next_R <- function(a) {
  a <- (a + 0x6D2B79F5) %% .U32
  t <- a
  t <- .imul(.xor(t, .shr(t, 15)), .or(t, 1))
  t <- .xor(t, (t + .imul(.xor(t, .shr(t, 7)), .or(t, 61))) %% .U32)
  list(a = a, u = .xor(t, .shr(t, 14)) / .U32)
}

# new_rng(seed): a stream object with $u() (one uniform) and $un(n) (n uniforms)
new_rng <- function(seed) {
  env <- new.env()
  env$state <- as.numeric(seed) %% .U32
  env$un <- function(n) {
    if (.HAS_CPP) {
      r <- mb32_draw(env$state, as.integer(n)); env$state <- r$state; r$u
    } else {
      u <- numeric(n)
      for (i in seq_len(n)) {
        r <- .mb32_next_R(env$state); env$state <- r$a; u[i] <- r$u
      }
      u
    }
  }
  env$u <- function() env$un(1L)
  env
}

# -----------------------------------------------------------------------------
# Variate generators (draw order matches the workbook runs exactly)
# -----------------------------------------------------------------------------
# Standard normal: Box–Muller with the sine value cached and returned on the
# next call (the cache lives in the sampler, so it persists across companies).
new_samplers <- function(rng) {
  env <- new.env()
  env$spare <- NULL
  env$norm <- function() {
    if (!is.null(env$spare)) { s <- env$spare; env$spare <- NULL; return(s) }
    u1 <- rng$u(); if (u1 == 0) u1 <- 1e-300
    u2 <- rng$u()
    R <- sqrt(-2 * log(u1))
    env$spare <- R * sin(2 * pi * u2)
    R * cos(2 * pi * u2)
  }
  # Gamma(a, 1): Marsaglia–Tsang; for a < 1 uses Gamma(a+1) * U^(1/a), with U
  # drawn BEFORE the Gamma(a+1) variate.
  env$gamma <- function(a) {
    if (a < 1) { u <- rng$u(); return(env$gamma(a + 1) * u^(1 / a)) }
    d <- a - 1/3; cc <- 1 / sqrt(9 * d)
    repeat {
      repeat { x <- env$norm(); v <- 1 + cc * x; if (v > 0) break }
      v <- v^3
      u <- rng$u()
      if (u < 1 - 0.0331 * x^4) return(d * v)
      if (log(u) < 0.5 * x^2 + d * (1 - v + log(v))) return(d * v)
    }
  }
  env$beta <- function(a, b) { x <- env$gamma(a); y <- env$gamma(b); x / (x + y) }
  env
}

# Dirichlet(2,2,2) used for the CTQ scenarios: Gamma(2,1) = -ln(U1 * U2)
dirichlet2_draws <- function(seed, n) {
  rng <- new_rng(seed)
  u <- rng$un(6L * n)
  m <- matrix(u, ncol = 6, byrow = TRUE)
  g <- cbind(-log(m[, 1] * m[, 2]), -log(m[, 3] * m[, 4]), -log(m[, 5] * m[, 6]))
  w <- g / rowSums(g)
  colnames(w) <- c("Q", "T", "C")
  w
}

# -----------------------------------------------------------------------------
# Excel-equivalent statistics helpers
# -----------------------------------------------------------------------------
pinc     <- function(x, p) unname(quantile(x, p, type = 7))   # PERCENTILE.INC / QUARTILE.INC
rank_eq  <- function(x) vapply(x, function(v) 1L + sum(x > v), 1L)  # RANK.EQ, descending
avedev   <- function(x) mean(abs(x - mean(x)))
excel_skew <- function(x) { n <- length(x); s <- sd(x)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / s)^3) }
excel_kurt <- function(x) { n <- length(x); s <- sd(x)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - mean(x)) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3)) }
modal_rank <- function(r) { tb <- table(r); as.integer(names(tb)[which.max(tb)]) }
fmt_pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)

# -----------------------------------------------------------------------------
# Reconciliation log: every module compares its results with the workbook
# -----------------------------------------------------------------------------
.CHECKS <- data.frame(module = character(), check = character(),
                      max_abs_diff = numeric(), tol = numeric(),
                      pass = logical(), stringsAsFactors = FALSE)
check <- function(module, name, computed, reference, tol = CFG$tol) {
  if (is.character(computed) || is.character(reference)) {
    d <- as.numeric(sum(as.character(computed) != as.character(reference)))
    pass <- d == 0
  } else {
    d <- suppressWarnings(max(abs(as.numeric(computed) - as.numeric(reference)),
                              na.rm = TRUE))
    pass <- is.finite(d) && d <= tol
  }
  .CHECKS[nrow(.CHECKS) + 1, ] <<- list(module, name, d, tol, pass)
  cat(sprintf("  [%s] %-58s max diff = %.3g\n", if (pass) "PASS" else "FAIL",
              name, d))
  invisible(pass)
}
check_summary <- function() {
  cat("\n================ RECONCILIATION WITH WORKBOOK ================\n")
  cat(sprintf("%d checks: %d passed, %d failed\n", nrow(.CHECKS),
              sum(.CHECKS$pass), sum(!.CHECKS$pass)))
  if (any(!.CHECKS$pass)) print(.CHECKS[!.CHECKS$pass, ], row.names = FALSE)
  write.csv(.CHECKS, file.path(CFG$out_dir, "reconciliation_checks.csv"),
            row.names = FALSE)
  invisible(.CHECKS)
}
save_csv <- function(x, name) write.csv(x, file.path(CFG$out_dir, name),
                                        row.names = FALSE)




# File 3: R/01_data_prep.r                                        

# =============================================================================
# 01_data_prep.R — FDA inspection records → WHAT (customer requirement) counts
# =============================================================================
# Workbook equivalents: 'FDA DATA', 'FDA Record Summary', 'CV Inputs'!A5:C29
# Output: object DATA (list) used by every later module.

cat("\n== 01 Data preparation ==\n")

N_FDA <- 38400L
fda <- xl_raw("FDA DATA", sprintf("A2:Q%d", N_FDA + 1))   # data rows only
names(fda) <- c("cfr_number", "act_cfr", "part", "inspection_id", "fei",
                "legal_name", "end_date", "program", "short_desc", "long_desc",
                "category", paste0("cv_rep", 1:5), "company_tier")

# WHAT requirements W01–W25: id, CTQ family, description (HOQ!A9:C33)
what <- xl_chr("HOQ", "A9:C33")
what <- data.frame(id = what[, 1], family = what[, 2], desc = what[, 3],
                   stringsAsFactors = FALSE)
what$ctq <- ifelse(grepl("^Quality", what$family), "Q",
            ifelse(grepl("^Delivery", what$family), "T", "C"))

# FDA category key for W01–W17 exactly as written in FDA DATA column K
# ('CV Inputs'!B5:B21). The labels on 'FDA Record Summary' are shortened for two
# categories, so the CV Inputs keys are used for matching.
cat_key <- xl_vec_chr("CV Inputs", "B5:B21")

count_what <- function(category_vec) {
  vapply(cat_key, function(k) sum(category_vec == k, na.rm = TRUE), 1L)
}
what_counts <- count_what(fda$category)
names(what_counts) <- what$id[1:17]

# Fixed (non-FDA) importances for the Time and Cost WHATs W18–W25 (HOQ!H26:H33)
fixed_importance <- xl_vec_num("HOQ", "H26:H33")

DATA <- list(fda = fda, what = what, cat_key = cat_key,
             what_counts = what_counts, fixed_importance = fixed_importance,
             N = sum(what_counts))

# ---- reconciliation --------------------------------------------------------
check("01", "Total FDA records = 38,400", DATA$N, N_FDA, 0)
check("01", "WHAT counts vs 'FDA Record Summary'!C4:C20", what_counts,
      xl_vec_num("FDA Record Summary", "C4:C20"), 0)
check("01", "Fixed Time/Cost importances = 0.25", fixed_importance, rep(0.25, 8), 0)

save_csv(data.frame(what_id = names(what_counts), category = cat_key,
                    count = what_counts), "01_what_counts.csv")


File 4 of 11: R/02_hoq.R

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

### File 5 of 11: R/03_topsis.R

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


### File 6 of 11: R/04_rq1_bootstrap.R

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

### File 7 of 11: R/05_scores.R

# =============================================================================
# 05_scores.R — synthetic supplier scores (two-stage Beta model) and the
#               score-distribution fit check
# =============================================================================
# Workbook equivalents: Config!A8:AU47 (scores, firm means), 'Simulation
#   Parameters'!A12:D15, 'RH2 Score Distribution Fit'.
#
# Model, per tier (Tier 1 = Comp1–Comp8, Beta(5,2); Tier 2 = Comp9–Comp40,
# Beta(4,3)); one mulberry32 stream per tier, companies drawn in order:
#   Stage 1  firm mean      m_i  ~ Beta(κ·μ, κ·(1−μ)),  μ = α/(α+β), κ = 30
#   Stage 2  35 criteria    x_ij ~ Beta(7·m_i, 7·(1−m_i))
#   Score                   s_ij = min(5, 1 + floor(5·x_ij))

cat("\n== 05 Synthetic scores ==\n")

companies <- data.frame(id = xl_vec_chr("Config", "A8:A47"),
                        tier = xl_vec_chr("Config", "B8:B47"),
                        alpha = xl_vec_num("Config", "D8:D47"),
                        beta = xl_vec_num("Config", "E8:E47"),
                        stringsAsFactors = FALSE)

generate_scores <- function(companies, kappa = CFG$kappa, ab = CFG$ab_within) {
  n_how <- length(HOQ$how_id)
  S <- matrix(NA_integer_, nrow(companies), n_how,
              dimnames = list(companies$id, HOQ$how_id))
  m <- setNames(numeric(nrow(companies)), companies$id)
  streams <- list("Tier 1" = CFG$seed_tier1, "Tier 2" = CFG$seed_tier2)
  for (tier in names(streams)) {
    smp <- new_samplers(new_rng(streams[[tier]]))
    for (i in which(companies$tier == tier)) {
      mu <- companies$alpha[i] / (companies$alpha[i] + companies$beta[i])
      m[i] <- smp$beta(kappa * mu, kappa * (1 - mu))
      for (j in seq_len(n_how)) {
        x <- smp$beta(ab * m[i], ab * (1 - m[i]))
        S[i, j] <- min(5L, 1L + as.integer(floor(5 * x)))
      }
    }
  }
  list(scores = S, firm_mean = m)
}

SC <- generate_scores(companies)
SCORES <- SC$scores

# ---- score-distribution fit ('RH2 Score Distribution Fit') ----------------
# Expected share of score k integrates P(score = k | m) over the firm-mean
# distribution on a 200-point midpoint grid.
score_fit <- function(tier, a, b, grid_n = 200, kappa = CFG$kappa, ab = CFG$ab_within) {
  idx <- companies$tier == tier
  s <- as.vector(SCORES[idx, ]); n <- length(s)
  mu <- a / (a + b)
  mg <- (seq_len(grid_n) - 0.5) / grid_n
  wm <- pbeta(mg + 0.5 / grid_n, kappa * mu, kappa * (1 - mu)) -
        pbeta(mg - 0.5 / grid_n, kappa * mu, kappa * (1 - mu))
  pk <- sapply(1:5, function(k) pbeta(k / 5, ab * mg, ab * (1 - mg)) -
                                pbeta((k - 1) / 5, ab * mg, ab * (1 - mg)))
  e_m <- as.vector(pk %*% (1:5)); v_m <- as.vector(pk %*% (1:5)^2) - e_m^2
  obs <- tabulate(s, 5); exp_share <- as.vector(wm %*% pk)
  exp_cnt <- exp_share * n
  chi <- sum((obs - exp_cnt)^2 / exp_cnt)
  p1 <- pbeta((1:5) / 5, a, b) - pbeta((0:4) / 5, a, b)
  firm_avg <- rowMeans(SCORES[idx, ])
  list(table = data.frame(score = 1:5, observed = obs, obs_share = obs / n,
                          exp_share = exp_share, exp_count = exp_cnt,
                          diff_pts = (obs / n - exp_share) * 100,
                          chi_contrib = (obs - exp_cnt)^2 / exp_cnt),
       n = n, companies = sum(idx),
       obs_mean = mean(s), ex



### File 8 of 11: R/06_rq2_pairs.R

# =============================================================================
# 06_rq2_pairs.R — RQ2 / RH2: does HoQ weighting separate alternatives more
#                  than equal weighting?
# =============================================================================
# Workbook equivalents: 'Pairwise Comparison Results', 'RQ2 Findings T2-T2'
#   (primary), 'RQ2 Findings T1-T2', 'RQ2 Findings T1-T1', 'RQ2 Findings All
#   Pairs', 'Gap Distribution', 'RQ2 Spread 40-Alt'.
# Design: all C(40,2) = 780 pairs; each pair is solved as a 3-alternative TOPSIS
# set {A, B, anchor}, anchor = Comp22's score profile, under HoQ base weights
# and equal weights (1/35).

cat("\n== 06 RQ2 pairwise separation ==\n")

ids <- rownames(SCORES)
.ord <- match(ids, sort(ids, method = "radix"))   # C-locale string order ("Comp1" < "Comp10" < "Comp2")
pairs <- do.call(rbind, lapply(seq_along(ids), function(i)
  do.call(rbind, lapply(seq_along(ids), function(j)
    if (.ord[i] < .ord[j]) data.frame(A = ids[i], B = ids[j], stringsAsFactors = FALSE)))))

tier_of <- setNames(sub("Tier ", "", companies$tier), companies$id)

pair_ccs <- function(anchor, weights) {
  t(vapply(seq_len(nrow(pairs)), function(k) {
    M <- SCORES[c(pairs$A[k], pairs$B[k], anchor), ]
    rownames(M) <- c("A", "B", "anchor")
    topsis(M, weights)$cc[1:2]
  }, numeric(2)))
}

build_pairs <- function(anchor) {
  h <- pair_ccs(anchor, HOQ$base); e <- pair_ccs(anchor, HOQ$equal)
  P <- data.frame(pairs, hoq_cc_A = h[, 1], hoq_cc_B = h[, 2],
                  eq_cc_A = e[, 1], eq_cc_B = e[, 2], stringsAsFactors = FALSE)
  P$hoq_gap <- P$hoq_cc_A - P$hoq_cc_B
  P$eq_gap  <- P$eq_cc_A - P$eq_cc_B
  P$pct_change <- ifelse(P$eq_gap == 0, NA, (P$hoq_gap - P$eq_gap) / P$eq_gap)
  P$concordance <- ifelse(P$hoq_gap == 0 | P$eq_gap == 0, "tied",
                   ifelse(sign(P$hoq_gap) == sign(P$eq_gap), "concordant", "discordant"))
  P$pct_change_mag <- ifelse(P$eq_gap == 0, NA, (abs(P$hoq_gap) - abs(P$eq_gap)) / abs(P$eq_gap))
  ta <- tier_of[P$A]; tb <- tier_of[P$B]
  P$tier_group <- paste0("T", pmin(ta, tb), "\u2013T", pmax(ta, tb))
  P$delta_abs_gap <- abs(P$hoq_gap) - abs(P$eq_gap)
  P
}

# rh2_stats(): the RQ2 Findings panels for one pair group
rh2_stats <- function(P, group) {
  sel <- if (group == "All pairs") rep(TRUE, nrow(P)) else P$tier_group == group
  g <- abs(P$hoq_gap[sel]); e <- abs(P$eq_gap[sel])
  k <- P$pct_change_mag[sel]; kk <- k[!is.na(k)]
  n <- length(kk); d <- g - e
  inc <- sum(kk > 0); dec <- sum(kk < 0)
  md <- mean(d); sdd <- sd(d); se <- sdd / sqrt(n); tq <- qt(0.975, n - 1)
  out <- list(
    group = group, n = n,
    mean_spread = c(hoq = mean(g), equal = mean(e)),
    median_spread = c(hoq = median(g), equal = median(e)),
    sd_spread = c(hoq = sd(g), equal = sd(e)),
    max_spread = c(hoq = max(g), equal = max(e)),
    mean_change = mean(g) / mean(e) - 1, median_change = median(g) / median(e) - 1,
    n_increase = inc, n_decrease = dec, n_over_10pct = sum(kk > 0.1),
    n_discordant = sum(P$concordance[sel] == "discordant"),
    median_pair_change = median(kk),
    mean_diff = md, sd_diff = sdd, se = se, t = md / se,
    p_t = 2 * pt(abs(md / se), n - 1, lower.tail = FALSE),
    p_sign = 2 * pbinom(min(inc, dec), n, 0.5),
    cohen_d = md / sdd, ci = c(md - tq * se, md + tq * se))
  out$ci_pct <- out$ci / mean(e)
  tests <- c(mean = out$mean_change, median = out$median_change,
             median_pair = out$median_pair_change,
             ci_low = out$ci_pct[1], ci_high = out$ci_pct[2])
  out$tests <- tests; out$tests_passed <- sum(tests > CFG$rh2_threshold)
  out$verdict <- if (out$ci_pct[1] > CFG$rh2_threshold) "RH2 SUPPORTED" else
                 if (out$ci_pct[2] < CFG$rh2_threshold) "RH2 REJECTED" else
                 "RH2 NOT ESTABLISHED (95% CI straddles threshold)"
  out
}

PAIRS <- build_pairs(CFG$anchor)
SENS  <- lapply(setNames(CFG$anchor_sens, CFG$anchor_sens), build_pairs)

groups <- c("T2\u2013T2", "T1\u2013T2", "T1\u2013T1", "All pairs")
RQ2 <- lapply(setNames(groups, groups), function(g) rh2_stats(PAIRS, g))
RQ2_SENS <- lapply(SENS, function(P) rh2_stats(P, "T2\u2013T2"))
for (g in groups) cat(sprintf("   %-9s n=%3d  mean %+.1f%%  median %+.1f%%  95%% CI %+.1f%% to %+.1f%%  -> %s\n",
  g, RQ2[[g]]$n, 100 * RQ2[[g]]$mean_change, 100 * RQ2[[g]]$median_change,
  100 * RQ2[[g]]$ci_pct[1], 100 * RQ2[[g]]$ci_pct[2], RQ2[[g]]$verdict))
for (a in names(RQ2_SENS)) cat(sprintf("   anchor %-6s T2-T2 mean change %+.1f%%\n", a,
                                       100 * RQ2_SENS[[a]]$mean_change))

# ---- Gap distribution (|HoQ gap|, signed gap) -----------------------------
gap_summary <- with(PAIRS, list(
  abs_mean = mean(abs(hoq_gap)), abs_median = median(abs(hoq_gap)),
  abs_sd_pop = sqrt(mean((abs(hoq_gap) - mean(abs(hoq_gap)))^2)),
  abs_max = max(abs(hoq_gap)), n_below_0.01 = sum(abs(hoq_gap) < 0.01),
  signed = c(mean = mean(hoq_gap), median = median(hoq_gap), sd = sd(hoq_gap),
             skew = excel_skew(hoq_gap), kurt = excel_kurt(hoq_gap),
             min = min(hoq_gap), max = max(hoq_gap)),
  wider = sum(abs(hoq_gap) > abs(eq_gap)), narrower = sum(abs(hoq_gap) < abs(eq_gap)),
  equal = sum(abs(hoq_gap) == abs(eq_gap)),
  histogram = table(cut(abs(hoq_gap), breaks = seq(0, ceiling(max(abs(hoq_gap)) / 0.025) * 0.025, 0.025),
                        right = FALSE, include.lowest = TRUE))))

# ---- Spread across all 40 alternatives in one TOPSIS run -------------------
spread40 <- function(w) {
  cc <- topsis(SCORES, w)$cc
  c(range = max(cc) - min(cc), sd = sd(cc), iqr = pinc(cc, 0.75) - pinc(cc, 0.25),
    avedev = avedev(cc), cv = sd(cc) / mean(cc),
    gini_md = sum(abs(outer(cc, cc, "-"))) / 2 / choose(length(cc), 2))
}
SPREAD40 <- rbind(hoq = spread40(HOQ$base), equal = spread40(HOQ$equal))

# ---- reconciliation --------------------------------------------------------
ref <- xl_raw("Pairwise Comparison Results", "A2:AU781")
check("06", "Pair list vs 'Pairwise Comparison Results'!A:B",
      paste(PAIRS$A, PAIRS$B), paste(ref[[1]], ref[[2]]))
check("06", "HoQ & equal CCs (anchor Comp22) vs C:F",
      as.matrix(PAIRS[, 3:6]), sapply(ref[3:6], as.numeric), 1e-12)
check("06", "Anchor sensitivity Comp36 vs AM:AP",
      as.matrix(SENS$Comp36[, 3:6]), sapply(ref[39:42], as.numeric), 1e-12)
check("06", "Anchor sensitivity Comp6 vs AR:AU",
      as.matrix(SENS$Comp6[, 3:6]), sapply(ref[44:47], as.numeric), 1e-12)
check("06", "Concordance labels vs J", PAIRS$concordance, as.character(ref[[10]]))
tabs <- c("T2\u2013T2" = "RQ2 Findings T2-T2", "T1\u2013T2" = "RQ2 Findings T1-T2",
          "T1\u2013T1" = "RQ2 Findings T1-T1", "All pairs" = "RQ2 Findings All Pairs")
for (g in names(tabs)) {
  s <- RQ2[[g]]; tab <- tabs[[g]]
  check("06", paste0(g, ": mean/median/SD/max spread vs B5:C8"),
        rbind(s$mean_spread, s$median_spread, s$sd_spread, s$max_spread),
        xl_num(tab, "B5:C8"), 1e-12)
  check("06", paste0(g, ": per-pair counts vs B11:B15"),
        c(s$n_increase, s$n_decrease, s$n_over_10pct, s$n_discordant, s$n),
        xl_vec_num(tab, "B11:B15"), 0)
  check("06", paste0(g, ": paired tests vs B19:B27"),
        c(s$mean_diff, s$sd_diff, s$se, s$t, s$p_t, s$p_sign, s$cohen_d, s$ci),
        xl_vec_num(tab, "B19:B27"), 1e-9)
  check("06", paste0(g, ": five RH2 operationalisations vs B31:B35"),
        s$tests, xl_vec_num(tab, "B31:B35"), 1e-9)
}

save_csv(PAIRS, "06_rq2_pairs_anchor_Comp22.csv")
save_csv(do.call(rbind, lapply(RQ2, function(s) data.frame(
  group = s$group, n = s$n, mean_hoq = s$mean_spread[1], mean_equal = s$mean_spread[2],
  mean_change = s$mean_change, median_change = s$median_change,
  median_pair_change = s$median_pair_change, n_discordant = s$n_discordant,
  t = s$t, p_t = s$p_t, p_sign = s$p_sign, cohen_d = s$cohen_d,
  ci_low_pct = s$ci_pct[1], ci_high_pct = s$ci_pct[2],
  tests_passed = s$tests_passed, verdict = s$verdict))), "06_rq2_summary.csv")


### File 9 of 11: R/07_rq3_ctq.R

# =============================================================================
# 07_rq3_ctq.R — RQ3 / RH3: stability of the top-ranked alternative under
#                uncertainty in the CTQ (Quality / Time / Cost) weighting
# =============================================================================
# Workbook equivalents: 'RQ3 Tier 2 Sample' (primary), 'RQ3 Population Sample'
#   (secondary), and the five RH3 worked-example tabs.
# Design: 1,000 common Dirichlet(2,2,2) CTQ draws; a 30-triple pilot sizes the
# study, then an independent 200-triple main sample (no triple repeats the
# pilot). For each triple: baseline winner at equal CTQ shares; stability share
# = fraction of draws in which that winner keeps rank 1; near-tie share =
# fraction of draws with top − runner-up CC < 0.01.

cat("\n== 07 RQ3 CTQ-weight robustness ==\n")

# Closeness coefficients for one alternative set under many weight vectors.
# Because every weight is positive, PIS_j = w_j·max_i r_ij and NIS_j = w_j·min_i r_ij,
# so S± reduce to sqrt(Σ_j w_j² (r_ij − r_max/min,j)²) — identical to the
# long-form TOPSIS, computed for all draws at once.
cc_many <- function(alts, Wmat) {
  M <- SCORES[alts, , drop = FALSE]
  R <- sweep(M, 2, sqrt(colSums(M^2)), "/")
  dp <- (t(R) - apply(R, 2, max))^2                 # 35 x n_alt
  dm <- (t(R) - apply(R, 2, min))^2
  W2 <- Wmat^2
  sp <- sqrt(W2 %*% dp); sm <- sqrt(W2 %*% dm)
  cc <- sm / (sp + sm); colnames(cc) <- alts; cc
}
ctq_weight_matrix <- function(D) t(apply(D, 1, function(d) ctq_weights(d[1], d[2], d[3])))
w_equal_ctq <- ctq_weights(1/3, 1/3, 1/3)

evaluate_triple <- function(alts, Wd) {
  cc0 <- cc_many(alts, matrix(w_equal_ctq, 1))[1, ]
  s0 <- sort(cc0, decreasing = TRUE)
  win <- alts[which.max(cc0)]
  cc <- cc_many(alts, Wd)
  top <- max.col(cc, ties.method = "first")
  srt <- t(apply(cc, 1, sort, decreasing = TRUE))
  data.frame(alt1 = alts[1], alt2 = alts[2], alt3 = alts[3], baseline_winner = win,
             baseline_margin = s0[1] - s0[2],
             stability = mean(alts[top] == win),
             near_tie = mean(srt[, 1] - srt[, 2] < CFG$near_tie),
             stringsAsFactors = FALSE)
}

# Triple sampling: draw companies until 3 distinct, sort, reject repeats; the
# pilot and main samples share the "already used" set.
sample_triples <- function(pool, seeds, n_pilot = CFG$rq3_pilot_n, n_main = CFG$rq3_main_n) {
  num <- as.integer(sub("Comp", "", pool)); seen <- character(0); out <- list()
  for (stage in c("pilot", "main")) {
    rng <- new_rng(seeds[[stage]]); n <- if (stage == "pilot") n_pilot else n_main; k <- 0
    while (k < n) {
      st <- integer(0)
      while (length(st) < 3) {
        x <- num[floor(rng$u() * length(num)) + 1]
        if (!(x %in% st)) st <- c(st, x)
      }
      st <- sort(st); key <- paste(st, collapse = "-")
      if (key %in% seen) next
      seen <- c(seen, key); k <- k + 1
      out[[length(out) + 1]] <- data.frame(stage = if (stage == "pilot") "Pilot" else "Main",
                                           a = st[1], b = st[2], c = st[3])
    }
  }
  tt <- do.call(rbind, out)
  data.frame(stage = tt$stage, alt1 = paste0("Comp", tt$a), alt2 = paste0("Comp", tt$b),
             alt3 = paste0("Comp", tt$c), stringsAsFactors = FALSE)
}

rh3_panels <- function(R) {
  pil <- R$stability[R$stage == "Pilot"]; m <- R[R$stage == "Main", ]
  g <- m$stability; n <- length(g); sdp <- sd(pil); se <- sd(g) / sqrt(n)
  tq <- qt(0.975, n - 1); q1 <- pinc(m$baseline_margin, 0.25); q3 <- pinc(m$baseline_margin, 0.75)
  band <- cut(m$baseline_margin, c(-Inf, q1, q3, Inf), labels = c("Q1", "Q2-Q3", "Q4"))
  list(pilot_mean = mean(pil), pilot_sd = sdp,
       n_required_precision = ceiling((1.96 * sdp / 0.03)^2),
       n_required_power = ceiling(7.849 * sdp^2 / (5 / 100)^2),
       n = n, mean = mean(g), sd = sd(g), se = se,
       ci = c(mean(g) - tq * se, mean(g) + tq * se), median = median(g),
       min = min(g), max = max(g), n_fully_stable = sum(g == 1),
       n_below = sum(g < CFG$rq3_threshold), share_below = mean(g < CFG$rq3_threshold),
       t = (mean(g) - CFG$rq3_threshold) / se,
       p_one_tailed = pt((mean(g) - CFG$rq3_threshold) / se, n - 1, lower.tail = FALSE),
       verdict = if (mean(g) - tq * se >= CFG$rq3_threshold) "RH3 SUPPORTED" else "RH3 NOT SUPPORTED",
       by_margin = data.frame(band = levels(band), cutoff = c(q1, q3, max(m$baseline_margin)),
                              triples = as.vector(table(band)),
                              mean_stability = as.vector(tapply(g, band, mean)),
                              below_threshold = as.vector(tapply(g < CFG$rq3_threshold, band, sum))))
}

DRAWS <- dirichlet2_draws(CFG$rq3_draw_seed, CFG$rq3_draws)
WD <- ctq_weight_matrix(DRAWS)

run_rq3 <- function(pool, seeds) {
  tr <- sample_triples(pool, seeds)
  res <- do.call(rbind, lapply(seq_len(nrow(tr)), function(i)
    evaluate_triple(c(tr$alt1[i], tr$alt2[i], tr$alt3[i]), WD)))
  R <- cbind(stage = tr$stage, res, stringsAsFactors = FALSE)
  list(results = R, panels = rh3_panels(R))
}

RQ3 <- list(
  tier2      = run_rq3(companies$id[companies$tier == "Tier 2"], CFG$rq3_t2_seeds),
  population = run_rq3(companies$id, CFG$rq3_pop_seeds))
for (k in names(RQ3)) with(RQ3[[k]]$panels, cat(sprintf(
  "   %-10s mean stability %s (95%% CI %s to %s), %d of %d below 70%%, %d fully stable -> %s\n",
  k, fmt_pct(mean), fmt_pct(ci[1]), fmt_pct(ci[2]), n_below, n, n_fully_stable, verdict)))

# ---- Worked examples (10,000 draws each) ------------------------------------
worked_tabs <- c("RH3 T2 Mean (C14-C16-C22)", "RH3 T2 Typical (C28-C29-C37)",
                 "RH3 Best (C21-C36-C40)", "RH3 Worst (C13-C14-C26)",
                 "RH3 Flip (C27-C31-C34)")
worked_example <- function(tab) {
  alts <- xl_vec_chr(tab, "C8:E8")
  seed <- CFG$worked_seed; n <- CFG$worked_draws
  D <- dirichlet2_draws(seed, n)
  cc <- cc_many(alts, ctq_weight_matrix(D))
  winner <- alts[max.col(cc, ties.method = "first")]
  base <- cc_many(alts, matrix(w_equal_ctq, 1))[1, ]
  bw <- alts[which.max(base)]
  stab <- mean(winner == bw)
  list(tab = tab, alts = alts, draws = D, cc = cc, winner = winner,
       panel_c = data.frame(alt = alts, times_rank1 = sapply(alts, function(a) sum(winner == a)),
                            share = sapply(alts, function(a) mean(winner == a)),
                            mean_cc = colMeans(cc), sd_cc = apply(cc, 2, sd),
                            min_cc = apply(cc, 2, min)),
       baseline_cc = base, baseline_winner = bw, stability = stab,
       ci = stab + c(-1, 1) * 1.96 * sqrt(stab * (1 - stab) / n))
}
WORKED <- lapply(setNames(worked_tabs, worked_tabs), worked_example)
for (w in WORKED) cat(sprintf("   %-30s winner %s, stability %s\n", w$tab, w$baseline_winner,
                              fmt_pct(w$stability)))

# ---- reconciliation --------------------------------------------------------
check("07", "Dirichlet draws vs 'RQ3 Tier 2 Sample'!T5:V1004", DRAWS,
      xl_num("RQ3 Tier 2 Sample", "T5:V1004"), 1e-12)
rq3_tabs <- c(tier2 = "RQ3 Tier 2 Sample", population = "RQ3 Population Sample")
for (nm in names(rq3_tabs)) {
  k <- rq3_tabs[[nm]]
  R <- RQ3[[nm]]$results; ref <- xl_raw(k, "A53:H282")
  check("07", paste0(k, ": sampled triples + winners (A:E)"),
        apply(R[, 1:5], 1, paste, collapse = "|"),
        apply(sapply(ref[1:5], as.character), 1, paste, collapse = "|"))
  check("07", paste0(k, ": margin, stability, near-tie (F:H)"),
        as.matrix(R[, 6:8]), sapply(ref[6:8], as.numeric), 1e-9)
  p <- RQ3[[nm]]$panels
  check("07", paste0(k, ": mean, SD, SE, CI (B27:B31)"),
        c(p$mean, p$sd, p$se, p$ci), xl_vec_num(k, "B27:B31"), 1e-12)
  check("07", paste0(k, ": t and one-tailed p (B41, B43)"),
        c(p$t, p$p_one_tailed), xl_vec_num(k, "B41:B43")[c(1, 3)], 1e-9)
}
for (w in WORKED) {
  check("07", paste0(w$tab, ": draws"), w$draws, xl_num(w$tab, "B60:D10059"), 1e-12)
  check("07", paste0(w$tab, ": Panel C (rank-1 counts, share, CC stats)"),
        as.matrix(w$panel_c[, -1]), xl_num(w$tab, "B31:F33"), 1e-9)
  check("07", paste0(w$tab, ": stability share (B43)"), w$stability,
        xl_vec_num(w$tab, "B43"), 1e-12)
}

save_csv(RQ3$tier2$results, "07_rq3_tier2_sample.csv")
save_csv(RQ3$population$results, "07_rq3_population_sample.csv")
save_csv(data.frame(draw = seq_len(nrow(DRAWS)), DRAWS), "07_rq3_dirichlet_draws.csv")



### File 10 of 11: R/08_cv.R  ---NOT USED IN PRAXIS

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


  
### File 11 of 11: Run_All.R

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


// mulberry32 pseudo-random number generator and the two hot loops of the
// pipeline (bootstrap multinomial counts, Fisher-Yates shuffle).
// Compiled on the fly by Rcpp::sourceCpp() from R/00_utils.R.
//
// mulberry32 (Tommy Ettinger, public domain), identical to the generator used
// to produce the static simulation results stored in the workbook:
//   a += 0x6D2B79F5; t = a;
//   t = imul(t ^ (t >>> 15), t | 1);
//   t ^= t + imul(t ^ (t >>> 7), t | 61);
//   u = ((t ^ (t >>> 14)) >>> 0) / 2^32
// uint32_t arithmetic wraps modulo 2^32, which is exactly Math.imul's low word.

#include <Rcpp.h>
#include <cstdint>
using namespace Rcpp;

static inline double mb32_next(uint32_t &a) {
  a += 0x6D2B79F5u;
  uint32_t t = a;
  t = (t ^ (t >> 15)) * (t | 1u);
  t ^= t + (t ^ (t >> 7)) * (t | 61u);
  return (double)(t ^ (t >> 14)) / 4294967296.0;
}

// Draw n uniforms from a stream whose internal state is `state` (a double
// holding an unsigned 32-bit integer). Returns the uniforms and the new state.
// [[Rcpp::export]]
List mb32_draw(double state, int n) {
  uint32_t a = (uint32_t)state;
  NumericVector u(n);
  for (int i = 0; i < n; ++i) u[i] = mb32_next(a);
  return List::create(_["u"] = u, _["state"] = (double)a);
}

// RQ1 bootstrap: B replicates, each drawing N records with replacement.
// A record falls in category j (1-based) when u < cum[j] for the first such j,
// with the last category taking everything else. cum is the running sum of
// count/N in WHAT order (W01..W17). Returns a B x K matrix of counts.
// [[Rcpp::export]]
IntegerMatrix mb32_boot_counts(double seed, int B, int N, NumericVector cum) {
  uint32_t a = (uint32_t)seed;
  int K = cum.size();
  IntegerMatrix out(B, K);
  for (int b = 0; b < B; ++b) {
    for (int i = 0; i < N; ++i) {
      double u = mb32_next(a);
      int j = 0;
      while (j < K - 1 && u >= cum[j]) ++j;
      out(b, j) += 1;
    }
  }
  return out;
}

// Backward Fisher-Yates shuffle of 0..N-1 (for i = N-1 down to 1,
// j = floor(u * (i + 1)), swap). Returns the shuffled 0-based indices.
// [[Rcpp::export]]
IntegerVector mb32_fisher_yates(double seed, int N) {
  uint32_t a = (uint32_t)seed;
  IntegerVector x(N);
  for (int i = 0; i < N; ++i) x[i] = i;
  for (int i = N - 1; i > 0; --i) {
    int j = (int)std::floor(mb32_next(a) * (i + 1));
    int tmp = x[i]; x[i] = x[j]; x[j] = tmp;
  }
  return x;
}


## Software

- RStudio
- Microsoft Excel

 

The data can be shared. It is not confidential or proprietary.

Chokshi, B. (2026). A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty. Praxis, George Washington University.

All rights reserved.
