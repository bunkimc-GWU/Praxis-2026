# =============================================================================
# 00_CONFIG_FROM_EXCEL.R — IMPORT ALL INPUTS FROM EXCEL TEMPLATE
# =============================================================================

library(readxl)

EXCEL_INPUT_PATH <- "../data/MCDM_Input_Template.xlsx"

if (!file.exists(EXCEL_INPUT_PATH)) {
  stop("Input template not found: ", EXCEL_INPUT_PATH,
       "\nPlace MCDM_Input_Template.xlsx in the data/ folder.")
}

cat("Loading inputs from Excel template...\n")

# ── 1. CTQ SETTINGS ──────────────────────────────────────────────────────────
ctq_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "CTQ Settings", col_names = FALSE)

CTQ_BASE <- list(
  Q = as.numeric(ctq_raw[[2]][5]),
  T = as.numeric(ctq_raw[[2]][6]),
  C = as.numeric(ctq_raw[[2]][7])
)

ctq_sum <- CTQ_BASE$Q + CTQ_BASE$T + CTQ_BASE$C
if (abs(ctq_sum - 1.0) > 0.001) {
  stop(sprintf("CTQ weights sum to %.4f, not 1.0.", ctq_sum))
}

SIM_PARAMS <- list(
  bootstrap_n        = as.integer(ctq_raw[[2]][13]),
  monte_carlo_n      = as.integer(ctq_raw[[2]][14]),
  dirichlet_alpha    = c(as.numeric(ctq_raw[[2]][15]),
                          as.numeric(ctq_raw[[2]][16]),
                          as.numeric(ctq_raw[[2]][17])),
  random_seed        = as.integer(ctq_raw[[2]][18]),
  ci_level           = as.numeric(ctq_raw[[2]][19]),
  sensitivity_levels = c(-0.50, -0.25, -0.10, 0.10, 0.25, 0.50)
)

# ── 2. FDA CATEGORIES ────────────────────────────────────────────────────────
fda_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "FDA Categories", col_names = FALSE)

FDA_CATEGORIES <- data.frame(
  what_id  = as.character(fda_raw[[1]][5:21]),
  category = as.character(fda_raw[[2]][5:21]),
  count    = as.integer(fda_raw[[3]][5:21]),
  stringsAsFactors = FALSE
)

# ── 3. HOW CRITERIA ──────────────────────────────────────────────────────────
how_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "HOW Criteria", col_names = FALSE)

HOW_DEFS <- data.frame(
  how_id      = as.character(how_raw[[1]][5:39]),
  description = as.character(how_raw[[2]][5:39]),
  difficulty  = as.integer(how_raw[[3]][5:39]),
  stringsAsFactors = FALSE
)

# ── 4. ALTERNATIVES MATRIX ──────────────────────────────────────────────────────
dm_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "Alternatives Matrix", col_names = FALSE)
MANUFACTURERS <- as.character(dm_raw[[1]][5:7])

ALTERNATIVES_MATRIX <- matrix(0, nrow = 3, ncol = 35)
for (i in 1:3) {
  for (j in 1:35) {
    ALTERNATIVES_MATRIX[i, j] <- as.numeric(dm_raw[[1 + j]][4 + i])
  }
}
rownames(ALTERNATIVES_MATRIX) <- MANUFACTURERS
colnames(ALTERNATIVES_MATRIX) <- HOW_DEFS$how_id

# ── 5. RELATIONSHIP MATRIX ──────────────────────────────────────────────────
rel_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "Relationship Matrix", col_names = FALSE)

REL_MATRIX <- matrix(0, nrow = 25, ncol = 35)
for (i in 1:25) {
  for (j in 1:35) {
    REL_MATRIX[i, j] <- as.numeric(rel_raw[[2 + j]][4 + i])
  }
}
what_ids_rel <- as.character(rel_raw[[1]][5:29])
rownames(REL_MATRIX) <- what_ids_rel
colnames(REL_MATRIX) <- HOW_DEFS$how_id

# ── 6. ROOF MATRIX ──────────────────────────────────────────────────────────
roof_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "Roof Matrix", col_names = FALSE)

ROOF_MATRIX <- matrix(0, nrow = 35, ncol = 35)
for (i in 1:35) {
  for (j in 1:35) {
    ROOF_MATRIX[i, j] <- as.numeric(roof_raw[[2 + j]][4 + i])
  }
}
rownames(ROOF_MATRIX) <- HOW_DEFS$how_id
colnames(ROOF_MATRIX) <- HOW_DEFS$how_id

# ── 7. WHAT DEFINITIONS ─────────────────────────────────────────────────────
what_raw <- read_excel(EXCEL_INPUT_PATH, sheet = "WHAT Definitions", col_names = FALSE)

WHAT_DEFS <- data.frame(
  what_id    = as.character(what_raw[[1]][5:29]),
  ctq_group  = as.character(what_raw[[2]][5:29]),
  importance = as.numeric(what_raw[[4]][5:29]),
  stringsAsFactors = FALSE
)

# ── 8. DERIVED PARAMETERS ───────────────────────────────────────────────────
PROJECT <- list(
  name = "HoQ-TOPSIS MCDM Contract Manufacturer Selection Tool",
  version = "5.0",
  n_whats = nrow(WHAT_DEFS),
  n_hows = nrow(HOW_DEFS),
  n_manufacturers = length(MANUFACTURERS),
  fda_total = sum(FDA_CATEGORIES$count),
  n_quality_whats = sum(WHAT_DEFS$ctq_group == "Quality"),
  n_time_whats = sum(WHAT_DEFS$ctq_group == "Time"),
  n_cost_whats = sum(WHAT_DEFS$ctq_group == "Cost")
)

CTQ_SCENARIOS <- data.frame(
  scenario_id   = 1:5,
  scenario_name = c("Risk Averse (Quality Dominant)", "Quality Focus",
                     "Balanced", "Cost Conscious (Base Case)",
                     "Risk Oblivious (Cost Dominant)"),
  Q = c(0.80, 0.60, 0.33, CTQ_BASE$Q, 0.05),
  T = c(0.10, 0.20, 0.33, CTQ_BASE$T, 0.05),
  C = c(0.10, 0.20, 0.34, CTQ_BASE$C, 0.90),
  stringsAsFactors = FALSE
)

EXCEL_VALIDATION <- list(wj = NULL, topsis_cc = NULL, topsis_rank = NULL)

ASSUMPTIONS <- list(
  data_representativeness = paste0(format(PROJECT$fda_total, big.mark=","),
    " FDA 483 records represent the compliance risk landscape for sterile injectable manufacturing."),
  quality_weights_method  = "Quality WHATs (W01-W17) derive importance from FDA violation frequency. Time and Cost WHATs use equal weights within family.",
  ctq_scaling_method      = "CTQ ratio applied as post-HoQ scaling on criterion weights, preserving HoQ technical priority structure while enabling CTQ-driven rank differentiation.",
  ctq_independence        = "Quality, Time, and Cost are treated as independent dimensions with no interaction effects modeled.",
  criteria_type           = "All 35 HOW criteria are benefit-type (higher score = better). No cost-type criteria requiring inverse normalization.",
  scoring_basis           = "Manufacturer scores based on expert rubric assessment imported from Excel.",
  missing_data            = "All matrices fully populated. No imputation required.",
  roof_scope              = "Roof correlation matrix models HOW-HOW interactions only. WHAT-WHAT correlations are not modeled.",
  dirichlet_justification = "Dirichlet distribution for CTQ Monte Carlo: Q+T+C must sum to 1 (compositional constraint)."
)


# ── 9. CTQ COMPOSITION PER HOW CRITERION ─────────────────────────────────────
# Derived from REL_MATRIX and WHAT_DEFS (already loaded from Excel above).
# Each HOW criterion carries a fractional Q/T/C composition based on which
# WHATs feed into it through the relationship matrix.
# Required by apply_ctq_scaling() used in 03_topsis_engine.R, 04_ctq_scenarios.R,
# 06_bootstrap.R, and 08_monte_carlo.R.

.fda_imp <- FDA_CATEGORIES$count / sum(FDA_CATEGORIES$count)

CTQ_COMPOSITION <- data.frame(how_id = HOW_DEFS$how_id, stringsAsFactors = FALSE)
CTQ_COMPOSITION$q_frac <- 0
CTQ_COMPOSITION$t_frac <- 0
CTQ_COMPOSITION$c_frac <- 0

for (j in seq_len(PROJECT$n_hows)) {
  q_raw <- sum(.fda_imp * REL_MATRIX[1:PROJECT$n_quality_whats, j])
  t_raw <- sum((1 / PROJECT$n_time_whats) * REL_MATRIX[(PROJECT$n_quality_whats + 1):(PROJECT$n_quality_whats + PROJECT$n_time_whats), j])
  c_raw <- sum((1 / PROJECT$n_cost_whats)  * REL_MATRIX[(PROJECT$n_quality_whats + PROJECT$n_time_whats + 1):PROJECT$n_whats, j])
  total <- q_raw + t_raw + c_raw
  if (total > 0) {
    CTQ_COMPOSITION$q_frac[j] <- q_raw / total
    CTQ_COMPOSITION$t_frac[j] <- t_raw / total
    CTQ_COMPOSITION$c_frac[j] <- c_raw / total
  }
}


# ── 10. POST-HOQ CTQ SCALING FUNCTION ────────────────────────────────────────
# Applies CTQ risk appetite AFTER HoQ weights are computed, not before.
# This prevents the HoQ pipeline from dampening the CTQ signal.
#
# Pipeline: Base WHATs → HoQ → base_wj → CTQ scaling → final_wj → TOPSIS
#
# final_wj[j] = base_wj[j] × (Q × q_frac[j] + T × t_frac[j] + C × c_frac[j])
# then renormalize so sum(final_wj) = 1

apply_ctq_scaling <- function(base_wj, Q, T_val, C_val) {
  ctq_mult <- Q   * CTQ_COMPOSITION$q_frac +
              T_val * CTQ_COMPOSITION$t_frac +
              C_val * CTQ_COMPOSITION$c_frac
  wj_scaled <- base_wj * ctq_mult
  wj_scaled / sum(wj_scaled)
}


# ── 11. EXCEL MODE FLAG ───────────────────────────────────────────────────────
# Set by this file so that downstream modules that guard on PROJECT's existence
# via `if (!exists("PROJECT")) source("00_config.R")` will skip re-sourcing
# the hardcoded config. The flag also signals to run_from_excel.R that the
# Excel pipeline is active (used in the summary footer).

EXCEL_MODE <- TRUE


cat(sprintf("  %d WHATs | %d HOWs | %d manufacturers | %s FDA records\n",
            PROJECT$n_whats, PROJECT$n_hows, PROJECT$n_manufacturers,
            format(PROJECT$fda_total, big.mark=",")))
cat(sprintf("  Manufacturers: %s\n", paste(MANUFACTURERS, collapse = " | ")))
cat(sprintf("  CTQ Base: Q=%.2f  T=%.2f  C=%.2f\n",
            CTQ_BASE$Q, CTQ_BASE$T, CTQ_BASE$C))
cat("00_config_from_excel.R loaded successfully.\n\n")

