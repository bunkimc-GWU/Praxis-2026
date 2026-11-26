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