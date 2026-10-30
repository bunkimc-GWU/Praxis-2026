# =============================================================================
# 10_EXCEL_EXPORT.R — COMPREHENSIVE EXCEL WORKBOOK EXPORT
# =============================================================================
if (!exists("REPORT_TABLES")) source("09_report_tables.R")
library(openxlsx)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MODULE 10 — EXCEL EXPORT\n")
cat("══════════════════════════════════════════════════════════════\n\n")

wb <- createWorkbook()

hdr <- createStyle(fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
                   fgFill = "#1e3a5f", halign = "center", valign = "center",
                   textDecoration = "bold", border = "TopBottomLeftRight")
dat <- createStyle(fontName = "Arial", fontSize = 10,
                   border = "TopBottomLeftRight", halign = "center")

WT <- function(wb, name, df, color = "#2E86C1") {
  addWorksheet(wb, name, tabColour = color)
  writeData(wb, name, df, startRow = 1, headerStyle = hdr)
  addStyle(wb, name, dat, rows = 2:(nrow(df)+1), cols = 1:ncol(df), gridExpand = TRUE)
  setColWidths(wb, name, cols = 1:ncol(df), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
}





cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ASSUMPTIONS AND CONFIGURATIONS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet: Assumptions
WT(wb, "Assumptions", REPORT_TABLES$assumptions, "#2980B9")

# Sheet: WHAT Weights
WT(wb, "WHAT Weights", REPORT_TABLES$what_weights, "#2980B9")








cat("\n══════════════════════════════════════════════════════════════\n")
cat("  House of Quality Execution\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet: House of Quality FUll Chain
WT(wb, "HoQ Calculation Chain", REPORT_TABLES$hoq_full_chain, "#2980B9")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  BOOTSTRAP ANALSYIS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet: EA1 Bootstrap Analsyis
WT(wb, "Bootstrap Analsyis", REPORT_TABLES$ea1_bootstrap_all, "#27AE60")






cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Alternatives Matrix\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet: ALTERNATIVES MATRIX
dm_df <- data.frame(Company = MANUFACTURERS, ALTERNATIVES_MATRIX, check.names = FALSE, stringsAsFactors = FALSE)
WT(wb, "ALTERNATIVES MATRIX", dm_df, "#2980B9")





cat("\n══════════════════════════════════════════════════════════════\n")
cat("  KENDALLS TAU RANK CORRELATION\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet 6: Base TOPSIS
WT(wb, "TOPSIS Base Case", REPORT_TABLES$base_topsis, "#E74C3C")


# Sheet 8: Correlation Coefficient Comparison
WT(wb, "CC Comparison", REPORT_TABLES$ea2_cc_comparison, "#E74C3C")

# Sheet 9: EA2 Pairwise Gaps
WT(wb, "H2 EA2 Pairwise Gaps", REPORT_TABLES$ea2_pairwise_gaps, "#E74C3C")

# Sheet 7: Benchmark Summary
WT(wb, "Kendall Tau Summary", REPORT_TABLES$ea2_benchmark_summary, "#E74C3C")





cat("\n══════════════════════════════════════════════════════════════\n")
cat("  CTQ SCENARIOS AND SENSTIVITY ANALSYIS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Sheet 10: CTQ Scenarios
WT(wb, "H3 CTQ Scenarios", REPORT_TABLES$ctq_scenarios, "#8E44AD")

# Sheet 11: Sensitivity ALL
WT(wb, "H3 Sensitivity All 35", REPORT_TABLES$sensitivity_all, "#8E44AD")

# Sheet 12: EA3 Rank Frequencies
WT(wb, "H3 EA3 Rank Frequencies", REPORT_TABLES$ea3_rank_freq, "#8E44AD")

# Sheet 13: EA3 Dominance
WT(wb, "H3 EA3 Dominance", REPORT_TABLES$ea3_dominance, "#8E44AD")

# Sheet 14: EA3 Tipping Point
WT(wb, "H3 EA3 Tipping Point", REPORT_TABLES$ea3_tipping_point, "#8E44AD")


cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO SIMULATIONS\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# Sheet 15: MC Iteration Detail (all iterations)
mc_detail <- MC_SUMMARY$iteration_detail
mc_detail_export <- mc_detail[, c("iteration", "manufacturer", "cc", "topsis_rank", "Q_weight", "T_weight", "C_weight")]
names(mc_detail_export) <- c("Iteration", "Manufacturer", "Closeness_Coeff", "TOPSIS_Rank",
                              "Quality_Weight", "Delivery_Weight", "Cost_Weight")
mc_detail_export <- mc_detail_export[order(mc_detail_export$Iteration, mc_detail_export$TOPSIS_Rank), ]
WT(wb, "H3 EA3 MC Iterations", mc_detail_export, "#8E44AD")


# Sheet 18: Validation Framework
WT(wb, "Validation Framework", REPORT_TABLES$validation_framework, "#E67E22")





# Sheet 17 (existing): CTQ Fractions & Scaled Weights
ctq_sheet <- "CTQ Fractions & Scaled Wts"
addWorksheet(wb, ctq_sheet, tabColour = "#059669")

# Styled header
sec_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
                         fgFill = "#059669", halign = "left", valign = "center",
                         textDecoration = "bold", border = "TopBottomLeftRight")




# Write section title
writeData(wb, ctq_sheet, "CTQ FRACTIONS (Normalized) & CTQ-SCALED WEIGHTS", startRow = 1, startCol = 1)
addStyle(wb, ctq_sheet, sec_style, rows = 1, cols = 1:ncol(REPORT_TABLES$ctq_fractions), gridExpand = TRUE)
mergeCells(wb, ctq_sheet, cols = 1:ncol(REPORT_TABLES$ctq_fractions), rows = 1)

# CTQ Base info
info_style <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#1e3a5f",
                          halign = "left", textDecoration = "italic")
writeData(wb, ctq_sheet, paste0("Base Case CTQ: Q=", CTQ_BASE$Q*100, "% | T=", CTQ_BASE$T*100, "% | C=", CTQ_BASE$C*100, "%"), startRow = 2, startCol = 1)
addStyle(wb, ctq_sheet, info_style, rows = 2, cols = 1:5, gridExpand = TRUE)
mergeCells(wb, ctq_sheet, cols = 1:5, rows = 2)

# Write data starting at row 4
writeData(wb, ctq_sheet, REPORT_TABLES$ctq_fractions, startRow = 4, headerStyle = hdr)
addStyle(wb, ctq_sheet, dat, rows = 5:(nrow(REPORT_TABLES$ctq_fractions)+4),
         cols = 1:ncol(REPORT_TABLES$ctq_fractions), gridExpand = TRUE)

# Percentage formatting for fraction columns
pct_fmt <- createStyle(numFmt = "0.0%")
num_fmt <- createStyle(numFmt = "0.000000")
num_fmt8 <- createStyle(numFmt = "0.00000000")
int_fmt <- createStyle(numFmt = "0")

# Apply number formats: Q/T/C Contribution (cols 4-7), Fractions (8-10), Multiplier (11), Scaled (12), Final (13), Rank (14)
data_rows <- 5:(nrow(REPORT_TABLES$ctq_fractions) + 4)
addStyle(wb, ctq_sheet, num_fmt, rows = data_rows, cols = 3, gridExpand = TRUE, stack = TRUE)  # Base_wj
addStyle(wb, ctq_sheet, num_fmt, rows = data_rows, cols = 4:7, gridExpand = TRUE, stack = TRUE)  # Contributions
addStyle(wb, ctq_sheet, pct_fmt, rows = data_rows, cols = 8:10, gridExpand = TRUE, stack = TRUE)  # Fractions
addStyle(wb, ctq_sheet, num_fmt, rows = data_rows, cols = 11, gridExpand = TRUE, stack = TRUE)  # Multiplier
addStyle(wb, ctq_sheet, num_fmt8, rows = data_rows, cols = 12:13, gridExpand = TRUE, stack = TRUE)  # Scaled/Final
addStyle(wb, ctq_sheet, int_fmt, rows = data_rows, cols = 14, gridExpand = TRUE, stack = TRUE)  # Rank

# Conditional formatting for Dominant CTQ column (15)
dom_q <- createStyle(fontColour = "#1d4ed8", fgFill = "#dbeafe", textDecoration = "bold")
dom_t <- createStyle(fontColour = "#059669", fgFill = "#d1fae5", textDecoration = "bold")
dom_c <- createStyle(fontColour = "#d97706", fgFill = "#fef3c7", textDecoration = "bold")

for (i in seq_len(nrow(REPORT_TABLES$ctq_fractions))) {
  r <- 4 + i
  dom <- REPORT_TABLES$ctq_fractions$Dominant_CTQ[i]
  sty <- if (dom == "Quality") dom_q else if (dom == "Time") dom_t else dom_c
  addStyle(wb, ctq_sheet, sty, rows = r, cols = 15, stack = TRUE)
}

setColWidths(wb, ctq_sheet, cols = 1:ncol(REPORT_TABLES$ctq_fractions), widths = "auto")
freezePane(wb, ctq_sheet, firstRow = TRUE, firstActiveRow = 5)


# ── Sheet 18: Bootstrap Sample Iterations (AUDIT TRAIL) ──────────────────────
# Shows first N iterations so the stakeholder review team can trace
# exactly how resampling → WHAT weights → HoQ → rank works.

boot_sheet <- "H1 Bootstrap Samples"
addWorksheet(wb, boot_sheet, tabColour = "#16A34A")

sec_green <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
                          fgFill = "#16A34A", halign = "left", valign = "center",
                          textDecoration = "bold", border = "TopBottomLeftRight")
note_style <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#475569",
                           halign = "left", textDecoration = "italic")
rank1_style <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#166534",
                            fgFill = "#D1FAE5", textDecoration = "bold", border = "TopBottomLeftRight")

# ── Part A: Resampled FDA Counts (17 categories × 10 iterations) ──
writeData(wb, boot_sheet, "BOOTSTRAP SAMPLE ITERATIONS — Calculation Audit Trail", startRow = 1, startCol = 1)
addStyle(wb, boot_sheet, sec_green, rows = 1, cols = 1:30, gridExpand = TRUE)
mergeCells(wb, boot_sheet, cols = 1:30, rows = 1)

writeData(wb, boot_sheet, paste0("Showing first ", length(BOOTSTRAP_SAMPLES),
  " of ", format(SIM_PARAMS$bootstrap_n, big.mark = ","),
  " iterations. Each iteration: resample FDA 483 counts (multinomial) → rebuild WHAT importance → run full HoQ 6-step pipeline → record ranks."),
  startRow = 2, startCol = 1)
addStyle(wb, boot_sheet, note_style, rows = 2, cols = 1:20, gridExpand = TRUE)
mergeCells(wb, boot_sheet, cols = 1:20, rows = 2)

# Section A header
writeData(wb, boot_sheet, "PART A: Resampled FDA Category Counts per Iteration", startRow = 4, startCol = 1)
addStyle(wb, boot_sheet, sec_green, rows = 4, cols = 1:20, gridExpand = TRUE)
mergeCells(wb, boot_sheet, cols = 1:20, rows = 4)

# Build resampled counts table
counts_df <- data.frame(
  WHAT_ID = FDA_CATEGORIES$what_id,
  Category = FDA_CATEGORIES$category,
  Original_Count = FDA_CATEGORIES$count,
  Original_Pct = round(FDA_CATEGORIES$count / sum(FDA_CATEGORIES$count) * 100, 2),
  stringsAsFactors = FALSE
)
for (s in seq_along(BOOTSTRAP_SAMPLES)) {
  counts_df[[paste0("Iter_", s, "_Count")]] <- BOOTSTRAP_SAMPLES[[s]]$resampled_counts
  counts_df[[paste0("Iter_", s, "_Pct")]] <- round(BOOTSTRAP_SAMPLES[[s]]$resampled_props * 100, 2)
}

writeData(wb, boot_sheet, counts_df, startRow = 5, headerStyle = hdr)
addStyle(wb, boot_sheet, dat, rows = 6:(5 + nrow(counts_df)), cols = 1:ncol(counts_df), gridExpand = TRUE)

#--Setting column Width on Bootstrap Samples Ouput-----
setColWidths(wb, boot_sheet, cols = 1:ncol(counts_df), widths = 10)
setColWidths(wb, boot_sheet, cols = 1, widths = 10)
setColWidths(wb, boot_sheet, cols = 2, widths = 28)



# Section B: Resulting wj weights and ranks
sec_b_row <- 5 + nrow(counts_df) + 2
writeData(wb, boot_sheet,
          "PART B: Resulting HoQ Weights (wj), Ranks, and Bootstrap Summary Statistics",
          startRow = sec_b_row, startCol = 1)
addStyle(wb, boot_sheet, sec_green, rows = sec_b_row, cols = 1:30, gridExpand = TRUE)
mergeCells(wb, boot_sheet, cols = 1:30, rows = sec_b_row)

# Match bootstrap summaries back to HOW order
rank_stats <- BOOTSTRAP_RESULTS[match(HOW_DEFS$how_id, BOOTSTRAP_RESULTS$how_id), ]
wj_stats   <- BOOTSTRAP_WJ_RESULTS[match(HOW_DEFS$how_id, BOOTSTRAP_WJ_RESULTS$how_id), ]

wj_df <- data.frame(
  HOW_ID        = HOW_DEFS$how_id,
  Description   = substr(HOW_DEFS$description, 1, 40),
  
  Base_wj       = round(as.numeric(WJ), 8),
  Mean_wj       = round(wj_stats$mean_wj, 8),
  Median_wj     = round(wj_stats$median_wj, 8),
  wj_CI_Lower   = round(wj_stats$wj_ci_lower, 8),
  wj_CI_Upper   = round(wj_stats$wj_ci_upper, 8),
  
  Base_Rank     = HOQ_RESULTS$wj_rank,
  Mean_Rank     = round(rank_stats$mean_rank, 2),
  Median_Rank   = round(rank_stats$median_rank, 2),
  Rank_CI_Lower = round(rank_stats$ci_lower, 2),
  Rank_CI_Upper = round(rank_stats$ci_upper, 2),
  
  stringsAsFactors = FALSE
)

#Alternating wj and Rank output
#for (s in seq_along(BOOTSTRAP_SAMPLES)) {
#  wj_df[[paste0("Iter_", s, "_wj")]]   <- round(BOOTSTRAP_SAMPLES[[s]]$wj, 8)
#  wj_df[[paste0("Iter_", s, "_Rank")]] <- BOOTSTRAP_SAMPLES[[s]]$wj_rank
#}

#Updated to report all sample wj first, then rank columns after
# Add all sample wj columns first
for (s in seq_along(BOOTSTRAP_SAMPLES)) {
  wj_df[[paste0("Iter_", s, "_wj")]] <- round(BOOTSTRAP_SAMPLES[[s]]$wj, 8)
}

# Add all sample rank columns after
for (s in seq_along(BOOTSTRAP_SAMPLES)) {
  wj_df[[paste0("Iter_", s, "_Rank")]] <- BOOTSTRAP_SAMPLES[[s]]$wj_rank
}


writeData(wb, boot_sheet, wj_df, startRow = sec_b_row + 1, headerStyle = hdr)
addStyle(wb, boot_sheet, dat, rows = (sec_b_row + 2):(sec_b_row + 1 + nrow(wj_df)),
         cols = 1:ncol(wj_df), gridExpand = TRUE)
setColWidths(wb, boot_sheet, cols = 1:ncol(wj_df), widths = "auto")








# Highlight Rank 1 cells in each iteration
for (s in seq_along(BOOTSTRAP_SAMPLES)) {
  rank_col <- which(names(wj_df) == paste0("Iter_", s, "_Rank"))
  for (row_i in 1:nrow(wj_df)) {
    if (BOOTSTRAP_SAMPLES[[s]]$wj_rank[row_i] == 1) {
      addStyle(wb, boot_sheet, rank1_style, rows = sec_b_row + 1 + row_i, cols = rank_col, stack = TRUE)
      addStyle(wb, boot_sheet, rank1_style, rows = sec_b_row + 1 + row_i, cols = rank_col - 1, stack = TRUE)
    }
  }
}

# Section C: Summary — which criterion won each iteration
sec_c_row <- sec_b_row + 1 + nrow(wj_df) + 2
writeData(wb, boot_sheet, "PART C: Rank 1 Winner per Sample Iteration", startRow = sec_c_row, startCol = 1)
addStyle(wb, boot_sheet, sec_green, rows = sec_c_row, cols = 1:6, gridExpand = TRUE)
mergeCells(wb, boot_sheet, cols = 1:6, rows = sec_c_row)

winner_df <- data.frame(
  Iteration = seq_along(BOOTSTRAP_SAMPLES),
  Rank1_Criterion = sapply(BOOTSTRAP_SAMPLES, function(s) s$rank1_criterion),
  Rank1_wj = round(sapply(BOOTSTRAP_SAMPLES, function(s) max(s$wj)), 8),
  Rank1_wj_Pct = paste0(round(sapply(BOOTSTRAP_SAMPLES, function(s) max(s$wj)) * 100, 2), "%"),
  stringsAsFactors = FALSE
)
writeData(wb, boot_sheet, winner_df, startRow = sec_c_row + 1, headerStyle = hdr)
addStyle(wb, boot_sheet, dat, rows = (sec_c_row + 2):(sec_c_row + 1 + nrow(winner_df)),
         cols = 1:ncol(winner_df), gridExpand = TRUE)

freezePane(wb, boot_sheet, firstRow = TRUE, firstActiveRow = 6)

dir.create("../outputs", showWarnings = FALSE)
output_file <- "../outputs/MCDM_Results.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat(sprintf("  Saved: %s (%d sheets)\n", output_file, length(wb$sheet_names)))
for (i in seq_along(wb$sheet_names)) cat(sprintf("    %2d. %s\n", i, wb$sheet_names[i]))




cat("\nMODULE 10 COMPLETE.\n")
