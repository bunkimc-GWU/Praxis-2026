# =============================================================================
# 09_REPORT_TABLES.R — STRUCTURED CONSOLE OUTPUT & REPORT TABLE ASSEMBLY
# =============================================================================
# Displays results following H1→H2→H3 methodology hierarchy.
# ALL values shown (no truncation). Contextual column names.
# =============================================================================

if (!exists("MC_SUMMARY"))           source("08_monte_carlo.R")
if (!exists("BENCHMARK"))            source("07_benchmark.R")
if (!exists("BOOTSTRAP_RESULTS"))    source("06_bootstrap.R")
if (!exists("SENSITIVITY_RESULTS"))  source("05_sensitivity.R")
if (!exists("CTQ_SUMMARY"))          source("04_ctq_scenarios.R")

library(dplyr)

S <- function(title, lvl = 1) {
  w <- 72
  if (lvl == 1) { cat("\n", strrep("=", w), "\n  ", title, "\n", strrep("=", w), "\n\n", sep = "") }
  else if (lvl == 2) { cat("\n", strrep("-", w), "\n  ", title, "\n", strrep("-", w), "\n\n", sep = "") }
  else { cat("\n  >> ", title, "\n\n", sep = "") }
}

cat("\n")
cat(strrep("=", 72), "\n")
cat("  BCHOKSHI GWID G39538196 GWU D. ENGR HYBRID MCDM TOOL INPUT SUMMARY\n")
cat("  Advisor, Dr. Brandon D. Shaw, DE\n")
cat(strrep("=", 72), "\n\n")






# ── PHASE 0 ──────────────────────────────────────────────────────────────────
S("PHASE 0: PROJECT CONFIGURATION SUMMARY", 1)
cat("  Project:       ", PROJECT$name, "\n")
cat("  FDA Records:   ", format(PROJECT$fda_total, big.mark = ","), "\n")
cat("  WHATs:         ", PROJECT$n_whats, " (Q:", PROJECT$n_quality_whats, " T:", PROJECT$n_time_whats, " C:", PROJECT$n_cost_whats, ")\n")
cat("  HOWs:          ", PROJECT$n_hows, "\n")
cat("  Manufacturers: ", paste(MANUFACTURERS, collapse = ", "), "\n")
cat("  CTQ Base:      Q=", CTQ_BASE$Q*100, "% T=", CTQ_BASE$T*100, "% C=", CTQ_BASE$C*100, "%\n\n", sep = "")








# ── PHASE 1: HYPOTHESIS 1 — HoQ ─────────────────────────────────────────────

S("PHASE 1 — HOUSE OF QUALITY EXECUTION", 1)

S("1.1  WHAT Requirements, Importance Weights and Normalized Wt% Summary", 2)
what_table <- data.frame(
  WHAT_ID = WHAT_DEFS$what_id, 
  CTQ_Group = WHAT_DEFS$ctq_group,
  Importance_Weight = WHAT_WEIGHTS,
  Weight_Pct = round(WHAT_WEIGHTS / sum(WHAT_WEIGHTS) * 100, 4),
  stringsAsFactors = FALSE)

cat(sprintf("  %-5s %-8s %14s %7s\n", "WHAT", "Group", "Importance", "Wt%"))
cat("  ", strrep("-", 40), "\n")
for (i in 1:nrow(what_table)) cat(sprintf("  %-5s %-8s %14.8f %6.2f%%\n", what_table$WHAT_ID[i], what_table$CTQ_Group[i], what_table$Importance_Weight[i], what_table$Weight_Pct[i]))
cat(sprintf("\n  SUM: %.8f (3 families × 1.0 each)\n", sum(WHAT_WEIGHTS)))




S("1.2  HOW Criteria & Difficulty Summary", 2)
for (i in 1:nrow(HOW_DEFS)) cat(sprintf("  %-5s %-52s Difficulty: %d/5\n", HOW_DEFS$how_id[i], substr(HOW_DEFS$description[i], 1, 52), HOW_DEFS$difficulty[i]))







S("1.3  HoQ Basement — Full Calculation Chain", 2)
hoq_full <- data.frame(
  HOW_ID = HOW_DEFS$how_id, Description = HOW_DEFS$description, Difficulty = HOW_DEFS$difficulty,
  Step1_Raw_Priority = HOQ_RESULTS$pj_raw, 
  Step2_Normalized = HOQ_RESULTS$pj_norm,
  Step3_Difficulty_Adjusted = HOQ_RESULTS$pj_diff, 
  Step4_Roof_Effect = HOQ_RESULTS$pj_roof,
  Step5_HoQ_Adjusted = HOQ_RESULTS$pj_hoq, 
  Step6_Final_wj = as.numeric(WJ),
  Weight_Percent = round(as.numeric(WJ) * 100, 2), 
  HoQ_Rank = as.integer(HOQ_RESULTS$wj_rank),
  stringsAsFactors = FALSE)
hoq_full <- hoq_full[order(hoq_full$HoQ_Rank), ]
cat(sprintf("  %-5s %4s %9s %9s %9s %9s %9s %7s %4s\n", "HOW", "Diff", "Raw Pj", "Norm Pj", "Diff Adj", "Roof Eff", "HoQ Adj", "wj %", "Rank"))
cat("  ", strrep("-", 72), "\n")
for (i in 1:nrow(hoq_full)) cat(sprintf("  %-5s %4d %9.5f %9.6f %9.6f %9.5f %9.5f %6.2f%% %4d\n", hoq_full$HOW_ID[i], hoq_full$Difficulty[i], hoq_full$Step1_Raw_Priority[i], hoq_full$Step2_Normalized[i], hoq_full$Step3_Difficulty_Adjusted[i], hoq_full$Step4_Roof_Effect[i], hoq_full$Step5_HoQ_Adjusted[i], hoq_full$Weight_Percent[i], hoq_full$HoQ_Rank[i]))
cat(sprintf("\n  Top: %s (wj=%.4f) | wj SUM=%.8f\n", hoq_full$HOW_ID[1], hoq_full$Step6_Final_wj[1], sum(WJ)))






S("1.4  Bootstrap Rank Stability (all 35)", 2)
boot_full <- BOOTSTRAP_RESULTS[order(BOOTSTRAP_RESULTS$mean_rank), ]
cat(sprintf("  %s resamples | Multinomial redraw of %s FDA records\n\n", format(SIM_PARAMS$bootstrap_n, big.mark=","), format(PROJECT$fda_total, big.mark=",")))
cat(sprintf("  %-5s %-35s %5s %5s %9s %7s %7s\n", "HOW", "Description", "Base", "Mean", "95% CI", "P(#1)", "P(T5)"))
cat("  ", strrep("-", 78), "\n")
for (i in 1:nrow(boot_full)) { r <- boot_full[i,]; cat(sprintf("  %-5s %-35s %5d %5.1f [%2.0f,%2.0f] %6.1f%% %6.1f%%\n", r$how_id, substr(r$description,1,35), r$base_rank, r$mean_rank, r$ci_lower, r$ci_upper, r$pct_rank1, r$pct_top5)) }
top_b <- boot_full[1,]
cat(sprintf("\n  EA1 VERDICT: %s holds Rank 1 in %.1f%% of resamples — %s\n", top_b$how_id, top_b$pct_rank1, ifelse(top_b$pct_rank1 > 50, "SUPPORTED", "NOT SUPPORTED")))







# ── PHASE 2: HYPOTHESIS 2 — TOPSIS ──────────────────────────────────────────
S("PHASE 2 — TOPSIS + HoQ RANKING", 1)

S("2.1  TOPSIS Base Case Results", 2)
cat(sprintf("  CTQ: Q=%.0f%% T=%.0f%% C=%.0f%%\n\n", CTQ_BASE$Q*100, CTQ_BASE$T*100, CTQ_BASE$C*100))
cat(sprintf("  %-8s %14s %14s %16s %7s %4s\n", "Company", "Dist to Ideal", "Dist to Worst", "Closeness Coeff", "CC%", "Rank"))
cat("  ", strrep("-", 68), "\n")
for (i in order(BASE_TOPSIS$topsis_rank)) { r <- BASE_TOPSIS[i,]; cat(sprintf("  %-8s %14.10f %14.10f %16.10f %6.1f%% %4d\n", r$manufacturer, r$s_plus, r$s_minus, r$cc, r$cc*100, r$topsis_rank)) }
cat(sprintf("\n  CC Spread: %.6f | Recommended: %s\n", max(BASE_TOPSIS$cc)-min(BASE_TOPSIS$cc), BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank==1]))







S("2.2  HoQ vs Equal-Weight Benchmark", 2)
cat("  Note: Uses pure HoQ weights (WJ_BASE, no CTQ scaling) vs equal weights.\n")
cat("  CTQ scaling is an H3 concern — EA2 isolates HoQ's discriminative value.\n\n")
cat(sprintf("  %-8s %10s %6s %10s %6s\n", "Company", "HoQ CC", "Rank", "Equal CC", "Rank"))
cat("  ", strrep("-", 44), "\n")
for (m in MANUFACTURERS) { h <- HOQ_PURE_TOPSIS[HOQ_PURE_TOPSIS$manufacturer==m,]; e <- EQUAL_TOPSIS[EQUAL_TOPSIS$manufacturer==m,]; cat(sprintf("  %-8s %10.6f %4d   %10.6f %4d\n", m, h$cc, h$topsis_rank, e$cc, e$topsis_rank)) }
cat(sprintf("\n  Kendall's τ:          %.3f\n  HoQ CC Spread:        %.6f\n  Equal CC Spread:      %.6f\n  Spread Improvement:   %+.1f%%\n", BENCHMARK$kendall_tau, BENCHMARK$hoq_spread, BENCHMARK$equal_spread, BENCHMARK$spread_improvement_pct))
cat(sprintf("\n  Pairwise Gaps:\n"))
for (i in 1:nrow(BENCHMARK$pairwise_gaps)) { r <- BENCHMARK$pairwise_gaps[i,]; cat(sprintf("    %-15s HoQ=%.6f Equal=%.6f Impact=%+.0f%%\n", r$pair, r$hoq_gap, r$equal_gap, r$pct_change)) }
cat(sprintf("\n  EA2 VERDICT: %s\n", ifelse(BENCHMARK$spread_improvement_pct >= 10, sprintf("SUPPORTED (+%.1f%%)", BENCHMARK$spread_improvement_pct), "NOT SUPPORTED")))








# ── PHASE 3: HYPOTHESIS 3 — CTQ VARIATION ────────────────────────────────────
S("PHASE 3 — CTQ VARIATION & ROBUSTNESS", 1)

S("3.1  CTQ Scenario Comparison (5 scenarios)", 2)
for (s in 1:nrow(CTQ_SCENARIOS)) {
  sc <- CTQ_SCENARIOS[s,]; m <- if(s==4) " ★" else ""
  cat(sprintf("  [%d] %s%s (Q=%.0f%% T=%.0f%% C=%.0f%%)\n", s, sc$scenario_name, m, sc$Q*100, sc$T*100, sc$C*100))
  sd <- CTQ_SUMMARY[CTQ_SUMMARY$scenario_id==s,]
  for (j in order(sd$topsis_rank)) cat(sprintf("      %-6s CC=%.6f Rank %d\n", sd$manufacturer[j], sd$cc[j], sd$topsis_rank[j]))
  cat("\n")
}




S("3.2  Sensitivity Analysis — ALL 35 Criteria (ranked by impact)", 2)
bw <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank==1]
sf <- TORNADO_DATA[order(TORNADO_DATA$cc_range, decreasing=TRUE),]
cat(sprintf("  Perturbation: ±10%%, ±25%%, ±50%% | Metric: CC of %s\n\n", bw))
cat(sprintf("  %-5s %-40s %9s %9s %9s %10s\n", "HOW", "Description", "CC Min", "CC Max", "Range", "Stable?"))
cat("  ", strrep("-", 86), "\n")
for (i in 1:nrow(sf)) cat(sprintf("  %-5s %-40s %9.6f %9.6f %9.6f %10s\n", sf$how_id[i], substr(sf$description[i],1,40), sf$cc_min[i], sf$cc_max[i], sf$cc_range[i], ifelse(sf$rank_stable[i],"Yes","FLIP")))
cat(sprintf("\n  Flips: %d/%d criteria\n", sum(!sf$rank_stable), nrow(sf)))

S("3.3  EA3: Monte Carlo Simulation", 2)
mc_rf <- MC_SUMMARY$rank_freq
cat(sprintf("  %s iterations | Dirichlet(%s) | Full pipeline per iteration\n\n", format(SIM_PARAMS$monte_carlo_n, big.mark=","), paste(SIM_PARAMS$dirichlet_alpha, collapse=",")))
cat(sprintf("  %-8s %8s %8s %8s %7s %9s %18s\n", "Company", "P(#1)", "P(#2)", "P(#3)", "AvgRk", "Mean CC", "CC 95% CI"))
cat("  ", strrep("-", 68), "\n")
for (i in 1:nrow(mc_rf)) { r <- mc_rf[i,]; cat(sprintf("  %-8s %7.1f%% %7.1f%% %7.1f%% %7.2f %9.6f [%.4f,%.4f]\n", r$manufacturer, r$pct_rank1, r$pct_rank2, r$pct_rank3, r$mean_rank, r$cc_mean, r$cc_ci_lower, r$cc_ci_upper)) }
cat("\n  Dominance:\n")
for (i in 1:nrow(MC_SUMMARY$dominance)) cat(sprintf("    %s = %s\n", MC_SUMMARY$dominance$comparison[i], MC_SUMMARY$dominance$probability[i]))

bo <- paste(MANUFACTURERS[order(BASE_TOPSIS$topsis_rank)], collapse=" > ")
mo <- tapply(1:nrow(MC_RESULTS), MC_RESULTS$iteration, function(idx) { d <- MC_RESULTS[idx,]; paste(d$manufacturer[order(d$topsis_rank)], collapse=" > ") })
rob <- mean(mo == bo) * 100
cat(sprintf("\n  Robustness Index: %.1f%% preserve %s\n", rob, bo))
wn <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank==1]
cat(sprintf("\n  EA3 VERDICT: %s holds Rank 1 in %.1f%% — %s\n", wn, mc_rf$pct_rank1[mc_rf$manufacturer==wn], ifelse(mc_rf$pct_rank1[mc_rf$manufacturer==wn]>=70, "SUPPORTED", "NOT SUPPORTED")))

# ── PHASE 4: CTQ FRACTIONS & SCALED WEIGHTS ──────────────────────────────────
S("CTQ FRACTIONS (Normalized) & CTQ-SCALED WEIGHTS", 1)

# Build a wide-format table: rows = metric, columns = H01..H35
ctq_frac_df <- data.frame(
  HOW_ID      = HOW_DEFS$how_id,
  Description = HOW_DEFS$description,
  Base_wj     = as.numeric(WJ_BASE),
  stringsAsFactors = FALSE
)

# Raw CTQ contributions per HOW (SUMPRODUCT of WHAT importance × relationship)
q_imp_vec <- WHAT_DEFS$importance[1:17]
t_imp_vec <- WHAT_DEFS$importance[18:21]
c_imp_vec <- WHAT_DEFS$importance[22:25]

ctq_frac_df$Q_Contribution <- sapply(1:PROJECT$n_hows, function(j) sum(q_imp_vec * REL_MATRIX[1:17, j]))
ctq_frac_df$T_Contribution <- sapply(1:PROJECT$n_hows, function(j) sum(t_imp_vec * REL_MATRIX[18:21, j]))
ctq_frac_df$C_Contribution <- sapply(1:PROJECT$n_hows, function(j) sum(c_imp_vec * REL_MATRIX[22:25, j]))
ctq_frac_df$Total_Contribution <- ctq_frac_df$Q_Contribution + ctq_frac_df$T_Contribution + ctq_frac_df$C_Contribution

# Normalized fractions
ctq_frac_df$Q_Fraction <- ifelse(ctq_frac_df$Total_Contribution > 0, ctq_frac_df$Q_Contribution / ctq_frac_df$Total_Contribution, 1/3)
ctq_frac_df$T_Fraction <- ifelse(ctq_frac_df$Total_Contribution > 0, ctq_frac_df$T_Contribution / ctq_frac_df$Total_Contribution, 1/3)
ctq_frac_df$C_Fraction <- ifelse(ctq_frac_df$Total_Contribution > 0, ctq_frac_df$C_Contribution / ctq_frac_df$Total_Contribution, 1/3)

# CTQ Multiplier = Q_base × Q_frac + T_base × T_frac + C_base × C_frac
ctq_frac_df$CTQ_Multiplier <- CTQ_BASE$Q * ctq_frac_df$Q_Fraction +
                                CTQ_BASE$T * ctq_frac_df$T_Fraction +
                                CTQ_BASE$C * ctq_frac_df$C_Fraction

# Scaled wj = Base_wj × CTQ_Multiplier
ctq_frac_df$Scaled_wj <- ctq_frac_df$Base_wj * ctq_frac_df$CTQ_Multiplier

# Final wj = Scaled / sum(Scaled)
ctq_frac_df$Final_wj <- ctq_frac_df$Scaled_wj / sum(ctq_frac_df$Scaled_wj)

# Rank by Final_wj
ctq_frac_df$CTQ_Rank <- rank(-ctq_frac_df$Final_wj, ties.method = "min")

# Dominant CTQ Dimension
ctq_frac_df$Dominant_CTQ <- ifelse(
  ctq_frac_df$Q_Fraction >= ctq_frac_df$T_Fraction & ctq_frac_df$Q_Fraction >= ctq_frac_df$C_Fraction, "Quality",
  ifelse(ctq_frac_df$T_Fraction >= ctq_frac_df$C_Fraction, "Time", "Cost")
)

# Console output
cat(sprintf("  CTQ Base Case: Q=%.0f%% T=%.0f%% C=%.0f%%\n\n", CTQ_BASE$Q*100, CTQ_BASE$T*100, CTQ_BASE$C*100))
cat(sprintf("  %-5s %8s %8s %8s %8s %8s %8s %9s %9s %4s %8s\n",
            "HOW", "Q_Frac", "T_Frac", "C_Frac", "Mult", "Base_wj", "Scaled", "Final_wj", "Rank", "", "Dominant"))
cat("  ", strrep("-", 100), "\n")
for (i in order(ctq_frac_df$CTQ_Rank)) {
  r <- ctq_frac_df[i,]
  cat(sprintf("  %-5s %7.1f%% %7.1f%% %7.1f%% %8.5f %8.6f %8.6f %9.8f %4d     %-8s\n",
              r$HOW_ID, r$Q_Fraction*100, r$T_Fraction*100, r$C_Fraction*100,
              r$CTQ_Multiplier, r$Base_wj, r$Scaled_wj, r$Final_wj, r$CTQ_Rank, r$Dominant_CTQ))
}
cat(sprintf("\n  Dominant CTQ: Quality=%d, Time=%d, Cost=%d of %d HOWs\n",
            sum(ctq_frac_df$Dominant_CTQ == "Quality"),
            sum(ctq_frac_df$Dominant_CTQ == "Time"),
            sum(ctq_frac_df$Dominant_CTQ == "Cost"),
            nrow(ctq_frac_df)))

# ── ASSEMBLE ─────────────────────────────────────────────────────────────────
S("REPORT TABLES ASSEMBLED", 1)

ea2_summary <- data.frame(Metric=c("Kendall τ","HoQ Spread","Equal Spread","Improvement"), Value=c(sprintf("%.3f",BENCHMARK$kendall_tau), sprintf("%.6f",BENCHMARK$hoq_spread), sprintf("%.6f",BENCHMARK$equal_spread), sprintf("%+.1f%%",BENCHMARK$spread_improvement_pct)), stringsAsFactors=FALSE)

REPORT_TABLES <- list(
  validation_framework = data.frame(
    Evaluation_Area=c("EA1","EA2","EA3"), Hypothesis=c("H1: Top criterion rank-stable","H2: CC spread ≥10% improvement","H3: Top mfr holds ≥70% of scenarios"),
    Result=c(sprintf("P(#1)=%.1f%%",top_b$pct_rank1), sprintf("+%.1f%%",BENCHMARK$spread_improvement_pct), sprintf("P(#1)=%.1f%%",mc_rf$pct_rank1[mc_rf$manufacturer==wn])),
    Verdict=c(ifelse(top_b$pct_rank1>50,"SUPPORTED","NOT SUPPORTED"), ifelse(BENCHMARK$spread_improvement_pct>=10,"SUPPORTED","NOT SUPPORTED"), ifelse(mc_rf$pct_rank1[mc_rf$manufacturer==wn]>=70,"SUPPORTED","NOT SUPPORTED")), stringsAsFactors=FALSE),
  what_weights = what_table,
  hoq_full_chain = hoq_full,
  base_topsis = BASE_TOPSIS,
  ea1_bootstrap_all = boot_full,
  ea2_benchmark_summary = ea2_summary,
  ea2_pairwise_gaps = BENCHMARK$pairwise_gaps,
  ea2_cc_comparison = data.frame(Company=MANUFACTURERS, HoQ_CC=sapply(MANUFACTURERS, function(m) HOQ_PURE_TOPSIS$cc[HOQ_PURE_TOPSIS$manufacturer==m]), Equal_CC=sapply(MANUFACTURERS, function(m) EQUAL_TOPSIS$cc[EQUAL_TOPSIS$manufacturer==m]), row.names=NULL),
  ctq_scenarios = CTQ_SUMMARY,
  sensitivity_all = sf,
  ea3_rank_freq = mc_rf,
  ea3_dominance = MC_SUMMARY$dominance,
  ea3_tipping_point = MC_SUMMARY$tipping_point,
  ctq_fractions = ctq_frac_df,
  assumptions = data.frame(ID=1:length(ASSUMPTIONS), Category=names(ASSUMPTIONS), Description=unlist(ASSUMPTIONS), row.names=NULL, stringsAsFactors=FALSE)
)

for (nm in names(REPORT_TABLES)) cat(sprintf("  %-25s %d × %d\n", nm, nrow(REPORT_TABLES[[nm]]), ncol(REPORT_TABLES[[nm]])))
cat("\nMODULE 09 COMPLETE.\n")
