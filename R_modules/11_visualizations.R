# =============================================================================
# 11_VISUALIZATIONS.R
# =============================================================================
# FULLY DYNAMIC: All colors and labels derived from MANUFACTURERS vector.
# =============================================================================
if (!exists("REPORT_TABLES")) source("10_excel_export.R")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

dir.create("../outputs/charts", showWarnings = FALSE, recursive = TRUE)

cat("\n", strrep("=", 72), "\n  MODULE 11 — VISUALIZATIONS (13 premium charts)\n",
    strrep("=", 72), "\n\n", sep = "")

# ═══════════════════════════════════════════════════════════════════════════════
# DYNAMIC COLOR SYSTEM — derived from MANUFACTURERS, never hardcoded
# ═══════════════════════════════════════════════════════════════════════════════

# Three vibrant, high-contrast, colorblind-accessible hues
.MFR_PALETTE <- c("#2563EB", "#F97316", "#8B5CF6")  # Blue, Orange, Purple

# Build named vector dynamically from MANUFACTURERS
MFR_COL <- setNames(.MFR_PALETTE[seq_along(MANUFACTURERS)], MANUFACTURERS)

# CTQ dimension palette
CTQ_COL <- c(Quality = "#2563EB", Time = "#10B981", Cost = "#F59E0B")

# Rank palette
RANK_COL <- c("Rank 1" = "#2563EB", "Rank 2" = "#F97316", "Rank 3" = "#8B5CF6")

# Tier palette (HOW weight chart)
TIER_COL <- c("Top 5" = "#2563EB", "Top 6\u201315" = "#60A5FA", "Rank 16\u201335" = "#CBD5E1")

# Sensitivity palette
STABLE_COL <- c("TRUE" = "#2563EB", "FALSE" = "#EF4444")

# EA2 model comparison
MODEL_COL <- c("HoQ-Weighted" = "#2563EB", "Equal-Weighted" = "#D1D5DB")

# Typography
SLATE   <- "#0f172a"
INK     <- "#334155"
MUTED   <- "#64748b"
SUBTLE  <- "#94a3b8"
GRID_COL <- "#f1f5f9"
DONUT_BG <- "#F1F5F9"  # Donut "Other" slice


# ═══════════════════════════════════════════════════════════════════════════════
# THEME
# ═══════════════════════════════════════════════════════════════════════════════
TH <- theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, color = SLATE,
                                     margin = margin(b = 2), lineheight = 1.1),
    plot.subtitle    = element_text(size = 10.5, color = MUTED,
                                     margin = margin(b = 16), lineheight = 1.2),
    plot.caption     = element_text(size = 7.5, color = SUBTLE, hjust = 0,
                                     margin = margin(t = 14)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = GRID_COL, linewidth = 0.3),
    axis.title       = element_text(size = 10, color = MUTED),
    axis.text        = element_text(size = 9, color = INK),
    legend.position  = "top",
    legend.text      = element_text(size = 10, color = INK),
    legend.key.size  = unit(0.5, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(20, 22, 14, 22)
  )

save_chart <- function(name, p, w = 12, h = 7) {
  path <- paste0("../outputs/charts/", name, ".png")
  ggsave(path, plot = p, width = w, height = h, dpi = 250, bg = "white")
  cat(sprintf("  \u2713 %s\n", name))
}


# ══════════════════════════════════════════════════════════════════════════════
# CHART 1 — WHAT Importance Weights
# ══════════════════════════════════════════════════════════════════════════════

df1 <- REPORT_TABLES$what_weights %>%
  mutate(label = paste0(WHAT_ID, "  ", CTQ_Group),
         label = factor(label, levels = rev(label)))

p1 <- ggplot(df1, aes(x = Importance_Weight, y = label, fill = CTQ_Group)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = paste0(Weight_Pct, "%")), hjust = -0.08, size = 2.6,
            color = INK, fontface = "bold") +
  scale_fill_manual(values = CTQ_COL, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(title = "H1: WHAT Importance Weights",
       subtitle = "Quality from FDA 483 frequency  \u00b7  Time/Cost equal within family  \u00b7  CTQ applied post-HoQ",
       x = "Importance Weight", y = NULL,
       caption = "Phase 1 \u2014 House of Quality") +
  TH + theme(panel.grid.major.y = element_blank())

save_chart("01_H1_WHAT_weights", p1, 13, 8.5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 2 — HOW wj Weights (ALL 35)
# ══════════════════════════════════════════════════════════════════════════════

df2 <- REPORT_TABLES$hoq_full_chain %>%
  mutate(label = paste0(HOW_ID, " \u2014 ", substr(Description, 1, 33)),
         label = factor(label, levels = rev(label[order(HoQ_Rank)])),
         tier = case_when(HoQ_Rank <= 5 ~ "Top 5", HoQ_Rank <= 15 ~ "Top 6\u201315", TRUE ~ "Rank 16\u201335"))

p2 <- ggplot(df2, aes(x = Step6_Final_wj, y = label, fill = tier)) +
  geom_col(width = 0.52) +
  geom_vline(xintercept = 1/35, linetype = "dashed", color = "#EF4444", linewidth = 0.4) +
  geom_text(aes(label = paste0(Weight_Percent, "%")), hjust = -0.04, size = 2.2,
            color = INK, fontface = "bold") +
  scale_fill_manual(values = TIER_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(0.1), expand = expansion(mult = c(0, 0.13))) +
  annotate("text", x = 1/35 + 0.0006, y = 1, label = paste0("Equal (", round(100/35, 1), "%)"),
           size = 2.2, color = "#EF4444", hjust = 0, fontface = "italic") +
  labs(title = "H1: HoQ Criterion Weights \u2014 All 35 HOWs",
       subtitle = "HoQ basement: Raw \u2192 Normalize \u2192 Difficulty \u2192 Roof \u2192 Final wj",
       x = "Normalized Weight (wj)", y = NULL,
       caption = "Red dashed = equal weight baseline (1/35)  |  Phase 1") +
  TH + theme(axis.text.y = element_text(size = 6.5))

save_chart("02_H1_HOW_wj_all35", p2, 14, 12.5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 3 — TOPSIS Base Case
# ══════════════════════════════════════════════════════════════════════════════

df3 <- BASE_TOPSIS %>% arrange(topsis_rank) %>%
  mutate(manufacturer = factor(manufacturer, levels = rev(manufacturer)),
         cc_pct = paste0(round(cc * 100, 1), "%"),
         ann = paste0("S\u207a = ", round(s_plus, 5), "   S\u207b = ", round(s_minus, 5)))

p3 <- ggplot(df3, aes(x = cc, y = manufacturer, fill = manufacturer)) +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = SUBTLE, linewidth = 0.35) +
  geom_col(width = 0.42) +
  geom_text(aes(label = cc_pct), hjust = 1.12, size = 7, fontface = "bold", color = "white") +
  geom_text(aes(label = paste0("Rank ", topsis_rank), x = cc + 0.006),
            hjust = 0, size = 4, color = INK, fontface = "bold") +
  geom_text(aes(label = ann, x = 0.01), hjust = 0, size = 2.8, color = MUTED) +
  scale_fill_manual(values = MFR_COL, guide = "none") +
  scale_x_continuous(labels = percent_format(1), limits = c(0, 0.72), breaks = seq(0, 0.7, 0.1)) +
  labs(title = "H2: TOPSIS Manufacturer Rankings \u2014 Base Case",
       subtitle = paste0("CTQ: Q=", CTQ_BASE$Q*100, "%  \u00b7  T=", CTQ_BASE$T*100, "%  \u00b7  C=", CTQ_BASE$C*100,
                         "%   |   CC = S\u207b / (S\u207a + S\u207b)  \u00b7  Higher = closer to ideal"),
       x = "Closeness Coefficient (CC)", y = NULL,
       caption = "Phase 2 \u2014 TOPSIS + HoQ") +
  TH + theme(panel.grid.major.y = element_blank())

save_chart("03_H2_TOPSIS_base_case", p3, 12, 5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 4 — EA1 Bootstrap (ALL 35)
# ══════════════════════════════════════════════════════════════════════════════

df4 <- REPORT_TABLES$ea1_bootstrap_all %>%
  mutate(label = paste0(how_id, " \u2014 ", substr(description, 1, 26)),
         label = factor(label, levels = rev(label)),
         is_top = base_rank <= 5)

p4 <- ggplot(df4, aes(y = label)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = is_top),
                width = 0.2, linewidth = 0.5, orientation = "y") +
  geom_point(aes(x = mean_rank, color = is_top), size = 2.5) +
  geom_text(aes(x = ci_upper + 0.5, label = sprintf("P(#1)=%.0f%%", pct_rank1)),
            size = 2.1, color = MUTED, hjust = 0) +
  scale_color_manual(values = c("TRUE" = "#2563EB", "FALSE" = "#A1A1AA"), guide = "none") +
  scale_x_reverse(breaks = seq(1, 35, 2)) +
  labs(title = "EA1: Bootstrap Rank Stability \u2014 All 35 Criteria",
       subtitle = paste0(format(SIM_PARAMS$bootstrap_n, big.mark = ","), " multinomial resamples  |  95% CI"),
       x = "Mean Rank (lower = higher priority)", y = NULL,
       caption = "Phase 1 Validation \u2014 H1  |  Blue = top 5 criteria") +
  TH + theme(axis.text.y = element_text(size = 6.5))

save_chart("04_EA1_bootstrap_all35", p4, 14, 13)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 5 — Top 10 Bootstrap results Boxplot
# ══════════════════════════════════════════════════════════════════════════════

# Order HOWs by best mean rank
top10_ids <- BOOTSTRAP_RESULTS %>%
  arrange(mean_rank) %>%
  slice(1:10) %>%
  pull(how_id)

BOOTSTRAP_LONG_PLOT <- BOOTSTRAP_LONG %>%
  filter(how_id %in% top10_ids) %>%
  mutate(
    how_id = factor(how_id, levels = top10_ids)
  )

p_rank <- ggplot(BOOTSTRAP_LONG_PLOT, aes(x = how_id, y = rank)) +
  geom_boxplot(fill = "#93C5FD", color = "#1D4ED8", outlier.alpha = 0.25) +
  scale_y_reverse(breaks = 1:10) +
  labs(
    title = "Bootstrap Rank Stability of Top 10 HOW Criteria",
    subtitle = "Boxplot not working!!!!",
    x = "HOW Criterion",
    y = "Rank"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_chart("05_Top10_Bootstrap_Results_Boxplot", p_rank, 10, 6)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 6 — EA2 Discriminative Power
# ══════════════════════════════════════════════════════════════════════════════

df5 <- REPORT_TABLES$ea2_pairwise_gaps %>%
  bind_rows(data.frame(pair = "Overall Spread", hoq_gap = BENCHMARK$hoq_spread,
                        equal_gap = BENCHMARK$equal_spread,
                        pct_change = BENCHMARK$spread_improvement_pct,
                        stringsAsFactors = FALSE)) %>%
  mutate(pair = factor(pair, levels = rev(pair))) %>%
  pivot_longer(c(hoq_gap, equal_gap), names_to = "model", values_to = "gap") %>%
  mutate(model = factor(model, levels = c("hoq_gap", "equal_gap"),
                        labels = c("HoQ-Weighted", "Equal-Weighted")))

p5 <- ggplot(df5, aes(x = gap, y = pair, fill = model)) +
  geom_col(position = position_dodge(0.65), width = 0.5) +
  geom_text(aes(label = paste0(round(gap * 100, 2), "%")),
            position = position_dodge(0.65), hjust = -0.06, size = 3, color = INK, fontface = "bold") +
  scale_fill_manual(values = MODEL_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(0.1), expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Hypothesis 2: TOPSIS Discriminative Power: HoQ vs Equal Weights",
       subtitle = paste0("Kendall's \u03c4 = ", round(BENCHMARK$kendall_tau, 3),
                         "   |   Spread improvement: +", round(BENCHMARK$spread_improvement_pct, 1),
                         "%   |   HoQ weights only"),
       x = "CC Gap", y = NULL,
       caption = "Phase 2 Validation \u2014 H2  |  Isolates HoQ methodology contribution") +
  TH + theme(panel.grid.major.y = element_blank())

save_chart("06_Hyp2_discriminative_power", p5, 12, 5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 7 — CTQ Scenarios
# ══════════════════════════════════════════════════════════════════════════════

sl <- paste0(CTQ_SCENARIOS$scenario_name, "\n(Q=", CTQ_SCENARIOS$Q*100, "%)")
df6 <- CTQ_SUMMARY %>%
  mutate(sl = factor(sl[scenario_id], levels = sl),
         manufacturer = factor(manufacturer, levels = MANUFACTURERS))

p6 <- ggplot(df6, aes(x = sl, y = cc, color = manufacturer, group = manufacturer)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 5, shape = 21, stroke = 1.8, aes(fill = manufacturer), color = "white") +
  geom_text(aes(label = paste0(round(cc * 100, 1), "%")), vjust = -1.5, size = 3, fontface = "bold") +
  scale_color_manual(values = MFR_COL, name = NULL) +
  scale_fill_manual(values = MFR_COL, name = NULL) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(title = "H3: Rankings Across CTQ Risk Scenarios",
       subtitle = "Post-HoQ CTQ scaling  \u00b7  Full pipeline rebuild per scenario",
       x = NULL, y = "Closeness Coefficient (CC)",
       caption = "Phase 3 \u2014 H3  |  Left = quality-focused  \u2192  Right = cost-focused") +
  TH + theme(axis.text.x = element_text(size = 8, lineheight = 1.15))

save_chart("07_H3_CTQ_scenarios", p6, 14, 7)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 8 — Sensitivity Tornado
# ══════════════════════════════════════════════════════════════════════════════

bw  <- BASE_TOPSIS$manufacturer[BASE_TOPSIS$topsis_rank == 1]
bcc <- BASE_TOPSIS$cc[BASE_TOPSIS$topsis_rank == 1]

df7 <- REPORT_TABLES$sensitivity_all %>%
  mutate(label = paste0(how_id, " \u2014 ", substr(description, 1, 30)),
         label = factor(label, levels = rev(label)))

p7 <- ggplot(df7, aes(y = label)) +
  geom_segment(aes(x = cc_min, xend = cc_max, yend = label, color = rank_stable),
               linewidth = 5.5, lineend = "round") +
  geom_vline(xintercept = bcc, linetype = "dashed", color = INK, linewidth = 0.35) +
  scale_color_manual(values = STABLE_COL,
                     labels = c("TRUE" = "Rank Stable", "FALSE" = "Rank Flipped"), name = NULL) +
  scale_x_continuous(labels = percent_format(0.1)) +
  labs(title = paste0("H3: Sensitivity Tornado \u2014 All 35 Criteria (", bw, " CC)"),
       subtitle = "\u00b110/25/50% weight perturbation  |  Dashed = base case",
       x = "Closeness Coefficient Range", y = NULL,
       caption = "Phase 3  |  Red = rank reversal detected") +
  TH + theme(axis.text.y = element_text(size = 6.2))

save_chart("08_H3_sensitivity_tornado", p7, 14, 13)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 9 — MC Rank Frequency (stacked bar)
# ══════════════════════════════════════════════════════════════════════════════

df8 <- REPORT_TABLES$ea3_rank_freq %>%
  select(manufacturer, pct_rank1, pct_rank2, pct_rank3) %>%
  pivot_longer(-manufacturer, names_to = "rank", values_to = "pct") %>%
  mutate(rank = gsub("pct_rank", "Rank ", rank),
         manufacturer = factor(manufacturer, levels = MANUFACTURERS))

p8 <- ggplot(df8, aes(x = manufacturer, y = pct, fill = rank)) +
  geom_col(position = "stack", width = 0.48) +
  geom_text(aes(label = ifelse(pct > 2, paste0(round(pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 5.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = RANK_COL, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.03))) +
  labs(title = "H3: Monte Carlo Rank Distribution",
       subtitle = paste0(format(SIM_PARAMS$monte_carlo_n, big.mark = ","), " Dirichlet draws"),
       x = NULL, y = "% of Iterations",
       caption = "Phase 3 Validation \u2014 H3") +
  TH + theme(panel.grid.major.x = element_blank())

save_chart("09_Hyp3_MC_rank_frequency", p8, 9, 6.5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 10 — Tipping Point
# ══════════════════════════════════════════════════════════════════════════════

df9 <- MC_SUMMARY$tipping_point %>%
  filter(!is.na(pct_rank1)) %>%
  mutate(manufacturer = factor(manufacturer, levels = MANUFACTURERS))

p9 <- ggplot(df9, aes(x = q_mid, y = pct_rank1, color = manufacturer)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4.5, shape = 21, stroke = 1.5, aes(fill = manufacturer), color = "white") +
  scale_color_manual(values = MFR_COL, name = NULL) +
  scale_fill_manual(values = MFR_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105)) +
  geom_hline(yintercept = 70, linetype = "dotted", color = "#EF4444", linewidth = 0.4) +
  annotate("text", x = 0.04, y = 73, label = "70% threshold (H3)",
           size = 2.8, color = "#EF4444", hjust = 0, fontface = "italic") +
  labs(title = "EA3: Quality Weight Tipping Point",
       subtitle = "P(Rank=1) by Quality weight bin from MC draws",
       x = "Quality Weight (%)", y = "P(Rank = 1)",
       caption = "Phase 3 \u2014 H3") + TH

save_chart("10_EA3_tipping_point", p9, 12, 6.5)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 11 — MC CC Distribution (density)
# ══════════════════════════════════════════════════════════════════════════════

df10 <- MC_RESULTS %>% mutate(manufacturer = factor(manufacturer, levels = MANUFACTURERS))
base_cc_df <- data.frame(manufacturer = factor(MANUFACTURERS, levels = MANUFACTURERS),
                          base_cc = sapply(MANUFACTURERS, function(m) BASE_TOPSIS$cc[BASE_TOPSIS$manufacturer == m]))

p10 <- ggplot(df10, aes(x = cc, fill = manufacturer, color = manufacturer)) +
  geom_density(alpha = 0.3, linewidth = 0.9) +
  geom_vline(data = base_cc_df, aes(xintercept = base_cc, color = manufacturer),
             linetype = "dashed", linewidth = 0.65, show.legend = FALSE) +
  scale_fill_manual(values = MFR_COL, name = NULL) +
  scale_color_manual(values = MFR_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(1)) +
  labs(title = "EA3: Monte Carlo CC Distribution",
       subtitle = paste0(format(SIM_PARAMS$monte_carlo_n, big.mark = ","), " iterations  |  Dashed = base case"),
       x = "Closeness Coefficient (CC)", y = "Density",
       caption = "Phase 3 \u2014 H3  |  Wider spread = more sensitive to CTQ variation") + TH

save_chart("11_EA3_MC_cc_distribution", p10, 12, 6)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 12 — MC Rank 1 Probability Donut Charts (DYNAMIC)
# ══════════════════════════════════════════════════════════════════════════════

mc_rf <- REPORT_TABLES$ea3_rank_freq

for (m_idx in seq_along(MANUFACTURERS)) {
  mfr <- MANUFACTURERS[m_idx]
  pct <- mc_rf$pct_rank1[mc_rf$manufacturer == mfr]
  col <- MFR_COL[[mfr]]  # Double bracket for named lookup

  donut_df <- data.frame(
    category = c("Rank 1", "Other"),
    value    = c(pct, 100 - pct)
  )

  p_donut <- ggplot(donut_df, aes(x = 2, y = value, fill = category)) +
    geom_col(width = 1, color = "white", linewidth = 2) +
    coord_polar("y", start = 0) +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = c("Rank 1" = col, "Other" = DONUT_BG), guide = "none") +
    annotate("text", x = 0.5, y = 0, label = paste0(round(pct, 0), "%"),
             size = 14, fontface = "bold", color = col) +
    labs(title = paste0(mfr, " \u2014 P(Rank 1)"),
         subtitle = paste0(format(SIM_PARAMS$monte_carlo_n, big.mark = ","), " simulated CTQ scenarios")) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = SLATE),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = MUTED, margin = margin(b = 4)),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(12, 12, 12, 12))

  save_chart(paste0("12_EA3_donut_", mfr), p_donut, 5, 5)
}


# ══════════════════════════════════════════════════════════════════════════════
# CHART 13 — CC vs Quality Weight Scatter
# ══════════════════════════════════════════════════════════════════════════════

df12 <- MC_RESULTS %>%
  mutate(manufacturer = factor(manufacturer, levels = MANUFACTURERS))

p12 <- ggplot(df12, aes(x = Q_weight, y = cc, color = manufacturer)) +
  geom_point(alpha = 0.2, size = 1, shape = 16) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.3, span = 0.4) +
  scale_color_manual(values = MFR_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(1)) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(title = "EA3: TOPSIS Closeness Coefficient vs Quality Weight",
       subtitle = "Each dot = one MC iteration  |  LOESS trend lines show directional relationship",
       x = "Quality Weight (%)", y = "Closeness Coefficient (CC)",
       caption = "Phase 3 \u2014 H3  |  Clear band separation = stable ranking across CTQ space") +
  TH + guides(color = guide_legend(override.aes = list(alpha = 1, size = 3.5)))

save_chart("13_EA3_CC_vs_quality_scatter", p12, 13, 7)


# ══════════════════════════════════════════════════════════════════════════════
# CHART 14 — Combined Donut Panel (FULLY DYNAMIC)
# ══════════════════════════════════════════════════════════════════════════════

donut_all <- data.frame()
for (m in MANUFACTURERS) {
  pct <- mc_rf$pct_rank1[mc_rf$manufacturer == m]
  donut_all <- rbind(donut_all,
    data.frame(manufacturer = m, category = "Rank 1", value = pct, stringsAsFactors = FALSE),
    data.frame(manufacturer = m, category = "Other", value = 100 - pct, stringsAsFactors = FALSE))
}
donut_all$manufacturer <- factor(donut_all$manufacturer, levels = MANUFACTURERS)

label_df <- mc_rf %>%
  mutate(manufacturer = factor(manufacturer, levels = MANUFACTURERS),
         label = paste0(round(pct_rank1, 0), "%"))

# Build donut fill mapping DYNAMICALLY from MANUFACTURERS
donut_fill <- setNames(
  c(rbind(unname(MFR_COL), rep(DONUT_BG, length(MANUFACTURERS)))),
  c(rbind(paste0(MANUFACTURERS, ".Rank 1"), paste0(MANUFACTURERS, ".Other")))
)

p13 <- ggplot(donut_all, aes(x = 2, y = value, fill = interaction(manufacturer, category))) +
  geom_col(width = 1, color = "white", linewidth = 1.5) +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +
  facet_wrap(~manufacturer, nrow = 1) +
  geom_text(data = label_df, aes(x = 0.5, y = 50, label = label),
            inherit.aes = FALSE, size = 12, fontface = "bold", color = SLATE) +
  scale_fill_manual(values = donut_fill, guide = "none") +
  labs(title = "EA3: Rank 1 Probability \u2014 All Manufacturers",
       subtitle = paste0(format(SIM_PARAMS$monte_carlo_n, big.mark = ","), " Dirichlet MC scenarios")) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 17, hjust = 0.5, color = SLATE),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = MUTED, margin = margin(b = 10)),
        strip.text = element_text(face = "bold", size = 14, color = SLATE),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(18, 18, 12, 18))

save_chart("14_EA3_donut_panel", p13, 15, 5.5)

cat(sprintf("\n  14 charts saved to ../outputs/charts/\n"))
cat("\nMODULE 11 COMPLETE.\n")
