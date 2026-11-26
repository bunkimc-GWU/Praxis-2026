# A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty

**Author:** Bunkim Girish Chokshi
**Degree / Institution: DE, Engineering Management, George Washington University, 2026
**Status:** Completed

## Overview

This repository contains the R code used in the praxis analysis. The praxis develops a hybrid decision model for selecting sterile injectable drug contract manufacturing organizations (CMOs):

- **House of Quality (HoQ):** translates stakeholder requirements into weighted evaluation criteria.
- **TOPSIS** (Technique for Order of Preference by Similarity to Ideal Solution): ranks candidate CMOs by their relative closeness to an ideal solution.
- **Uncertainty treatment:** Uses a Monte-Carlo Simulation to address changes in CTQ prefernces.

# HoQ–TOPSIS pipeline in R

R portion of the analysis: FDA inspection records → House of Quality weights → TOPSIS, plus the RQ1–RQ3 studies and the cross-validation (not used in praxis). Each module recomputes its stage and then compares its results with the values saved in the workbook; `outputs/reconciliation_checks.csv` lists every comparison with PASS/FAIL.

## Requirements
- R ≥ 4.1
- install.packages(c("readxl", "Rcpp"))
- A C++ compiler for Rcpp: Rtools (Windows), Xcode command-line tools (macOS),  build-essential (Linux). Without one, an exact pure-R generator is used; that  is fine for everything except the RQ1 bootstrap, which then takes hours.

## Run
1. Put "HOQ and TOPSIS 17.xlsm" in this folder (or set HOQ_WORKBOOK to its path).  Open it in Excel and save once first, so the saved formula values are current.
2. In R: setwd("path/to/hoq_topsis_R"); source("run_all.R") or from a terminal: Rscript run_all.R
   With Rcpp this takes a few minutes; the bootstrap is the slow step.
   Set SKIP_BOOTSTRAP=1 to skip it while testing.
4. Results go to outputs/: one CSV per stage, results_summary.txt and reconciliation_checks.csv.

## Modules
| File | Stage | Workbook tabs reproduced |
| R/00_utils.R | Settings, workbook reader, mulberry32, Box–Muller / Gamma / Beta / Dirichlet samplers, Excel-equivalent statistics, check log | Simulation Parameters | src/mulberry32.cpp | Fast generator, bootstrap counts, Fisher–Yates shuffle |
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

## Software
- RStudio
- Microsoft Excel

The data can be shared. It is not confidential or proprietary.
Chokshi, B. (2026). A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty. Praxis, George Washington University.

All rights reserved.
