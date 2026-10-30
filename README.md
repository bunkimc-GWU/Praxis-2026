# A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty

**Author:** B. Chokshi
**Degree / Institution: DE, Engineering Management, George Washington University, 2026
**Status:** Completed

## Overview

This repository contains the Excel workbooks and R code used in the praxis analysis. The praxis develops a hybrid decision model for selecting sterile injectable drug contract manufacturing organizations (CMOs):

- **House of Quality (HoQ):** translates stakeholder requirements into weighted evaluation criteria.
- **TOPSIS** (Technique for Order of Preference by Similarity to Ideal Solution): ranks candidate CMOs by their relative closeness to an ideal solution.
- **Uncertainty treatment:** Uses a Monte-Carlo Simulation to address changes in CTQ prefernces.

## Repository structure

```
├── README.md
├── praxis.Rproj
├── data/
│   ├── raw/           # original inputs (not committed; see Data availability)
│   └── processed/     # cleaned data produced by scripts
├── excel/             # HoQ and supporting workbooks
├── R/                 # reusable functions
├── scripts/           # analysis scripts, run in numbered order
├── output/
│   ├── figures/
│   └── tables/
└── docs/              # methodology notes and codebook
```

## Software

- RStudio
- Microsoft Excel

## Reproducing the analysis

1. Clone the repository and open RStudio files.
2. Place the input data in `00_config_from_excel.R`.
4. Run the scripts in `scripts/` in numbered order:
   - `01_waht_engine.R`
   - `02_hoq_engine.R`
   - `03_topsis.R`
   - `04_ctq_scenarios.R`
   - `05_sensitivity.R`
   - `06_bootstrap.R`
   - `07_benchmark.R`
   - `08_montecarlo.R`
   - `09_report_tables.R`
5. Results are written to `10_excel_export.r`.
6. Visualizations are created in  '11_visualizations.R'

7. The entire pipeline can be run using the "run_from_excel.R" module

The data can be shared. It is not confidential or proprietary.

Chokshi, B. (2026). A Hybrid HoQ-TOPSIS Decision Model for Selecting Sterile Injectable Drug Contract Manufacturers Under Cost-Time-Quality Uncertainty. Praxis, George Washington University.

All rights reserved.
