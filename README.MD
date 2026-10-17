# SIMPL-Based EPA Assessment in General Surgery Residency

Analysis code for a prospective single-center study of intraoperative autonomy (Zwisch scale) and performance (SIMPL scale) ratings of general surgery residents across three index procedures (appendectomy, cholecystectomy, inguinal hernia repair), April–August 2026.

## Repository structure

```
surg_training_evaluation/
├── R Scripts/
│   ├── run_all.R                          # runs the full analysis in order (English -> Results/)
│   ├── run_all_es.R                       # same analysis, Spanish outputs -> Resultados/
│   ├── EPA_helpers.R                      # paths, data loading, formatting, tests, table/figure writers
│   ├── Tables/
│   │   ├── EPA_Tables_1-3.R               # Tables 1–3 (residents, cases, ratings by PGY)
│   │   ├── EPA_Table4_Figure1.R           # Table 4, Table S1, Figure 1 (mixed-effects ordinal models)
│   │   ├── EPA_Tables_5-6_Supplementary.R # Tables 5–6, Tables S2–S6
│   │   └── EPA_Tables_to_docx.R           # all HTML tables -> All_Tables.docx
│   └── Figures/
│       ├── EPA_Figures_2-4.R              # Figures 2–4
│       ├── EPA_Figures_5-6.R              # Figures 5–6
│       ├── EPA_Supplementary_Figures.R    # Figures S1–S3
│       └── EPA_Flowchart.R                # study flow diagram (Mermaid)
├── Dataset/                               # NOT tracked (see Data availability)
└── Results/
    ├── Tables/  MD/ · HTML/ · All_Tables.docx
    ├── Figures/ PNG/ · Legends/ · Results/
    └── session_info.txt                   # R and package versions
Resultados/                                # Spanish outputs (run_all_es.R)
    ├── Tablas/   MD/ · HTML/ · Todas_las_Tablas.docx
    └── Figuras/  PNG/ · Leyendas/ · Resultados/
```

## Running the analysis

1. Place the study database at `Dataset/EPA_SIMPL_Database.xlsx` (sheet `Data`).
2. Set `project_dir` at the top of `R Scripts/EPA_helpers.R` and `scripts_root` in `R Scripts/run_all.R` to your local paths.
3. Run `R Scripts/run_all.R` (about 2–3 minutes). Missing packages are installed automatically.
4. For the Spanish version of every table, figure, legend and results file, run `R Scripts/run_all_es.R`
   (or set `Sys.setenv(EPA_LANG = "es")` before sourcing a single script). The analysis is identical;
   only the language and output folders change.

Main packages: `readxl`, `dplyr`, `tidyr`, `knitr`, `ordinal`, `ggplot2`, `patchwork`, `forestploter`, `MASS`.

## Methods summary

- **Descriptive comparisons:** χ² or Fisher exact tests (Monte Carlo for tables larger than 2×2) and Mann–Whitney U / Kruskal–Wallis tests.
- **Primary models:** mixed-effects cumulative logit models (`ordinal::clmm`) of attending-rated Zwisch and SIMPL scores, with postgraduate year, case complexity, procedure, approach, attending, and time as fixed effects and a random intercept for resident.
- **Self vs attending agreement:** quadratic-weighted κ with cluster-bootstrap CIs (residents resampled).
- **Sensitivity analyses:** exclusion of cases flagged during data verification; postgraduate year as a categorical variable.

## Data availability

The study database contains information on individual residents and evaluators and is not included in this repository. De-identified data may be available from the corresponding author on reasonable request and with institutional approval.