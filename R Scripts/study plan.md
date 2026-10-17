# Updated Analysis Plan

The new variables let us do three things we couldn't before:

1. **Case ID:** Conduct a true self-vs-attending agreement analysis.
2. **Attending ID:** Adjust for which attending rated the case.
3. **Date:** Evaluate changes over the 5-month study period.

## Analysis Datasets

| Dataset | Unit | n | Purpose |
|----------|------|------|---------|
| **A. Attending ratings** | Form | 171 | Primary analysis: autonomy and performance |
| **B. Paired cases** | Case | 80 (once Data Checks are fixed) | Self vs. attending agreement |
| **C. Respondents** | Person | 35 residents + 3 attendings | Perception and feasibility |

Using only attending ratings for the primary analysis avoids mixing two types of raters. Self-assessments will be analyzed separately.

---

# Tables

## Table 1. Resident Characteristics
- **n = 35**
- Stratified by PGY
- Age
- Sex
- Number of evaluations

## Table 2. Case Characteristics
- **n = 159 cases**
- Stratified by PGY
- Procedure
- Open vs. laparoscopic approach
- Complexity
- Attending
- Study month

## Table 3. Attending-Rated Zwisch and SIMPL by PGY
- Report n (%)
- Include trend test
- Same layout as the 2021 paper for direct comparison

## Table 4. Mixed-Effects Ordinal Models (`clmm`)

### Zwisch Model
```r
Zwisch ~ PGY + complexity + procedure + approach +
         attending + time + (1 | resident)
```

### SIMPL Model
Same model structure as above.

### Reporting
- Odds ratios (ORs)
- 95% confidence intervals (CIs)

## Table 5. Self vs. Attending Agreement (80 Pairs)

Overall and stratified by PGY:

- % exact agreement
- % agreement within one level
- Weighted kappa with bootstrap CIs (resampled by resident)
- Direction of disagreement:
  - % over-rating themselves
  - % under-rating themselves

## Table 6. Implementation Outcomes

- Self-reported evaluation time
- Utility
- Feedback quality
- Ease of use
- Sustainability
- Summary per respondent
- Comparison of attendings vs. residents
- Self-assessment uptake:
  - Proportion of cases with a self-assessment (~50%)

### Supplementary Tables

1. PGY × complexity (case assignment patterns)
2. Ratings by attending
3. Results by procedure and by open vs. laparoscopic approach
4. Sensitivity analysis excluding flagged cases

---

# Figures

## Figure 1. Study Flow Diagram

```text
252 forms
    ↓
159 cases
    ↓
Flagged / Excluded cases
    ↓
Datasets A, B, and C
```

## Figure 2. Zwisch and SIMPL by PGY

- 100% stacked bar charts
- Attending ratings only
- Two-panel figure:
  - Zwisch
  - SIMPL
- Main manuscript figure

## Figure 3. Predicted Probabilities

From mixed-effects models:

- Probability of "Supervision Only"
- Probability of "Practice Ready"
- Displayed by PGY
- Separate lines by complexity level

## Figure 4. Self vs. Attending Assessment

### A. Agreement Heatmaps

- Zwisch: 4 × 4 grid
- SIMPL: 5 × 5 grid
- Based on 80 paired evaluations

### B. Mean Difference by PGY

```text
Self-rating − Attending rating
```

- Mean difference
- 95% confidence intervals
- Tests whether:
  - Junior residents rate themselves higher than attendings do
  - Senior residents rate themselves lower than attendings do

## Figure 5. Trends Over Time

- Zwisch scores across study months
- SIMPL scores across study months
- Stratified by PGY
- Cumulative evaluations per month to demonstrate adoption

## Figure 6. Feasibility

- Diverging Likert plot
- Distribution of evaluation completion times

### Supplementary Figures

- Zwisch × SIMPL heatmap (correlation between scales)
- Rating distributions by attending
- Resident-level variation (caterpillar plot)

---

# Key Caveats for the Methods Section

1. **Time Effect**
   - Five months and approximately seven forms per resident support an exploratory time variable.
   - Insufficient to model a true learning curve.

2. **Complexity**
   - Use attending-rated complexity.
   - Interpret complexity within PGY due to case assignment patterns.

3. **Ceiling Effects**
   - Ease of use and future use scores are 5 for every response.
   - Report descriptively only.

4. **Completion Rate**
   - No valid denominator is available.
   - Exclude completion-rate analyses.
   - Use self-assessment uptake instead.

5. **Data Validation Requirements**
   - Dataset B and Table 1 depend on resolution of the Data Checks sheet.
   - Key issues:
     - Sex coding inconsistencies
     - 13 cases with two attending evaluations

Before running the final analyses, all Data Checks issues should be resolved.