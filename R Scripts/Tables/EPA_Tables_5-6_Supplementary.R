# =============================================================================
# EPA / SIMPL study — Tables 5–6 and Supplementary Tables S2–S6
#
#   Table 5.  Agreement between resident self-assessment and attending rating
#             (paired cases), overall and by postgraduate year
#   Table 6.  Implementation outcomes, attending surgeons vs residents
#   Table S2. Case complexity within postgraduate year: assignment and ratings
#   Table S3. Attending-rated autonomy and performance by evaluating attending
#   Table S4. Attending-rated autonomy and performance by procedure and approach
#   Table S5. Sensitivity analysis of Table 4 models excluding flagged cases
#   Table S6. Sensitivity analysis of Table 5 agreement excluding flagged pairs
#   (Table S1 is produced by EPA_Table4_Figure1.R)
#
# Weighted kappa: quadratic weights; 95% CI by cluster bootstrap (residents
# resampled with replacement).
#
# Requires EPA_helpers.R in the same folder (paths are set there).
# =============================================================================

# ---- Options ----------------------------------------------------------------
SHOW_PGY_KAPPA <- TRUE   # FALSE = report weighted kappa for the overall column only
B_BOOT         <- 2000   # bootstrap replicates for kappa CIs

# ---- Load shared helpers ----------------------------------------------------
if (!exists("EPA_HELPERS_LOADED")) {
  script_dir <- local({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    f <- gsub("~+~", " ", f, fixed = TRUE)   # Rscript encodes spaces in the path as ~+~
    if (length(f)) return(dirname(normalizePath(f)))
    for (i in rev(seq_len(sys.nframe()))) if (!is.null(o <- sys.frame(i)$ofile)) return(dirname(normalizePath(o)))
    getwd()
  })
  # EPA_helpers.R may sit next to this script, one folder up ("R Scripts"),
  # or in the sibling Tables/Figures folder
  cand <- c(file.path(script_dir, "EPA_helpers.R"),
            file.path(dirname(script_dir), "EPA_helpers.R"),
            file.path(dirname(script_dir), c("Tables", "Figures"), "EPA_helpers.R"))
  helpers <- cand[file.exists(cand)][1]
  if (is.na(helpers)) stop("EPA_helpers.R not found near ", script_dir)
  source(helpers, encoding = "UTF-8")
}
load_packages("ordinal")
set.seed(2026)

d   <- load_epa()
att <- d %>% filter(Evaluator_Type == 1)
case_flags  <- flag_cases(d)
flagged_ids <- case_flags$Case_ID[case_flags$flagged]
pgy_note    <- "Comparisons are across postgraduate years."

# =============================================================================
# Agreement helpers (Table 5 and S6)
# =============================================================================
make_pairs <- function(df) {
  df %>%
    group_by(Case_ID) %>%
    filter(n() == 2, sum(Evaluator_Type == 1) == 1, sum(Evaluator_Type == 2) == 1,
           n_distinct(Resident_ID) == 1) %>%
    summarise(Resident_ID = first(Resident_ID), PGY = first(PGY),
              Z_att = Zwisch_Autonomy[Evaluator_Type == 1],   Z_self = Zwisch_Autonomy[Evaluator_Type == 2],
              S_att = SIMPL_Performance[Evaluator_Type == 1], S_self = SIMPL_Performance[Evaluator_Type == 2],
              .groups = "drop") %>%
    mutate(Z_diff = Z_self - Z_att, S_diff = S_self - S_att)
}

wkappa <- function(r1, r2, K) {          # quadratic-weighted kappa, fixed categories 1..K
  if (length(unique(r1)) < 2 || length(unique(r2)) < 2) return(NA_real_)
  O <- table(factor(r1, 1:K), factor(r2, 1:K)) / length(r1)
  E <- outer(rowSums(O), colSums(O))
  W <- 1 - outer(1:K, 1:K, function(i, j) (i - j)^2) / (K - 1)^2
  (sum(W * O) - sum(W * E)) / (1 - sum(W * E))
}
kappa_boot <- function(pp, a, s, K) {
  est <- wkappa(pp[[a]], pp[[s]], K)
  if (is.na(est)) return("NE")
  ids <- unique(pp$Resident_ID)
  bs <- replicate(B_BOOT, {
    smp <- sample(ids, length(ids), replace = TRUE)
    bb  <- do.call(rbind, lapply(smp, function(i) pp[pp$Resident_ID == i, ]))
    wkappa(bb[[a]], bb[[s]], K)
  })
  ci <- quantile(bs, c(.025, .975), na.rm = TRUE, names = FALSE)
  fmt_ci(est, ci[1], ci[2])
}

build_agreement <- function(pp) {
  groups <- levels(pp$PGY)
  subsets <- c(list(pp), lapply(groups, function(l) pp[pp$PGY == l, , drop = FALSE]))
  rows <- list()
  add <- function(label, vals, p = "") rows[[length(rows) + 1]] <<- c(label, vals, p)
  for (sc in list(list("Z", "Zwisch autonomy level (1–4)", 4), list("S", "SIMPL performance rating (1–5)", 5))) {
    a <- paste0(sc[[1]], "_att"); s <- paste0(sc[[1]], "_self"); df <- paste0(sc[[1]], "_diff")
    exact  <- factor(ifelse(pp[[df]] == 0, "yes", "no"), levels = c("yes", "no"))
    within <- factor(ifelse(abs(pp[[df]]) <= 1, "yes", "no"), levels = c("yes", "no"))
    direc  <- factor(sign(pp[[df]]), levels = c(1, 0, -1))
    add(bold(sc[[2]]), rep("", length(subsets)))
    t <- test_cat(exact, pp$PGY)
    add(indent("Exact agreement, n (%)"),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] == 0), nrow(x)), ""), mark(t$p, t$test))
    t <- test_cat(within, pp$PGY)
    add(indent("Agreement within one level, n (%)"),
        vapply(subsets, function(x) fmt_npct(sum(abs(x[[df]]) <= 1), nrow(x)), ""), mark(t$p, t$test))
    t <- test_cat(direc, pp$PGY)
    add(indent("Self-rating higher than attending, n (%)"),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] > 0), nrow(x)), ""), mark(t$p, t$test))
    add(indent("Self-rating lower than attending, n (%)"),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] < 0), nrow(x)), ""))
    t <- test_cont(pp[[df]], pp$PGY)
    add(indent(paste0("Self ", MINUS, " attending difference, mean (SD)")),
        vapply(subsets, function(x) fmt_meansd(x[[df]]), ""), mark(t$p, t$test))
    kap <- vapply(seq_along(subsets), function(i)
      if (i == 1 || SHOW_PGY_KAPPA) kappa_boot(subsets[[i]], a, s, sc[[3]]) else "", "")
    add(indent("Weighted κ (95% CI)"), kap)
  }
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  cols <- c("Overall", groups)
  names(tab) <- c("Characteristic", cols, "P value")
  ns <- vapply(subsets, nrow, 0)
  list(table = tab, header = c("Characteristic", paste0(cols, " (n = ", ns, ")"), "P value"))
}

agreement_notes <- function(pp) c(
  sprintf("Analysis restricted to cases with exactly one attending evaluation and one resident self-assessment (n = %d pairs from %d residents); column n values are pairs.",
          nrow(pp), n_distinct(pp$Resident_ID)),
  "Direction compares the resident's self-rating with the attending's rating of the same case; the P value for direction tests the distribution of higher, identical, and lower self-ratings.",
  sprintf("Weighted κ uses quadratic weights; 95%% CIs from a cluster bootstrap resampling residents (%s replicates).%s",
          format(B_BOOT, big.mark = ","),
          if (SHOW_PGY_KAPPA) " NE, not estimable because one rater gave the same rating to every case in the stratum." else
            " κ is reported overall only because ratings within a single postgraduate year span too narrow a range for a stable estimate."),
  if (SHOW_PGY_KAPPA) "Within-year κ values are attenuated by the narrow range of ratings within each postgraduate year and should be interpreted with the overall estimate.",
  paste(pgy_note, "Pairs are not independent (several per resident); P values are descriptive."),
  if (SHOW_PGY_KAPPA) abbr("CI", "NE", "PGY", "SD") else abbr("CI", "PGY", "SD")
)

# =============================================================================
# TABLE 5 — Self vs attending agreement
# =============================================================================
pairs_all <- make_pairs(d)
t5 <- build_agreement(pairs_all)
write_table(
  t5$table, "5",
  "Agreement Between Resident Self-Assessment and Attending Rating by Postgraduate Year",
  "Agreement between residents' self-assessed and attendings' ratings of intraoperative autonomy (Zwisch) and performance (SIMPL) for the same surgical case, overall and by postgraduate year.",
  agreement_notes(pairs_all), t5$header
)

# =============================================================================
# TABLE 6 — Implementation outcomes
#   Respondent level: each respondent's mean across their forms (items on
#   attending forms -> attending; items on self-assessment forms -> resident).
#   Form level: descriptive distribution across all forms.
# =============================================================================
items <- list(
  list("Eval_Time_Min",         "Time to complete evaluation, minutes", FALSE),
  list("Utility_Performance",   "Usefulness for assessing performance (1–5)", TRUE),
  list("Utility_Feedback",      "Usefulness of feedback (1–5)", TRUE),
  list("Feasibility_Ease",      "Ease of use (1–5)", TRUE),
  list("Sustain_Institutional", "Support for permanent institutional use (1–5)", TRUE),
  list("Sustain_FutureUse",     "Willingness to continue using the model (1–5)", TRUE)
)
resp <- d %>%
  mutate(Respondent = ifelse(Evaluator_Type == 1, paste0("ATT", Evaluator_ID), paste0("RES_", Resident_ID)),
         Role = factor(ifelse(Evaluator_Type == 1, "Attending", "Resident"), levels = c("Attending", "Resident"))) %>%
  group_by(Respondent, Role) %>%
  summarise(across(all_of(vapply(items, `[[`, "", 1)), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
resp_att <- resp %>% filter(Role == "Attending")
resp_res <- resp %>% filter(Role == "Resident")
form_att <- d %>% filter(Evaluator_Type == 1)
form_res <- d %>% filter(Evaluator_Type == 2)

rows6 <- list()
add6 <- function(...) rows6[[length(rows6) + 1]] <<- c(...)
for (it in items) {
  v <- it[[1]]
  t <- test_cont(resp[[v]], resp$Role)
  add6(bold(it[[2]]), "", "", "", "", "")
  add6(indent("Median (IQR)"), fmt_mediqr(resp_att[[v]]), fmt_mediqr(resp_res[[v]]), mark(t$p, t$test),
       fmt_mediqr(form_att[[v]]), fmt_mediqr(form_res[[v]]))
  if (it[[3]]) {
    add6(indent("Highest rating (5), n (%)"), "—", "—", "",
         fmt_npct(sum(form_att[[v]] == 5), nrow(form_att)), fmt_npct(sum(form_res[[v]] == 5), nrow(form_res)))
  } else {
    add6(indent("≤3 minutes, n (%)"), "—", "—", "",
         fmt_npct(sum(form_att[[v]] <= 3), nrow(form_att)), fmt_npct(sum(form_res[[v]] <= 3), nrow(form_res)))
  }
}
t6 <- as.data.frame(do.call(rbind, rows6), stringsAsFactors = FALSE)
names(t6) <- c("Characteristic", "ra", "rr", "P value", "fa", "fr")

cases_all  <- n_distinct(d$Case_ID)
cases_self <- n_distinct(d$Case_ID[d$Evaluator_Type == 2])
write_table(
  t6, "6",
  "Implementation Outcomes of the SIMPL-Based EPA Assessment Model Among Attending Surgeons and Residents",
  sprintf("Time burden, perceived usefulness, ease of use, and sustainability reported by attending surgeons and residents, summarized per respondent and per evaluation form. A resident self-assessment was completed in %d of %d evaluated cases (%s%%).",
          cases_self, cases_all, num(100 * cases_self / cases_all)),
  c("Per-respondent values are the median (IQR) of each respondent's mean score across all of their evaluation forms; P values compare attending surgeons with residents at the respondent level.",
    "Per-form columns are descriptive and count each evaluation form separately; proportions are reported at the form level only because the number of forms per respondent differed widely. —, not applicable.",
    "Items on attending forms were attributed to the evaluating attending, and items on self-assessment forms to the resident. Likert items: 1, lowest to 5, highest agreement or usefulness. Evaluation time was self-reported in whole minutes.",
    sprintf("Only %d attending surgeons participated; respondent-level comparisons are exploratory.", nrow(resp_att)),
    "Self-assessment uptake by postgraduate year is shown in Table 2.",
    abbr("IQR")),
  c("Outcome",
    sprintf("Attendings, per respondent (n = %d)", nrow(resp_att)),
    sprintf("Residents, per respondent (n = %d)", nrow(resp_res)), "P value",
    sprintf("Attending forms (n = %d)", nrow(form_att)),
    sprintf("Self-assessment forms (n = %d)", nrow(form_res))),
  c("Outcome",
    sprintf("Attending surgeons<br>(n = %d)", nrow(resp_att)),
    sprintf("Residents<br>(n = %d)", nrow(resp_res)), "P value",
    sprintf("Attending forms<br>(n = %d)", nrow(form_att)),
    sprintf("Self-assessment forms<br>(n = %d)", nrow(form_res))),
  spanners = list(list("", 1), list("Per respondent", 3), list("Per evaluation form", 2))
)

# =============================================================================
# TABLE S2 — Complexity within PGY: assignment and attending ratings
# =============================================================================
pgys <- levels(att$PGY)
rowsS2 <- list()
addS2 <- function(...) rowsS2[[length(rowsS2) + 1]] <<- c(...)
t <- test_cat(att$Complexity, att$PGY)
addS2(bold("Case assignment, n (%)"), rep("", length(pgys) + 1), mark(t$p, t$test))
for (cx in levels(att$Complexity))
  addS2(indent(cx), fmt_npct(sum(att$Complexity == cx), nrow(att)),
        vapply(pgys, function(g) fmt_npct(sum(att$Complexity == cx & att$PGY == g), sum(att$PGY == g)), ""), "")
for (oc in list(list("Zwisch_Autonomy", "Zwisch score (1–4), median (IQR)"),
                list("SIMPL_Performance", "SIMPL score (1–5), median (IQR)"))) {
  addS2(bold(oc[[2]]), rep("", length(pgys) + 1), "")
  for (cx in levels(att$Complexity)) {
    sub_cx <- att[att$Complexity == cx, ]
    t <- test_cont(sub_cx[[oc[[1]]]], sub_cx$PGY)
    addS2(indent(cx), fmt_mediqr(sub_cx[[oc[[1]]]]),
          vapply(pgys, function(g) fmt_mediqr(sub_cx[[oc[[1]]]][sub_cx$PGY == g]), ""), mark(t$p, t$test))
  }
}
tS2 <- as.data.frame(do.call(rbind, rowsS2), stringsAsFactors = FALSE)
write_table(
  tS2, "S2",
  "Case Complexity Within Postgraduate Year: Case Assignment and Attending Ratings",
  sprintf("Distribution of attending-rated case complexity across postgraduate years and the corresponding Zwisch and SIMPL ratings within each complexity stratum (%d attending evaluations).", nrow(att)),
  c("Data are n (%) or median (IQR). Case assignment percentages use each postgraduate year's evaluations as denominator.",
    paste(pgy_note, "For ratings, postgraduate years are compared within each complexity stratum."),
    "Evaluations are not independent (multiple per resident); P values are descriptive. Adjusted estimates are shown in Table 4.",
    abbr("IQR", "PGY")),
  c("Characteristic", sprintf("Overall (n = %d)", nrow(att)),
    sprintf("%s (n = %d)", pgys, as.integer(table(att$PGY))), "P value")
)

# =============================================================================
# TABLE S3 — Ratings by evaluating attending
# =============================================================================
outcome_specs <- list(
  list(type = "cat",  var = "Zwisch",            label = "Zwisch autonomy level"),
  list(type = "cont", var = "Zwisch_Autonomy",   label = "Zwisch score (1–4)"),
  list(type = "cat",  var = "SIMPL",             label = "SIMPL performance rating"),
  list(type = "cont", var = "SIMPL_Performance", label = "SIMPL score (1–5)")
)
tS3 <- build_table(att, "Attending",
                   c(list(list(type = "cat", var = "PGY", label = "Postgraduate year of resident")), outcome_specs))
write_table(
  tS3$table, "S3",
  "Attending-Rated Autonomy and Performance by Evaluating Attending",
  sprintf("Case mix by resident postgraduate year and distribution of Zwisch and SIMPL ratings assigned by each of the %d evaluating attendings (%d attending evaluations).",
          nlevels(att$Attending), nrow(att)),
  c("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
    "Comparisons are across attendings. Unadjusted; differences in ratings partly reflect differences in the postgraduate year of residents evaluated. Attending-adjusted estimates are shown in Table 4.",
    "Evaluations are not independent (multiple per resident); P values are descriptive.",
    abbr("IQR", "PGY")),
  tS3$header
)

# =============================================================================
# TABLE S4 — Ratings by procedure and by surgical approach
# =============================================================================
specs_S4 <- c(list(list(type = "cont", var = "PGY_Year", label = "Postgraduate year")), outcome_specs)
a <- build_table(att, "Procedure_f", specs_S4, overall = TRUE)
b <- build_table(att, "Approach",    specs_S4, overall = FALSE)
tS4 <- cbind(a$table, b$table[, -1])
names(tS4) <- make.unique(names(tS4))
write_table(
  tS4, "S4",
  "Attending-Rated Autonomy and Performance by Procedure and Surgical Approach",
  sprintf("Unadjusted distribution of Zwisch and SIMPL ratings across the three index procedures and by open vs laparoscopic approach (%d attending evaluations).", nrow(att)),
  c("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
    "The first P value column compares procedures; the second compares surgical approaches. Estimates adjusted for postgraduate year, complexity, attending, and time are shown in Table 4.",
    "Evaluations are not independent (multiple per resident); P values are descriptive.",
    abbr("IQR")),
  c(a$header, b$header[-1]), with_br(c(a$header, b$header[-1])),
  spanners = list(list("", 2), list("Procedure", 4), list("Surgical approach", 3))
)

# =============================================================================
# TABLE S5 — Sensitivity: Table 4 models excluding flagged cases
# =============================================================================
prep_model <- function(df) {
  df %>% mutate(
    Resident_ID = factor(Resident_ID),
    Months      = as.numeric(Eval_Date - min(d$Eval_Date)) / 30.4375,
    Zwisch_o    = factor(Zwisch_Autonomy, levels = 1:4, ordered = TRUE),
    SIMPL_o     = factor(SIMPL_Performance, levels = 1:5, ordered = TRUE)
  )
}
fit_clmm <- function(df, y) {
  clmm(as.formula(paste(y, "~ PGY_Year + Complexity + Procedure_f + Approach + Attending + Months + (1 | Resident_ID)")),
       data = droplevels(df), link = "logit", nAGQ = 10)
}
tidy_or <- function(m) {
  cf <- coef(summary(m)); cf <- cf[!grepl("\\|", rownames(cf)), , drop = FALSE]
  data.frame(term = rownames(cf), or = exp(cf[, 1]),
             lo = exp(cf[, 1] - qnorm(.975) * cf[, 2]), hi = exp(cf[, 1] + qnorm(.975) * cf[, 2]),
             p = cf[, 4], row.names = NULL)
}
m_dat_all  <- prep_model(att)
m_dat_sens <- prep_model(att %>% filter(!Case_ID %in% flagged_ids))
tid <- lapply(list(z_all = fit_clmm(m_dat_all, "Zwisch_o"), z_sen = fit_clmm(m_dat_sens, "Zwisch_o"),
                   s_all = fit_clmm(m_dat_all, "SIMPL_o"),  s_sen = fit_clmm(m_dat_sens, "SIMPL_o")), tidy_or)

layout <- data.frame(
  label = c("Postgraduate year, per 1-year increase",
            "Case complexity (attending-rated)", "Easy", "Moderate", "Difficult",
            "Procedure", "Appendectomy", "Cholecystectomy", "Inguinal hernia repair",
            "Surgical approach", "Open", "Laparoscopic",
            "Evaluating attending", "Attending 1", "Attending 2", "Attending 3",
            "Time since study start, per month"),
  term = c("PGY_Year", NA, NA, "ComplexityModerate", "ComplexityDifficult",
           NA, NA, "Procedure_fCholecystectomy", "Procedure_fInguinal hernia repair",
           NA, NA, "ApproachLaparoscopic", NA, NA, "AttendingAttending 2", "AttendingAttending 3", "Months"),
  type = c("single", "header", "ref", "level", "level", "header", "ref", "level", "level",
           "header", "ref", "level", "header", "ref", "level", "level", "single"),
  stringsAsFactors = FALSE
)
cell_or <- function(t, term, type) {
  if (type == "header") return(c("", ""))
  if (type == "ref")    return(c("1.00 [Reference]", ""))
  x <- t[t$term == term, ]
  if (!nrow(x)) return(c("—", ""))
  c(fmt_or(x$or, x$lo, x$hi), fmt_p(x$p))
}
tS5 <- as.data.frame(do.call(rbind, lapply(seq_len(nrow(layout)), function(i) {
  L <- layout[i, ]
  lab <- switch(L$type, header = bold(L$label), single = L$label, indent(L$label))
  c(lab, unlist(lapply(tid, cell_or, term = L$term, type = L$type)))
})), stringsAsFactors = FALSE)
n_all <- nrow(m_dat_all); n_sen <- nrow(m_dat_sens)
write_table(
  tS5, "S5",
  "Sensitivity Analysis Excluding Flagged Cases: Factors Associated With Autonomy and Performance",
  "Adjusted odds ratios from the Table 4 mixed-effects ordinal models fitted to all attending evaluations and after excluding cases flagged during data verification.",
  c(sprintf("Flagged cases (n = %d) were those with two attending forms and no self-assessment (n = %d), forms from different residents under one case number (n = %d), mismatched procedure, approach, or date between the two forms of a case (n = %d), or inconsistent evaluator coding (n = %d); categories overlap.",
            length(flagged_ids), sum(case_flags$two_attending), sum(case_flags$mixed_resident),
            sum(case_flags$mismatch), sum(case_flags$coding_error)),
    "Models as in Table 4: cumulative logit with a random intercept for resident; postgraduate year per 1-year increase. P values are from Wald tests.",
    abbr("aOR", "CI")),
  c("Characteristic",
    sprintf("Zwisch, all (n = %d), aOR (95%% CI)", n_all), "P value",
    sprintf("Zwisch, excluding flagged (n = %d), aOR (95%% CI)", n_sen), "P value",
    sprintf("SIMPL, all (n = %d), aOR (95%% CI)", n_all), "P value",
    sprintf("SIMPL, excluding flagged (n = %d), aOR (95%% CI)", n_sen), "P value"),
  c("Characteristic",
    sprintf("All cases<br>(n = %d)<br>aOR (95%% CI)", n_all), "P value",
    sprintf("Excluding flagged<br>(n = %d)<br>aOR (95%% CI)", n_sen), "P value",
    sprintf("All cases<br>(n = %d)<br>aOR (95%% CI)", n_all), "P value",
    sprintf("Excluding flagged<br>(n = %d)<br>aOR (95%% CI)", n_sen), "P value"),
  spanners = list(list("", 1), list("Zwisch autonomy", 4), list("SIMPL performance", 4))
)

# =============================================================================
# TABLE S6 — Sensitivity: agreement excluding flagged pairs
# =============================================================================
pairs_sens <- pairs_all %>% filter(!Case_ID %in% flagged_ids)
tS6 <- build_agreement(pairs_sens)
write_table(
  tS6$table, "S6",
  "Sensitivity Analysis Excluding Flagged Pairs: Agreement Between Self-Assessment and Attending Rating",
  sprintf("Agreement statistics as in Table 5 after excluding %d paired cases flagged during data verification (mismatched procedure, approach, date, or evaluator coding).",
          nrow(pairs_all) - nrow(pairs_sens)),
  agreement_notes(pairs_sens), tS6$header
)

rebuild_all_md()
save_session_info()
message("Tables 5, 6 and S2–S6 written to: ", tab_dir)
