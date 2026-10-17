# =============================================================================
# EPA / SIMPL study — Tables 1–3 (STROBE-style descriptive tables)
#   Table 1. Resident characteristics by postgraduate year      (unit: resident)
#   Table 2. Surgical case characteristics by postgraduate year (unit: case)
#   Table 3. Attending-rated Zwisch autonomy and SIMPL performance
#            by postgraduate year                               (unit: attending form)
#
# Requires EPA_helpers.R in the same folder (paths are set there).
# =============================================================================

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
set.seed(2026)  # reproducible Monte Carlo Fisher P values

d <- load_epa()
study_period <- if (format(min(d$Eval_Date), "%Y") == format(max(d$Eval_Date), "%Y")) {
  paste0(format(min(d$Eval_Date), "%B"), "–", format(max(d$Eval_Date), "%B %Y"))
} else {
  paste0(format(min(d$Eval_Date), "%B %Y"), "–", format(max(d$Eval_Date), "%B %Y"))
}
pgy_note <- "Comparisons are across postgraduate years."

# =============================================================================
# TABLE 1 — Residents (one row per resident)
# =============================================================================
mode_val <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }

residents <- d %>%
  group_by(Resident_ID) %>%
  summarise(
    n_sex = n_distinct(Sex), n_pgy = n_distinct(PGY),
    Age = mode_val(Resident_Age), Sex = mode_val(Sex), PGY = mode_val(PGY),
    Cases = n_distinct(Case_ID),
    Att_forms  = sum(Evaluator_Type == 1),
    Self_forms = sum(Evaluator_Type == 2),
    .groups = "drop"
  )
incons <- residents$Resident_ID[residents$n_sex > 1 | residents$n_pgy > 1]
if (length(incons))
  warning("Inconsistent sex/PGY within resident(s): ", paste(incons, collapse = ", "),
          " — most frequent value used. Resolve in the database before final tables.")

t1 <- build_table(residents, "PGY", list(
  list(type = "cont", var = "Age",        label = "Age, years"),
  list(type = "bin",  var = "Sex",        label = "Female sex", level = "Female"),
  list(type = "cont", var = "Cases",      label = "Surgical cases evaluated per resident"),
  list(type = "cont", var = "Att_forms",  label = "Attending evaluations per resident"),
  list(type = "cont", var = "Self_forms", label = "Self-assessments per resident")
))
write_table(
  t1$table, "1",
  "Characteristics of Participating General Surgery Residents by Postgraduate Year",
  sprintf("Demographic characteristics and evaluation volume of the %d residents assessed with the SIMPL-based EPA model at a single academic center (%s). Each resident is counted once.",
          nrow(residents), study_period),
  c("Data are n (%) or median (IQR). Percentages use the column total of non-missing values as denominator.",
    pgy_note, abbr("IQR", "PGY")),
  t1$header
)

# =============================================================================
# TABLE 2 — Surgical cases (one row per case; attributes from the attending form)
# =============================================================================
cases <- d %>%
  arrange(Case_ID, Evaluator_Type, Row_ID) %>%   # attending form first
  group_by(Case_ID) %>%
  summarise(
    PGY         = first(PGY),
    Procedure_f = first(Procedure_f),
    Approach    = first(Approach),
    Complexity  = if (first(Evaluator_Type) == 1) first(Complexity) else factor(NA, levels = levels(d$Complexity)),
    Attending   = if (first(Evaluator_Type) == 1) first(Attending)  else factor(NA, levels = levels(d$Attending)),
    Month       = format(first(Eval_Date), "%B"),
    Self_done   = factor(ifelse(any(Evaluator_Type == 2), "Yes", "No"), levels = c("Yes", "No")),
    .groups = "drop"
  ) %>%
  mutate(Month = factor(Month, levels = unique(format(sort(d$Eval_Date), "%B"))))

t2 <- build_table(cases, "PGY", list(
  list(type = "cat", var = "Procedure_f", label = "Procedure"),
  list(type = "cat", var = "Approach",    label = "Surgical approach"),
  list(type = "cat", var = "Complexity",  label = "Case complexity (attending-rated)"),
  list(type = "cat", var = "Attending",   label = "Evaluating attending"),
  list(type = "cat", var = "Month",       label = "Month of evaluation"),
  list(type = "bin", var = "Self_done",   label = "Resident self-assessment completed", level = "Yes")
))
write_table(
  t2$table, "2",
  "Characteristics of Evaluated Surgical Cases by Postgraduate Year",
  sprintf("Procedure, approach, complexity, evaluator, and timing of the %d surgical cases in which residents were assessed. Each case is counted once and attributed to the operating resident's postgraduate year.",
          nrow(cases)),
  c("Data are n (%). Percentages use the column total of non-missing values as denominator.",
    "Case attributes were taken from the attending evaluation; when more than one attending form existed for a case, the first recorded form was used. Missing indicates cases with a self-assessment only.",
    pgy_note, abbr("PGY")),
  t2$header
)

# =============================================================================
# TABLE 3 — Attending-rated autonomy and performance (one row per attending form)
# =============================================================================
att <- d %>% filter(Evaluator_Type == 1)
t3 <- build_table(att, "PGY", list(
  list(type = "cat",  var = "Zwisch",            label = "Zwisch autonomy level"),
  list(type = "cont", var = "Zwisch_Autonomy",   label = "Zwisch score (1–4)"),
  list(type = "cat",  var = "SIMPL",             label = "SIMPL performance rating"),
  list(type = "cont", var = "SIMPL_Performance", label = "SIMPL score (1–5)")
))
write_table(
  t3$table, "3",
  "Attending-Rated Intraoperative Autonomy and Performance by Postgraduate Year",
  sprintf("Distribution of Zwisch autonomy levels and SIMPL performance ratings assigned by attending surgeons across %d intraoperative evaluations. Resident self-assessments are excluded.",
          nrow(att)),
  c("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
    "Zwisch scale: 1, show and tell; 2, active help; 3, passive help; 4, supervision only. SIMPL scale: 1, unprepared/critical deficiency; 2, inexperienced with procedure; 3, intermediate; 4, practice-ready; 5, exceptional.",
    paste(pgy_note, "Evaluations are not independent (multiple per resident); P values are descriptive."),
    abbr("IQR", "PGY")),
  t3$header
)

rebuild_all_md()
save_session_info()
message("Tables 1–3 written to: ", tab_dir)
