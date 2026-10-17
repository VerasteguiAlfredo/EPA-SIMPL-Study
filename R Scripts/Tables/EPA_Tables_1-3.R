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
study_period <- period_text(d$Eval_Date)
pgy_note <- L("Comparisons are across postgraduate years.", "Las comparaciones son entre años de residencia.")

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
  list(type = "cont", var = "Age",        label = L("Age, years", "Edad, años")),
  list(type = "bin",  var = "Sex",        label = L("Female sex", "Sexo femenino"), level = levels(d$Sex)[2]),
  list(type = "cont", var = "Cases",      label = L("Surgical cases evaluated per resident", "Casos quirúrgicos evaluados por residente")),
  list(type = "cont", var = "Att_forms",  label = L("Attending evaluations per resident", "Evaluaciones del adscrito por residente")),
  list(type = "cont", var = "Self_forms", label = L("Self-assessments per resident", "Autoevaluaciones por residente"))
))
write_table(
  t1$table, "1",
  L("Characteristics of Participating General Surgery Residents by Postgraduate Year",
    "Características de los residentes de Cirugía General participantes por año de residencia"),
  sprintf(L("Demographic characteristics and evaluation volume of the %d residents assessed with the SIMPL-based EPA model at a single academic center (%s). Each resident is counted once.",
            "Características demográficas y volumen de evaluaciones de los %d residentes evaluados con el modelo de EPAs basado en SIMPL en un solo centro académico (%s). Cada residente se cuenta una vez."),
          nrow(residents), study_period),
  c(L("Data are n (%) or median (IQR). Percentages use the column total of non-missing values as denominator.",
      "Los datos son n (%) o mediana (RIC). Los porcentajes usan como denominador el total de valores no faltantes de cada columna."),
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
    Month       = cap1(month_name(first(Eval_Date))),
    Self_done   = factor(ifelse(any(Evaluator_Type == 2), "Yes", "No"), levels = c("Yes", "No")),
    .groups = "drop"
  ) %>%
  mutate(Month = factor(Month, levels = unique(cap1(month_name(sort(d$Eval_Date))))))

t2 <- build_table(cases, "PGY", list(
  list(type = "cat", var = "Procedure_f", label = L("Procedure", "Procedimiento")),
  list(type = "cat", var = "Approach",    label = L("Surgical approach", "Abordaje quirúrgico")),
  list(type = "cat", var = "Complexity",  label = L("Case complexity (attending-rated)", "Complejidad del caso (según el adscrito)")),
  list(type = "cat", var = "Attending",   label = L("Evaluating attending", "Adscrito evaluador")),
  list(type = "cat", var = "Month",       label = L("Month of evaluation", "Mes de evaluación")),
  list(type = "bin", var = "Self_done",   label = L("Resident self-assessment completed", "Autoevaluación del residente completada"), level = "Yes")
))
write_table(
  t2$table, "2",
  L("Characteristics of Evaluated Surgical Cases by Postgraduate Year",
    "Características de los casos quirúrgicos evaluados por año de residencia"),
  sprintf(L("Procedure, approach, complexity, evaluator, and timing of the %d surgical cases in which residents were assessed. Each case is counted once and attributed to the operating resident's postgraduate year.",
            "Procedimiento, abordaje, complejidad, evaluador y fecha de los %d casos quirúrgicos en los que se evaluó a los residentes. Cada caso se cuenta una vez y se asigna al año de residencia del residente que operó."),
          nrow(cases)),
  c(L("Data are n (%). Percentages use the column total of non-missing values as denominator.",
      "Los datos son n (%). Los porcentajes usan como denominador el total de valores no faltantes de cada columna."),
    L("Case attributes were taken from the attending evaluation; when more than one attending form existed for a case, the first recorded form was used. Missing indicates cases with a self-assessment only.",
      "Las características del caso se tomaron de la evaluación del adscrito; cuando un caso tuvo más de un formulario del adscrito, se usó el primero registrado. Sin dato indica casos con solo autoevaluación."),
    pgy_note, abbr("PGY")),
  t2$header
)

# =============================================================================
# TABLE 3 — Attending-rated autonomy and performance (one row per attending form)
# =============================================================================
att <- d %>% filter(Evaluator_Type == 1)
t3 <- build_table(att, "PGY", list(
  list(type = "cat",  var = "Zwisch",            label = L("Zwisch autonomy level", "Nivel de autonomía de Zwisch")),
  list(type = "cont", var = "Zwisch_Autonomy",   label = L("Zwisch score (1–4)", "Puntaje de Zwisch (1–4)")),
  list(type = "cat",  var = "SIMPL",             label = L("SIMPL performance rating", "Calificación de desempeño SIMPL")),
  list(type = "cont", var = "SIMPL_Performance", label = L("SIMPL score (1–5)", "Puntaje SIMPL (1–5)"))
))
write_table(
  t3$table, "3",
  L("Attending-Rated Intraoperative Autonomy and Performance by Postgraduate Year",
    "Autonomía y desempeño intraoperatorios calificados por el adscrito por año de residencia"),
  sprintf(L("Distribution of Zwisch autonomy levels and SIMPL performance ratings assigned by attending surgeons across %d intraoperative evaluations. Resident self-assessments are excluded.",
            "Distribución de los niveles de autonomía de Zwisch y de las calificaciones de desempeño SIMPL asignados por los cirujanos adscritos en %d evaluaciones intraoperatorias. Se excluyen las autoevaluaciones de los residentes."),
          nrow(att)),
  c(L("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
      "Los datos son n (%) o mediana (RIC). Los porcentajes usan como denominador el total de cada columna."),
    L("Zwisch scale: 1, show and tell; 2, active help; 3, passive help; 4, supervision only. SIMPL scale: 1, unprepared/critical deficiency; 2, inexperienced with procedure; 3, intermediate; 4, practice-ready; 5, exceptional.",
      "Escala de Zwisch: 1, demuestra y explica; 2, ayuda activa; 3, ayuda pasiva; 4, solo supervisión. Escala SIMPL: 1, no preparado/deficiencia crítica; 2, falta de experiencia con el procedimiento; 3, intermedio; 4, listo para la práctica; 5, excepcional."),
    paste(pgy_note, L("Evaluations are not independent (multiple per resident); P values are descriptive.",
                      "Las evaluaciones no son independientes (varias por residente); los valores de P son descriptivos.")),
    abbr("IQR", "PGY")),
  t3$header
)

rebuild_all_md()
save_session_info()
message(L("Tables 1–3 written to: ", "Tablas 1–3 guardadas en: "), tab_dir)
