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
pgy_note    <- L("Comparisons are across postgraduate years.", "Las comparaciones son entre años de residencia.")

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
  for (sc in list(list("Z", L("Zwisch autonomy level (1–4)", "Nivel de autonomía de Zwisch (1–4)"), 4),
                  list("S", L("SIMPL performance rating (1–5)", "Calificación de desempeño SIMPL (1–5)"), 5))) {
    a <- paste0(sc[[1]], "_att"); s <- paste0(sc[[1]], "_self"); df <- paste0(sc[[1]], "_diff")
    exact  <- factor(ifelse(pp[[df]] == 0, "yes", "no"), levels = c("yes", "no"))
    within <- factor(ifelse(abs(pp[[df]]) <= 1, "yes", "no"), levels = c("yes", "no"))
    direc  <- factor(sign(pp[[df]]), levels = c(1, 0, -1))
    add(bold(sc[[2]]), rep("", length(subsets)))
    t <- test_cat(exact, pp$PGY)
    add(indent(L("Exact agreement, n (%)", "Concordancia exacta, n (%)")),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] == 0), nrow(x)), ""), mark(t$p, t$test))
    t <- test_cat(within, pp$PGY)
    add(indent(L("Agreement within one level, n (%)", "Concordancia dentro de un nivel, n (%)")),
        vapply(subsets, function(x) fmt_npct(sum(abs(x[[df]]) <= 1), nrow(x)), ""), mark(t$p, t$test))
    t <- test_cat(direc, pp$PGY)
    add(indent(L("Self-rating higher than attending, n (%)", "Autoevaluación mayor que la del adscrito, n (%)")),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] > 0), nrow(x)), ""), mark(t$p, t$test))
    add(indent(L("Self-rating lower than attending, n (%)", "Autoevaluación menor que la del adscrito, n (%)")),
        vapply(subsets, function(x) fmt_npct(sum(x[[df]] < 0), nrow(x)), ""))
    t <- test_cont(pp[[df]], pp$PGY)
    add(indent(L(paste0("Self ", MINUS, " attending difference, mean (SD)"),
                 paste0("Diferencia autoevaluación ", MINUS, " adscrito, media (DE)"))),
        vapply(subsets, function(x) fmt_meansd(x[[df]]), ""), mark(t$p, t$test))
    kap <- vapply(seq_along(subsets), function(i)
      if (i == 1 || SHOW_PGY_KAPPA) kappa_boot(subsets[[i]], a, s, sc[[3]]) else "", "")
    add(indent(L("Weighted κ (95% CI)", "κ ponderada (IC 95%)")), kap)
  }
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  cols <- c(OVERALL, groups)
  names(tab) <- c(CHAR, cols, PVAL)
  ns <- vapply(subsets, nrow, 0)
  list(table = tab, header = c(CHAR, paste0(cols, " (n = ", ns, ")"), PVAL))
}

agreement_notes <- function(pp) c(
  sprintf(L("Analysis restricted to cases with exactly one attending evaluation and one resident self-assessment (n = %d pairs from %d residents); column n values are pairs.",
            "Análisis restringido a los casos con exactamente una evaluación del adscrito y una autoevaluación del residente (n = %d pares de %d residentes); los valores de n de cada columna son pares."),
          nrow(pp), n_distinct(pp$Resident_ID)),
  L("Direction compares the resident's self-rating with the attending's rating of the same case; the P value for direction tests the distribution of higher, identical, and lower self-ratings.",
    "La dirección compara la autoevaluación del residente con la calificación del adscrito del mismo caso; el valor de P de la dirección evalúa la distribución de autoevaluaciones mayores, iguales y menores."),
  sprintf(L("Weighted κ uses quadratic weights; 95%% CIs from a cluster bootstrap resampling residents (%s replicates).%s",
            "La κ ponderada usa pesos cuadráticos; IC 95%% por bootstrap por conglomerados con remuestreo de residentes (%s réplicas).%s"),
          format(B_BOOT, big.mark = ","),
          if (SHOW_PGY_KAPPA) L(" NE, not estimable because one rater gave the same rating to every case in the stratum.",
                                " NE, no estimable porque un evaluador asignó la misma calificación a todos los casos del estrato.") else
            L(" κ is reported overall only because ratings within a single postgraduate year span too narrow a range for a stable estimate.",
              " La κ se reporta solo en total porque las calificaciones dentro de un mismo año de residencia abarcan un rango demasiado estrecho para una estimación estable.")),
  if (SHOW_PGY_KAPPA) L("Within-year κ values are attenuated by the narrow range of ratings within each postgraduate year and should be interpreted with the overall estimate.",
                        "Los valores de κ dentro de cada año están atenuados por el rango estrecho de calificaciones en cada año de residencia y deben interpretarse junto con la estimación total."),
  paste(pgy_note, L("Pairs are not independent (several per resident); P values are descriptive.",
                    "Los pares no son independientes (varios por residente); los valores de P son descriptivos.")),
  if (SHOW_PGY_KAPPA) abbr("CI", "NE", "PGY", "SD") else abbr("CI", "PGY", "SD")
)

# =============================================================================
# TABLE 5 — Self vs attending agreement
# =============================================================================
pairs_all <- make_pairs(d)
t5 <- build_agreement(pairs_all)
write_table(
  t5$table, "5",
  L("Agreement Between Resident Self-Assessment and Attending Rating by Postgraduate Year",
    "Concordancia entre la autoevaluación del residente y la calificación del adscrito por año de residencia"),
  L("Agreement between residents' self-assessed and attendings' ratings of intraoperative autonomy (Zwisch) and performance (SIMPL) for the same surgical case, overall and by postgraduate year.",
    "Concordancia entre la autoevaluación de los residentes y la calificación de los adscritos de la autonomía (Zwisch) y el desempeño (SIMPL) intraoperatorios en el mismo caso quirúrgico, en total y por año de residencia."),
  agreement_notes(pairs_all), t5$header
)

# =============================================================================
# TABLE 6 — Implementation outcomes
#   Respondent level: each respondent's mean across their forms (items on
#   attending forms -> attending; items on self-assessment forms -> resident).
#   Form level: descriptive distribution across all forms.
# =============================================================================
items <- list(
  list("Eval_Time_Min",         L("Time to complete evaluation, minutes", "Tiempo para completar la evaluación, minutos"), FALSE),
  list("Utility_Performance",   L("Usefulness for assessing performance (1–5)", "Utilidad para evaluar el desempeño (1–5)"), TRUE),
  list("Utility_Feedback",      L("Usefulness of feedback (1–5)", "Utilidad de la retroalimentación (1–5)"), TRUE),
  list("Feasibility_Ease",      L("Ease of use (1–5)", "Facilidad de uso (1–5)"), TRUE),
  list("Sustain_Institutional", L("Support for permanent institutional use (1–5)", "Apoyo a su permanencia institucional (1–5)"), TRUE),
  list("Sustain_FutureUse",     L("Willingness to continue using the model (1–5)", "Disposición a seguir usando el modelo (1–5)"), TRUE)
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
  add6(indent(L("Median (IQR)", "Mediana (RIC)")), fmt_mediqr(resp_att[[v]]), fmt_mediqr(resp_res[[v]]), mark(t$p, t$test),
       fmt_mediqr(form_att[[v]]), fmt_mediqr(form_res[[v]]))
  if (it[[3]]) {
    add6(indent(L("Highest rating (5), n (%)", "Calificación máxima (5), n (%)")), "—", "—", "",
         fmt_npct(sum(form_att[[v]] == 5), nrow(form_att)), fmt_npct(sum(form_res[[v]] == 5), nrow(form_res)))
  } else {
    add6(indent(L("≤3 minutes, n (%)", "≤3 minutos, n (%)")), "—", "—", "",
         fmt_npct(sum(form_att[[v]] <= 3), nrow(form_att)), fmt_npct(sum(form_res[[v]] <= 3), nrow(form_res)))
  }
}
t6 <- as.data.frame(do.call(rbind, rows6), stringsAsFactors = FALSE)
names(t6) <- c(CHAR, "ra", "rr", PVAL, "fa", "fr")

cases_all  <- n_distinct(d$Case_ID)
cases_self <- n_distinct(d$Case_ID[d$Evaluator_Type == 2])
write_table(
  t6, "6",
  L("Implementation Outcomes of the SIMPL-Based EPA Assessment Model Among Attending Surgeons and Residents",
    "Desenlaces de implementación del modelo de evaluación de EPAs basado en SIMPL en cirujanos adscritos y residentes"),
  sprintf(L("Time burden, perceived usefulness, ease of use, and sustainability reported by attending surgeons and residents, summarized per respondent and per evaluation form. A resident self-assessment was completed in %d of %d evaluated cases (%s%%).",
            "Tiempo requerido, utilidad percibida, facilidad de uso y sostenibilidad reportados por los cirujanos adscritos y los residentes, resumidos por participante y por formulario de evaluación. Se completó la autoevaluación del residente en %d de %d casos evaluados (%s%%)."),
          cases_self, cases_all, num(100 * cases_self / cases_all)),
  c(L("Per-respondent values are the median (IQR) of each respondent's mean score across all of their evaluation forms; P values compare attending surgeons with residents at the respondent level.",
      "Los valores por participante son la mediana (RIC) del puntaje promedio de cada participante en todos sus formularios; los valores de P comparan a los cirujanos adscritos con los residentes a nivel de participante."),
    L("Per-form columns are descriptive and count each evaluation form separately; proportions are reported at the form level only because the number of forms per respondent differed widely. —, not applicable.",
      "Las columnas por formulario son descriptivas y cuentan cada formulario por separado; las proporciones se reportan solo por formulario porque el número de formularios por participante varió ampliamente. —, no aplica."),
    L("Items on attending forms were attributed to the evaluating attending, and items on self-assessment forms to the resident. Likert items: 1, lowest to 5, highest agreement or usefulness. Evaluation time was self-reported in whole minutes.",
      "Los reactivos de los formularios del adscrito se atribuyeron al adscrito evaluador, y los de las autoevaluaciones al residente. Reactivos tipo Likert: 1, menor a 5, mayor acuerdo o utilidad. El tiempo de evaluación fue autorreportado en minutos enteros."),
    sprintf(L("Only %d attending surgeons participated; respondent-level comparisons are exploratory.",
              "Solo participaron %d cirujanos adscritos; las comparaciones por participante son exploratorias."), nrow(resp_att)),
    L("Self-assessment uptake by postgraduate year is shown in Table 2.",
      "La proporción de autoevaluaciones por año de residencia se muestra en la Tabla 2."),
    abbr("IQR")),
  c(L("Outcome", "Desenlace"),
    sprintf(L("Attendings, per respondent (n = %d)", "Adscritos, por participante (n = %d)"), nrow(resp_att)),
    sprintf(L("Residents, per respondent (n = %d)", "Residentes, por participante (n = %d)"), nrow(resp_res)), PVAL,
    sprintf(L("Attending forms (n = %d)", "Formularios del adscrito (n = %d)"), nrow(form_att)),
    sprintf(L("Self-assessment forms (n = %d)", "Formularios de autoevaluación (n = %d)"), nrow(form_res))),
  c(L("Outcome", "Desenlace"),
    sprintf(L("Attending surgeons<br>(n = %d)", "Cirujanos adscritos<br>(n = %d)"), nrow(resp_att)),
    sprintf(L("Residents<br>(n = %d)", "Residentes<br>(n = %d)"), nrow(resp_res)), PVAL,
    sprintf(L("Attending forms<br>(n = %d)", "Formularios del adscrito<br>(n = %d)"), nrow(form_att)),
    sprintf(L("Self-assessment forms<br>(n = %d)", "Formularios de autoevaluación<br>(n = %d)"), nrow(form_res))),
  spanners = list(list("", 1), list(L("Per respondent", "Por participante"), 3),
                  list(L("Per evaluation form", "Por formulario de evaluación"), 2))
)

# =============================================================================
# TABLE S2 — Complexity within PGY: assignment and attending ratings
# =============================================================================
pgys <- levels(att$PGY)
rowsS2 <- list()
addS2 <- function(...) rowsS2[[length(rowsS2) + 1]] <<- c(...)
t <- test_cat(att$Complexity, att$PGY)
addS2(bold(L("Case assignment, n (%)", "Asignación de casos, n (%)")), rep("", length(pgys) + 1), mark(t$p, t$test))
for (cx in levels(att$Complexity))
  addS2(indent(cx), fmt_npct(sum(att$Complexity == cx), nrow(att)),
        vapply(pgys, function(g) fmt_npct(sum(att$Complexity == cx & att$PGY == g), sum(att$PGY == g)), ""), "")
for (oc in list(list("Zwisch_Autonomy", L("Zwisch score (1–4), median (IQR)", "Puntaje de Zwisch (1–4), mediana (RIC)")),
                list("SIMPL_Performance", L("SIMPL score (1–5), median (IQR)", "Puntaje SIMPL (1–5), mediana (RIC)")))) {
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
  L("Case Complexity Within Postgraduate Year: Case Assignment and Attending Ratings",
    "Complejidad del caso dentro de cada año de residencia: asignación de casos y calificaciones del adscrito"),
  sprintf(L("Distribution of attending-rated case complexity across postgraduate years and the corresponding Zwisch and SIMPL ratings within each complexity stratum (%d attending evaluations).",
            "Distribución de la complejidad del caso calificada por el adscrito entre años de residencia y calificaciones de Zwisch y SIMPL correspondientes dentro de cada estrato de complejidad (%d evaluaciones del adscrito)."), nrow(att)),
  c(L("Data are n (%) or median (IQR). Case assignment percentages use each postgraduate year's evaluations as denominator.",
      "Los datos son n (%) o mediana (RIC). Los porcentajes de asignación usan como denominador las evaluaciones de cada año de residencia."),
    paste(pgy_note, L("For ratings, postgraduate years are compared within each complexity stratum.",
                      "Para las calificaciones, los años de residencia se comparan dentro de cada estrato de complejidad.")),
    L("Evaluations are not independent (multiple per resident); P values are descriptive. Adjusted estimates are shown in Table 4.",
      "Las evaluaciones no son independientes (varias por residente); los valores de P son descriptivos. Las estimaciones ajustadas se muestran en la Tabla 4."),
    abbr("IQR", "PGY")),
  c(CHAR, sprintf("%s (n = %d)", OVERALL, nrow(att)),
    sprintf("%s (n = %d)", pgys, as.integer(table(att$PGY))), PVAL)
)

# =============================================================================
# TABLE S3 — Ratings by evaluating attending
# =============================================================================
outcome_specs <- list(
  list(type = "cat",  var = "Zwisch",            label = L("Zwisch autonomy level", "Nivel de autonomía de Zwisch")),
  list(type = "cont", var = "Zwisch_Autonomy",   label = L("Zwisch score (1–4)", "Puntaje de Zwisch (1–4)")),
  list(type = "cat",  var = "SIMPL",             label = L("SIMPL performance rating", "Calificación de desempeño SIMPL")),
  list(type = "cont", var = "SIMPL_Performance", label = L("SIMPL score (1–5)", "Puntaje SIMPL (1–5)"))
)
tS3 <- build_table(att, "Attending",
                   c(list(list(type = "cat", var = "PGY", label = L("Postgraduate year of resident", "Año de residencia del residente"))), outcome_specs))
write_table(
  tS3$table, "S3",
  L("Attending-Rated Autonomy and Performance by Evaluating Attending",
    "Autonomía y desempeño calificados por el adscrito según el adscrito evaluador"),
  sprintf(L("Case mix by resident postgraduate year and distribution of Zwisch and SIMPL ratings assigned by each of the %d evaluating attendings (%d attending evaluations).",
            "Distribución de casos por año de residencia y de las calificaciones de Zwisch y SIMPL asignadas por cada uno de los %d adscritos evaluadores (%d evaluaciones del adscrito)."),
          nlevels(att$Attending), nrow(att)),
  c(L("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
      "Los datos son n (%) o mediana (RIC). Los porcentajes usan como denominador el total de cada columna."),
    L("Comparisons are across attendings. Unadjusted; differences in ratings partly reflect differences in the postgraduate year of residents evaluated. Attending-adjusted estimates are shown in Table 4.",
      "Las comparaciones son entre adscritos. Sin ajustar; las diferencias en las calificaciones reflejan en parte diferencias en el año de residencia de los residentes evaluados. Las estimaciones ajustadas por adscrito se muestran en la Tabla 4."),
    L("Evaluations are not independent (multiple per resident); P values are descriptive.",
      "Las evaluaciones no son independientes (varias por residente); los valores de P son descriptivos."),
    abbr("IQR", "PGY")),
  tS3$header
)

# =============================================================================
# TABLE S4 — Ratings by procedure and by surgical approach
# =============================================================================
specs_S4 <- c(list(list(type = "cont", var = "PGY_Year", label = L("Postgraduate year", "Año de residencia"))), outcome_specs)
a <- build_table(att, "Procedure_f", specs_S4, overall = TRUE)
b <- build_table(att, "Approach",    specs_S4, overall = FALSE)
tS4 <- cbind(a$table, b$table[, -1])
names(tS4) <- make.unique(names(tS4))
write_table(
  tS4, "S4",
  L("Attending-Rated Autonomy and Performance by Procedure and Surgical Approach",
    "Autonomía y desempeño calificados por el adscrito según el procedimiento y el abordaje quirúrgico"),
  sprintf(L("Unadjusted distribution of Zwisch and SIMPL ratings across the three index procedures and by open vs laparoscopic approach (%d attending evaluations).",
            "Distribución sin ajustar de las calificaciones de Zwisch y SIMPL en los tres procedimientos índice y por abordaje abierto vs laparoscópico (%d evaluaciones del adscrito)."), nrow(att)),
  c(L("Data are n (%) or median (IQR). Percentages use the column total as denominator.",
      "Los datos son n (%) o mediana (RIC). Los porcentajes usan como denominador el total de cada columna."),
    L("The first P value column compares procedures; the second compares surgical approaches. Estimates adjusted for postgraduate year, complexity, attending, and time are shown in Table 4.",
      "La primera columna de valores de P compara los procedimientos; la segunda compara los abordajes quirúrgicos. Las estimaciones ajustadas por año de residencia, complejidad, adscrito y tiempo se muestran en la Tabla 4."),
    L("Evaluations are not independent (multiple per resident); P values are descriptive.",
      "Las evaluaciones no son independientes (varias por residente); los valores de P son descriptivos."),
    abbr("IQR")),
  c(a$header, b$header[-1]), with_br(c(a$header, b$header[-1])),
  spanners = list(list("", 2), list(L("Procedure", "Procedimiento"), 4), list(L("Surgical approach", "Abordaje quirúrgico"), 3))
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
  label = c(L("Postgraduate year, per 1-year increase", "Año de residencia, por cada año adicional"),
            L("Case complexity (attending-rated)", "Complejidad del caso (según el adscrito)"), CX_LAB,
            L("Procedure", "Procedimiento"), PROC_LAB,
            L("Surgical approach", "Abordaje quirúrgico"), APP_LAB,
            L("Evaluating attending", "Adscrito evaluador"), ATT_LAB,
            L("Time since study start, per month", "Tiempo desde el inicio del estudio, por mes")),
  # model terms are built from the factor levels, so they work in either language
  term = c("PGY_Year", NA, NA, paste0("Complexity", CX_LAB[2:3]),
           NA, NA, paste0("Procedure_f", PROC_LAB[2:3]),
           NA, NA, paste0("Approach", APP_LAB[2]), NA, NA, paste0("Attending", ATT_LAB[2:3]), "Months"),
  type = c("single", "header", "ref", "level", "level", "header", "ref", "level", "level",
           "header", "ref", "level", "header", "ref", "level", "level", "single"),
  stringsAsFactors = FALSE
)
cell_or <- function(t, term, type) {
  if (type == "header") return(c("", ""))
  if (type == "ref")    return(c(REF_CELL, ""))
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
  L("Sensitivity Analysis Excluding Flagged Cases: Factors Associated With Autonomy and Performance",
    "Análisis de sensibilidad excluyendo los casos marcados: factores asociados con la autonomía y el desempeño"),
  L("Adjusted odds ratios from the Table 4 mixed-effects ordinal models fitted to all attending evaluations and after excluding cases flagged during data verification.",
    "Razones de momios ajustadas de los modelos ordinales de efectos mixtos de la Tabla 4 ajustados a todas las evaluaciones del adscrito y tras excluir los casos marcados durante la verificación de datos."),
  c(sprintf(L("Flagged cases (n = %d) were those with two attending forms and no self-assessment (n = %d), forms from different residents under one case number (n = %d), mismatched procedure, approach, or date between the two forms of a case (n = %d), or inconsistent evaluator coding (n = %d); categories overlap.",
              "Los casos marcados (n = %d) fueron los que tuvieron dos formularios del adscrito y ninguna autoevaluación (n = %d), formularios de distintos residentes bajo un mismo número de caso (n = %d), discrepancia de procedimiento, abordaje o fecha entre los dos formularios de un caso (n = %d) o codificación inconsistente del evaluador (n = %d); las categorías se superponen."),
            length(flagged_ids), sum(case_flags$two_attending), sum(case_flags$mixed_resident),
            sum(case_flags$mismatch), sum(case_flags$coding_error)),
    L("Models as in Table 4: cumulative logit with a random intercept for resident; postgraduate year per 1-year increase. P values are from Wald tests.",
      "Modelos como en la Tabla 4: logit acumulado con intercepto aleatorio por residente; año de residencia por cada año adicional. Los valores de P provienen de pruebas de Wald."),
    abbr("aOR", "CI")),
  c(CHAR,
    sprintf(L("Zwisch, all (n = %d), %s", "Zwisch, todos (n = %d), %s"), n_all, AOR_CI), PVAL,
    sprintf(L("Zwisch, excluding flagged (n = %d), %s", "Zwisch, sin marcados (n = %d), %s"), n_sen, AOR_CI), PVAL,
    sprintf(L("SIMPL, all (n = %d), %s", "SIMPL, todos (n = %d), %s"), n_all, AOR_CI), PVAL,
    sprintf(L("SIMPL, excluding flagged (n = %d), %s", "SIMPL, sin marcados (n = %d), %s"), n_sen, AOR_CI), PVAL),
  c(CHAR,
    sprintf(L("All cases<br>(n = %d)<br>%s", "Todos los casos<br>(n = %d)<br>%s"), n_all, AOR_CI), PVAL,
    sprintf(L("Excluding flagged<br>(n = %d)<br>%s", "Sin casos marcados<br>(n = %d)<br>%s"), n_sen, AOR_CI), PVAL,
    sprintf(L("All cases<br>(n = %d)<br>%s", "Todos los casos<br>(n = %d)<br>%s"), n_all, AOR_CI), PVAL,
    sprintf(L("Excluding flagged<br>(n = %d)<br>%s", "Sin casos marcados<br>(n = %d)<br>%s"), n_sen, AOR_CI), PVAL),
  spanners = list(list("", 1), list(L("Zwisch autonomy", "Autonomía de Zwisch"), 4), list(L("SIMPL performance", "Desempeño SIMPL"), 4))
)

# =============================================================================
# TABLE S6 — Sensitivity: agreement excluding flagged pairs
# =============================================================================
pairs_sens <- pairs_all %>% filter(!Case_ID %in% flagged_ids)
tS6 <- build_agreement(pairs_sens)
write_table(
  tS6$table, "S6",
  L("Sensitivity Analysis Excluding Flagged Pairs: Agreement Between Self-Assessment and Attending Rating",
    "Análisis de sensibilidad excluyendo los pares marcados: concordancia entre la autoevaluación y la calificación del adscrito"),
  sprintf(L("Agreement statistics as in Table 5 after excluding %d paired cases flagged during data verification (mismatched procedure, approach, date, or evaluator coding).",
            "Estadísticos de concordancia como en la Tabla 5 tras excluir %d casos pareados marcados durante la verificación de datos (discrepancia de procedimiento, abordaje, fecha o codificación del evaluador)."),
          nrow(pairs_all) - nrow(pairs_sens)),
  agreement_notes(pairs_sens), tS6$header
)

rebuild_all_md()
save_session_info()
message(L("Tables 5, 6 and S2–S6 written to: ", "Tablas 5, 6 y S2–S6 guardadas en: "), tab_dir)
