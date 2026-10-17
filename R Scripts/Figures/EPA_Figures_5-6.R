# =============================================================================
# EPA / SIMPL study — Figures 5–6
#
#   Figure 5. Trends over the study period
#             (A) mean attending-rated Zwisch score by month, by PGY
#             (B) mean attending-rated SIMPL score by month, by PGY
#             (C) cumulative evaluations over time (adoption)
#   Figure 6. Feasibility and acceptability
#             (A) diverging stacked bars of the Likert items, attending vs resident forms
#             (B) distribution of self-reported evaluation completion time
#
# Output (Results/Figures): PNG (600 dpi, white background), legend .md and
#   results .md per figure; All_Figure_Results.md is rebuilt.
#
# Requires EPA_helpers.R in the same folder (paths and figure style are set there).
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
load_packages(c("ordinal", "ggplot2", "patchwork"))

d   <- load_epa()
att <- d %>% filter(Evaluator_Type == 1)
months <- unique(cap1(month_name(sort(d$Eval_Date))))
month_of <- function(x) cap1(month_name(x))
d   <- d   %>% mutate(Month = factor(month_of(Eval_Date), levels = months))
att <- att %>% mutate(Month = factor(month_of(Eval_Date), levels = months))
n_att <- nrow(att); n_res <- n_distinct(att$Resident_ID)
pgy_cols <- setNames(RAMP5, levels(att$PGY))

# =============================================================================
# FIGURE 5 — Trends over time
# =============================================================================
trend <- att %>%
  group_by(PGY, Month) %>%
  summarise(n = n(), Zwisch = mean(Zwisch_Autonomy), SIMPL = mean(SIMPL_Performance), .groups = "drop") %>%
  mutate(m = as.integer(Month))

# nudge end-of-line labels apart so they never overlap (keeps order, minimal moves)
spread_labels <- function(y, min_gap, cap = Inf) {
  o <- order(y); ys <- y[o]; k <- length(ys)
  for (i in seq_len(k)[-1]) if (ys[i] - ys[i - 1] < min_gap) ys[i] <- ys[i - 1] + min_gap
  if (ys[k] > cap) {                         # pushed past the axis: pack downward from the cap
    ys[k] <- cap
    for (i in rev(seq_len(k - 1))) if (ys[i + 1] - ys[i] < min_gap) ys[i] <- ys[i + 1] - min_gap
  }
  out <- numeric(k); out[o] <- ys; out
}
trend_panel <- function(var, ylab, ylim, title) {
  last <- trend %>% group_by(PGY) %>% filter(m == max(m)) %>% ungroup() %>%
    mutate(y_lab = spread_labels(.data[[var]], min_gap = 0.09 * diff(ylim), cap = ylim[2]))
  ggplot(trend, aes(m, .data[[var]], colour = PGY, group = PGY)) +
    geom_hline(yintercept = seq(ylim[1], ylim[2], 1), colour = GRID, linewidth = 0.3) +
    geom_line(linewidth = 0.6) +
    geom_point(aes(size = n), shape = 21, fill = "white", stroke = 0.8) +
    geom_text(data = last, aes(y = y_lab, label = PGY), hjust = 0, nudge_x = 0.18, size = 2.6, show.legend = FALSE) +
    scale_colour_manual(values = pgy_cols, guide = "none") +
    scale_size_area(max_size = 3.6, breaks = c(2, 6, 10), name = L("Evaluations, n", "Evaluaciones, n")) +
    scale_x_continuous(breaks = seq_along(months), labels = substr(months, 1, 3),
                       expand = expansion(add = c(0.25, 0.9))) +
    scale_y_continuous(limits = ylim, breaks = ylim[1]:ylim[2]) +
    labs(x = L("Month of evaluation (2026)", "Mes de evaluación (2026)"), y = ylab, title = title) +
    theme_pub() + theme(legend.position = "bottom")
}
p5a <- trend_panel("Zwisch", L("Mean Zwisch score (1–4)", "Puntaje medio de Zwisch (1–4)"), c(1, 4), L("Zwisch autonomy", "Autonomía de Zwisch"))
p5b <- trend_panel("SIMPL",  L("Mean SIMPL score (1–5)", "Puntaje medio SIMPL (1–5)"),  c(1, 5), L("SIMPL performance", "Desempeño SIMPL"))

cum <- d %>%
  mutate(Role = ifelse(Evaluator_Type == 1, "Attending", "Resident")) %>%
  count(Role, Eval_Date) %>% arrange(Role, Eval_Date) %>%
  group_by(Role) %>% mutate(cum = cumsum(n)) %>% ungroup()
cum <- bind_rows(cum, cum %>% distinct(Role) %>% mutate(Eval_Date = min(d$Eval_Date) - 1, n = 0, cum = 0)) %>%
  arrange(Role, Eval_Date)
cum_lab <- cum %>% group_by(Role) %>% slice_tail(n = 1) %>% ungroup() %>%
  mutate(lab = paste0(ifelse(Role == "Attending", L("Attending evaluations", "Evaluaciones del adscrito"),
                             L("Self-assessments", "Autoevaluaciones")), " (", cum, ")"))
p5c <- ggplot(cum, aes(Eval_Date, cum, colour = Role)) +
  geom_step(linewidth = 0.7) +
  geom_text(data = cum_lab, aes(label = lab), hjust = 1, vjust = -0.6, size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = ROLE, guide = "none") +
  scale_x_date(date_breaks = "1 month", labels = function(x) { m <- month_abb(x); m[is.na(m)] <- ""; cap1(m) },
               expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = L("Date of evaluation (2026)", "Fecha de evaluación (2026)"), y = L("Cumulative forms, n", "Formularios acumulados, n"),
       title = L("Cumulative evaluations (adoption)", "Evaluaciones acumuladas (adopción)")) +
  theme_pub() + theme(panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))

p5 <- ((p5a | p5b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")) / p5c +
  plot_layout(heights = c(1.25, 1)) + plot_annotation(tag_levels = "A")
save_fig(p5, "Figure_5_Trends_Over_Time.png", 7.5, 6.6)

# Time trend from the Table 4 models (per month, adjusted)
mdat <- att %>% mutate(Resident_ID = factor(Resident_ID),
                       Months = as.numeric(Eval_Date - min(d$Eval_Date)) / 30.4375,
                       Zwisch_o = factor(Zwisch_Autonomy, levels = 1:4, ordered = TRUE),
                       SIMPL_o  = factor(SIMPL_Performance, levels = 1:5, ordered = TRUE))
rhs <- "PGY_Year + Complexity + Procedure_f + Approach + Attending + Months + (1 | Resident_ID)"
time_or <- function(y) {
  m <- clmm(as.formula(paste(y, "~", rhs)), data = mdat, nAGQ = 10)
  cf <- coef(summary(m))["Months", ]
  sprintf(L("%s (95%% CI %s–%s; P = %s)", "%s (IC 95%% %s–%s; P = %s)"), num(exp(cf[1])), num(exp(cf[1] - 1.96 * cf[2])),
          num(exp(cf[1] + 1.96 * cf[2])), fmt_p(cf[4]))
}
or_z <- time_or("Zwisch_o"); or_s <- time_or("SIMPL_o")

leg5 <- write_legend("Figure_5_Legend.md",
  L("Figure 5. Ratings and Adoption Over the Study Period", "Figura 5. Calificaciones y adopción durante el periodo de estudio"),
  sprintf(L("Mean attending-rated (A) Zwisch autonomy score and (B) SIMPL performance score by month of evaluation and postgraduate year (%d attending evaluations, %d residents); point size reflects the number of evaluations in each month. (C) Cumulative number of attending evaluations and resident self-assessments completed over the study period (%s). In the adjusted models of Table 4, the odds ratio per study month was %s for Zwisch and %s for SIMPL.",
            "Puntaje medio calificado por el adscrito de (A) autonomía de Zwisch y (B) desempeño SIMPL por mes de evaluación y año de residencia (%d evaluaciones del adscrito, %d residentes); el tamaño del punto refleja el número de evaluaciones de cada mes. (C) Número acumulado de evaluaciones del adscrito y de autoevaluaciones de los residentes durante el periodo de estudio (%s). En los modelos ajustados de la Tabla 4, la razón de momios por mes de estudio fue %s para Zwisch y %s para SIMPL."),
          n_att, n_res, period_text(d$Eval_Date), or_z, or_s))

wide_trend <- function(var) {
  w <- trend %>% mutate(cell = sprintf("%s (n=%d)", num(.data[[var]]), n)) %>%
    dplyr::select(PGY, Month, cell) %>% tidyr::pivot_wider(names_from = Month, values_from = cell, values_fill = "—")
  as.data.frame(w)
}
monthly <- d %>% mutate(Role = ifelse(Evaluator_Type == 1, "Attending forms", "Self-assessments")) %>%
  count(Month, Role) %>% tidyr::pivot_wider(names_from = Role, values_from = n, values_fill = 0) %>%
  mutate(Cases = as.integer(table(factor(month_of(d$Eval_Date[!duplicated(d$Case_ID)]), levels = months))),
         `Cumulative attending` = cumsum(`Attending forms`), `Cumulative self` = cumsum(`Self-assessments`),
         `Self per attending form, %` = num(100 * `Self-assessments` / `Attending forms`, 1)) %>% as.data.frame()
if (IS_ES) names(monthly) <- c("Mes", "Formularios del adscrito", "Autoevaluaciones", "Casos",
                               "Acumulado adscrito", "Acumulado autoevaluación", "Autoevaluaciones por formulario del adscrito, %")
wt_z <- wide_trend("Zwisch"); wt_s <- wide_trend("SIMPL")
names(wt_z)[1] <- names(wt_s)[1] <- L("PGY", "Año")
write_fig_results("5", L("Trends over the study period", "Tendencias durante el periodo de estudio"), leg5, list(
  list(heading = L("Adjusted time trend (Table 4 models, per month)", "Tendencia temporal ajustada (modelos de la Tabla 4, por mes)"),
       text = sprintf("Zwisch: %s. SIMPL: %s.", or_z, or_s)),
  list(heading = L("A. Mean Zwisch score by month and PGY (attending forms)", "A. Puntaje medio de Zwisch por mes y año de residencia (formularios del adscrito)"), data = wt_z),
  list(heading = L("B. Mean SIMPL score by month and PGY (attending forms)", "B. Puntaje medio SIMPL por mes y año de residencia (formularios del adscrito)"), data = wt_s),
  list(heading = L("C. Forms and cases per month", "C. Formularios y casos por mes"), data = monthly)
))

# =============================================================================
# FIGURE 6 — Feasibility and acceptability
# =============================================================================
items <- setNames(
  L(c("Useful for assessing performance", "Useful feedback", "Easy to use",
      "Should remain in place institutionally", "Willing to continue using"),
    c("Útil para evaluar el desempeño", "Retroalimentación útil", "Fácil de usar",
      "Debe mantenerse institucionalmente", "Dispuesto a seguir usándolo")),
  c("Utility_Performance", "Utility_Feedback", "Feasibility_Ease", "Sustain_Institutional", "Sustain_FutureUse"))
ROLE_LAB <- L(c("Attending surgeons", "Residents"), c("Cirujanos adscritos", "Residentes"))
lik <- d %>%
  mutate(Role = factor(ifelse(Evaluator_Type == 1, ROLE_LAB[1], ROLE_LAB[2]), levels = ROLE_LAB)) %>%
  dplyr::select(Role, all_of(names(items))) %>%
  tidyr::pivot_longer(-Role, names_to = "Item", values_to = "Score") %>%
  count(Role, Item, Score) %>%
  tidyr::complete(Role, Item, Score = 1:5, fill = list(n = 0)) %>%
  group_by(Role, Item) %>% mutate(N = sum(n), pct = 100 * n / N) %>% ungroup() %>%
  mutate(Item_lab = factor(items[Item], levels = rev(items)))

# Segment geometry: neutral (3) split across zero
seg <- lik %>% group_by(Role, Item) %>% arrange(Score, .by_group = TRUE) %>%
  mutate(neg_start = -(pct[Score == 1] + pct[Score == 2] + pct[Score == 3] / 2),
         xmin = neg_start + cumsum(dplyr::lag(pct, default = 0)), xmax = xmin + pct) %>% ungroup() %>%
  mutate(y = as.integer(Item_lab),
         ink = ifelse(Score == 5, "white", INK),
         lab = ifelse(pct >= 8, sprintf("%.0f%%", pct), ""))
top2 <- lik %>% filter(Score >= 4) %>% group_by(Role, Item_lab) %>% summarise(t = sum(pct), .groups = "drop")
score_lab <- L(c("1 Lowest", "2", "3 Neutral", "4", "5 Highest"), c("1 Mínimo", "2", "3 Neutral", "4", "5 Máximo"))
x_left <- -max(8, ceiling(max(-seg$xmin) / 5) * 5 + 3)   # left room only as wide as the data needs

p6a <- ggplot(seg) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - 0.34, ymax = y + 0.34, fill = factor(Score, levels = 1:5)),
            colour = "white", linewidth = 0.4) +
  geom_text(aes(x = (xmin + xmax) / 2, y = y, label = lab, colour = ink), size = 2.4) +
  geom_vline(xintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_text(data = top2, aes(x = 104, y = as.integer(Item_lab), label = sprintf("%.0f%%", t)),
            hjust = 0, size = 2.4, colour = INK2) +
  facet_wrap(~ Role, ncol = 2) +
  scale_fill_manual(values = setNames(LIKERT, 1:5), labels = score_lab, name = L("Response", "Respuesta"), drop = FALSE) +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq_along(levels(lik$Item_lab)), labels = levels(lik$Item_lab),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(limits = c(x_left, 118), breaks = c(if (x_left <= -25) -25, 0, 25, 50, 75, 100),
                     labels = function(x) paste0(abs(x), "%")) +
  labs(x = L("Share of evaluation forms (right label: % rating 4 or 5)", "Proporción de formularios (etiqueta derecha: % con calificación 4 o 5)"), y = NULL,
       title = L("Perceived usefulness, ease of use and sustainability", "Utilidad percibida, facilidad de uso y sostenibilidad")) +
  theme_pub() +
  theme(legend.position = "bottom", axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3)) +
  guides(fill = guide_legend(nrow = 1))

tm <- d %>% mutate(Role = ifelse(Evaluator_Type == 1, "Attending", "Resident")) %>%
  count(Role, Eval_Time_Min) %>% tidyr::complete(Role, Eval_Time_Min, fill = list(n = 0)) %>%
  group_by(Role) %>% mutate(N = sum(n), pct = 100 * n / N) %>% ungroup() %>%
  mutate(RoleLab = ifelse(Role == "Attending", L("Attending forms", "Formularios del adscrito"),
                          L("Self-assessment forms", "Formularios de autoevaluación")))
p6b <- ggplot(tm, aes(factor(Eval_Time_Min), pct, fill = Role)) +
  geom_col(position = position_dodge(0.8), width = 0.72, colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%", pct), y = pct + 2.5), position = position_dodge(0.8),
            size = 2.4, colour = INK2) +
  scale_fill_manual(values = ROLE, labels = c(Attending = sprintf(L("Attending forms (n = %d)", "Formularios del adscrito (n = %d)"), sum(d$Evaluator_Type == 1)),
                                              Resident = sprintf(L("Self-assessment forms (n = %d)", "Formularios de autoevaluación (n = %d)"), sum(d$Evaluator_Type == 2))),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = L("Self-reported time to complete the form (minutes)", "Tiempo autorreportado para completar el formulario (minutos)"),
       y = L("Evaluation forms", "Formularios de evaluación"),
       title = L("Evaluation completion time", "Tiempo de llenado de la evaluación")) +
  theme_pub() + theme(legend.position = "bottom", panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))

if (exists("free", asNamespace("patchwork"))) p6b <- patchwork::free(p6b)   # do not align B to A's long labels
p6 <- (p6a / p6b) + plot_layout(heights = c(1.5, 1)) + plot_annotation(tag_levels = "A")
save_fig(p6, "Figure_6_Feasibility.png", 7.5, 6.8)

leg6 <- write_legend("Figure_6_Legend.md",
  L("Figure 6. Feasibility and Acceptability of the SIMPL-Based EPA Assessment Model",
    "Figura 6. Factibilidad y aceptabilidad del modelo de evaluación de EPAs basado en SIMPL"),
  sprintf(L("(A) Diverging stacked bars of responses to the 5-point perception items on attending evaluation forms (n = %d) and resident self-assessment forms (n = %d); bars are centered on the neutral response, with lower ratings to the left and higher ratings to the right. Numbers at the right give the percentage of forms rating the item 4 or 5. (B) Distribution of self-reported time to complete each evaluation form.",
            "(A) Barras apiladas divergentes de las respuestas a los reactivos de percepción de 5 puntos en los formularios de evaluación del adscrito (n = %d) y de autoevaluación del residente (n = %d); las barras están centradas en la respuesta neutral, con las calificaciones menores a la izquierda y las mayores a la derecha. Los números a la derecha indican el porcentaje de formularios que calificaron el reactivo con 4 o 5. (B) Distribución del tiempo autorreportado para completar cada formulario de evaluación."),
          sum(d$Evaluator_Type == 1), sum(d$Evaluator_Type == 2)))

lik_tab <- lik %>% mutate(cell = sprintf("%d (%s%%)", n, num(pct, 1))) %>%
  dplyr::select(Role, Item = Item_lab, N, Score, cell) %>%
  tidyr::pivot_wider(names_from = Score, values_from = cell, names_prefix = L("Score ", "Puntaje ")) %>%
  arrange(Item, Role) %>% as.data.frame()
names(lik_tab)[1:2] <- L(c("Role", "Item"), c("Rol", "Reactivo"))
tm_tab <- tm %>% mutate(cell = sprintf("%d (%s%%)", n, num(pct, 1))) %>%
  dplyr::select(Forms = RoleLab, Minutes = Eval_Time_Min, cell) %>%
  tidyr::pivot_wider(names_from = Minutes, values_from = cell, names_prefix = "Min ") %>% as.data.frame()
names(tm_tab)[1] <- L("Forms", "Formularios")
tm_sum <- d %>% mutate(Forms = ifelse(Evaluator_Type == 1, L("Attending forms", "Formularios del adscrito"),
                                      L("Self-assessment forms", "Formularios de autoevaluación"))) %>%
  group_by(Forms) %>% summarise(N = n(), mi = fmt_mediqr(Eval_Time_Min),
                                ms = fmt_meansd(Eval_Time_Min), le3 = num(100 * mean(Eval_Time_Min <= 3), 1)) %>%
  as.data.frame()
names(tm_sum) <- c(L("Forms", "Formularios"), "N", L("Median (IQR)", "Mediana (RIC)"), L("Mean (SD)", "Media (DE)"), "≤3 min, %")
write_fig_results("6", L("Feasibility and acceptability", "Factibilidad y aceptabilidad"), leg6, list(
  list(heading = L("A. Likert responses by item and form type, n (%)", "A. Respuestas tipo Likert por reactivo y tipo de formulario, n (%)"), data = lik_tab),
  list(heading = L("B. Completion time distribution, n (%)", "B. Distribución del tiempo de llenado, n (%)"), data = tm_tab),
  list(heading = L("B. Completion time summary (minutes)", "B. Resumen del tiempo de llenado (minutos)"), data = tm_sum)
))

rebuild_all_md()
save_session_info()
message(L("Figures 5–6 written to: ", "Figuras 5–6 guardadas en: "), fig_dir)
