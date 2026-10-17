# =============================================================================
# EPA / SIMPL study — Figures 2–4
#
#   Figure 2. Attending-rated Zwisch autonomy and SIMPL performance by PGY
#             (100% stacked bars; two panels)
#   Figure 3. Model-predicted probability of "supervision only" (Zwisch) and of
#             a practice-ready or better rating (SIMPL) by PGY, by complexity
#   Figure 4. Self vs attending ratings: (A) agreement heatmaps for the paired
#             cases; (B) mean self − attending difference by PGY with 95% CIs
#
# Output (Results/Figures): Figure_2..4 PNG (600 dpi, white background),
#   Figure_2..4_Legend.md, Figure_3_Predicted_Probabilities.csv,
#   Figure_4B_Mean_Difference.csv
#
# Colors: ordinal scales use one blue hue, light -> dark (validated ramp);
#         the two-series panel (4B) uses blue/orange plus distinct point shapes.
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
load_packages(c("ordinal", "ggplot2", "patchwork"))
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")   # used as MASS:: only
set.seed(2026)
B_SIM <- 2000   # parameter draws for prediction CIs / bootstrap replicates

# Figure style (colors, theme_pub, save_fig, write_legend, ...) is defined in EPA_helpers.R

d   <- load_epa()
att <- d %>% filter(Evaluator_Type == 1)
n_att <- nrow(att); n_res <- n_distinct(att$Resident_ID)

# =============================================================================
# FIGURE 2 — 100% stacked bars by PGY (attending ratings)
# =============================================================================
stack_panel <- function(var, labels, ramp, title, legend_title) {
  df <- att %>%
    count(PGY, level = .data[[var]]) %>%
    tidyr::complete(PGY, level = 1:length(labels), fill = list(n = 0)) %>%
    group_by(PGY) %>% mutate(N = sum(n), pct = 100 * n / N) %>% ungroup() %>%
    mutate(level_f = factor(level, levels = rev(seq_along(labels)), labels = rev(labels)),
           ink = label_ink(level, length(labels)),
           lab = ifelse(pct >= 8, sprintf("%.0f%%", pct), ""))
  ns <- df %>% distinct(PGY, N)
  ggplot(df, aes(PGY, pct, fill = level_f)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.4) +
    geom_text(aes(label = lab, colour = ink, group = level_f), position = position_stack(vjust = 0.5),
              size = 2.6, show.legend = FALSE) +
    geom_text(data = ns, aes(PGY, 103, label = paste0("n = ", N)), inherit.aes = FALSE,
              size = 2.5, colour = INK2) +
    scale_fill_manual(values = setNames(rev(ramp), rev(labels)), breaks = labels, name = legend_title) +
    scale_colour_identity() +
    scale_y_continuous(limits = c(0, 106), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = L("Postgraduate year", "Año de residencia"), y = L("Attending evaluations", "Evaluaciones del adscrito"), title = title) +
    theme_pub() +
    theme(legend.position = "bottom", legend.direction = "vertical",
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))
}
# zw_short / sm_short (numbered level labels) are defined in EPA_helpers.R
p2 <- (stack_panel("Zwisch_Autonomy", zw_short, RAMP4, L("Zwisch autonomy level", "Nivel de autonomía de Zwisch"), L("Zwisch level", "Nivel de Zwisch")) |
       stack_panel("SIMPL_Performance", sm_short, RAMP5, L("SIMPL performance rating", "Calificación de desempeño SIMPL"), L("SIMPL rating", "Calificación SIMPL"))) +
  plot_annotation(tag_levels = "A")
save_fig(p2, "Figure_2_Ratings_by_PGY.png", 7.5, 4.6)
leg2 <- write_legend("Figure_2_Legend.md",
  L("Figure 2. Attending-Rated Intraoperative Autonomy and Performance by Postgraduate Year",
    "Figura 2. Autonomía y desempeño intraoperatorios calificados por el adscrito por año de residencia"),
  sprintf(L("Distribution of (A) Zwisch autonomy levels and (B) SIMPL performance ratings assigned by attending surgeons across %d intraoperative evaluations of %d residents, shown as the percentage of evaluations within each postgraduate year. Darker shading indicates greater autonomy or higher performance. Numbers above bars are evaluations per postgraduate year; segment labels are shown for categories representing at least 8%% of evaluations.",
            "Distribución de (A) los niveles de autonomía de Zwisch y (B) las calificaciones de desempeño SIMPL asignados por los cirujanos adscritos en %d evaluaciones intraoperatorias de %d residentes, expresada como porcentaje de evaluaciones dentro de cada año de residencia. El tono más oscuro indica mayor autonomía o mejor desempeño. Los números sobre las barras son evaluaciones por año de residencia; se muestran etiquetas en los segmentos que representan al menos 8%% de las evaluaciones."), n_att, n_res))

dist_table <- function(var, labels) {
  w <- t(sapply(levels(att$PGY), function(g) {
    x <- att[[var]][att$PGY == g]
    c(N = length(x), vapply(seq_along(labels), function(k) pct_cell(sum(x == k), length(x)), ""))
  }))
  all <- c(N = nrow(att), vapply(seq_along(labels), function(k) pct_cell(sum(att[[var]] == k), nrow(att)), ""))
  out <- data.frame(PGY = c(levels(att$PGY), OVERALL), rbind(w, all), check.names = FALSE, row.names = NULL)
  names(out) <- c(L("PGY", "Año"), "N", labels)
  out
}
med_row <- function(var) {
  q <- tapply(att[[var]], att$PGY, function(x) fmt_mediqr(x))
  out <- data.frame(names(q), as.character(q), row.names = NULL)
  names(out) <- c(L("PGY", "Año"), L("Median (IQR)", "Mediana (RIC)"))
  out
}
write_fig_results("2", L("Attending ratings by postgraduate year", "Calificaciones del adscrito por año de residencia"), leg2, list(
  list(heading = L("A. Zwisch autonomy level, n (row %)", "A. Nivel de autonomía de Zwisch, n (% de la fila)"), data = dist_table("Zwisch_Autonomy", zw_short)),
  list(heading = L("A. Zwisch score by PGY", "A. Puntaje de Zwisch por año de residencia"), data = med_row("Zwisch_Autonomy")),
  list(heading = L("B. SIMPL performance rating, n (row %)", "B. Calificación de desempeño SIMPL, n (% de la fila)"), data = dist_table("SIMPL_Performance", sm_short)),
  list(heading = L("B. SIMPL score by PGY", "B. Puntaje SIMPL por año de residencia"), data = med_row("SIMPL_Performance"))
))

# =============================================================================
# FIGURE 3 — Predicted probabilities from the Table 4 models
#   P(Zwisch = supervision only) and P(SIMPL >= practice-ready), by PGY and
#   complexity; marginal standardization over the observed procedure, approach,
#   attending and timing, for a resident with an average random effect (= 0).
#   95% CIs from 2,000 draws of the fixed-effect parameters (multivariate normal).
# =============================================================================
mdat <- att %>%
  mutate(Resident_ID = factor(Resident_ID),
         Months   = as.numeric(Eval_Date - min(d$Eval_Date)) / 30.4375,
         Zwisch_o = factor(Zwisch_Autonomy, levels = 1:4, ordered = TRUE),
         SIMPL_o  = factor(SIMPL_Performance, levels = 1:5, ordered = TRUE))
rhs <- "PGY_Year + Complexity + Procedure_f + Approach + Attending + Months"
m_z <- clmm(as.formula(paste("Zwisch_o ~", rhs, "+ (1 | Resident_ID)")), data = mdat, nAGQ = 10)
m_s <- clmm(as.formula(paste("SIMPL_o ~",  rhs, "+ (1 | Resident_ID)")), data = mdat, nAGQ = 10)

# P(Y > cut) = 1 - plogis(theta_cut - eta)
predict_upper <- function(m, cut) {
  fixed <- c(names(m$alpha), names(m$beta))
  V     <- vcov(m)[fixed, fixed]
  draws <- MASS::mvrnorm(B_SIM, mu = c(m$alpha, m$beta), Sigma = V)
  grid  <- expand.grid(PGY_Year = 1:5, Complexity = levels(mdat$Complexity), stringsAsFactors = FALSE)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    nd <- mdat; nd$PGY_Year <- grid$PGY_Year[i]
    nd$Complexity <- factor(grid$Complexity[i], levels = levels(mdat$Complexity))
    X  <- model.matrix(as.formula(paste("~", rhs)), nd)[, names(m$beta), drop = FALSE]
    pr <- function(theta, beta) mean(1 - plogis(theta - X %*% beta))
    est <- pr(m$alpha[cut], m$beta)
    sim <- apply(draws, 1, function(b) pr(b[names(m$alpha)[cut]], b[names(m$beta)]))
    data.frame(grid[i, ], est = est, lo = quantile(sim, .025), hi = quantile(sim, .975), row.names = NULL)
  })
  do.call(rbind, out)
}
OUT_Z <- L("Zwisch: supervision only", "Zwisch: solo supervisión")
OUT_S <- L("SIMPL: practice-ready or exceptional", "SIMPL: listo para la práctica o excepcional")
pz <- predict_upper(m_z, 3) %>% mutate(Outcome = OUT_Z)
ps <- predict_upper(m_s, 3) %>% mutate(Outcome = OUT_S)
pp <- rbind(pz, ps) %>%
  mutate(Complexity = factor(Complexity, levels = CX_LAB),
         Outcome = factor(Outcome, levels = c(OUT_Z, OUT_S)))
write.csv(pp %>% mutate(across(c(est, lo, hi), ~ round(.x, 4))),
          file.path(fig_res_dir, L("Figure_3_Predicted_Probabilities.csv", "Figura_3_Probabilidades_Predichas.csv")), row.names = FALSE)

prob_panel <- function(df, title) {
  dodge <- position_dodge(width = 0.38)
  ggplot(df, aes(PGY_Year, est, colour = Complexity, shape = Complexity, linetype = Complexity, group = Complexity)) +
    geom_hline(yintercept = c(0.25, 0.5, 0.75), colour = GRID, linewidth = 0.3) +
    geom_line(position = dodge, linewidth = 0.6) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45, position = dodge, linetype = "solid") +
    geom_point(position = dodge, size = 2.1, fill = "white", stroke = 0.7) +
    scale_colour_manual(values = setNames(RAMP3, CX_LAB), name = CX_NAME) +
    scale_shape_manual(values = setNames(c(21, 22, 24), CX_LAB), name = CX_NAME) +
    scale_linetype_manual(values = setNames(c("dotted", "dashed", "solid"), CX_LAB), name = CX_NAME) +
    scale_x_continuous(breaks = 1:5, labels = PGY_LAB, expand = expansion(add = 0.3)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = function(x) paste0(x * 100, "%"),
                       expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = L("Postgraduate year", "Año de residencia"), y = L("Predicted probability", "Probabilidad predicha"), title = title) +
    theme_pub()
}
CX_NAME <- L("Case complexity", "Complejidad del caso")
p3 <- (prob_panel(filter(pp, Outcome == levels(pp$Outcome)[1]), OUT_Z) |
       prob_panel(filter(pp, Outcome == levels(pp$Outcome)[2]), OUT_S)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")
save_fig(p3, "Figure_3_Predicted_Probabilities.png", 7.5, 3.9)
leg3 <- write_legend("Figure_3_Legend.md",
  L("Figure 3. Model-Predicted Probability of Supervision-Only Autonomy and Practice-Ready Performance",
    "Figura 3. Probabilidad predicha por el modelo de autonomía de solo supervisión y de desempeño listo para la práctica"),
  sprintf(L("Predicted probability that the attending rated (A) the Zwisch level as supervision only and (B) SIMPL performance as practice-ready or exceptional, by postgraduate year and attending-rated case complexity. Estimates come from the mixed-effects ordinal models in Table 4 (%d attending evaluations, %d residents), standardized over the observed procedure, surgical approach, evaluating attending, and timing, for a resident with an average random effect. Error bars are 95%% CIs from %s simulations of the fixed-effect parameters. Points are offset horizontally for legibility.",
            "Probabilidad predicha de que el adscrito calificara (A) el nivel de Zwisch como solo supervisión y (B) el desempeño SIMPL como listo para la práctica o excepcional, por año de residencia y complejidad del caso calificada por el adscrito. Las estimaciones provienen de los modelos ordinales de efectos mixtos de la Tabla 4 (%d evaluaciones del adscrito, %d residentes), estandarizadas sobre el procedimiento, el abordaje quirúrgico, el adscrito evaluador y la fecha observados, para un residente con efecto aleatorio promedio. Las barras de error son IC 95%% de %s simulaciones de los parámetros fijos. Los puntos se desplazan horizontalmente para facilitar la lectura."),
          n_att, n_res, format(B_SIM, big.mark = ",")))

prob_table <- function(outcome) {
  x <- pp[pp$Outcome == outcome, ]
  w <- reshape(x[, c("PGY_Year", "Complexity", "est", "lo", "hi")] %>%
                 mutate(cell = ci_pct(est, lo, hi)) %>% dplyr::select(PGY_Year, Complexity, cell),
               idvar = "PGY_Year", timevar = "Complexity", direction = "wide")
  names(w) <- sub("^cell\\.", "", names(w))
  w[[1]] <- PGY_LAB[w[[1]]]; names(w)[1] <- L("PGY", "Año")
  rownames(w) <- NULL
  w
}
or_txt <- function(m, term) {
  cf <- coef(summary(m))[term, ]
  sprintf(L("%s (95%% CI %s–%s)", "%s (IC 95%% %s–%s)"), num(exp(cf[1])), num(exp(cf[1] - 1.96 * cf[2])), num(exp(cf[1] + 1.96 * cf[2])))
}
write_fig_results("3", L("Model-predicted probabilities", "Probabilidades predichas por el modelo"), leg3, list(
  list(heading = L("Method", "Método"), text = sprintf(
    L("Predicted probability of the top category(ies) from the Table 4 mixed models (%d attending evaluations, %d residents), standardized over observed procedure, approach, attending and timing; random effect = 0; 95%% CIs from %s draws of the fixed effects. Model PGY aOR per year: Zwisch %s; SIMPL %s.",
      "Probabilidad predicha de la(s) categoría(s) superior(es) con los modelos mixtos de la Tabla 4 (%d evaluaciones del adscrito, %d residentes), estandarizada sobre procedimiento, abordaje, adscrito y fecha observados; efecto aleatorio = 0; IC 95%% de %s simulaciones de los efectos fijos. RMa del año de residencia por año: Zwisch %s; SIMPL %s."),
    n_att, n_res, format(B_SIM, big.mark = ","), or_txt(m_z, "PGY_Year"), or_txt(m_s, "PGY_Year"))),
  list(heading = L("A. P(Zwisch = supervision only), % (95% CI), by case complexity",
                   "A. P(Zwisch = solo supervisión), % (IC 95%), por complejidad del caso"),
       data = prob_table(OUT_Z)),
  list(heading = L("B. P(SIMPL = practice-ready or exceptional), % (95% CI), by case complexity",
                   "B. P(SIMPL = listo para la práctica o excepcional), % (IC 95%), por complejidad del caso"),
       data = prob_table(OUT_S))
))

# =============================================================================
# FIGURE 4 — Self vs attending
# =============================================================================
pairs <- d %>%
  group_by(Case_ID) %>%
  filter(n() == 2, sum(Evaluator_Type == 1) == 1, sum(Evaluator_Type == 2) == 1, n_distinct(Resident_ID) == 1) %>%
  summarise(Resident_ID = first(Resident_ID), PGY = first(PGY), PGY_Year = first(PGY_Year),
            Z_att = Zwisch_Autonomy[Evaluator_Type == 1],   Z_self = Zwisch_Autonomy[Evaluator_Type == 2],
            S_att = SIMPL_Performance[Evaluator_Type == 1], S_self = SIMPL_Performance[Evaluator_Type == 2],
            .groups = "drop")
n_pairs <- nrow(pairs)

heat_panel <- function(a, s, labels, title) {
  K  <- length(labels)
  df <- as.data.frame(table(Attending = factor(pairs[[a]], 1:K), Self = factor(pairs[[s]], 1:K)))
  df <- df %>% mutate(Attending = as.integer(Attending), Self = as.integer(Self),
                      ink = ifelse(Freq >= max(Freq) * 0.55, "white", INK),
                      lab = ifelse(Freq > 0, Freq, ""))
  diag_df <- data.frame(x = 1:K, y = 1:K)
  ggplot(df, aes(Self, Attending)) +
    geom_tile(aes(fill = Freq), colour = "white", linewidth = 0.8) +
    geom_tile(data = diag_df, aes(x, y), fill = NA, colour = INK, linewidth = 0.5, width = 0.94, height = 0.94) +
    geom_text(aes(label = lab, colour = ink), size = 2.8) +
    scale_fill_gradientn(colours = HEAT, limits = c(0, max(df$Freq)), name = L("Pairs, n", "Pares, n"),
                         guide = guide_colourbar(barheight = unit(60, "pt"), barwidth = unit(7, "pt"))) +
    scale_colour_identity() +
    scale_x_continuous(breaks = 1:K, labels = labels, expand = c(0, 0)) +
    scale_y_continuous(breaks = 1:K, labels = labels, expand = c(0, 0)) +
    coord_equal() +
    labs(x = L("Resident self-rating", "Autoevaluación del residente"), y = L("Attending rating", "Calificación del adscrito"), title = title) +
    theme_pub() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 35, hjust = 1))
}
# zw_ax / sm_ax (short level labels) are defined in EPA_helpers.R
p4a <- heat_panel("Z_att", "Z_self", zw_ax, L("Zwisch autonomy level", "Autonomía de Zwisch"))   # short Spanish titles fit the panel
p4b <- heat_panel("S_att", "S_self", sm_ax, L("SIMPL performance rating", "Desempeño SIMPL"))

# Mean difference (self − attending) by PGY; 95% CI by cluster bootstrap of residents
boot_mean <- function(x, id) {
  ids <- unique(id)
  bs <- replicate(B_SIM, {
    smp <- sample(ids, length(ids), replace = TRUE)
    mean(unlist(lapply(smp, function(i) x[id == i])))
  })
  c(est = mean(x), lo = quantile(bs, .025, names = FALSE), hi = quantile(bs, .975, names = FALSE))
}
md <- do.call(rbind, lapply(c("Zwisch", "SIMPL"), function(sc) {
  a <- if (sc == "Zwisch") "Z" else "S"
  do.call(rbind, lapply(c(OVERALL, levels(pairs$PGY)), function(g) {
    sub <- if (g == OVERALL) pairs else pairs[pairs$PGY == g, ]
    diff <- sub[[paste0(a, "_self")]] - sub[[paste0(a, "_att")]]
    r <- boot_mean(diff, sub$Resident_ID)
    data.frame(Scale = sc, Group = g, n = nrow(sub), est = r["est"], lo = r["lo"], hi = r["hi"], row.names = NULL)
  }))
}))
md$Group <- factor(md$Group, levels = c(levels(pairs$PGY), OVERALL))
md$Scale <- factor(md$Scale, levels = c("Zwisch", "SIMPL"))
write.csv(md %>% mutate(across(c(est, lo, hi), ~ round(.x, 4))),
          file.path(fig_res_dir, L("Figure_4B_Mean_Difference.csv", "Figura_4C_Diferencia_Media.csv")), row.names = FALSE)

ylim_b <- max(abs(c(md$lo, md$hi)), 1) * 1.08
n_lab  <- md %>% filter(Scale == "Zwisch") %>% transmute(Group, lab = paste0("n = ", n))
p4c <- ggplot(md, aes(Group, est, colour = Scale, shape = Scale)) +
  geom_blank() +                                                     # trains the discrete x axis first
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "#f3f3f1") +   # "Overall" band
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.6, position = position_dodge(0.45)) +
  geom_point(size = 2.3, position = position_dodge(0.45), fill = "white", stroke = 0.8) +
  geom_text(data = n_lab, aes(Group, -ylim_b * 0.96, label = lab), inherit.aes = FALSE, size = 2.4, colour = INK2) +
  annotate("text", x = 0.55, y = ylim_b * 0.93, label = L("Self-rating higher than attending", "Autoevaluación mayor que la del adscrito"),
           hjust = 0, size = 2.5, colour = INK2, fontface = "italic") +
  annotate("text", x = 0.55, y = -ylim_b * 0.80, label = L("Self-rating lower than attending", "Autoevaluación menor que la del adscrito"),
           hjust = 0, size = 2.5, colour = INK2, fontface = "italic") +
  scale_colour_manual(values = PAIR, name = NULL) +
  scale_shape_manual(values = c(Zwisch = 21, SIMPL = 22), name = NULL) +
  scale_y_continuous(limits = c(-ylim_b, ylim_b), labels = function(x) sub("^-", MINUS, sprintf("%.1f", x))) +
  labs(x = NULL, y = L(paste0("Self ", MINUS, " attending rating (points)"), paste0("Autoevaluación ", MINUS, " adscrito (puntos)")),
       title = L("Mean difference by postgraduate year", "Diferencia media por año de residencia")) +
  theme_pub() +
  theme(legend.position = "top", legend.justification = "right",
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))

p4 <- ((p4a | p4b) / p4c) + plot_layout(heights = c(1.15, 1)) + plot_annotation(tag_levels = "A")
save_fig(p4, "Figure_4_Self_vs_Attending.png", 7.5, 6.6)
leg4 <- write_legend("Figure_4_Legend.md",
  L("Figure 4. Agreement Between Resident Self-Assessment and Attending Rating",
    "Figura 4. Concordancia entre la autoevaluación del residente y la calificación del adscrito"),
  sprintf(L("(A, B) Cross-classification of attending ratings (rows) and resident self-ratings (columns) for the %d surgical cases with both an attending evaluation and a self-assessment, for (A) Zwisch autonomy level and (B) SIMPL performance rating; cell values are numbers of pairs and outlined cells mark exact agreement. (C) Mean difference between self-rating and attending rating (positive values indicate a higher self-rating) by postgraduate year and overall, with 95%% CIs from a cluster bootstrap resampling residents (%s replicates). The dashed line marks no difference; numbers below the axis are pairs per group.",
            "(A, B) Clasificación cruzada de las calificaciones del adscrito (filas) y las autoevaluaciones del residente (columnas) en los %d casos quirúrgicos con evaluación del adscrito y autoevaluación, para (A) el nivel de autonomía de Zwisch y (B) la calificación de desempeño SIMPL; los valores de las celdas son números de pares y las celdas remarcadas indican concordancia exacta. (C) Diferencia media entre la autoevaluación y la calificación del adscrito (los valores positivos indican una autoevaluación mayor) por año de residencia y en total, con IC 95%% por bootstrap por conglomerados con remuestreo de residentes (%s réplicas). La línea discontinua indica ausencia de diferencia; los números bajo el eje son pares por grupo."),
          n_pairs, format(B_SIM, big.mark = ",")))

xtab <- function(a, sv, labels) {
  K <- length(labels)
  tb <- table(factor(pairs[[a]], 1:K, labels = labels), factor(pairs[[sv]], 1:K, labels = labels))
  out <- data.frame(labels, matrix(as.integer(tb), nrow = K), check.names = FALSE)
  names(out) <- c(L("Attending \\ Self", "Adscrito \\ Autoevaluación"), labels)
  out
}
agree_txt <- function(a, sv) {
  dd <- pairs[[sv]] - pairs[[a]]
  sprintf(L("Exact agreement %d/%d (%s%%); within one level %d/%d (%s%%); self higher %d, self lower %d.",
            "Concordancia exacta %d/%d (%s%%); dentro de un nivel %d/%d (%s%%); autoevaluación mayor %d, menor %d."),
          sum(dd == 0), length(dd), num(100 * mean(dd == 0), 1), sum(abs(dd) <= 1), length(dd),
          num(100 * mean(abs(dd) <= 1), 1), sum(dd > 0), sum(dd < 0))
}
md_tab <- md %>% transmute(Scale, Group = as.character(Group), n,
                           diff = fmt_ci(est, lo, hi),
                           excl = ifelse(lo > 0 | hi < 0, L("yes", "sí"), "no")) %>%
  arrange(Scale, factor(Group, levels = c(levels(pairs$PGY), OVERALL))) %>% as.data.frame()
names(md_tab) <- c(L("Scale", "Escala"), L("Group", "Grupo"), "n",
                   L("Mean difference (95% CI)", "Diferencia media (IC 95%)"), L("CI excludes 0", "El IC excluye 0"))
write_fig_results("4", L("Self vs attending ratings", "Autoevaluación vs calificación del adscrito"), leg4, list(
  list(heading = L("A. Zwisch cross-classification (rows = attending, columns = self; counts)",
                   "A. Clasificación cruzada de Zwisch (filas = adscrito, columnas = autoevaluación; conteos)"),
       text = agree_txt("Z_att", "Z_self"), data = xtab("Z_att", "Z_self", zw_ax)),
  list(heading = L("B. SIMPL cross-classification (rows = attending, columns = self; counts)",
                   "B. Clasificación cruzada de SIMPL (filas = adscrito, columnas = autoevaluación; conteos)"),
       text = agree_txt("S_att", "S_self"), data = xtab("S_att", "S_self", sm_ax)),
  list(heading = L("C. Mean self − attending difference by PGY (cluster-bootstrap 95% CI)",
                   "C. Diferencia media autoevaluación − adscrito por año de residencia (IC 95% por bootstrap por conglomerados)"),
       text = L("Positive = resident rated self higher than the attending.",
                "Positivo = el residente se calificó más alto que el adscrito."), data = md_tab)
))

rebuild_all_md()
save_session_info()
message(L("Figures 2–4 written to: ", "Figuras 2–4 guardadas en: "), fig_dir)
