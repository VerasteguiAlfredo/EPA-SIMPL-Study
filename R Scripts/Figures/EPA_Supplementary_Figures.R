# =============================================================================
# EPA / SIMPL study — Supplementary Figures S1–S3
#
#   Figure S1. Zwisch × SIMPL cross-classification heatmap (attending forms),
#              with Spearman and Kendall tau-b correlations
#   Figure S2. Rating distributions by evaluating attending (100% stacked bars)
#   Figure S3. Resident-level variation: caterpillar plot of resident random
#              intercepts from the Table 4 mixed models
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
set.seed(2026)
B_BOOT <- 2000

d   <- load_epa()
att <- d %>% filter(Evaluator_Type == 1)
n_att <- nrow(att); n_res <- n_distinct(att$Resident_ID)
# zw_ax / sm_ax / zw_short / sm_short (level labels) are defined in EPA_helpers.R

# =============================================================================
# FIGURE S1 — Zwisch × SIMPL heatmap
# =============================================================================
rho  <- cor(att$Zwisch_Autonomy, att$SIMPL_Performance, method = "spearman")
tau  <- cor(att$Zwisch_Autonomy, att$SIMPL_Performance, method = "kendall")   # tau-b (handles ties)
ids  <- unique(att$Resident_ID)
boot <- replicate(B_BOOT, {
  smp <- sample(ids, length(ids), replace = TRUE)
  bb  <- do.call(rbind, lapply(smp, function(i) att[att$Resident_ID == i, c("Zwisch_Autonomy", "SIMPL_Performance")]))
  c(cor(bb[[1]], bb[[2]], method = "spearman"), cor(bb[[1]], bb[[2]], method = "kendall"))
})
rho_ci <- quantile(boot[1, ], c(.025, .975), na.rm = TRUE)
tau_ci <- quantile(boot[2, ], c(.025, .975), na.rm = TRUE)
rho_txt <- sprintf(L("Spearman ρ = %s (95%% CI %s–%s)", "ρ de Spearman = %s (IC 95%% %s–%s)"), num(rho), num(rho_ci[1]), num(rho_ci[2]))
tau_txt <- sprintf(L("Kendall τ-b = %s (95%% CI %s–%s)", "τ-b de Kendall = %s (IC 95%% %s–%s)"), num(tau), num(tau_ci[1]), num(tau_ci[2]))

hm <- as.data.frame(table(Zwisch = factor(att$Zwisch_Autonomy, 1:4), SIMPL = factor(att$SIMPL_Performance, 1:5))) %>%
  mutate(Zwisch = as.integer(Zwisch), SIMPL = as.integer(SIMPL),
         ink = ifelse(Freq >= max(Freq) * 0.55, "white", INK), lab = ifelse(Freq > 0, Freq, ""))
pS1 <- ggplot(hm, aes(SIMPL, Zwisch)) +
  geom_tile(aes(fill = Freq), colour = "white", linewidth = 0.8) +
  geom_text(aes(label = lab, colour = ink), size = 3) +
  scale_fill_gradientn(colours = HEAT, limits = c(0, max(hm$Freq)), name = L("Evaluations, n", "Evaluaciones, n"),
                       guide = guide_colourbar(barheight = unit(70, "pt"), barwidth = unit(7, "pt"))) +
  scale_colour_identity() +
  scale_x_continuous(breaks = 1:5, labels = sm_ax, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:4, labels = zw_ax, expand = c(0, 0)) +
  coord_equal() +
  labs(x = L("SIMPL performance rating", "Calificación de desempeño SIMPL"), y = L("Zwisch autonomy level", "Nivel de autonomía de Zwisch"),
       title = L("Zwisch autonomy vs SIMPL performance", "Autonomía de Zwisch vs desempeño SIMPL"), subtitle = paste0(rho_txt, "\n", tau_txt)) +
  theme_pub() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        plot.subtitle = element_text(size = 8, colour = INK2))
save_fig(pS1, "Figure_S1_Zwisch_vs_SIMPL.png", 5.6, 4.2)
legS1 <- write_legend("Figure_S1_Legend.md",
  L("Supplementary Figure S1. Relationship Between Zwisch Autonomy and SIMPL Performance Ratings",
    "Figura suplementaria S1. Relación entre las calificaciones de autonomía de Zwisch y de desempeño SIMPL"),
  sprintf(L("Cross-classification of Zwisch autonomy level and SIMPL performance rating assigned by the attending in the same evaluation (%d attending evaluations, %d residents); cell values are numbers of evaluations. Correlations are Spearman ρ and Kendall τ-b, with 95%% CIs from a cluster bootstrap resampling residents (%s replicates).",
            "Clasificación cruzada del nivel de autonomía de Zwisch y de la calificación de desempeño SIMPL asignados por el adscrito en la misma evaluación (%d evaluaciones del adscrito, %d residentes); los valores de las celdas son números de evaluaciones. Las correlaciones son ρ de Spearman y τ-b de Kendall, con IC 95%% por bootstrap por conglomerados con remuestreo de residentes (%s réplicas)."),
          n_att, n_res, format(B_BOOT, big.mark = ",")))
xt <- as.data.frame.matrix(table(factor(att$Zwisch_Autonomy, 1:4, labels = zw_ax), factor(att$SIMPL_Performance, 1:5, labels = sm_ax)))
xt <- data.frame(`Zwisch \\ SIMPL` = rownames(xt), xt, check.names = FALSE, row.names = NULL)
s1_sum <- att %>% group_by(Zwisch) %>%
  summarise(n = n(), mi = fmt_mediqr(SIMPL_Performance), ms = fmt_meansd(SIMPL_Performance)) %>% as.data.frame()
names(s1_sum) <- c("Zwisch", "n", L("SIMPL median (IQR)", "SIMPL mediana (RIC)"), L("SIMPL mean (SD)", "SIMPL media (DE)"))
write_fig_results("S1", "Zwisch × SIMPL", legS1, list(
  list(heading = L("Correlation", "Correlación"), text = paste0(rho_txt, ". ", tau_txt, ".")),
  list(heading = L("Cross-classification (rows = Zwisch, columns = SIMPL; counts)",
                   "Clasificación cruzada (filas = Zwisch, columnas = SIMPL; conteos)"), data = xt),
  list(heading = L("Mean SIMPL score by Zwisch level", "Puntaje SIMPL por nivel de Zwisch"), data = s1_sum)
))

# =============================================================================
# FIGURE S2 — Rating distributions by attending
# =============================================================================
stack_by_att <- function(var, labels, ramp, title, legend_title) {
  df <- att %>% count(Attending, level = .data[[var]]) %>%
    tidyr::complete(Attending, level = seq_along(labels), fill = list(n = 0)) %>%
    group_by(Attending) %>% mutate(N = sum(n), pct = 100 * n / N) %>% ungroup() %>%
    mutate(level_f = factor(level, levels = rev(seq_along(labels)), labels = rev(labels)),
           ink = label_ink(level, length(labels)), lab = ifelse(pct >= 8, sprintf("%.0f%%", pct), ""))
  ns <- df %>% distinct(Attending, N)
  ggplot(df, aes(Attending, pct, fill = level_f)) +
    geom_col(width = 0.66, colour = "white", linewidth = 0.4) +
    geom_text(aes(label = lab, colour = ink, group = level_f), position = position_stack(vjust = 0.5), size = 2.6) +
    geom_text(data = ns, aes(Attending, 103, label = paste0("n = ", N)), inherit.aes = FALSE, size = 2.5, colour = INK2) +
    scale_fill_manual(values = setNames(rev(ramp), rev(labels)), breaks = labels, name = legend_title) +
    scale_colour_identity() +
    scale_y_continuous(limits = c(0, 106), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = L("Attending evaluations", "Evaluaciones del adscrito"), title = title) +
    theme_pub() +
    theme(legend.position = "bottom", legend.direction = "vertical",
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))
}
pS2 <- (stack_by_att("Zwisch_Autonomy", zw_short, RAMP4, L("Zwisch autonomy level", "Nivel de autonomía de Zwisch"), L("Zwisch level", "Nivel de Zwisch")) |
        stack_by_att("SIMPL_Performance", sm_short, RAMP5, L("SIMPL performance rating", "Calificación de desempeño SIMPL"), L("SIMPL rating", "Calificación SIMPL"))) +
  plot_annotation(tag_levels = "A")
save_fig(pS2, "Figure_S2_Ratings_by_Attending.png", 7.5, 4.6)
legS2 <- write_legend("Figure_S2_Legend.md",
  L("Supplementary Figure S2. Rating Distributions by Evaluating Attending",
    "Figura suplementaria S2. Distribución de las calificaciones por adscrito evaluador"),
  sprintf(L("Distribution of (A) Zwisch autonomy levels and (B) SIMPL performance ratings assigned by each of the %d evaluating attendings (%d attending evaluations). Distributions are unadjusted and partly reflect the postgraduate year of the residents each attending evaluated (Supplementary Table S3); attending-adjusted estimates are shown in Table 4.",
            "Distribución de (A) los niveles de autonomía de Zwisch y (B) las calificaciones de desempeño SIMPL asignados por cada uno de los %d adscritos evaluadores (%d evaluaciones del adscrito). Las distribuciones no están ajustadas y reflejan en parte el año de residencia de los residentes que evaluó cada adscrito (Tabla suplementaria S3); las estimaciones ajustadas por adscrito se muestran en la Tabla 4."),
          nlevels(att$Attending), n_att))
by_att <- function(var, labels) {
  w <- t(sapply(levels(att$Attending), function(a) {
    x <- att[[var]][att$Attending == a]
    c(N = length(x), vapply(seq_along(labels), function(k) sprintf("%d (%s%%)", sum(x == k), num(100 * mean(x == k), 1)), ""),
      `Median (IQR)` = fmt_mediqr(x))
  }))
  out <- data.frame(Attending = rownames(w), w, check.names = FALSE, row.names = NULL)
  names(out) <- c(L("Attending", "Adscrito"), "N", labels, L("Median (IQR)", "Mediana (RIC)"))
  out
}
pgy_mix <- as.data.frame.matrix(table(att$Attending, att$PGY))
pgy_mix <- data.frame(Attending = rownames(pgy_mix), pgy_mix, check.names = FALSE, row.names = NULL)
names(pgy_mix)[1] <- L("Attending", "Adscrito")
write_fig_results("S2", L("Ratings by attending", "Calificaciones por adscrito"), legS2, list(
  list(heading = L("A. Zwisch by attending, n (row %)", "A. Zwisch por adscrito, n (% de la fila)"), data = by_att("Zwisch_Autonomy", zw_short)),
  list(heading = L("B. SIMPL by attending, n (row %)", "B. SIMPL por adscrito, n (% de la fila)"), data = by_att("SIMPL_Performance", sm_short)),
  list(heading = L("Case mix: PGY of residents evaluated, n", "Distribución de casos: año de residencia de los residentes evaluados, n"), data = pgy_mix)
))

# =============================================================================
# FIGURE S3 — Caterpillar plot of resident random intercepts
# =============================================================================
mdat <- att %>% mutate(Resident_ID = factor(Resident_ID),
                       Months = as.numeric(Eval_Date - min(d$Eval_Date)) / 30.4375,
                       Zwisch_o = factor(Zwisch_Autonomy, levels = 1:4, ordered = TRUE),
                       SIMPL_o  = factor(SIMPL_Performance, levels = 1:5, ordered = TRUE))
rhs <- "PGY_Year + Complexity + Procedure_f + Approach + Attending + Months + (1 | Resident_ID)"
re_df <- function(y, label) {
  m  <- clmm(as.formula(paste(y, "~", rhs)), data = mdat, nAGQ = 10)
  re <- ranef(m)$Resident_ID; cv <- condVar(m)$Resident_ID
  res <- mdat %>% group_by(Resident_ID) %>% summarise(PGY = first(PGY), n = n(), .groups = "drop")
  data.frame(Resident_ID = rownames(re), est = re[, 1], se = sqrt(cv[, 1])) %>%
    left_join(res %>% mutate(Resident_ID = as.character(Resident_ID)), by = "Resident_ID") %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se, Model = label,
           sd_re = sqrt(as.numeric(VarCorr(m)$Resident_ID)))
}
MOD_Z <- L("Zwisch autonomy", "Autonomía de Zwisch"); MOD_S <- L("SIMPL performance", "Desempeño SIMPL")
cat_df <- rbind(re_df("Zwisch_o", MOD_Z), re_df("SIMPL_o", MOD_S))
cat_panel <- function(df, title) {
  df <- df %>% arrange(est) %>% mutate(rank = row_number())
  ggplot(df, aes(est, rank, colour = PGY)) +
    geom_vline(xintercept = 0, colour = INK2, linewidth = 0.4, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = setNames(RAMP5, levels(att$PGY)), name = L("Postgraduate year", "Año de residencia"), drop = FALSE) +
    scale_y_continuous(breaks = df$rank, labels = df$Resident_ID, expand = expansion(add = 0.8)) +
    labs(x = L("Resident random intercept (log-odds)", "Intercepto aleatorio del residente (log-odds)"), y = NULL, title = title) +
    theme_pub() +
    theme(axis.text.y = element_text(size = 6.5), panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3))
}
pS3 <- (cat_panel(filter(cat_df, Model == MOD_Z), MOD_Z) |
        cat_panel(filter(cat_df, Model == MOD_S), MOD_S)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")
save_fig(pS3, "Figure_S3_Resident_Variation.png", 7.5, 6.4)
legS3 <- write_legend("Figure_S3_Legend.md",
  L("Supplementary Figure S3. Resident-Level Variation in Autonomy and Performance Ratings",
    "Figura suplementaria S3. Variación entre residentes en las calificaciones de autonomía y desempeño"),
  sprintf(L("Caterpillar plots of resident-specific random intercepts (conditional modes, log-odds scale) with 95%% intervals from the mixed-effects ordinal models of Table 4 for (A) Zwisch autonomy and (B) SIMPL performance (%d attending evaluations, %d residents). Values above 0 indicate residents rated higher than expected for their postgraduate year, case complexity, procedure, approach, attending, and timing; residents are ordered by their estimate and colored by postgraduate year.",
            "Gráficos de oruga de los interceptos aleatorios de cada residente (modos condicionales, escala log-odds) con intervalos del 95%% de los modelos ordinales de efectos mixtos de la Tabla 4 para (A) autonomía de Zwisch y (B) desempeño SIMPL (%d evaluaciones del adscrito, %d residentes). Los valores mayores de 0 indican residentes calificados por encima de lo esperado para su año de residencia, la complejidad del caso, el procedimiento, el abordaje, el adscrito y la fecha; los residentes se ordenan según su estimación y se colorean por año de residencia."),
          n_att, n_res))
cat_tab <- cat_df %>% mutate(cell = sprintf(L("%s (%s to %s)", "%s (%s a %s)"), num(est), num(lo), num(hi))) %>%
  dplyr::select(Resident_ID, PGY, n, Model, cell) %>%
  tidyr::pivot_wider(names_from = Model, values_from = cell) %>% arrange(PGY, Resident_ID) %>% as.data.frame()
names(cat_tab)[1:3] <- L(c("Resident", "PGY", "Attending forms"), c("Residente", "Año", "Formularios del adscrito"))
sum_tab <- cat_df %>% group_by(Model) %>%
  summarise(sd = num(first(sd_re)), above = sum(lo > 0), below = sum(hi < 0),
            rng = sprintf(L("%s to %s", "%s a %s"), num(min(est)), num(max(est)))) %>% as.data.frame()
names(sum_tab) <- L(c("Model", "Random-intercept SD", "Residents with interval above 0", "Residents with interval below 0", "Range of estimates"),
                    c("Modelo", "DE del intercepto aleatorio", "Residentes con intervalo mayor de 0", "Residentes con intervalo menor de 0", "Rango de estimaciones"))
write_fig_results("S3", L("Resident-level variation", "Variación entre residentes"), legS3, list(
  list(heading = L("Summary", "Resumen"), data = sum_tab),
  list(heading = L("Resident random intercepts, conditional mode (95% interval), log-odds",
                   "Interceptos aleatorios de los residentes, modo condicional (intervalo del 95%), log-odds"), data = cat_tab)
))

rebuild_all_md()
save_session_info()
message(L("Supplementary Figures S1–S3 written to: ", "Figuras suplementarias S1–S3 guardadas en: "), fig_dir)
