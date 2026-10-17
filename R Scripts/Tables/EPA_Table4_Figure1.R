# =============================================================================
# EPA / SIMPL study — Table 4, Supplementary Table S1, and Figure 1
#
#   Table 4.  Mixed-effects ordinal (cumulative logit) models of attending-rated
#             Zwisch autonomy and SIMPL performance; PGY per 1-year increase
#             (primary analysis).
#   Table S1. Same models with PGY as a categorical variable (sensitivity).
#   Figure 1. Forest plot of adjusted odds ratios from Table 4 (forestploter),
#             600 dpi PNG, white background, plus a legend file.
#
#   Model: outcome ~ PGY + complexity + procedure + approach + attending + time
#                    + (1 | resident)          [attending forms only]
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
load_packages(c("ordinal", "forestploter"))
library(grid)

# ---- Data: attending evaluations only ---------------------------------------
d <- load_epa()
att <- d %>%
  filter(Evaluator_Type == 1) %>%
  mutate(
    Resident_ID = factor(Resident_ID),
    PGY_num     = PGY_Year,
    PGY_cat     = PGY,
    Months      = as.numeric(Eval_Date - min(d$Eval_Date)) / 30.4375,   # months since study start
    Zwisch_o    = factor(Zwisch_Autonomy, levels = 1:4, ordered = TRUE),
    SIMPL_o     = factor(SIMPL_Performance, levels = 1:5, ordered = TRUE)
  )
n_obs <- nrow(att)
n_res <- nlevels(droplevels(att$Resident_ID))

# ---- Fit models ---------------------------------------------------------------
covars <- "Complexity + Procedure_f + Approach + Attending + Months + (1 | Resident_ID)"
fit <- function(outcome, pgy) {
  clmm(as.formula(paste(outcome, "~", pgy, "+", covars)), data = att, link = "logit", nAGQ = 10)
}
m_z  <- fit("Zwisch_o", "PGY_num");  m_s  <- fit("SIMPL_o", "PGY_num")
m_zc <- fit("Zwisch_o", "PGY_cat");  m_sc <- fit("SIMPL_o", "PGY_cat")

tidy_or <- function(m) {
  cf <- coef(summary(m))
  cf <- cf[!grepl("\\|", rownames(cf)), , drop = FALSE]   # drop thresholds
  data.frame(term = rownames(cf), or = exp(cf[, 1]),
             lo = exp(cf[, 1] - qnorm(.975) * cf[, 2]),
             hi = exp(cf[, 1] + qnorm(.975) * cf[, 2]),
             p  = cf[, 4], row.names = NULL)
}
re_stats <- function(m) {
  v <- as.numeric(VarCorr(m)$Resident_ID)
  c(var = v, icc = v / (v + pi^2 / 3))
}

# ---- Row layout -------------------------------------------------------------
# Model terms are built from the factor levels, so they work in either language
r <- function(label, term, type) data.frame(label = label, term = term, type = type, stringsAsFactors = FALSE)
layout_rows <- function(pgy_block) rbind(
  pgy_block,
  r(L("Case complexity (attending-rated)", "Complejidad del caso (según el adscrito)"), NA, "header"),
  r(CX_LAB[1], NA, "ref"), r(CX_LAB[2:3], paste0("Complexity", CX_LAB[2:3]), "level"),
  r(L("Procedure", "Procedimiento"), NA, "header"),
  r(PROC_LAB[1], NA, "ref"), r(PROC_LAB[2:3], paste0("Procedure_f", PROC_LAB[2:3]), "level"),
  r(L("Surgical approach", "Abordaje quirúrgico"), NA, "header"),
  r(APP_LAB[1], NA, "ref"), r(APP_LAB[2], paste0("Approach", APP_LAB[2]), "level"),
  r(L("Evaluating attending", "Adscrito evaluador"), NA, "header"),
  r(ATT_LAB[1], NA, "ref"), r(ATT_LAB[2:3], paste0("Attending", ATT_LAB[2:3]), "level"),
  r(L("Time since study start, per month", "Tiempo desde el inicio del estudio, por mes"), "Months", "single")
)
pgy_cont <- r(L("Postgraduate year, per 1-year increase", "Año de residencia, por cada año adicional"), "PGY_num", "single")
pgy_cat  <- rbind(r(L("Postgraduate year", "Año de residencia"), NA, "header"), r(PGY_LAB[1], NA, "ref"),
                  r(PGY_LAB[2:5], paste0("PGY_cat", PGY_LAB[2:5]), "level"))

build_or_table <- function(rows, mz, ms) {
  tz <- tidy_or(mz); ts <- tidy_or(ms)
  cell <- function(t, R) {
    if (R$type == "header") return(c("", ""))
    if (R$type == "ref")    return(c(REF_CELL, ""))
    x <- t[t$term == R$term, ]
    c(fmt_or(x$or, x$lo, x$hi), fmt_p(x$p))
  }
  out <- lapply(seq_len(nrow(rows)), function(i) {
    R <- rows[i, ]
    lab <- switch(R$type, header = bold(R$label), single = R$label, indent(R$label))
    c(lab, cell(tz, R), cell(ts, R))
  })
  rz <- re_stats(mz); rs <- re_stats(ms)
  out <- c(out, list(
    c(bold(L("Random effect (resident)", "Efecto aleatorio (residente)")), "", "", "", ""),
    c(indent(L("Intercept variance", "Varianza del intercepto")), num(rz["var"]), "", num(rs["var"]), ""),
    c(indent(L("Intraclass correlation coefficient", "Coeficiente de correlación intraclase")), num(rz["icc"]), "", num(rs["icc"]), "")
  ))
  tab <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(tab) <- c("Characteristic", "z_or", "z_p", "s_or", "s_p")
  tab
}

ZW_AUT <- L("Zwisch autonomy", "Autonomía de Zwisch")
SM_PERF <- L("SIMPL performance", "Desempeño SIMPL")
hdr_md   <- c(CHAR, paste0(ZW_AUT, ", ", AOR_CI), PVAL, paste0(SM_PERF, ", ", AOR_CI), PVAL)
hdr_html <- c(CHAR, AOR_CI, PVAL, AOR_CI, PVAL)
span4    <- list(list("", 1), list(ZW_AUT, 2), list(SM_PERF, 2))

model_notes <- function(extra = NULL) c(
  sprintf(L("Mixed-effects cumulative logit (proportional odds) models fitted to %d attending evaluations of %d residents, with a random intercept for resident (adaptive Gauss–Hermite quadrature, 10 nodes). All variables shown were entered simultaneously.",
            "Modelos de efectos mixtos de logit acumulado (odds proporcionales) ajustados a %d evaluaciones del adscrito de %d residentes, con un intercepto aleatorio por residente (cuadratura adaptativa de Gauss-Hermite, 10 nodos). Todas las variables mostradas se incluyeron simultáneamente."), n_obs, n_res),
  L("An aOR >1 indicates higher odds of a higher Zwisch autonomy level or SIMPL performance rating. P values are from Wald tests.",
    "Una RMa >1 indica mayor probabilidad de un nivel de autonomía de Zwisch o de una calificación de desempeño SIMPL más altos. Los valores de P provienen de pruebas de Wald."),
  extra,
  L("Intraclass correlation coefficient computed on the latent scale as σ²<sub>resident</sub> / (σ²<sub>resident</sub> + π²/3).",
    "Coeficiente de correlación intraclase calculado en la escala latente como σ²<sub>residente</sub> / (σ²<sub>residente</sub> + π²/3)."),
  abbr("aOR", "CI", "PGY")
)

# ---- Table 4 and Table S1 ---------------------------------------------------
write_table(
  build_or_table(layout_rows(pgy_cont), m_z, m_s), "4",
  L("Factors Associated With Attending-Rated Intraoperative Autonomy and Performance",
    "Factores asociados con la autonomía y el desempeño intraoperatorios calificados por el adscrito"),
  L("Adjusted odds ratios from mixed-effects ordinal regression models of Zwisch autonomy level and SIMPL performance rating, accounting for repeated evaluations of the same resident.",
    "Razones de momios ajustadas de modelos de regresión ordinal de efectos mixtos del nivel de autonomía de Zwisch y de la calificación de desempeño SIMPL, que consideran las evaluaciones repetidas del mismo residente."),
  model_notes(L("Postgraduate year was modeled as a continuous variable (per 1-year increase); a categorical specification is shown in Supplementary Table S1. Case complexity reflects the attending's rating.",
                "El año de residencia se modeló como variable continua (por cada año adicional); la especificación categórica se muestra en la Tabla suplementaria S1. La complejidad del caso corresponde a la calificación del adscrito.")),
  hdr_md, hdr_html, span4
)
write_table(
  build_or_table(layout_rows(pgy_cat), m_zc, m_sc), "S1",
  L("Sensitivity Analysis With Postgraduate Year Modeled as a Categorical Variable",
    "Análisis de sensibilidad con el año de residencia modelado como variable categórica"),
  L("Adjusted odds ratios from the same mixed-effects ordinal models as Table 4, with each postgraduate year compared with PGY-1.",
    "Razones de momios ajustadas de los mismos modelos ordinales de efectos mixtos de la Tabla 4, comparando cada año de residencia con R1."),
  model_notes(L("Very large odds ratios and wide confidence intervals for later postgraduate years reflect near-complete separation (almost no PGY-1 resident reached the highest categories and almost all PGY-5 residents did); the per-year specification in Table 4 is therefore the primary analysis.",
                "Las razones de momios muy grandes y los intervalos de confianza amplios en los años de residencia superiores reflejan separación casi completa (casi ningún R1 alcanzó las categorías más altas y casi todos los R5 sí); por ello, la especificación por año de la Tabla 4 es el análisis principal.")),
  hdr_md, hdr_html, span4
)

# ---- Figure 1: forest plot --------------------------------------------------
rows <- layout_rows(pgy_cont)
tz <- tidy_or(m_z); ts <- tidy_or(m_s)
pick <- function(t, term, what) if (is.na(term)) NA_real_ else t[[what]][t$term == term]
fp <- data.frame(
  Characteristic = ifelse(rows$type %in% c("ref", "level"), paste0("   ", rows$label), rows$label),
  z_or = sapply(rows$term, pick, t = tz, what = "or"), z_lo = sapply(rows$term, pick, t = tz, what = "lo"),
  z_hi = sapply(rows$term, pick, t = tz, what = "hi"), s_or = sapply(rows$term, pick, t = ts, what = "or"),
  s_lo = sapply(rows$term, pick, t = ts, what = "lo"), s_hi = sapply(rows$term, pick, t = ts, what = "hi"),
  stringsAsFactors = FALSE
)
# Commas inside the CI: the en dash does not render in every PNG font
txt_or <- function(or, lo, hi, type) ifelse(type == "ref", L("Reference", "Referencia"),
                                     ifelse(is.na(or), "", sprintf("%.2f (%.2f, %.2f)", or, lo, hi)))
hz <- paste0(ZW_AUT, "\n", AOR_CI); hs <- paste0(SM_PERF, "\n", AOR_CI)
fp[[hz]] <- txt_or(fp$z_or, fp$z_lo, fp$z_hi, rows$type)
fp[[hs]] <- txt_or(fp$s_or, fp$s_lo, fp$s_hi, rows$type)
fp$` `  <- strrep(" ", 48)   # plotting column, Zwisch
fp$`  ` <- strrep(" ", 48)   # plotting column, SIMPL
plot_df <- fp[, c("Characteristic", " ", hz, "  ", hs)]
names(plot_df)[1] <- CHAR

all_ci <- c(fp$z_lo, fp$z_hi, fp$s_lo, fp$s_hi)
ticks  <- c(0.1, 0.3, 1, 3, 10, 30, 100)
xmin   <- max(ticks[ticks <= min(all_ci, na.rm = TRUE)], 0.1)
xmax   <- min(ticks[ticks >= max(all_ci, na.rm = TRUE)], 100)
xticks <- ticks[ticks >= xmin & ticks <= xmax]

tm <- forest_theme(
  base_size = 9, base_family = "sans",
  ci_pch = 15, ci_col = "black", ci_lwd = 1.2, ci_Theight = 0.15,
  refline_gp = gpar(lty = "dashed", col = "grey45", lwd = 0.8),
  xaxis_gp = gpar(fontsize = 8),
  core = list(bg_params = list(fill = "white")),
  colhead = list(fg_params = list(hjust = 0.5, fontface = "bold"))
)
p <- forest(
  plot_df,
  est = list(fp$z_or, fp$s_or), lower = list(fp$z_lo, fp$s_lo), upper = list(fp$z_hi, fp$s_hi),
  ci_column = c(2, 4), ref_line = 1, x_trans = "log",
  xlim = c(xmin, xmax), ticks_at = xticks,
  xlab = rep(L("Adjusted odds ratio (log scale)", "Razón de momios ajustada (escala log)"), 2),
  arrow_lab = L(c("Lower rating", "Higher rating"), c("Menor calificación", "Mayor calificación")),
  theme = tm
)
p <- edit_plot(p, row = which(rows$type %in% c("header", "single")), col = 1, gp = gpar(fontface = "bold"))
p <- edit_plot(p, col = c(1, 3, 5), part = "header", which = "text",
               hjust = unit(0, "npc"), x = unit(0.02, "npc"))

wh <- get_wh(plot = p, unit = "in")
png(file.path(fig_png_dir, fig_file("Figure_1_Forest_Plot.png")),
    width = wh[1] + 0.2, height = wh[2] + 0.2, units = "in", res = 600, bg = "white")
grid.newpage(); grid.draw(p)
invisible(dev.off())

fig1_legend <- write_legend("Figure_1_Legend.md",
  L("Figure 1. Adjusted Associations With Attending-Rated Intraoperative Autonomy and Performance",
    "Figura 1. Asociaciones ajustadas con la autonomía y el desempeño intraoperatorios calificados por el adscrito"),
  sprintf(L("Forest plot of adjusted odds ratios (aORs) and 95%% CIs from mixed-effects ordinal regression models of the Zwisch autonomy level (left) and SIMPL performance rating (right), based on %d attending evaluations of %d general surgery residents. Models included postgraduate year (per 1-year increase), case complexity, procedure, surgical approach, evaluating attending, and time since study start, with a random intercept for resident. Values >1 indicate higher odds of a higher rating. Squares mark adjusted odds ratios and horizontal lines their 95%% CIs; the dashed line marks an odds ratio of 1; the x-axis is on a logarithmic scale.",
            "Gráfico de bosque de las razones de momios ajustadas (RMa) e IC 95%% de los modelos de regresión ordinal de efectos mixtos del nivel de autonomía de Zwisch (izquierda) y de la calificación de desempeño SIMPL (derecha), con base en %d evaluaciones del adscrito de %d residentes de Cirugía General. Los modelos incluyeron el año de residencia (por cada año adicional), la complejidad del caso, el procedimiento, el abordaje quirúrgico, el adscrito evaluador y el tiempo desde el inicio del estudio, con un intercepto aleatorio por residente. Los valores >1 indican mayor probabilidad de una calificación más alta. Los cuadros marcan las razones de momios ajustadas y las líneas horizontales sus IC 95%%; la línea discontinua marca una razón de momios de 1; el eje x está en escala logarítmica."),
          n_obs, n_res))

# Numbers behind Figure 1 (for interpretation)
res_row <- function(t, term) {
  if (is.na(term)) return(c("", ""))
  x <- t[t$term == term, ]
  c(fmt_or(x$or, x$lo, x$hi), fmt_p(x$p))
}
f1 <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
  R <- rows[i, ]
  if (R$type == "header") return(c(paste0("**", R$label, "**"), "", "", "", ""))
  if (R$type == "ref")    return(c(paste0("&emsp;", R$label, L(" (reference)", " (referencia)")), "1.00", "", "1.00", ""))
  c(if (R$type == "single") R$label else paste0("&emsp;", R$label), res_row(tz, R$term), res_row(ts, R$term))
}))
f1 <- as.data.frame(f1, stringsAsFactors = FALSE)
names(f1) <- c(L("Term", "Término"), paste("Zwisch", AOR_CI), "Zwisch P", paste("SIMPL", AOR_CI), "SIMPL P")
rz <- re_stats(m_z); rs <- re_stats(m_s)
re_tab <- data.frame(c("Zwisch", "SIMPL"), c(num(rz["var"]), num(rs["var"])), c(num(rz["icc"]), num(rs["icc"])))
names(re_tab) <- c(L("Model", "Modelo"), L("Intercept variance", "Varianza del intercepto"), L("ICC", "CCI"))
write_fig_results("1", L("Forest plot of adjusted odds ratios", "Gráfico de bosque de las razones de momios ajustadas"), fig1_legend, list(
  list(heading = L("Model", "Modelo"), text = sprintf(
    L("Mixed-effects cumulative logit models; %d attending evaluations, %d residents; random intercept per resident. aOR >1 = higher odds of a higher rating. Axis range plotted: %s to %s (log scale).",
      "Modelos de efectos mixtos de logit acumulado; %d evaluaciones del adscrito, %d residentes; intercepto aleatorio por residente. RMa >1 = mayor probabilidad de una calificación más alta. Rango del eje: %s a %s (escala log)."),
    n_obs, n_res, num(xmin), num(xmax))),
  list(heading = L("Adjusted odds ratios", "Razones de momios ajustadas"), data = f1),
  list(heading = L("Random effect", "Efecto aleatorio"), data = re_tab)
))

rebuild_all_md()
save_session_info()
message(L("Table 4, Table S1 and Figure 1 written to:\n  ", "Tabla 4, Tabla S1 y Figura 1 guardadas en:\n  "), tab_dir, "\n  ", fig_dir)
