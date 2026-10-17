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
r <- function(label, term, type) data.frame(label = label, term = term, type = type, stringsAsFactors = FALSE)
layout_rows <- function(pgy_block) rbind(
  pgy_block,
  r("Case complexity (attending-rated)", NA, "header"),
  r("Easy", NA, "ref"), r("Moderate", "ComplexityModerate", "level"), r("Difficult", "ComplexityDifficult", "level"),
  r("Procedure", NA, "header"),
  r("Appendectomy", NA, "ref"), r("Cholecystectomy", "Procedure_fCholecystectomy", "level"),
  r("Inguinal hernia repair", "Procedure_fInguinal hernia repair", "level"),
  r("Surgical approach", NA, "header"),
  r("Open", NA, "ref"), r("Laparoscopic", "ApproachLaparoscopic", "level"),
  r("Evaluating attending", NA, "header"),
  r("Attending 1", NA, "ref"), r("Attending 2", "AttendingAttending 2", "level"),
  r("Attending 3", "AttendingAttending 3", "level"),
  r("Time since study start, per month", "Months", "single")
)
pgy_cont <- r("Postgraduate year, per 1-year increase", "PGY_num", "single")
pgy_cat  <- rbind(r("Postgraduate year", NA, "header"), r("PGY-1", NA, "ref"),
                  r(paste0("PGY-", 2:5), paste0("PGY_catPGY-", 2:5), "level"))

build_or_table <- function(rows, mz, ms) {
  tz <- tidy_or(mz); ts <- tidy_or(ms)
  cell <- function(t, R) {
    if (R$type == "header") return(c("", ""))
    if (R$type == "ref")    return(c("1.00 [Reference]", ""))
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
    c(bold("Random effect (resident)"), "", "", "", ""),
    c(indent("Intercept variance"), num(rz["var"]), "", num(rs["var"]), ""),
    c(indent("Intraclass correlation coefficient"), num(rz["icc"]), "", num(rs["icc"]), "")
  ))
  tab <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(tab) <- c("Characteristic", "z_or", "z_p", "s_or", "s_p")
  tab
}

hdr_md   <- c("Characteristic", "Zwisch autonomy, aOR (95% CI)", "P value",
              "SIMPL performance, aOR (95% CI)", "P value")
hdr_html <- c("Characteristic", "aOR (95% CI)", "P value", "aOR (95% CI)", "P value")
span4    <- list(list("", 1), list("Zwisch autonomy", 2), list("SIMPL performance", 2))

model_notes <- function(extra = NULL) c(
  sprintf("Mixed-effects cumulative logit (proportional odds) models fitted to %d attending evaluations of %d residents, with a random intercept for resident (adaptive Gauss–Hermite quadrature, 10 nodes). All variables shown were entered simultaneously.", n_obs, n_res),
  "An aOR >1 indicates higher odds of a higher Zwisch autonomy level or SIMPL performance rating. P values are from Wald tests.",
  extra,
  "Intraclass correlation coefficient computed on the latent scale as σ²<sub>resident</sub> / (σ²<sub>resident</sub> + π²/3).",
  abbr("aOR", "CI", "PGY")
)

# ---- Table 4 and Table S1 ---------------------------------------------------
write_table(
  build_or_table(layout_rows(pgy_cont), m_z, m_s), "4",
  "Factors Associated With Attending-Rated Intraoperative Autonomy and Performance",
  "Adjusted odds ratios from mixed-effects ordinal regression models of Zwisch autonomy level and SIMPL performance rating, accounting for repeated evaluations of the same resident.",
  model_notes("Postgraduate year was modeled as a continuous variable (per 1-year increase); a categorical specification is shown in Supplementary Table S1. Case complexity reflects the attending's rating."),
  hdr_md, hdr_html, span4
)
write_table(
  build_or_table(layout_rows(pgy_cat), m_zc, m_sc), "S1",
  "Sensitivity Analysis With Postgraduate Year Modeled as a Categorical Variable",
  "Adjusted odds ratios from the same mixed-effects ordinal models as Table 4, with each postgraduate year compared with PGY-1.",
  model_notes("Very large odds ratios and wide confidence intervals for later postgraduate years reflect near-complete separation (almost no PGY-1 resident reached the highest categories and almost all PGY-5 residents did); the per-year specification in Table 4 is therefore the primary analysis."),
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
txt_or <- function(or, lo, hi, type) ifelse(type == "ref", "Reference",
                                     ifelse(is.na(or), "", sprintf("%.2f (%.2f, %.2f)", or, lo, hi)))
fp$`Zwisch autonomy\naOR (95% CI)`   <- txt_or(fp$z_or, fp$z_lo, fp$z_hi, rows$type)
fp$`SIMPL performance\naOR (95% CI)` <- txt_or(fp$s_or, fp$s_lo, fp$s_hi, rows$type)
fp$` `  <- strrep(" ", 48)   # plotting column, Zwisch
fp$`  ` <- strrep(" ", 48)   # plotting column, SIMPL
plot_df <- fp[, c("Characteristic", " ", "Zwisch autonomy\naOR (95% CI)", "  ", "SIMPL performance\naOR (95% CI)")]

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
  xlab = c("Adjusted odds ratio (log scale)", "Adjusted odds ratio (log scale)"),
  arrow_lab = c("Lower rating", "Higher rating"),
  theme = tm
)
p <- edit_plot(p, row = which(rows$type %in% c("header", "single")), col = 1, gp = gpar(fontface = "bold"))
p <- edit_plot(p, col = c(1, 3, 5), part = "header", which = "text",
               hjust = unit(0, "npc"), x = unit(0.02, "npc"))

wh <- get_wh(plot = p, unit = "in")
png(file.path(fig_png_dir, "Figure_1_Forest_Plot.png"),
    width = wh[1] + 0.2, height = wh[2] + 0.2, units = "in", res = 600, bg = "white")
grid.newpage(); grid.draw(p)
invisible(dev.off())

fig1_legend <- sprintf("Forest plot of adjusted odds ratios (aORs) and 95%% CIs from mixed-effects ordinal regression models of the Zwisch autonomy level (left) and SIMPL performance rating (right), based on %d attending evaluations of %d general surgery residents. Models included postgraduate year (per 1-year increase), case complexity, procedure, surgical approach, evaluating attending, and time since study start, with a random intercept for resident. Values >1 indicate higher odds of a higher rating. Squares mark adjusted odds ratios and horizontal lines their 95%% CIs; the dashed line marks an odds ratio of 1; the x-axis is on a logarithmic scale.", n_obs, n_res)
writeLines(c("**Figure 1. Adjusted Associations With Attending-Rated Intraoperative Autonomy and Performance**",
             "", fig1_legend), file.path(fig_legend_dir, "Figure_1_Legend.md"), useBytes = TRUE)

# Numbers behind Figure 1 (for interpretation)
res_row <- function(t, term) {
  if (is.na(term)) return(c("", ""))
  x <- t[t$term == term, ]
  c(fmt_or(x$or, x$lo, x$hi), fmt_p(x$p))
}
f1 <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
  R <- rows[i, ]
  if (R$type == "header") return(c(paste0("**", R$label, "**"), "", "", "", ""))
  if (R$type == "ref")    return(c(paste0("&emsp;", R$label, " (reference)"), "1.00", "", "1.00", ""))
  c(if (R$type == "single") R$label else paste0("&emsp;", R$label), res_row(tz, R$term), res_row(ts, R$term))
}))
f1 <- as.data.frame(f1, stringsAsFactors = FALSE)
names(f1) <- c("Term", "Zwisch aOR (95% CI)", "Zwisch P", "SIMPL aOR (95% CI)", "SIMPL P")
rz <- re_stats(m_z); rs <- re_stats(m_s)
write_fig_results("1", "Forest plot of adjusted odds ratios", fig1_legend, list(
  list(heading = "Model", text = sprintf(
    "Mixed-effects cumulative logit models; %d attending evaluations, %d residents; random intercept per resident. aOR >1 = higher odds of a higher rating. Axis range plotted: %s to %s (log scale).",
    n_obs, n_res, num(xmin), num(xmax))),
  list(heading = "Adjusted odds ratios", data = f1),
  list(heading = "Random effect", data = data.frame(
    Model = c("Zwisch", "SIMPL"), `Intercept variance` = c(num(rz["var"]), num(rs["var"])),
    ICC = c(num(rz["icc"]), num(rs["icc"])), check.names = FALSE))
))

rebuild_all_md()
save_session_info()
message("Table 4, Table S1 and Figure 1 written to:\n  ", tab_dir, "\n  ", fig_dir)
