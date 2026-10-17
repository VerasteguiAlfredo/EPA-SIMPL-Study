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
zw_ax <- c("Show and tell", "Active help", "Passive help", "Supervision only")
sm_ax <- c("Unprepared", "Inexperienced", "Intermediate", "Practice-ready", "Exceptional")

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
rho_txt <- sprintf("Spearman ρ = %s (95%% CI %s–%s)", num(rho), num(rho_ci[1]), num(rho_ci[2]))
tau_txt <- sprintf("Kendall τ-b = %s (95%% CI %s–%s)", num(tau), num(tau_ci[1]), num(tau_ci[2]))

hm <- as.data.frame(table(Zwisch = factor(att$Zwisch_Autonomy, 1:4), SIMPL = factor(att$SIMPL_Performance, 1:5))) %>%
  mutate(Zwisch = as.integer(Zwisch), SIMPL = as.integer(SIMPL),
         ink = ifelse(Freq >= max(Freq) * 0.55, "white", INK), lab = ifelse(Freq > 0, Freq, ""))
pS1 <- ggplot(hm, aes(SIMPL, Zwisch)) +
  geom_tile(aes(fill = Freq), colour = "white", linewidth = 0.8) +
  geom_text(aes(label = lab, colour = ink), size = 3) +
  scale_fill_gradientn(colours = HEAT, limits = c(0, max(hm$Freq)), name = "Evaluations, n",
                       guide = guide_colourbar(barheight = unit(70, "pt"), barwidth = unit(7, "pt"))) +
  scale_colour_identity() +
  scale_x_continuous(breaks = 1:5, labels = sm_ax, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:4, labels = zw_ax, expand = c(0, 0)) +
  coord_equal() +
  labs(x = "SIMPL performance rating", y = "Zwisch autonomy level",
       title = "Zwisch autonomy vs SIMPL performance", subtitle = paste0(rho_txt, "\n", tau_txt)) +
  theme_pub() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1),
        plot.subtitle = element_text(size = 8, colour = INK2))
save_fig(pS1, "Figure_S1_Zwisch_vs_SIMPL.png", 5.6, 4.2)
legS1 <- write_legend("Figure_S1_Legend.md", "Supplementary Figure S1. Relationship Between Zwisch Autonomy and SIMPL Performance Ratings",
  sprintf("Cross-classification of Zwisch autonomy level and SIMPL performance rating assigned by the attending in the same evaluation (%d attending evaluations, %d residents); cell values are numbers of evaluations. Correlations are Spearman ρ and Kendall τ-b, with 95%% CIs from a cluster bootstrap resampling residents (%s replicates).",
          n_att, n_res, format(B_BOOT, big.mark = ",")))
xt <- as.data.frame.matrix(table(factor(att$Zwisch_Autonomy, 1:4, labels = zw_ax), factor(att$SIMPL_Performance, 1:5, labels = sm_ax)))
xt <- data.frame(`Zwisch \\ SIMPL` = rownames(xt), xt, check.names = FALSE, row.names = NULL)
write_fig_results("S1", "Zwisch × SIMPL", legS1, list(
  list(heading = "Correlation", text = paste0(rho_txt, ". ", tau_txt, ".")),
  list(heading = "Cross-classification (rows = Zwisch, columns = SIMPL; counts)", data = xt),
  list(heading = "Mean SIMPL score by Zwisch level", data = att %>% group_by(Zwisch) %>%
         summarise(n = n(), `SIMPL median (IQR)` = fmt_mediqr(SIMPL_Performance),
                   `SIMPL mean (SD)` = fmt_meansd(SIMPL_Performance)) %>% as.data.frame())
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
    labs(x = NULL, y = "Attending evaluations", title = title) +
    theme_pub() +
    theme(legend.position = "bottom", legend.direction = "vertical",
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3))
}
zw_short <- c("1 Show and tell", "2 Active help", "3 Passive help", "4 Supervision only")
sm_short <- c("1 Unprepared/critical deficiency", "2 Inexperienced", "3 Intermediate", "4 Practice-ready", "5 Exceptional")
pS2 <- (stack_by_att("Zwisch_Autonomy", zw_short, RAMP4, "Zwisch autonomy level", "Zwisch level") |
        stack_by_att("SIMPL_Performance", sm_short, RAMP5, "SIMPL performance rating", "SIMPL rating")) +
  plot_annotation(tag_levels = "A")
save_fig(pS2, "Figure_S2_Ratings_by_Attending.png", 7.5, 4.6)
legS2 <- write_legend("Figure_S2_Legend.md", "Supplementary Figure S2. Rating Distributions by Evaluating Attending",
  sprintf("Distribution of (A) Zwisch autonomy levels and (B) SIMPL performance ratings assigned by each of the %d evaluating attendings (%d attending evaluations). Distributions are unadjusted and partly reflect the postgraduate year of the residents each attending evaluated (Supplementary Table S3); attending-adjusted estimates are shown in Table 4.",
          nlevels(att$Attending), n_att))
by_att <- function(var, labels) {
  w <- t(sapply(levels(att$Attending), function(a) {
    x <- att[[var]][att$Attending == a]
    c(N = length(x), vapply(seq_along(labels), function(k) sprintf("%d (%s%%)", sum(x == k), num(100 * mean(x == k), 1)), ""),
      `Median (IQR)` = fmt_mediqr(x))
  }))
  out <- data.frame(Attending = rownames(w), w, check.names = FALSE, row.names = NULL)
  names(out) <- c("Attending", "N", labels, "Median (IQR)")
  out
}
pgy_mix <- as.data.frame.matrix(table(att$Attending, att$PGY))
pgy_mix <- data.frame(Attending = rownames(pgy_mix), pgy_mix, check.names = FALSE, row.names = NULL)
write_fig_results("S2", "Ratings by attending", legS2, list(
  list(heading = "A. Zwisch by attending, n (row %)", data = by_att("Zwisch_Autonomy", zw_short)),
  list(heading = "B. SIMPL by attending, n (row %)", data = by_att("SIMPL_Performance", sm_short)),
  list(heading = "Case mix: PGY of residents evaluated, n", data = pgy_mix)
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
cat_df <- rbind(re_df("Zwisch_o", "Zwisch autonomy"), re_df("SIMPL_o", "SIMPL performance"))
cat_panel <- function(df, title) {
  df <- df %>% arrange(est) %>% mutate(rank = row_number())
  ggplot(df, aes(est, rank, colour = PGY)) +
    geom_vline(xintercept = 0, colour = INK2, linewidth = 0.4, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.5) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = setNames(RAMP5, levels(att$PGY)), name = "Postgraduate year", drop = FALSE) +
    scale_y_continuous(breaks = df$rank, labels = df$Resident_ID, expand = expansion(add = 0.8)) +
    labs(x = "Resident random intercept (log-odds)", y = NULL, title = title) +
    theme_pub() +
    theme(axis.text.y = element_text(size = 6.5), panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3))
}
pS3 <- (cat_panel(filter(cat_df, Model == "Zwisch autonomy"), "Zwisch autonomy") |
        cat_panel(filter(cat_df, Model == "SIMPL performance"), "SIMPL performance")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")
save_fig(pS3, "Figure_S3_Resident_Variation.png", 7.5, 6.4)
legS3 <- write_legend("Figure_S3_Legend.md", "Supplementary Figure S3. Resident-Level Variation in Autonomy and Performance Ratings",
  sprintf("Caterpillar plots of resident-specific random intercepts (conditional modes, log-odds scale) with 95%% intervals from the mixed-effects ordinal models of Table 4 for (A) Zwisch autonomy and (B) SIMPL performance (%d attending evaluations, %d residents). Values above 0 indicate residents rated higher than expected for their postgraduate year, case complexity, procedure, approach, attending, and timing; residents are ordered by their estimate and colored by postgraduate year.",
          n_att, n_res))
cat_tab <- cat_df %>% mutate(cell = sprintf("%s (%s to %s)", num(est), num(lo), num(hi))) %>%
  dplyr::select(Resident_ID, PGY, n, Model, cell) %>%
  tidyr::pivot_wider(names_from = Model, values_from = cell) %>% arrange(PGY, Resident_ID) %>% as.data.frame()
names(cat_tab)[1:3] <- c("Resident", "PGY", "Attending forms")
sum_tab <- cat_df %>% group_by(Model) %>%
  summarise(`Random-intercept SD` = num(first(sd_re)),
            `Residents with interval above 0` = sum(lo > 0), `Residents with interval below 0` = sum(hi < 0),
            `Range of estimates` = sprintf("%s to %s", num(min(est)), num(max(est)))) %>% as.data.frame()
write_fig_results("S3", "Resident-level variation", legS3, list(
  list(heading = "Summary", data = sum_tab),
  list(heading = "Resident random intercepts, conditional mode (95% interval), log-odds", data = cat_tab)
))

rebuild_all_md()
save_session_info()
message("Supplementary Figures S1–S3 written to: ", fig_dir)
