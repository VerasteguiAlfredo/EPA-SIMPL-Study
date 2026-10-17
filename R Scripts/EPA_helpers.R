# =============================================================================
# EPA / SIMPL study — shared helpers
#   Sourced by EPA_Tables_1-3.R, EPA_Table4_Figure1.R and
#   EPA_Tables_5-6_Supplementary.R. Keep all four scripts (and run_all.R) in
#   the same folder.
#
#   Contents: language switch, project paths, packages, data loading/labelling,
#   number formatting, statistical tests, descriptive table builder, MD/HTML
#   table writer, combined Markdown rebuild, session info.
#
#   Language: English by default (outputs in Results/). Set the environment
#   variable EPA_LANG = "es" (run_all_es.R does this) to write every table,
#   figure, legend and results file in Spanish to Resultados/Tablas and
#   Resultados/Figuras. The analysis itself is identical in both languages.
# =============================================================================

EPA_HELPERS_LOADED <- TRUE

# ---- Language ---------------------------------------------------------------
EPA_LANG <- tolower(Sys.getenv("EPA_LANG", "en"))
IS_ES    <- EPA_LANG == "es"
L <- function(en, es) if (IS_ES) es else en        # pick the text for the current language

MESES     <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
               "agosto", "septiembre", "octubre", "noviembre", "diciembre")
MESES_ABR <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
month_name <- function(date) if (IS_ES) MESES[as.integer(format(date, "%m"))] else format(date, "%B")
month_abb  <- function(date) if (IS_ES) MESES_ABR[as.integer(format(date, "%m"))] else format(date, "%b")
cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
# "April–August 2026" / "abril–agosto de 2026"
period_text <- function(dates) {
  a <- min(dates); b <- max(dates)
  same_year <- format(a, "%Y") == format(b, "%Y")
  if (IS_ES) {
    if (same_year) sprintf("%s–%s de %s", month_name(a), month_name(b), format(b, "%Y"))
    else sprintf("%s de %s–%s de %s", month_name(a), format(a, "%Y"), month_name(b), format(b, "%Y"))
  } else {
    if (same_year) paste0(month_name(a), "–", month_name(b), " ", format(b, "%Y"))
    else paste0(month_name(a), " ", format(a, "%Y"), "–", month_name(b), " ", format(b, "%Y"))
  }
}

# ---- Project paths (edit here only) -----------------------------------------
project_dir <- Sys.getenv(
  "EPA_PROJECT_DIR",
  "C:/Users/m320532.MFAD/Desktop/Research/VS Code Workbook/surg_training_evaluation"
)
data_path <- file.path(project_dir, "Dataset", "EPA_SIMPL_Database.xlsx")
res_dir   <- file.path(project_dir, L("Results", "Resultados"))
tab_dir   <- file.path(res_dir, L("Tables", "Tablas"))
fig_dir   <- file.path(res_dir, L("Figures", "Figuras"))
md_dir    <- file.path(tab_dir, "MD")
html_dir  <- file.path(tab_dir, "HTML")
fig_png_dir    <- file.path(fig_dir, "PNG")                          # figure images (+ flowchart .mmd source)
fig_legend_dir <- file.path(fig_dir, L("Legends", "Leyendas"))      # figure legends (.md)
fig_res_dir    <- file.path(fig_dir, L("Results", "Resultados"))    # numbers behind each figure (.md, .csv)
TAB  <- L("Table", "Tabla")                 # word used in titles and file names
FIG  <- L("Figure", "Figura")
SFIG <- L("Supplementary Figure", "Figura suplementaria")
STAB <- L("Supplementary Table", "Tabla suplementaria")
# Output file names: English names are kept as written in the scripts;
# in Spanish "Figure_2_Ratings_by_PGY.png" becomes "Figura_2.png",
# "Figure_2_Legend.md" -> "Figura_2_Leyenda.md", other files keep their suffix.
fig_file <- function(name) {
  if (!IS_ES) return(name)
  if (grepl("_Legend\\.md$", name))  return(sub("^Figure_(.*)_Legend\\.md$", "Figura_\\1_Leyenda.md", name))
  if (grepl("_Results\\.md$", name)) return(sub("^Figure_(.*)_Results\\.md$", "Figura_\\1_Resultados.md", name))
  if (grepl("^Figure_S?[0-9]+_.*\\.png$", name)) return(sub("^Figure_(S?[0-9]+)_.*\\.png$", "Figura_\\1.png", name))
  sub("^Figure_", "Figura_", name)
}
for (p in c(md_dir, html_dir, fig_png_dir, fig_legend_dir, fig_res_dir))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)

# ---- Packages ---------------------------------------------------------------
load_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) install.packages(missing_pkgs)
  suppressPackageStartupMessages(for (p in pkgs) library(p, character.only = TRUE))
}
load_packages(c("readxl", "dplyr", "tidyr", "knitr"))

# ---- Data -------------------------------------------------------------------
zw_labels <- L(c("Show and tell", "Active help", "Passive help", "Supervision only"),
               c("Demuestra y explica", "Ayuda activa", "Ayuda pasiva", "Solo supervisión"))
sm_labels <- L(c("Unprepared/critical deficiency", "Inexperienced with procedure",
                 "Intermediate performance", "Practice-ready performance", "Exceptional performance"),
               c("No preparado/deficiencia crítica", "Falta de experiencia con el procedimiento",
                 "Desempeño intermedio", "Listo para la práctica", "Desempeño excepcional"))
# Short labels for figure axes and legends
zw_ax <- L(c("Show and tell", "Active help", "Passive help", "Supervision only"),
           c("Demuestra y explica", "Ayuda activa", "Ayuda pasiva", "Solo supervisión"))
sm_ax <- L(c("Unprepared", "Inexperienced", "Intermediate", "Practice-ready", "Exceptional"),
           c("No preparado", "Falta de experiencia", "Intermedio", "Listo para la práctica", "Excepcional"))
zw_short <- paste(1:4, zw_ax)
sm_short <- paste(1:5, c(L("Unprepared/critical deficiency", "No preparado/deficiencia crítica"), sm_ax[-1]))
PGY_LAB  <- L(paste0("PGY-", 1:5), paste0("R", 1:5))   # postgraduate year labels (R1–R5 in Spanish)
ATT_LAB  <- L(paste0("Attending ", 1:3), paste0("Adscrito ", 1:3))
CX_LAB   <- L(c("Easy", "Moderate", "Difficult"), c("Fácil", "Moderada", "Difícil"))
PROC_LAB <- L(c("Appendectomy", "Cholecystectomy", "Inguinal hernia repair"),
              c("Apendicectomía", "Colecistectomía", "Hernioplastía inguinal"))
APP_LAB  <- L(c("Open", "Laparoscopic"), c("Abierto", "Laparoscópico"))

load_epa <- function(path = data_path) {
  read_excel(path, sheet = "Data") %>%
    mutate(
      Resident_ID = trimws(Resident_ID),
      Eval_Date   = as.Date(Eval_Date),
      PGY         = factor(PGY_Year, levels = 1:5, labels = PGY_LAB),
      Sex         = factor(Resident_Sex, levels = c(0, 1), labels = L(c("Male", "Female"), c("Masculino", "Femenino"))),
      Attending   = factor(Evaluator_ID, levels = 1:3, labels = ATT_LAB),
      Procedure_f = factor(Procedure, levels = 1:3, labels = PROC_LAB),
      Approach    = factor(Surgical_Approach, levels = 1:2, labels = APP_LAB),
      Complexity  = factor(Case_Complexity, levels = 1:3, labels = CX_LAB),
      Zwisch      = factor(Zwisch_Autonomy, levels = 1:4, labels = zw_labels),
      SIMPL       = factor(SIMPL_Performance, levels = 1:5, labels = sm_labels)
    )
}

# Cases flagged during data verification (same rules as the Data Checks sheet)
flag_cases <- function(d) {
  d %>%
    group_by(Case_ID) %>%
    summarise(
      two_attending  = sum(Evaluator_Type == 1) > 1,
      mixed_resident = n_distinct(Resident_ID) > 1,
      mismatch       = n() > 1 & (n_distinct(Procedure) > 1 | n_distinct(Surgical_Approach) > 1 |
                                    n_distinct(Eval_Date) > 1),
      coding_error   = any((Evaluator_Type == 2 & Evaluator_ID != 0) | (Evaluator_Type == 1 & Evaluator_ID == 0)),
      .groups = "drop"
    ) %>%
    mutate(flagged = two_attending | mixed_resident | mismatch | coding_error)
}

# ---- Number formatting ------------------------------------------------------
MINUS <- "\u2212"   # typographic minus sign
num <- function(x, digits = 2) {
  s <- formatC(x, format = "f", digits = digits, big.mark = ",")
  s <- sub(paste0("^-(0\\.", strrep("0", digits), ")$"), "\\1", s)   # "-0.00" -> "0.00"
  sub("^-", MINUS, s)
}
fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) return("<.001")
  sub("^0", "", sprintf("%.3f", p))
}
fmt_npct   <- function(n, N) if (N == 0) "—" else sprintf("%d (%s)", as.integer(n), num(100 * n / N))
fmt_mediqr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  q <- quantile(x, c(.25, .5, .75), names = FALSE, type = 7)
  sprintf("%s (%s–%s)", num(q[2]), num(q[1]), num(q[3]))
}
fmt_meansd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return("—")
  sprintf("%s (%s)", num(mean(x)), num(sd(x)))
}
fmt_ci <- function(est, lo, hi) {          # "to" when the interval includes negatives
  sep <- ifelse(!is.na(lo) & lo < 0, L(" to ", " a "), "–")   # vectorized
  sprintf("%s (%s%s%s)", num(est), num(lo), sep, num(hi))
}
fmt_or <- function(or, lo, hi) fmt_ci(or, lo, hi)

indent <- function(x) paste0("&emsp;", x)
bold   <- function(x) paste0("<b>", x, "</b>")
sup    <- function(x) paste0("<sup>", x, "</sup>")
with_br <- function(h) sub(" \\(n = ", "<br>(n = ", h)

abbr <- function(...) {
  all <- if (IS_ES)
    c(aOR = "RMa, razón de momios ajustada", CI = "IC, intervalo de confianza", IQR = "RIC, rango intercuartílico",
      NE = "NE, no estimable", PGY = "R1–R5, residente de primer a quinto año", SD = "DE, desviación estándar")
  else
    c(aOR = "aOR, adjusted odds ratio", CI = "CI, confidence interval", IQR = "IQR, interquartile range",
      NE = "NE, not estimable", PGY = "PGY, postgraduate year", SD = "SD, standard deviation")
  paste0(L("Abbreviations: ", "Abreviaturas: "), paste(all[c(...)], collapse = "; "), ".")
}
REF_CELL <- L("1.00 [Reference]", "1.00 [Referencia]")
CHAR <- L("Characteristic", "Característica")
PVAL <- L("P value", "Valor de P")
OVERALL <- L("Overall", "Total")
MEDIQR <- L("median (IQR)", "mediana (RIC)")
AOR_CI <- L("aOR (95% CI)", "RMa (IC 95%)")

# ---- Statistical tests (markers resolved to letters when a table is written) --
SYM  <- c(kw = "{kw}", mwu = "{mwu}", chi = "{chi}", fisher = "{fisher}")
mark <- function(p, test) if (is.na(p)) "" else paste0(fmt_p(p), sup(SYM[test]))
test_text <- if (IS_ES) {
  c(kw = "prueba de Kruskal-Wallis", mwu = "prueba U de Mann-Whitney", chi = "prueba de χ²",
    fisher = "prueba exacta de Fisher, usada cuando alguna frecuencia esperada fue <5 (valor de P por Monte Carlo, 100,000 réplicas, en tablas mayores de 2×2)")
} else {
  c(kw = "Kruskal–Wallis test", mwu = "Mann–Whitney U test", chi = "χ² test",
    fisher = "Fisher exact test, used when any expected cell count was <5 (Monte Carlo P value, 100,000 replicates, for tables larger than 2×2)")
}

test_cat <- function(x, g) {
  tab <- table(x, g)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(list(p = NA, test = NA))
  expct <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
  if (any(expct < 5)) {
    p <- if (all(dim(tab) == 2)) fisher.test(tab)$p.value
         else fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value
    list(p = p, test = "fisher")
  } else list(p = chisq.test(tab, correct = FALSE)$p.value, test = "chi")
}
test_cont <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  g <- droplevels(factor(g[ok])); x <- x[ok]
  if (nlevels(g) < 2 || length(unique(x)) < 2) return(list(p = NA, test = NA))
  if (nlevels(g) == 2) list(p = wilcox.test(x ~ g, exact = FALSE)$p.value, test = "mwu")
  else                 list(p = kruskal.test(x ~ g)$p.value, test = "kw")
}

# ---- Descriptive table builder ----------------------------------------------
# specs: list of list(type = "cont" | "cat" | "bin", var, label, level = <for bin>)
build_table <- function(df, group, specs, overall = TRUE) {
  g <- df[[group]]; groups <- levels(g)
  cols <- c(if (overall) OVERALL, groups)
  subsets <- c(if (overall) list(df), lapply(groups, function(l) df[g == l, , drop = FALSE]))
  rows <- list()
  add <- function(label, vals, p = "") rows[[length(rows) + 1]] <<- c(label, vals, p)
  for (s in specs) {
    x <- df[[s$var]]
    if (s$type == "cont") {
      t <- test_cont(x, g)
      add(paste0(s$label, ", ", MEDIQR),
          vapply(subsets, function(sd) fmt_mediqr(sd[[s$var]]), ""), mark(t$p, t$test))
    } else if (s$type == "bin") {
      t <- test_cat(x, g)
      add(paste0(s$label, ", n (%)"), vapply(subsets, function(sd) {
        v <- sd[[s$var]]; fmt_npct(sum(v == s$level, na.rm = TRUE), sum(!is.na(v)))
      }, ""), mark(t$p, t$test))
    } else {
      t <- test_cat(x, g)
      add(bold(paste0(s$label, ", n (%)")), rep("", length(cols)), mark(t$p, t$test))
      for (lv in levels(x)) add(indent(lv), vapply(subsets, function(sd) {
        v <- sd[[s$var]]; fmt_npct(sum(v == lv, na.rm = TRUE), sum(!is.na(v)))
      }, ""))
      n_miss <- vapply(subsets, function(sd) sum(is.na(sd[[s$var]])), 0)
      if (any(n_miss > 0)) add(indent(L("Missing", "Sin dato")), as.character(n_miss))
    }
  }
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(tab) <- make.unique(c(CHAR, cols, PVAL))
  ns <- vapply(subsets, nrow, 0)
  list(table = tab, header = c(CHAR, paste0(cols, " (n = ", ns, ")"), PVAL))
}

# ---- MD + HTML writer -------------------------------------------------------
html_css <- "
body { font-family: Arial, Helvetica, sans-serif; color: #000; margin: 32px; }
.tbl-wrap { display: inline-block; max-width: 100%; }
.tbl-title { font-size: 11pt; font-weight: bold; margin: 0 0 4px 0; }
.tbl-caption { font-size: 10pt; margin: 0 0 10px 0; }
.tbl-caption, .tbl-notes { width: 0; min-width: 100%; }
table.pub { border-collapse: collapse; border: none; font-size: 10pt; }
table.pub th, table.pub td { border: none !important; padding: 3px 12px; vertical-align: top; }
table.pub th { font-weight: bold; }
table.pub td:first-child, table.pub th:first-child { text-align: left; padding-left: 2px; }
table.pub thead tr:first-child th { border-top: 1.5px solid #000 !important; }
table.pub thead tr:last-child th  { border-bottom: 1px solid #000 !important; }
table.pub tbody tr:last-child td  { border-bottom: 1.5px solid #000 !important; }
.tbl-notes { font-size: 9pt; margin-top: 6px; line-height: 1.4; }
"

# header_md: Markdown column names; header_html: HTML (second) header row;
# spanners: optional list(list(label, n_cols), ...) for an HTML top header row.
# Test markers in the table are lettered a, b, c... in order of first appearance,
# and a "P values: ..." footnote is appended just before the abbreviations line.
write_table <- function(tab, id, title, caption, notes, header_md,
                        header_html = with_br(header_md), spanners = NULL) {
  all_txt <- unlist(tab)
  ord <- gsub("[{}]", "", unique(unlist(regmatches(all_txt, gregexpr("\\{[a-z]+\\}", all_txt)))))
  if (length(ord)) {
    p_note <- paste0(L("P values: ", "Valores de P: "), paste(sprintf("%s %s", sup(letters[seq_along(ord)]), test_text[ord]),
                                        collapse = "; "), ".")
    is_abbr <- grepl("^(Abbreviations|Abreviaturas):", notes)
    notes <- if (any(is_abbr)) c(notes[!is_abbr], p_note, notes[is_abbr]) else c(notes, p_note)
    for (i in seq_along(ord))
      tab[] <- lapply(tab, function(x) gsub(paste0("{", ord[i], "}"), letters[i], x, fixed = TRUE))
  }
  align <- c("l", rep("c", ncol(tab) - 1))

  md_tab <- kable(tab, format = "pipe", col.names = header_md, align = align, escape = FALSE)
  md_txt <- c(sprintf("**%s %s. %s**", TAB, id, title), "", caption, "",
              md_tab, "", paste0("*", notes, "*", collapse = "  \n"), "")
  writeLines(md_txt, file.path(md_dir, sprintf("%s_%s.md", TAB, id)), useBytes = TRUE)

  html_body <- as.character(kable(tab, format = "html", col.names = header_html, align = align,
                                  escape = FALSE, table.attr = 'class="pub"'))
  if (!is.null(spanners)) {
    span <- paste0("<tr>", paste(vapply(spanners, function(s)
      sprintf('<th colspan="%d" style="text-align:center;">%s</th>', s[[2]], s[[1]]), ""), collapse = ""), "</tr>")
    html_body <- sub("<thead>\\s*<tr>", paste0("<thead>\n", span, "\n<tr>"), html_body)
  }
  html_txt <- c("<!DOCTYPE html>", sprintf("<html lang=\"%s\">", EPA_LANG), "<head>", "<meta charset=\"utf-8\">",
                sprintf("<title>%s %s</title>", TAB, id), "<style>", html_css, "</style>", "</head>",
                "<body>", "<div class=\"tbl-wrap\">",
                sprintf("<p class=\"tbl-title\">%s %s. %s</p>", TAB, id, title),
                sprintf("<p class=\"tbl-caption\">%s</p>", caption), html_body,
                sprintf("<div class=\"tbl-notes\">%s</div>", paste(notes, collapse = "<br>")),
                "</div>", "</body>", "</html>")
  writeLines(html_txt, file.path(html_dir, sprintf("%s_%s.html", TAB, id)), useBytes = TRUE)
  invisible(md_txt)
}

# ---- Combined Markdown: every Table_*.md in the MD folder, main then S -------
rebuild_all_md <- function() {
  md_files <- list.files(md_dir, pattern = paste0("^", TAB, "_.*\\.md$"), full.names = TRUE)
  if (!length(md_files)) return(invisible(NULL))
  key <- sub(paste0("^", TAB, "_(.*)\\.md$"), "\\1", basename(md_files))
  ord <- order(grepl("^S", key), suppressWarnings(as.numeric(sub("^S", "", key))))
  all_md <- c(L("# EPA/SIMPL Study — Tables", "# Estudio EPA/SIMPL — Tablas"), "")
  for (f in md_files[ord]) all_md <- c(all_md, readLines(f, encoding = "UTF-8"), "---", "")
  writeLines(head(all_md, -2), file.path(md_dir, L("All_Tables.md", "Todas_las_Tablas.md")), useBytes = TRUE)
}

# ---- Figure style (shared by all figure scripts) ----------------------------
# Colors validated with the dataviz palette checks: ordinal scales use one blue
# hue light -> dark; two-series panels use blue/orange plus distinct shapes;
# diverging (Likert) uses red <-> blue with a gray neutral midpoint.
# Functions below need ggplot2 attached (each figure script loads it).
INK  <- "#1f1f1f"; INK2 <- "#5c5c5c"; GRID <- "#e6e6e3"
RAMP4 <- c("#86b6ef", "#3987e5", "#1c5cab", "#0d366b")              # Zwisch (ordinal)
RAMP5 <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#0d366b")   # SIMPL  (ordinal)
RAMP3 <- c("#86b6ef", "#2a78d6", "#0d366b")                         # complexity (ordinal)
PAIR  <- c(Zwisch = "#2a78d6", SIMPL = "#eb6834")                   # two series (4B)
HEAT  <- c("#ffffff", "#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
LIKERT <- c("#c93a39", "#ee9a99", "#f0efec", "#86b6ef", "#1c5cab")  # 1..5: red <- gray -> blue
ROLE  <- c(Attending = "#2a78d6", Resident = "#eb6834")               # two series

theme_pub <- function(base = 9) {
  theme_classic(base_size = base, base_family = "sans") +
    theme(
      text = element_text(colour = INK),
      axis.text = element_text(colour = INK2, size = base - 1),
      axis.line = element_line(colour = INK2, linewidth = 0.3),
      axis.ticks = element_line(colour = INK2, linewidth = 0.3),
      plot.title = element_text(face = "bold", size = base + 1, hjust = 0),
      plot.tag = element_text(face = "bold", size = base + 3),
      legend.title = element_text(face = "bold", size = base - 0.5),
      legend.text = element_text(size = base - 1),
      legend.key.size = unit(9, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(6, 8, 6, 6)
    )
}
save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_png_dir, fig_file(name)), p, width = w, height = h, units = "in", dpi = 600, bg = "white")
}
write_legend <- function(name, title, text) {
  writeLines(c(sprintf("**%s**", title), "", text), file.path(fig_legend_dir, fig_file(name)), useBytes = TRUE)
  invisible(text)
}
pct_cell <- function(n, N) sprintf("%d (%s%%)", as.integer(n), num(100 * n / N, 1))
ci_pct   <- function(e, l, h) sprintf("%s%% (%s–%s)", num(100 * e, 1), num(100 * l, 1), num(100 * h, 1))
# white label text on the two darkest steps, dark ink on the lighter ones
label_ink <- function(level_index, n_levels) ifelse(level_index > n_levels / 2, "white", INK)

# ---- Figure results (Markdown) ----------------------------------------------
# Writes Results/Figures/Figure_<id>_Results.md with the numbers behind a
# figure, then rebuilds All_Figure_Results.md. `blocks` is a list of
# list(heading = "...", data = <data.frame> or NULL, text = "..." or NULL).
write_fig_results <- function(id, title, legend, blocks) {
  out <- c(sprintf(L("# Figure %s results — %s", "# Resultados de la Figura %s — %s"), id, title), "",
           sprintf(L("_Generated %s from %s_", "_Generado el %s a partir de %s_"),
                   format(Sys.time(), "%Y-%m-%d %H:%M"), basename(data_path)), "",
           L("**Legend.** ", "**Leyenda.** "), legend, "")
  for (b in blocks) {
    out <- c(out, paste("##", b$heading), "")
    if (!is.null(b$text)) out <- c(out, b$text, "")
    if (!is.null(b$data)) out <- c(out, kable(b$data, format = "pipe", align = "l", escape = FALSE), "")
  }
  writeLines(out, file.path(fig_res_dir, fig_file(sprintf("Figure_%s_Results.md", id))), useBytes = TRUE)
  rebuild_all_fig_md()
}
rebuild_all_fig_md <- function() {
  f <- list.files(fig_res_dir, pattern = L("^Figure_.*_Results\\.md$", "^Figura_.*_Resultados\\.md$"), full.names = TRUE)
  if (!length(f)) return(invisible(NULL))
  key <- sub("^Figur[ea]_(S?[0-9]+)_.*", "\\1", basename(f))      # main figures first, then S1, S2, ...
  f <- f[order(grepl("^S", key), suppressWarnings(as.numeric(sub("^S", "", key))))]
  all <- c(L("# EPA/SIMPL Study — Figure Results", "# Estudio EPA/SIMPL — Resultados de las figuras"), "")
  for (x in f) all <- c(all, readLines(x, encoding = "UTF-8"), "---", "")
  writeLines(head(all, -2), file.path(fig_res_dir, L("All_Figure_Results.md", "Todos_los_Resultados_de_Figuras.md")),
             useBytes = TRUE)
}

# ---- Session info for the methods section -----------------------------------
save_session_info <- function() {
  writeLines(c(sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
               capture.output(sessionInfo())),
             file.path(res_dir, "session_info.txt"))
}
