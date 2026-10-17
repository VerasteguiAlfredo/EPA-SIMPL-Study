# =============================================================================
# EPA / SIMPL study — Study flow diagram (STROBE-style), Mermaid Markdown
#
#   Writes Results/Figures/Figure_Flowchart.md: a Mermaid flowchart whose
#   numbers are computed from the database (forms, cases, residents, pairs,
#   flagged cases), plus a legend and a plain-text table of the counts.
#   Render in VS Code (Markdown Preview Mermaid Support extension), GitHub,
#   or https://mermaid.live, and export as SVG/PNG.
#
# Requires EPA_helpers.R (same folder, one level up, or sibling Tables/Figures).
# =============================================================================

# ---- Numbers not contained in the database (edit here) ----------------------
N_INCOMPLETE <- 41   # forms discarded because they were incomplete

# ---- Load shared helpers ----------------------------------------------------
if (!exists("EPA_HELPERS_LOADED")) {
  script_dir <- local({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    f <- gsub("~+~", " ", f, fixed = TRUE)   # Rscript encodes spaces in the path as ~+~
    if (length(f)) return(dirname(normalizePath(f)))
    for (i in rev(seq_len(sys.nframe()))) if (!is.null(o <- sys.frame(i)$ofile)) return(dirname(normalizePath(o)))
    getwd()
  })
  cand <- c(file.path(script_dir, "EPA_helpers.R"),
            file.path(dirname(script_dir), "EPA_helpers.R"),
            file.path(dirname(script_dir), c("Tables", "Figures"), "EPA_helpers.R"))
  helpers <- cand[file.exists(cand)][1]
  if (is.na(helpers)) stop("EPA_helpers.R not found near ", script_dir)
  source(helpers, encoding = "UTF-8")
}

d <- load_epa()
fmt_n <- function(x) format(x, big.mark = ",")

# ---- Counts -----------------------------------------------------------------
n_forms   <- nrow(d)
n_submit  <- n_forms + N_INCOMPLETE
n_att_f   <- sum(d$Evaluator_Type == 1)
n_self_f  <- sum(d$Evaluator_Type == 2)
n_cases   <- n_distinct(d$Case_ID)
n_res     <- n_distinct(d$Resident_ID)
n_attg    <- n_distinct(d$Evaluator_ID[d$Evaluator_Type == 1])
period    <- paste0(format(min(d$Eval_Date), "%B"), "–", format(max(d$Eval_Date), "%B %Y"))

case_type <- d %>% group_by(Case_ID) %>%
  summarise(n_att = sum(Evaluator_Type == 1), n_self = sum(Evaluator_Type == 2),
            n_rid = n_distinct(Resident_ID), .groups = "drop") %>%
  mutate(type = case_when(n_att == 1 & n_self == 1 & n_rid == 1 ~ "pair",
                          n_att == 1 & n_self == 0 ~ "att_only",
                          n_att == 0 & n_self >= 1 ~ "self_only",
                          n_att >= 2               ~ "two_att",
                          TRUE                     ~ "other"))
ct <- table(factor(case_type$type, levels = c("pair", "att_only", "self_only", "two_att", "other")))
pair_ids  <- case_type$Case_ID[case_type$type == "pair"]
n_pairs   <- length(pair_ids)
n_pair_res <- n_distinct(d$Resident_ID[d$Case_ID %in% pair_ids])
n_att_res <- n_distinct(d$Resident_ID[d$Evaluator_Type == 1])

flags     <- flag_cases(d)
flag_ids  <- flags$Case_ID[flags$flagged]
n_flag    <- length(flag_ids)
n_att_sens  <- sum(d$Evaluator_Type == 1 & !d$Case_ID %in% flag_ids)
n_pair_sens <- sum(!pair_ids %in% flag_ids)

# ---- Mermaid diagram --------------------------------------------------------
# Main flow runs top to bottom; exclusions sit to the right (dashed borders).
box <- function(id, ...) sprintf('    %s["%s"]', id, paste(..., sep = "<br/>"))
mmd <- c(
  "```mermaid",
  "%%{init: {'theme': 'base', 'themeVariables': {'fontFamily': 'Arial, Helvetica, Liberation Sans, sans-serif', 'fontSize': '14px', 'primaryColor': '#ffffff', 'primaryBorderColor': '#000000', 'primaryTextColor': '#000000', 'lineColor': '#000000'}, 'flowchart': {'curve': 'linear', 'nodeSpacing': 45, 'rankSpacing': 50, 'padding': 12, 'wrappingWidth': 420, 'htmlLabels': true}}}%%",
  "flowchart TB",
  box("A", sprintf("<b>Evaluation forms submitted</b>"),
      sprintf("SIMPL-based EPA assessments, %s", period), sprintf("n = %s", fmt_n(n_submit))),
  box("X1", "<b>Excluded</b>", sprintf("Incomplete forms, n = %s", fmt_n(N_INCOMPLETE))),
  box("B", "<b>Complete evaluation forms</b>", sprintf("n = %s", fmt_n(n_forms)),
      sprintf("Attending evaluations, n = %s", fmt_n(n_att_f)),
      sprintf("Resident self-assessments, n = %s", fmt_n(n_self_f)),
      sprintf("%s surgical cases", fmt_n(n_cases)),
      sprintf("%s residents (PGY-1 to PGY-5) · %s attending surgeons", n_res, n_attg)),
  box("C1", "<b>Attending-rated outcomes</b>",
      sprintf("n = %s attending evaluations", fmt_n(n_att_f)), sprintf("%s residents", n_att_res),
      "<i>Tables 3–4; Figures 1–3, 5</i>"),
  box("C2", "<b>Self vs attending agreement</b>",
      sprintf("n = %s paired cases", fmt_n(n_pairs)), sprintf("%s residents", n_pair_res),
      "<i>Table 5; Figure 4</i>"),
  box("C3", "<b>Implementation outcomes</b>",
      sprintf("n = %s forms", fmt_n(n_forms)),
      sprintf("%s attending surgeons · %s residents", n_attg, n_res),
      "<i>Table 6; Figure 6</i>"),
  box("X2", "<b>Cases without a valid pair</b>", sprintf("n = %s", fmt_n(n_cases - n_pairs)),
      sprintf("Attending evaluation only, n = %s", ct["att_only"]),
      sprintf("Self-assessment only, n = %s", ct["self_only"]),
      sprintf("Two attending evaluations, n = %s", ct["two_att"]),
      if (ct["other"] > 0) sprintf("Other, n = %s", ct["other"]) else NULL),
  box("S1", "<b>Sensitivity analysis</b>",
      sprintf("Excluding %s flagged cases", n_flag),
      sprintf("n = %s attending evaluations", fmt_n(n_att_sens)), "<i>Table S5</i>"),
  box("S2", "<b>Sensitivity analysis</b>",
      sprintf("Excluding flagged pairs, n = %s", n_pairs - n_pair_sens),
      sprintf("n = %s paired cases", fmt_n(n_pair_sens)), "<i>Table S6</i>"),
  "",
  "    A --> B",
  "    A -.-> X1",
  "    B --> C1",
  "    B --> C2",
  "    B --> C3",
  "    C2 -.-> X2",
  "    C1 -.-> S1",
  "    C2 -.-> S2",
  "",
  "    classDef main fill:#ffffff,stroke:#000000,stroke-width:1.2px,color:#000000,font-family:Arial\\,Helvetica\\,Liberation Sans\\,sans-serif;",
  "    classDef excl fill:#f4f4f4,stroke:#555555,stroke-width:1px,stroke-dasharray:4 3,color:#000000,font-family:Arial\\,Helvetica\\,Liberation Sans\\,sans-serif;",
  "    classDef sens fill:#ffffff,stroke:#555555,stroke-width:1px,stroke-dasharray:4 3,color:#000000,font-family:Arial\\,Helvetica\\,Liberation Sans\\,sans-serif;",
  "    class A,B,C1,C2,C3 main;",
  "    class X1,X2 excl;",
  "    class S1,S2 sens;",
  "    linkStyle default stroke:#000000,stroke-width:1.2px;",
  "```"
)

legend <- sprintf("Flow of evaluation forms, surgical cases, and residents through the study (%s). Solid boxes show the analytic samples and the tables and figures that use them; dashed boxes show exclusions and sensitivity analyses. Flagged cases were identified during data verification (two attending forms for one case, forms from different residents under one case number, mismatched procedure, approach, or date between paired forms, or inconsistent evaluator coding). Abbreviations: EPA, entrustable professional activity; PGY, postgraduate year; SIMPL, System for Improving and Measuring Procedural Learning.", period)

counts <- data.frame(
  Step = c("Forms submitted", "Excluded: incomplete forms", "Complete forms", "Attending evaluations",
           "Resident self-assessments", "Surgical cases", "Residents", "Attending surgeons",
           "Paired cases (1 attending + 1 self, same resident)", "Cases with attending evaluation only",
           "Cases with self-assessment only", "Cases with two attending evaluations", "Other unpaired cases",
           "Flagged cases", "Attending evaluations after excluding flagged cases", "Paired cases after excluding flagged cases"),
  n = c(n_submit, N_INCOMPLETE, n_forms, n_att_f, n_self_f, n_cases, n_res, n_attg,
        n_pairs, ct["att_only"], ct["self_only"], ct["two_att"], ct["other"],
        n_flag, n_att_sens, n_pair_sens),
  row.names = NULL
)

writeLines(c("# Study Flow Diagram", "",
             "**Figure. Study Flow Diagram**", "",
             mmd, "",
             legend, "",
             "## Counts used in the diagram", "",
             kable(counts, format = "pipe", align = c("l", "r")), "",
             sprintf("_Generated %s from %s. Incomplete forms (n = %d) are entered manually in EPA_Flowchart.R._",
                     format(Sys.time(), "%Y-%m-%d %H:%M"), basename(data_path), N_INCOMPLETE)),
           file.path(fig_res_dir, "Figure_Flowchart.md"), useBytes = TRUE)

# Plain .mmd copy for mermaid.live or the mermaid CLI (mmdc)
writeLines(mmd[2:(length(mmd) - 1)], file.path(fig_png_dir, "Figure_Flowchart.mmd"), useBytes = TRUE)

writeLines(c("**Figure. Study Flow Diagram**", "", legend), file.path(fig_legend_dir, "Figure_Flowchart_Legend.md"), useBytes = TRUE)
message("Flowchart written to: ", fig_res_dir, " (.md), ", fig_png_dir, " (.mmd), ", fig_legend_dir, " (legend)")
