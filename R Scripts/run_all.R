# =============================================================================
# EPA / SIMPL study — run the full analysis
#   Runs, in order:
#     Tables:  Tables 1–3; Table 4 + Table S1 + Figure 1; Tables 5–6 + S2–S6;
#              All_Tables.docx (every HTML table in one Word file)
#     Figures: Figures 2–4; Figures 5–6; Supplementary Figures S1–S3; study flowchart
#   Figures are saved to Results/Figures/PNG, /Legends and /Results.
#   Rebuilds All_Tables.md and All_Figure_Results.md and writes
#   Results/session_info.txt (R and package versions for the methods section).
#
# Script locations (edit here if you move them):
#   R Scripts/Tables   - table scripts
#   R Scripts/Figures  - figure scripts
#   EPA_helpers.R      - in "R Scripts", "R Scripts/Tables" or "R Scripts/Figures"
# Data and output paths are set once in EPA_helpers.R.
# =============================================================================

scripts_root <- Sys.getenv(
  "EPA_SCRIPTS_DIR",
  "C:/Users/m320532.MFAD/Desktop/Research/VS Code Workbook/surg_training_evaluation/R Scripts"
)
tables_dir  <- file.path(scripts_root, "Tables")
figures_dir <- file.path(scripts_root, "Figures")

# ---- Locate and load the shared helpers -------------------------------------
cand <- file.path(c(scripts_root, tables_dir, figures_dir), "EPA_helpers.R")
helpers <- cand[file.exists(cand)][1]
if (is.na(helpers)) stop("EPA_helpers.R not found in:\n  ", paste(dirname(cand), collapse = "\n  "))
source(helpers, encoding = "UTF-8")

# ---- Scripts, in run order --------------------------------------------------
scripts <- c(
  file.path(tables_dir,  "EPA_Tables_1-3.R"),
  file.path(tables_dir,  "EPA_Table4_Figure1.R"),
  file.path(tables_dir,  "EPA_Tables_5-6_Supplementary.R"),
  file.path(tables_dir,  "EPA_Tables_to_docx.R"),
  file.path(figures_dir, "EPA_Figures_2-4.R"),
  file.path(figures_dir, "EPA_Figures_5-6.R"),
  file.path(figures_dir, "EPA_Supplementary_Figures.R"),
  file.path(figures_dir, "EPA_Flowchart.R")
)
missing <- scripts[!file.exists(scripts)]
if (length(missing)) stop("Script(s) not found:\n  ", paste(missing, collapse = "\n  "))

t_all <- Sys.time()
for (s in scripts) {
  message("\n=== Running ", basename(dirname(s)), "/", basename(s), " ===")
  t0 <- Sys.time()
  source(s, encoding = "UTF-8", local = new.env())
  message(sprintf("    finished in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

rebuild_all_md()
rebuild_all_fig_md()
save_session_info()

# Files left loose in Results/Figures by older versions of the scripts
# (figures now go to PNG/, Legends/ and Results/ subfolders). Listed, not deleted.
loose <- list.files(fig_dir, full.names = FALSE, include.dirs = FALSE)
loose <- loose[!dir.exists(file.path(fig_dir, loose))]
if (length(loose))
  message("\nNote: ", length(loose), " older file(s) remain directly in Results/Figures and can be deleted:\n  ",
          paste(loose, collapse = "\n  "))
message(sprintf("\nAll outputs written to: %s  (total %.1f min)",
                file.path(project_dir, "Results"), as.numeric(difftime(Sys.time(), t_all, units = "mins"))))
