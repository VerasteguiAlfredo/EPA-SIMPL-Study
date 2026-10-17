# =============================================================================
# Estudio EPA / SIMPL — análisis completo en español
#
#   Corre exactamente el mismo análisis que run_all.R, pero escribe todas las
#   tablas, figuras, leyendas y archivos de resultados en español en:
#     Resultados/Tablas    MD/ · HTML/ · Todas_las_Tablas.docx
#     Resultados/Figuras   PNG/ · Leyendas/ · Resultados/
#   Los resultados en inglés (carpeta Results/) no se modifican.
#
#   Guarde este archivo en "R Scripts", junto a run_all.R.
# =============================================================================

scripts_root <- Sys.getenv(
  "EPA_SCRIPTS_DIR",
  "C:/Users/m320532.MFAD/Desktop/Research/VS Code Workbook/surg_training_evaluation/R Scripts"
)

old_lang <- Sys.getenv("EPA_LANG", unset = NA)
Sys.setenv(EPA_LANG = "es")
tryCatch(
  source(file.path(scripts_root, "run_all.R"), encoding = "UTF-8", local = new.env()),
  finally = {
    # back to the previous language so later runs in this R session are not affected
    if (is.na(old_lang)) Sys.unsetenv("EPA_LANG") else Sys.setenv(EPA_LANG = old_lang)
  }
)
