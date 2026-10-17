# =============================================================================
# EPA / SIMPL study — combine all HTML tables into one Word document
#
#   Reads every Table_*.html in Results/Tables/HTML (Tables 1–6, then S1–S6)
#   and writes Results/Tables/All_Tables.docx, keeping the HTML format.
#   With EPA_LANG = "es" (run_all_es.R) it reads Resultados/Tablas/HTML/Tabla_*.html
#   and writes Resultados/Tablas/Todas_las_Tablas.docx. The Word file keeps:
#     - bold title and plain caption above each table
#     - spanner header row (e.g. "Zwisch autonomy" over two columns)
#     - three horizontal lines only (top of header, below header, bottom)
#     - bold row labels, indented sub-rows, superscript test letters,
#       subscripts and line breaks in headers
#     - footnotes below each table
#   Layout: Times New Roman 10 pt throughout, every page portrait (Letter,
#   0.5-inch margins), one table per page; column widths are fitted to the
#   page from the text (long cells wrap between words, never mid-word).
#
#   Runs on its own or from run_all.R (after the table scripts).
# =============================================================================

# ---- Paths (edit here) ------------------------------------------------------
if (!exists("project_dir"))          # already set when run from run_all.R
  project_dir <- Sys.getenv(
    "EPA_PROJECT_DIR",
    "C:/Users/m320532.MFAD/Desktop/Research/VS Code Workbook/surg_training_evaluation"
  )
IS_ES_DOCX  <- tolower(Sys.getenv("EPA_LANG", "en")) == "es"
TAB_WORD    <- if (IS_ES_DOCX) "Tabla" else "Table"
tables_out  <- if (IS_ES_DOCX) file.path(project_dir, "Resultados", "Tablas") else file.path(project_dir, "Results", "Tables")
folder_path <- file.path(tables_out, "HTML")
out_docx    <- file.path(tables_out, if (IS_ES_DOCX) "Todas_las_Tablas.docx" else "All_Tables.docx")

# ---- Layout settings --------------------------------------------------------
FONT          <- "Times New Roman"
SIZE_BODY     <- 10    # table text and caption (pt)
SIZE_TITLE    <- 10    # table title (bold)
SIZE_NOTES    <- 10    # footnotes
MARGIN        <- 0.5   # page margins (in); Letter portrait. 0.5 in lets the widest tables (1 and S4) fit at 10 pt
PAGE_W        <- 8.5 - 2 * MARGIN   # usable table width (in) = 7.5
FIRST_COL_MAX <- 2.0   # widest the row-label column may get before its labels wrap (in)
FIRST_COL_MIN <- 1.5   # row-label column gets at least this much (if the page allows) before data columns widen
CELL_PAD      <- 2.5   # left/right padding inside each cell (pt)

# ---- Packages ---------------------------------------------------------------
pkgs <- c("xml2", "officer", "flextable")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss)
suppressPackageStartupMessages({ library(xml2); library(officer); library(flextable) })

# ---- Find the HTML files, main tables first, then supplementary -------------
tab_pat <- paste0("^", TAB_WORD, "_(.*)\\.html$")
files <- list.files(folder_path, pattern = tab_pat, full.names = TRUE)
if (!length(files)) stop("No ", TAB_WORD, "_*.html files found in: ", folder_path)
key   <- sub(tab_pat, "\\1", basename(files))
files <- files[order(grepl("^S", key), suppressWarnings(as.numeric(sub("^S", "", key))))]

# ---- HTML -> runs of formatted text -----------------------------------------
# Each node becomes a data frame of runs: text + bold/italic/superscript/subscript.
# <br> becomes a line break.
node_runs <- function(node, b = FALSE, i = FALSE, va = "baseline") {
  if (xml_type(node) == "text")
    return(data.frame(text = xml_text(node), bold = b, italic = i, va = va, br = FALSE))
  tag <- xml_name(node)
  if (tag == "br") return(data.frame(text = "", bold = b, italic = i, va = va, br = TRUE))
  b  <- b || tag %in% c("b", "strong")
  i  <- i || tag %in% c("i", "em")
  va <- if (tag == "sup") "superscript" else if (tag == "sub") "subscript" else va
  kids <- xml_contents(node)
  if (!length(kids)) return(NULL)
  do.call(rbind, lapply(kids, node_runs, b = b, i = i, va = va))
}

cell_runs <- function(node) {
  r <- node_runs(node)
  if (is.null(r) || !nrow(r)) return(data.frame(text = "", bold = FALSE, italic = FALSE, va = "baseline", br = FALSE))
  r$text <- gsub("[ \t\r\n]+", " ", r$text)           # collapse HTML whitespace (keeps em spaces)
  r$text[1] <- sub("^ +", "", r$text[1])              # trim kable's padding spaces
  r$text[nrow(r)] <- sub(" +$", "", r$text[nrow(r)])
  r$text <- gsub("\u2013", "\u2013\u2060", r$text)   # no line break inside "(28.00–30.00)"
  r$text <- gsub("/", "/\u200B", r$text, fixed = TRUE) # allow a line break after "/" in long labels
  r[r$text != "" | r$br, , drop = FALSE]
}

run_props <- function(r, size)
  fp_text_default(font.family = FONT, font.size = size, bold = r$bold, italic = r$italic,
                  vertical.align = r$va)

# runs -> flextable paragraph
as_ft_par <- function(r, size = SIZE_BODY) {
  if (!nrow(r)) return(as_paragraph(""))
  chunks <- lapply(seq_len(nrow(r)), function(k)
    if (r$br[k]) as_chunk("\n") else as_chunk(r$text[k], props = run_props(r[k, ], size)))
  as_paragraph(list_values = chunks)
}

# runs -> officer paragraph (title, caption, notes)
as_fpar <- function(r, size, bold_all = FALSE, space_after = 4) {
  if (bold_all) r$bold <- TRUE
  runs <- lapply(seq_len(nrow(r)), function(k)
    if (r$br[k]) run_linebreak()
    else ftext(r$text[k], prop = fp_text(font.family = FONT, font.size = size, bold = r$bold[k],
                                         italic = r$italic[k], vertical.align = r$va[k])))
  do.call(fpar, c(runs, list(fp_p = fp_par(padding.bottom = space_after, line_spacing = 1.1))))
}

# ---- Read one HTML table ----------------------------------------------------
read_table_html <- function(f) {
  txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  txt <- gsub("<(?=[.0-9= ])", "&lt;", txt, perl = TRUE)   # "<.001" is text, not a tag
  h   <- read_html(txt, encoding = "UTF-8")

  title   <- cell_runs(xml_find_first(h, "//p[@class='tbl-title']"))
  caption <- cell_runs(xml_find_first(h, "//p[@class='tbl-caption']"))
  notes_node <- xml_find_first(h, "//div[@class='tbl-notes']")
  notes <- if (inherits(notes_node, "xml_missing")) NULL else {
    r <- cell_runs(notes_node)                       # split notes into paragraphs at <br>
    grp <- cumsum(r$br)
    lapply(split(r[!r$br, , drop = FALSE], grp[!r$br]), function(x) {
      x$text[1] <- sub("^ +", "", x$text[1]); x })
  }

  head_rows <- xml_find_all(h, "//table//thead/tr")
  body_rows <- xml_find_all(h, "//table//tbody/tr")
  header <- lapply(head_rows, function(tr) {
    cells <- xml_find_all(tr, "./th|./td")
    span  <- as.integer(xml_attr(cells, "colspan")); span[is.na(span)] <- 1L
    list(runs = lapply(cells, cell_runs), span = span)
  })
  body <- lapply(body_rows, function(tr) lapply(xml_find_all(tr, "./td|./th"), cell_runs))
  list(id = sub(tab_pat, "\\1", basename(f)),
       title = title, caption = caption, notes = notes, header = header, body = body)
}

# ---- Build the flextable ----------------------------------------------------
build_ft <- function(tb, size = SIZE_BODY) {
  nc <- length(tb$body[[1]]); nr <- length(tb$body)
  keys <- paste0("c", seq_len(nc))
  plain <- function(r) paste(ifelse(r$br, "\n", r$text), collapse = "")
  df <- as.data.frame(matrix(vapply(unlist(tb$body, recursive = FALSE), plain, ""),
                             nrow = nr, byrow = TRUE), stringsAsFactors = FALSE)
  names(df) <- keys

  ft <- flextable(df, col_keys = keys)
  ft <- delete_part(ft, part = "header")
  # add header rows bottom-up (add_header_row puts each new row on top)
  for (hr in rev(tb$header))
    ft <- add_header_row(ft, values = vapply(hr$runs, plain, ""), colwidths = hr$span, top = TRUE)

  # rich text in every cell
  for (k in seq_along(tb$header)) {
    hr <- tb$header[[k]]; j0 <- cumsum(c(1, head(hr$span, -1)))
    for (m in seq_along(hr$runs))
      ft <- compose(ft, i = k, j = j0[m], part = "header", value = as_ft_par(hr$runs[[m]], size))
  }
  for (i in seq_len(nr)) for (j in seq_len(nc))
    ft <- compose(ft, i = i, j = j, part = "body", value = as_ft_par(tb$body[[i]][[j]], size))

  # format: three lines only, first column left, others centred
  ft <- border_remove(ft)
  ft <- hline_top(ft, part = "header", border = fp_border(color = "black", width = 1.5))
  ft <- hline_bottom(ft, part = "header", border = fp_border(color = "black", width = 1))
  ft <- hline_bottom(ft, part = "body", border = fp_border(color = "black", width = 1.5))
  ft <- font(ft, fontname = FONT, part = "all")
  ft <- fontsize(ft, size = size, part = "all")
  ft <- flextable::bold(ft, part = "header")   # helpers define their own bold() for HTML
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- valign(ft, valign = "bottom", part = "header")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = CELL_PAD, padding.right = CELL_PAD, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- height_all(ft, height = 0.05, part = "all")    # rows as tall as their text only
  ft <- hrule(ft, rule = "atleast", part = "all")
  ft
}

# ---- Column widths (portrait pages only) ------------------------------------
# Every table goes on a portrait page at a fixed 10-pt font. Column widths are
# set from the text itself:
#   minimum   = widest single word in the column (cells may wrap, words never split)
#   preferred = widest cell on one line
# Each column starts at its minimum and the remaining page width is shared out in
# proportion to how much each column still needs to reach its preferred width.
# P value columns stay narrow; the first column is capped so long row labels wrap.
measure_font <- function() {
  if (requireNamespace("gdtools", quietly = TRUE) && !gdtools::font_family_exists(FONT)) {
    message("Note: font '", FONT, "' not found on this computer; column widths measured with 'Liberation Serif' (same metrics).")
    return("Liberation Serif")
  }
  FONT
}
MEAS_FONT <- measure_font()

plain_txt <- function(r) paste(ifelse(r$br, "\n", r$text), collapse = "")
is_bold   <- function(r) nrow(r) > 0 && any(r$bold)
longest_word <- function(x) {          # Word breaks lines at spaces, after "/" and after hyphens
  w <- unlist(strsplit(x, "[ \n\u200B]+|(?<=-)", perl = TRUE))
  if (!length(w)) "" else w[which.max(nchar(w))]
}
# width (inches) of the widest string in each column of a character matrix
col_widths <- function(m, bold_m) {
  df <- as.data.frame(m, stringsAsFactors = FALSE); names(df) <- paste0("c", seq_len(ncol(df)))
  ft <- flextable(df)
  ft <- delete_part(ft, part = "header")
  ft <- font(ft, fontname = MEAS_FONT, part = "all")
  ft <- fontsize(ft, size = SIZE_BODY, part = "all")
  ft <- padding(ft, padding = 0, part = "all")
  for (j in seq_len(ncol(m))) if (any(bold_m[, j])) ft <- flextable::bold(ft, i = which(bold_m[, j]), j = j)
  dim_pretty(ft, part = "body")$widths
}

fit_table <- function(tb) {
  ft  <- build_ft(tb)
  nc  <- length(tb$body[[1]])
  pad <- 2 * CELL_PAD / 72 + 0.02                         # cell padding + a small safety margin (in)

  body_txt  <- t(sapply(tb$body, function(row) vapply(row, plain_txt, "")))
  body_bold <- t(sapply(tb$body, function(row) vapply(row, is_bold, logical(1))))
  if (nc == 1) { body_txt <- t(body_txt); body_bold <- t(body_bold) }

  last_hdr <- tb$header[[length(tb$header)]]
  hdr_txt  <- rep(vapply(last_hdr$runs, plain_txt, ""), last_hdr$span)
  hdr_txt[rep(last_hdr$span > 1, last_hdr$span)] <- ""  # spanned cells do not set a column width

  word_m <- rbind(apply(body_txt, c(1, 2), longest_word), vapply(hdr_txt, longest_word, ""))
  word_b <- rbind(body_bold, rep(TRUE, nc))               # header is bold
  w_min  <- col_widths(word_m, word_b) + pad
  w_pref <- pmax(col_widths(body_txt, body_bold) + pad, w_min)

  is_p <- grepl("^(P\\s*value|Valor de P)", hdr_txt)
  w_pref[is_p] <- w_min[is_p] <- pmax(w_min[is_p], 0.42)
  w_pref[1] <- max(w_min[1], min(w_pref[1], FIRST_COL_MAX))
  # row labels get first claim on space (up to FIRST_COL_MIN) so they do not wrap onto many lines
  lab_min <- max(w_min[1], min(w_pref[1], FIRST_COL_MIN))
  if (sum(w_min[-1]) + lab_min <= PAGE_W) w_min[1] <- lab_min

  if (sum(w_pref) <= PAGE_W) {
    w <- w_pref                                            # fits on one line per cell
  } else if (sum(w_min) <= PAGE_W) {
    need <- w_pref - w_min
    w <- w_min + need * (PAGE_W - sum(w_min)) / sum(need)  # share the spare width
  } else {
    w <- w_min * PAGE_W / sum(w_min)                       # very wide table: scale down evenly
    warning(sprintf("%s %s: the longest words need %.1f in but the page has %.1f in; some words may break.",
                    TAB_WORD, tb$id, sum(w_min), PAGE_W))
  }
  ft <- width(ft, width = w)
  ft <- set_table_properties(ft, layout = "fixed", align = "left")
  list(ft = ft, width = sum(w))
}

# ---- Assemble the Word document (portrait throughout) -----------------------
doc <- read_docx()
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(width = 8.5, height = 11, orient = "portrait"),   # US Letter
  page_margins = page_mar(top = MARGIN, bottom = MARGIN, left = MARGIN, right = MARGIN)))
for (k in seq_along(files)) {
  tb <- read_table_html(files[k])
  ft_fit <- fit_table(tb)

  doc <- body_add_fpar(doc, as_fpar(tb$title, SIZE_TITLE, bold_all = TRUE, space_after = 2))
  doc <- body_add_fpar(doc, as_fpar(tb$caption, SIZE_BODY, space_after = 6))
  doc <- body_add_flextable(doc, ft_fit$ft, align = "left", split = FALSE, keepnext = FALSE)
  for (n in tb$notes) doc <- body_add_fpar(doc, as_fpar(n, SIZE_NOTES, space_after = 1))
  if (k < length(files)) doc <- body_add_break(doc)        # one table per page
  message(sprintf("%s %-3s  portrait  (%.2f in)", TAB_WORD, tb$id, ft_fit$width))
}

print(doc, target = out_docx)
message(if (IS_ES_DOCX) "Documento de Word guardado en: " else "Word document written to: ", out_docx)