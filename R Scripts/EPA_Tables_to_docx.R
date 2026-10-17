# =============================================================================
# EPA / SIMPL study — combine all HTML tables into one Word document
#
#   Reads every Table_*.html in Results/Tables/HTML (Tables 1–6, then S1–S6)
#   and writes Results/Tables/All_Tables.docx, keeping the HTML format:
#     - bold title and plain caption above each table
#     - spanner header row (e.g. "Zwisch autonomy" over two columns)
#     - three horizontal lines only (top of header, below header, bottom)
#     - bold row labels, indented sub-rows, superscript test letters,
#       subscripts and line breaks in headers
#     - footnotes below each table in a smaller font
#   One table per page; wide tables get a landscape page.
#
#   Runs on its own or from run_all.R (after the table scripts).
# =============================================================================

# ---- Paths (edit here) ------------------------------------------------------
if (!exists("project_dir"))          # already set when run from run_all.R
  project_dir <- Sys.getenv(
    "EPA_PROJECT_DIR",
    "C:/Users/m320532.MFAD/Desktop/Research/VS Code Workbook/surg_training_evaluation"
  )
folder_path <- file.path(project_dir, "Results", "Tables", "HTML")
out_docx    <- file.path(project_dir, "Results", "Tables", "All_Tables.docx")

# ---- Layout settings --------------------------------------------------------
FONT          <- "Arial"
SIZE_BODY     <- 10    # table text and caption (pt)
SIZE_TITLE    <- 11
SIZE_NOTES    <- 9
PORTRAIT_W    <- 7.0   # usable width in inches (Letter, 0.75-inch margins)
MARGIN        <- 0.75  # page margins (in)
LANDSCAPE_W   <- 9.5
FORCE_LANDSCAPE <- character(0)   # e.g. c("2", "S4") to force landscape pages

# ---- Packages ---------------------------------------------------------------
pkgs <- c("xml2", "officer", "flextable")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss)
suppressPackageStartupMessages({ library(xml2); library(officer); library(flextable) })

# ---- Find the HTML files, main tables first, then supplementary -------------
files <- list.files(folder_path, pattern = "^Table_.*\\.html$", full.names = TRUE)
if (!length(files)) stop("No Table_*.html files found in: ", folder_path)
key   <- sub("^Table_(.*)\\.html$", "\\1", basename(files))
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
  list(id = sub("^Table_(.*)\\.html$", "\\1", basename(f)),
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
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- valign(ft, valign = "bottom", part = "header")
  ft <- valign(ft, valign = "top", part = "body")
  ft <- padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 4, padding.right = 4, part = "all")
  ft <- line_spacing(ft, space = 1, part = "all")
  ft <- height_all(ft, height = 0.05, part = "all")    # rows as tall as their text only
  ft <- hrule(ft, rule = "atleast", part = "all")
  ft
}

# ---- Column widths and page orientation -------------------------------------
# Widths come from the body text (headers wrap). Wide tables go landscape; if a
# table is still too wide, the font drops to 9 pt and then columns are narrowed
# so long cells wrap onto two lines (font never goes below 9 pt).
fit_table <- function(tb) {
  id  <- tb$id
  ft  <- build_ft(tb)
  pad <- 8 / 72                                         # left + right padding (in)
  w   <- dim_pretty(ft, part = "body")$widths + pad
  w   <- pmax(w, 0.55)
  # P value columns keep a fixed width so "<.001" and its superscript never wrap
  last_hdr <- tb$header[[length(tb$header)]]
  hdr_txt  <- rep(vapply(last_hdr$runs, function(r) paste(r$text, collapse = ""), ""), last_hdr$span)
  is_p <- grepl("^P\\s*value", hdr_txt)
  w[is_p] <- 0.72
  landscape <- id %in% FORCE_LANDSCAPE || sum(w[-1]) + min(w[1], 2.3) > PORTRAIT_W
  max_w <- if (landscape) LANDSCAPE_W else PORTRAIT_W
  if (sum(w) > max_w) {                                 # step 1: 9-pt font
    ft <- build_ft(tb, size = 9)
    w[!is_p] <- w[!is_p] * 0.9
  }
  w[1] <- min(w[1], if (landscape) 2.6 else 2.3)        # step 2: wrap long row labels
  flex <- !is_p & seq_along(w) > 1                      # step 3: narrow the data columns
  if (sum(w) > max_w) w[flex] <- w[flex] * (max_w - sum(w[!flex])) / sum(w[flex])
  ft <- width(ft, width = w)
  ft <- set_table_properties(ft, layout = "fixed", align = "left")
  list(ft = ft, landscape = landscape, width = sum(w))
}

# ---- Assemble the Word document ---------------------------------------------
sect <- function(landscape) {
  ps <- if (landscape)
    prop_section(page_size = page_size(orient = "landscape"), type = "nextPage",
                 page_margins = page_mar(top = MARGIN, bottom = MARGIN, left = MARGIN, right = MARGIN))
  else
    prop_section(page_size = page_size(orient = "portrait"), type = "nextPage",
                 page_margins = page_mar(top = MARGIN, bottom = MARGIN, left = MARGIN, right = MARGIN))
  block_section(ps)
}

doc <- read_docx()
for (f in files) {
  tb <- read_table_html(f)
  ft_fit <- fit_table(tb)
  ft <- ft_fit$ft; landscape <- ft_fit$landscape

  doc <- body_add_fpar(doc, as_fpar(tb$title, SIZE_TITLE, bold_all = TRUE, space_after = 2))
  doc <- body_add_fpar(doc, as_fpar(tb$caption, SIZE_BODY, space_after = 6))
  doc <- body_add_flextable(doc, ft, align = "left", split = FALSE, keepnext = FALSE)
  for (n in tb$notes) doc <- body_add_fpar(doc, as_fpar(n, SIZE_NOTES, space_after = 1))
  doc <- body_end_block_section(doc, sect(landscape))
  message(sprintf("Table %-3s  %s  (%.1f in)", tb$id, if (landscape) "landscape" else "portrait ", ft_fit$width))
}

print(doc, target = out_docx)
message("Word document written to: ", out_docx)
