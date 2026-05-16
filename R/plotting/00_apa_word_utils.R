# Helper utilities for exporting APA 7th–compliant figures
apa_title_case <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")
  words <- strsplit(text, "\\s+")[[1]]
  is_plain <- function(w) grepl("^[A-Za-z][A-Za-z'’-]*$", w)
  convert <- function(w) {
    if (!nzchar(w)) return(w)
    if (!is_plain(w)) return(w)
    if (grepl("^[A-Z]{2,}$", w)) return(w)
    paste0(toupper(substr(w, 1, 1)), tolower(substring(w, 2)))
  }
  paste(vapply(words, convert, character(1)), collapse = " ")
}

apa_clean_note <- function(text) {
  note <- sub("^\\s*Figure\\s+\\d+\\.?\\s*", "", text, ignore.case = TRUE)
  note <- trimws(note)
  if (!nzchar(note)) return("")
  if (!grepl("\\.$", note)) note <- paste0(note, ".")
  note
}

apa_add_figure_block <- function(
  doc,
  plot,
  fig_number,
  title,
  note = NULL,
  width = 6.5,
  height = 5,
  remove_plot_titles = TRUE,
  plot_par_style = "Normal"
) {
  stopifnot(inherits(doc, "rdocx"))

  fp_text_base <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
fp_par_line <- officer::fp_par(line_spacing = 2, text.align = "left")

  if (remove_plot_titles) {
    plot <- plot +
      ggplot2::ggtitle(NULL) +
      ggplot2::labs(subtitle = NULL)
  }

  label_line <- officer::fpar(
    officer::ftext(paste0("Figure ", fig_number), officer::fp_text(font.size = 12, font.family = "Times New Roman", bold = TRUE)),
    fp_p = fp_par_line
  )

  title_line <- officer::fpar(
    officer::ftext(apa_title_case(title), officer::fp_text(font.size = 12, font.family = "Times New Roman", italic = TRUE)),
    fp_p = fp_par_line
  )

  doc <- officer::body_add_fpar(doc, value = label_line)
  doc <- officer::body_add_fpar(doc, value = title_line)

  doc <- officer::body_add_gg(doc, value = plot, width = width, height = height, style = plot_par_style)

  clean_note <- ""
  if (!is.null(note) && nzchar(note)) {
    clean_note <- apa_clean_note(note)
  }
  if (nzchar(clean_note)) {
    note_line <- officer::fpar(
      officer::ftext("Note.", officer::fp_text(font.size = 12, font.family = "Times New Roman", italic = TRUE)),
      officer::ftext(paste0(" ", clean_note), fp_text_base),
      fp_p = fp_par_line
    )
    doc <- officer::body_add_fpar(doc, value = note_line)
  }

  doc
}
