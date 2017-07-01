texToRmd <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  where <- rstudioapi::primary_selection(context)
  rstudioapi::insertText(where$range,
                         translate_tex(where$text),
                         context$id)
}

unbreak_lines <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  where <- rstudioapi::primary_selection(context)
  rstudioapi::insertText(where$range,
                         simplify_paragraphs(where$text),
                         context$id)
}
