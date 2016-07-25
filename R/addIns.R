texToRmd <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  where <- rstudioapi::primary_selection(context)
  rstudioapi::insertText(where$range,
                         translate_tex(where$text),
                         context$id)
}
