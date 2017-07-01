#' Eliminate within-paragraph line breaks

#' @export
simplify_paragraphs <- function(text) {
  tmp <- gsub("\n{2,}", " special code ", text)
  tmp <- gsub("\n", " ", tmp)
  tmp <- gsub(" special code ", "\n\n", tmp)
  # smart quotes
  tmp <- gsub("(``[^`]\\{r|'')", "\"", tmp)

  tmp
}
