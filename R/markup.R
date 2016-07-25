#' Mark a word as being introduced and indexed.
#' @export
new_word <- function(word) {
  I(paste0("***", word, "***"))
  # add indexing later
}
#' @export
dataset <- function(word) {
  I(paste0("`", word, "`"))
  # add indexing later
}

#' @export
verbatim_input <- function(file_name) {
  text <- paste(readLines(file_name), collapse = "\n")
  # escape quotes
  text <- gsub("\"", "\\\"", text)
  gsub("\'", "\\\'", text)
}
