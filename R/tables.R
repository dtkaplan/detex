#' Convert latex to md tables (roughly)

#' @export
latex2md_tables <- function(text) {
  tmp <- gsub("(\\\\|\\\\hline|\\\\bigskip|\\\\centerline|\\\\begin\\{|tabular\\}|\\\\end\\{)", "",
              text)
  tmp <- gsub("&", "|", tmp)
  tmp <- gsub("\\VN\\{([a-zA-Z0-9_\\.]*)\\}", "`\\1`", tmp)
  bar <- make_table_bar(text)
  tmp <- paste0(bar, "\n\n", tmp)
  gsub("\\\\", "", tmp)
}

#' @export
make_table_bar <- function(text) {
  header <- stringr::str_extract(text, "\\\\begin\\{tabular\\}\\{[rcl|]+\\}")
  tmp <- gsub("\\\\begin\\{tabular}\\{([rcl|]*)}", "\\1", header)
  tmp <- gsub("[|]", "", tmp)
  strings <- list(r = "----:|", c = ":-----:|", l = ":----|")
  keys <- unlist(strsplit(tmp, ""))

  tmp <- paste0(unlist(strings[keys]), collapse = "")
  gsub("\\|$", "", tmp)
}

example <- "\\bigskip\n\\centerline{\\begin{tabular}{llr}\n\\VN{BookpageID} &  Person &  Age\\\\\\hline\\hline\nB230p539 & Bride & 28.7\\\nB230p539 & Groom & 32.6\\\\\\hline\nB230p677 & Bride & 52.6\\\\\nB230p677 & Groom & 32.3\\\\\\hline\nB230p766 & Bride & 26.7\\\\\nB230p766 & Groom & 34.8\\\\\\hline\nB230p892 & Bride & 39.6\\\\\nB230p892 & Groom & 40.6\\\\\n\\multicolumn{3}{l}{... and so on for 49 }\\\\\n\\multicolumn{3}{l}{couples altogether}\n\\end{tabular}}\n\\bigskip"
