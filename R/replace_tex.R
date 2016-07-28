# Translate tex sources to markdown

#' @export
translate_tex_file <-
  function(in_name, dir = ".",
           out_name = paste0("inst/translated/new-", in_name)) {
    out_name <- gsub(".tex$", ".Rmd", out_name,
                     ignore.case = TRUE)
    out_name <- gsub(".rnw$", ".Rmd", out_name,
                     ignore.case = TRUE)
    input_text <- readLines(paste0(dir, in_name))
    results <- translate_tex(input_text)

    cat("Wrote to file ", out_name, "\n\n\n\n")
    writeLines(results, out_name)
    results
  }

#' @export
translate_tex <- function(text_vector) {
  # handle tex comments specially
  result <- character(length(text_vector))
  for (k in 1:length(text_vector)) {
    tmp <- translate_tex_comments(text_vector[k])
    # quotes before chunk to preserve chunk format
    tmp <- simplify_double_quotes(tmp)
    tmp <- verbatim_fix(tmp)
    result[k] <- sweave_chunk(tmp)
  }
  # put in one line
  # so that commands spanning multiple lines can be handled
  text <- paste(result, collapse = "\n")


  tmp <- inline_font_translate(text)
  # kill off optional argument to \includegraphics
  tmp <- gsub("\\[width=[0-9\\.]+(in|cm|mm)\\]", "", tmp)


  tmp <- replace_tex_command_2(tmp)
  tmp <- replace_tex_command_1(tmp)
  # handle nested commands
  tmp <- replace_tex_command_1(tmp)
  tmp <- replace_tex_command_1(tmp)
  tmp <- replace_tex_command_0(tmp)

  tmp
}

#' @export
verbatim_fix <- function(tex_string) {
  string <- gsub("\\\\(begin|end)\\{verbatim\\}", "\`\`\`", tex_string)
  #pattern <- "\\\\verb\\+([^\\+]*)\\+"
  delimiters <- c("\\+", "\\.", "\\^", "\\|", "\\*")
  patterns <- sprintf("\\\\verb%s([^%s]*)%s",
                      delimiters, delimiters, delimiters)
  matches <- NULL
  for (pattern in patterns) {
  matches <- rbind(matches,
                   stringr::str_match_all(string, pattern)[[1]])
  }
  if (nrow(matches) == 0) return(string)
  working <- string
  for (k in 1:nrow(matches)) {
    working <-
      gsub(matches[k,1],
           paste0("\`",matches[k,2], "\`"),
           working, fixed = TRUE)
  }

  working
  }


#' @export
sweave_chunk <- function(tex_string) {
  pattern <- "<<(.*)>>=\n"
  matches <- stringr::str_match_all(tex_string, pattern)[[1]]
  result <- if (nrow(matches) > 0) {
    for (k in 1:nrow(matches)) {
      tex_string <- gsub(matches[k, 1],
                         paste0("\`\`\`{r ", matches[k,2], "}\n"),
                         tex_string, fixed = TRUE)
    }
    tex_string <- gsub("@", "\`\`\`\n", tex_string)
  } else {
    tex_string
  }

  result <-
    gsub("=true", " = TRUE",
    gsub("=false", " = FALSE", result))
  result <- gsub("results=hide", 'results = "hide"', result)
  result
}

construct_comment <- function(content) {
  sprintf(paste0("<!-- ", content, " -->"))
}

translate_tex_comments <- function(tex_string) {
  pattern <- "%+(.*)($|\n)"
  matches <- stringr::str_match_all(tex_string, pattern)[[1]]
  if (nrow(matches) == 0) {
    # nothing to do
  } else {
    for (k in 1:nrow(matches)) {
    tex_string <- gsub(matches[k, 1],
         paste0(construct_comment(matches[k,2]), "\n"),
         tex_string, fixed = TRUE)
    }
  }
  tex_string
}

simplify_double_quotes <- function(tex_string) {
  gsub("''", '"', gsub(" ``", ' "', tex_string))
}

#' @export
tex_command_translate <- function(command, arg1, arg2) {
  format <-
    switch(command,
      wrong = "- %s",
      correct = "- RIGHT %s",
      begin = "<!-- begin %s -->\n",
      end   = "<!-- end %s -->",
      centerline = "%s",
      variableName = "`%s`",
      VN    = "`%s`",
      includegraphics = " %s ",
      model = "%s ~ %s",
      bigskip = "\n",
      medskip = "\n",
      section = "# %s",
      subsection = "## %s",
      subsubsection = "### %s",
      centerline = " %s \n",
      item = "#. ",
      code = "`%s`",
      texttt = "`%s`",
      paragraph = "\n",
      # bookdown- and tufte-specific things
      ref = "\\@ref(\"%s\")", # for bookdown references
      newthought = "`r tufte::newthought(\"%s\")`",
      marginnote = "`r tufte::margin_note(\"%s\")`",
      VerbatimInput = "```{r echo = FALSE, comment = ''}\ncat(ISMmd::verbatim_input(\"%s\"))\n```\n",
      noindent = "",
      cite = "[@%s]", # bookdown citations
      em = "*%s*",
      bf = "**%s**",
      sqrt = "\\sqrt",
      cos = "\\cos",
      sin = "\\sin",
      pi = "\\pi",
      "function" = "`%s()`",
      pkg = "**`%s`**",
      times = "\\times",
      TextEntry = "YOUR ANSWER HERE.",
      index = "`r index_entry('%s', '%s')` ",
      newword = "`r new_word('%s')`",
      newworddef = "`r new_word('%s')` `r index_entry(%s)`",
      dataset = "`r dataset('%s')`",
      datasetCPS = "`r dataset('%s')`",
      matchSelect = "CHOICES %s:  CORRECT %s",
      paste("TEX COMMAND NOT FOUND", command,
            ifelse(!missing(arg1), arg1, ""),
            ifelse(!missing(arg2), arg2, ""))
    )
  if (missing(arg1)) return(format)
  else if (missing(arg2)) return(sprintf(format, arg1))
  else return(sprintf(format, arg1, arg2))
}

# Put {\em } font commands into \em{ } form.
inline_font_translate <- function(string) {
  tmp <- gsub("\\{\\\\em ", "\\\\em\\{", string)
  gsub("\\{\\\\bf ", "\\\\bf\\{", tmp)
}

#' @export
replace_tex_command_0 <- function(string) {
  if (length(string) > 1) stop("Just one string at a time, please.")
  # matching simple latex commands
  # must be letter after \
  pattern <- "\\\\([a-zA-Z]+)[^\\{a-zA-Z]"
  matches <- stringr::str_match_all(string, pattern)[[1]]
  if (nrow(matches) == 0) return(string)
  working <- string
  for (k in 1:nrow(matches)) {
    working <-
      gsub(matches[k,1],
           tex_command_translate(matches[k,2]),
           working, fixed = TRUE)
  }

  working
}

#' @export
replace_tex_command_1 <- function(string) {
  if (length(string) > 1) stop("Just one string at a time, please.")
  # matching simple latex commands
  pattern <- "\\\\([a-zA-Z]*)\\{([^\\{\\}]*)\\}"
  matches <- stringr::str_match_all(string, pattern)[[1]]
  if (nrow(matches) == 0) return(string)
  working <- string
  for (k in 1:nrow(matches)) {
    working <-
      gsub(matches[k,1],
           tex_command_translate(matches[k,2], matches[k,3]),
           working, fixed = TRUE)
  }

  working
}

#' @export
replace_tex_command_2 <- function(string) {
  if (length(string) > 1) stop("Just one string at a time, please.")
  # matching simple latex commands
  pattern <- "\\\\([a-zA-Z]*)\\{(.*)\\}\\{(.*)\\}"
  matches <- stringr::str_match_all(string, pattern)[[1]]
  if (nrow(matches) == 0) return(string)
  working <- string
  for (k in 1:nrow(matches)) {
    working <-
      gsub(matches[k,1],
           tex_command_translate(matches[k,2], matches[k,3], matches[k,4]),
           working, fixed = TRUE)
  }

  working
}



