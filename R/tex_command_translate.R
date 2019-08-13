#' Replace latex index directives
#' @export
tex_command_translate <- function(command, arg1, arg2) {
  format <-
    switch(command,
           "%" = "%",
           times = "x",
           wrong = "- %s",
           correct = "- RIGHT %s",
           begin = if(arg1 == "boxedText")"\n----\n" else "<!-- begin %s -->\n",
           end   = if(arg1 == "boxedText")"\n----\n" else "<!-- end %s -->",
           centerline = "%s",
           variableName = "`%s`",
           VN    = "`%s`",
           var = "`%s`",
           textit = "*%s*",
           includegraphics = "![Caption](%s.png)", # displays as image in markdown
           model = "%s ~ %s",
           bigskip = "\n",
           medskip = "\n",
           `section*` = "## %s  {-}",
           section = "## %s", # Set Chapter as the highest heading, so section is second highest
           `subsection*` = "### %s {-}",
           subsection = "### %s",
           subsubsection = "#### %s",
           `subsubsection*` = "#### %s {-}",
           centerline = " %s \n",
           item = "#. ",
           code = "`%s`",
           texttt = "`%s`",
           frac = "\\frac{%s}{%s}",
           paragraph = "\n",
           # bookdown- and tufte-specific things
           ref = "\\@ref(%s)", # for bookdown references
           newthought = "`r tufte::newthought(\"%s\")`",
           marginnote = "`r tufte::margin_note(\"%s\")`",
           VerbatimInput = "```{r echo = FALSE, comment = ''}\ncat(detex::verbatim_input(\"%s\"))\n```\n",
           noindent = "",
           cite = "@%s", # bookdown citations
           R = "R ",
           RStudio = "RStudio ",
           em = "*%s*",
           bf = "**%s**",
           sqrt = "\\sqrt{%s}",
           cos = "\\cos",
           sin = "\\sin",
           pi = "\\pi",
           "function" = "`%s()`",
           pkg = "**`%s`**",
           pm = "plus-or-minus",
           modelValues = "*%s*",
           indicatorVar = "`%s%s`",
           TextEntry = "YOUR ANSWER HERE.",
           index = "`r detex::index_entry('%s', '%s')` ",
           newword = "`r detex::new_word('%s')`",
           vocab = "`r detex::new_word('%s')`",
           newworddef = "`r detex::new_word('%s')` `r detex::index_entry('%s')`",
           dataset = "`r detex::dataset('%s')`",
           datasetCPS = "`r detex::dataset(\"CPS\")`",
           matchSelect = "CHOICES %s:  CORRECT %s",
           
           ### Additions ###
           
           tab = '<span class="tab"> %s </span>',
           Sexpr = "`r %s`",
           setcounter = "`r %s <- %s`",
           Chapter = "# %s",
           textwidth = "`r fig.width`",
           href = "[%s](%s)",
           url = "%s",
           Rstudio = "RStudio ",
           emph = "_%s_", # Emphasis translated as italics
           vspace = "<br>", # Vertical Space
           underline = "**%s**", # Underlines are deprecated in Rmarkdown, replaced with bold
           term = "**%s**",
           dataframe = '<span style="color:blue">%s</span>',
           option = '<span style="color:brown">%s</span>',
           variable = '<span style="color:green">%s</span>',
           textbf = "**%s**",
           texttt = "<tt>%s</tt>",
           verb = "",
           textit = "*%s*",
           Rindex = "<tt>%s</tt> <!-- This term should be indexed -->",
           myindex = "<!-- %s This term should be indexed -->",
           noindent = "\n",
           footnote = "^[%s]",
           nocite = "<!-- citation not used --- %s -->",
           dots = "...",
           R = "R ",
           iftrue = "",
           fi = "<!-- Until Here -->",
           rule = "\n----\n",
           iffalse = "<!-- The block below should be commented out -->",
           argument = '<span style="color:brown">%s</span>',
           authNote = '<!-- AuthNote --- %s -->',
           '~' = '~ ',
           large = '<span style="font-size:larger;">%s</span>',
           label = '%s',
           
           ## End of Additions ###
           
           paste("TEX COMMAND NOT FOUND", command,
                 ifelse(!missing(arg1), arg1, ""),
                 ifelse(!missing(arg2), arg2, ""))
    )
  if("TEX COMMAND NOT FOUND" %in% format) {
    stop("Command Not Found!")
  }
  # fix situations where the format relies on an inline r chunk
  # and the arguments might have a backquote in them.
  if (grepl("^`", format)) {
    if (!missing(arg1)) arg1 <- gsub("`", " ", arg1)
    if (!missing(arg2)) arg2 <- gsub("`", " ", arg2)
  }
  
  if (missing(arg1)) return(format)
  else if (missing(arg2)) return(sprintf(format, arg1))
  else return(sprintf(format, arg1, arg2))
}

# Put {\em } font commands into \em{ } form.
inline_font_translate <- function(string) {
  tmp <- gsub("\\{\\\\em ", "\\\\em\\{", string)
  gsub("\\{\\\\bf ", "\\\\bf\\{", tmp)
}

 
