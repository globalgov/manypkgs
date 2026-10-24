#' Checking whether functions work across different types of objects
#'
#' @description
#'   `functions_work()` calls each of the functions on each of the objects,
#'   and reports which calls run without an error.
#' @param functions A character vector of the names of functions.
#'   Each function must take the object as its first argument.
#' @param objects A character vector of the names of objects.
#' @return A logical matrix with a row for each function and
#'   a column for each object.
#'   An element is `TRUE` where the function runs on the object without an error.
#' @examplesIf rlang::is_installed("manynet")
#'  library(manynet)
#'  functions_work(c("net_nodes", "net_ties", "net_density"),
#'                 c("ison_adolescents", "ison_southern_women"))
#' @export
functions_work <- function(functions, objects){
  opts <- expand.grid(functions, objects, stringsAsFactors = FALSE)
  out <- apply(opts, 1, function(x) {
    tryCatch({
      get(x[1], mode = "function")(get(x[2]))
      TRUE
    }, error = function(e) FALSE)
  })
  out <- matrix(out, nrow = length(functions))
  rownames(out) <- functions
  colnames(out) <- objects
  out
}

#' Printing a help page to the console
#'
#' @description
#'   `print_help()` prints the help page of a function to the console,
#'   or into a knitr document, as text, HTML, LaTeX, or Rd.
#' @param topic The name of the function or help topic,
#'   as a character string or unquoted.
#' @param format The format to print the help page in,
#'   one of "text" (the default), "html", "latex", or "Rd".
#' @param lines An optional integer vector that selects the lines to print.
#' @param before,after Optional character vectors to print
#'   before or after the help page.
#' @return The lines of the help page, invisibly.
#' @source https://www.r-bloggers.com/2013/06/printing-r-help-files-in-the-console-or-in-knitr-documents/
#' @examples
#' print_help("depends", lines = 1:5)
#' @export
print_help <- function(topic, format = c("text", "html", "latex", "Rd"),
                       lines = NULL, before = NULL, after = NULL) {
  format <- match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  path <- as.character(utils::help(topic))
  if (length(path) == 0)
    stop("No help page found for ", topic, call. = FALSE)
  path <- path[1]
  pkg <- basename(dirname(dirname(path)))
  helpfile <- tools::Rd_db(pkg)[[paste0(basename(path), ".Rd")]]
  hs <- utils::capture.output(switch(format,
                                     text = tools::Rd2txt(helpfile),
                                     html = tools::Rd2HTML(helpfile),
                                     latex = tools::Rd2latex(helpfile),
                                     Rd = cat(as.character(helpfile), sep = "")
  ))
  if (!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep = "\n")
  invisible(hs)
}
