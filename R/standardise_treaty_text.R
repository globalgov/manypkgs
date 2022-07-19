#' Standardise treaty texts
#'
#' The function standardises treaty texts by removing punctuation and markers,
#' while splitting these texts into articles and annexes.
#' @name standardise_treaty_text
#' @param textvar A text variable.
#' @importFrom purrr map
#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom stringi stri_trans_general
#' @importFrom tm stripWhitespace
#' @return A list of treaty sections of the same length.
#' @details Treaty texts are not always similar when imported to R.
#' Some treaty texts, for example, contain paragraph markers while others
#' come in one text chunk.
#' `standardise_treaty_text()` facilitates the cleaning and annotation of
#' these treaty texts so that information about clauses can be retrieved
#' at a later stage with the `retrieve_clauses()` function.
#' @examples
#' \dontrun{
#' standardise_treaty_text(sample(manyenviron::texts$AGR_TXT$Text, 30))
#' }
#' @export
standardise_treaty_text <- function(textvar) {
  t <- purrr::map(textvar, function(x) {
    x <- unlist(x)
    x <- stringi::stri_trans_general(tolower(as.character(x)),
                                     id = "Latin-ASCII")
    x <- stringr::str_replace_all(x, "\nannex|\n annex|\\.\\sannex\\s|\\.annex\\s|
                                  |\\d\\sannex\\s", " ANNEX ")
    x <- stringr::str_replace_all(x, "\narticle|\n article|\nart\\.|\n art\\.|
                                  |\\.\\sarticle\\s|\\.article\\s", " ARTICLE ")
    x <- stringr::str_replace_all(x, "\nappendix|\n appendix|\\.\\sappendix\\s|
                                  |\\.appendix\\s", " APPENDIX ")
    x <- stringr::str_replace_all(x, "\nprotocol|\n protocol|\\.\\sprotocol\\s|
                                  |\\.protocol\\s|\\d\\sprotocol\\s",
                                  " PROTOCOL ")
    x <- stringr::str_remove_all(x, "<.*?>")
    x <- stringr::str_remove_all(x, "\r")
    x <- stringr::str_remove_all(x, "\t")
    x <- stringr::str_remove_all(x, "\n")
    x <- tm::stripWhitespace(x)
    x
  })
  t <- ifelse(lengths(t) == 0, NA_character_, t)
  t
}

#' @rdname standardise_treaty_text
#' @export
standardize_treaty_text <- standardise_treaty_text
