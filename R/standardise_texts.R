#' Standardise treaty texts
#'
#' Standardise treaty texts by removing punctuation and markers,
#' while splitting these into articles and annexes.
#' @param textvar A text variable.
#' @return A list of treaty sections of the same length.
#' @examples
#' \dontrun{
#' standardise_texts(sample(manyenviron::texts$AGR_TXT$Text, 30))
#' }
#' @export
standardise_texts <- function(textvar) {
  t <- purrr::map(textvar, function(x) {
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
