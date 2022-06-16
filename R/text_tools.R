#' Standardise treaty texts
#' 
#' Standardise treaty texts by removing punctuation and markers,
#' while splitting these into articles and annexes. 
#' @param textvar A text variable.
#' @return A list of treaty sections of the same length.
#' @examples
#' \dontrun{
#' t <- standardise_texts(sample(manyenviron::texts$AGR_TXT$Text, 30))
#' }
#' @export
standardise_texts <- function(textvar) {
  t <- purrr::map(textvar, function(x) {
    x <- stringi::stri_trans_general(tolower(as.character(x)),
                                     id = "Latin-ASCII")
    x <- stringr::str_replace_all(x, "\nannex|\n annex|\\.\\sannex\\s|\\.annex\\s|
                                  |\\d\\sannex\\s"," ANNEX ")
    x <- stringr::str_replace_all(x, "\narticle|\n article|\nart\\.|\n art\\.|
                                  |\\.\\sarticle\\s|\\.article\\s", " ARTICLE ")
    x <- stringr::str_replace_all(x, "\nappendix|\n appendix|\\.\\sappendix\\s|
                                  |\\.appendix\\s", " APPENDIX ")
    x <- stringr::str_replace_all(x, "\nprotocol|\n protocol|\\.\\sprotocol\\s|
                                  |\\.protocol\\s|\\d\\sprotocol\\s", " PROTOCOL ")
    x <- stringr::str_remove_all(x, "<.*?>")
    x <- tm::stripWhitespace(x)
    x <- stringr::str_remove_all(x, "\r")
    x <- stringr::str_remove_all(x, "\t")
    x
  })
  t <- ifelse(lengths(t) == 0, NA_character_, t)
  t
}

#' Convert PDFs to text
#'
#' @param path The file(s) path.
#' The path can also be specified to a folder with multiple PDF files,
#' in this case all PDF files in the folder will be converted.
#' @return A list with the converted texts.
#' @importFrom pdftools pdf_text pdf_ocr_text
#' @importFrom stringi stri_trans_general
#' @importFrom tm stripWhitespace
#' @source https://stackoverflow.com/questions/71064939/how-to-check-if-pdf-is-scanned-image-or-contains-text-in-r
#' @examples
#' \dontrun{
#' treaty_pdf(path = getwd())
#' }
#' @export
treaty_pdf <- function(path) {
  # get all PDFs
  files <- list.files(path, pattern = ".pdf$")
  # transform to text if readable, or ocr if not
  pdfs <- lapply(files, function(f) {
    pdfs <- pdftools::pdf_text(f)
    contains_text <- nchar(pdfs) > 15
    pdfs <- ifelse(!contains_text, pdftools::pdf_ocr_text(pdfs), pdfs)
  })
  # unlist and do some pre-processing
  out <- lapply(pdfs, function(g) {
    out <- paste(g, collapse = " ")
    out <- stringi::stri_trans_general(out, "latin-ascii")
    out <- tm::stripWhitespace(out)
  })
  out
}
