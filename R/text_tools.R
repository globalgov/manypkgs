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
