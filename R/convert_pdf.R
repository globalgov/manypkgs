#' Get and convert PDFs to text
#'
#' @param path The file(s) path.
#' The path can also be specified to a folder with multiple PDF files,
#' in that case all PDF files in the folder will be converted.
#' @return A list with the converted texts.
#' @details If PDF file is not in "readable" format, function attempts to apply
#' optical character recognition (OCR) to convert these to text.
#' @importFrom pdftools pdf_text pdf_ocr_text
#' @importFrom stringi stri_trans_general
#' @importFrom tm stripWhitespace
#' @source https://stackoverflow.com/questions/71064939/how-to-check-if-pdf-is-scanned-image-or-contains-text-in-r
#' @examples
#' \dontrun{
#' convert_pdf(path = getwd())
#' }
#' @export
convert_pdf <- function(path) {
  # get all PDFs
  files <- list.files(path, pattern = ".pdf$")
  # transform PDF to text if readable, or OCR these if not readable
  pdfs <- lapply(files, function(f) {
    pdfs <- pdftools::pdf_text(f)
    # check texts not properly converted so that we can try OCR in those
    contains_text <- nchar(pdfs) > 15
    pdfs <- ifelse(!contains_text, pdftools::pdf_ocr_text(pdfs), pdfs)
  })
  # unlist and do some basic pre-processing
  out <- lapply(pdfs, function(g) {
    out <- paste(g, collapse = " ")
    out <- stringi::stri_trans_general(out, "latin-ascii")
    out <- tm::stripWhitespace(out)
  })
  out
}
