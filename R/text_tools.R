#' Convert treaty PDFs to text
#'
#' @param path The file path.
#' The path can also be specified to a folder with multiple PDF files,
#' in this case all PDF files in the folder will be converted.
#' @return A list with the converted texts.
#' @importFrom pdftools pdf_text pdf_ocr_text
#' @source https://stackoverflow.com/questions/71064939/how-to-check-if-pdf-is-scanned-image-or-contains-text-in-r
#' @export
treaty_pdf <- function(path) {
  files <- list.files(path, pattern = ".pdf$")
  out <- lapply(files, function(f) {
    out <- pdftools::pdf_text(f)
    contains_text <- nchar(out) > 15
    out <- ifelse(!contains_text, pdftools::pdf_ocr_text(f), out)
  })
  out
}
