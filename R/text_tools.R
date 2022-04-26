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

#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text.
#' @param v Text variable/object
#' @return A list of dates in messydt format
#' @import stringr
#' @importFrom messydates as_messydate
#' @examples
#' v <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021")
#' extract_date(v)
#' @export
extract_date <- function(v) {
  # make all lower case
  out <- tolower(v)
  # remove commas
  out <- gsub(",", "", out)
  # remove ordinal signs and date related articles
  out <- gsub("de |of |st |nd |rd |th ", " ", out)
  # correct double white space left
  out <- stringr::str_squish(out)
  # get the first date per row
  out <- stringr::str_extract(out,
                              "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|
  |[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|
  |[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|
  |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|
  |[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|
  |[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|
  |[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
  |[:alpha:]{3}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{4}\\s[:digit:]{2}\\s[:digit:]{4}|
  |[:alpha:]{5}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{6}\\s[:digit:]{2}\\s[:digit:]{4}|
  |[:alpha:]{7}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{8}\\s[:digit:]{2}\\s[:digit:]{4}|
  |[:alpha:]{9}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{3}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:alpha:]{4}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{5}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:alpha:]{6}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{7}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:alpha:]{8}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{9}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|
  |[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|
  |[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|
  |[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|
  |[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|
  |[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|
  |[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|
  |[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|
  |[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|
  |[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|
  |[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|
  |[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{1}/[:digit:]{1}|
  |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{2}|
  |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
  |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{2}|
  |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{2}|
  |[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{1}|
  |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|
  |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{1}|
  |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{1}")
  # re-order dates if necessary
  out <- lapply(out, re_order)
  # get the months into numeric form
  months <- as.data.frame(months)
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # standardize separators
  out <- stringr::str_replace_all(out, " |/", "-")
  out <- stringr::str_replace_all(out, "[a-z]|[A-Z]", "?")
  out <- messydates::as_messydate(ifelse(out == "", NA, out))
  out
}

re_order <- function(l) {
  l <- stringr::str_squish(l)
  st <- stringr::word(l, 1)
  mi <- stringr::word(l, 2)
  ed <- stringr::word(l, 3)
  out <- ifelse(stringr::str_starts(l, "[:digit:]{4}"), paste0(ed, "-", mi, "-", st), l)
  out <- ifelse(stringr::str_starts(out, "[:alpha:]"), paste0(mi, "-", st, "-", ed), out)
  out <- stringr::str_remove_all(out, "-NA|NA-|NA")
  out
}
