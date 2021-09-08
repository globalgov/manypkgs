#' Code treaty termination date
#'
#' Creates a column with the date of termination
#' of the treaty.
#' @param title title column variable.
#' @param text treaty text column variable
#' @return a character vector with the term date of the
#' treaty
#' @importFrom stringr str_remove_all str_extract_all
#' @examples
#' GNEVAR <- dplyr::slice_sample(qEnviron::agreements$GNEVAR, n = 200)
#' GNEVAR$Term_dates <- code_term(GNEVAR$Title)
#' @export
code_term <- function(title, text = NULL) {
  # Step one: extract term date if present in treaty title
  title <- as.character(title)
  # Treaties with the term date in the title had the word "For the Period"
  date <- ifelse(grepl("For The Period", title),
                 stringr::str_extract_all(title, "\\d{1,2}.\\w{3,}.\\d{4}"), NA)
  # Extract only the term date
  date <- suppressWarnings(stringr::str_remove_all(date, "^c|\\(|\\)|\""))
  date <- stringr::str_extract_all(date, "\\d{1,2}.\\w{3,}.\\d{4}$")
  # Standardise the date format
  date <- as.Date(as.character(date), format = "%d %B %Y")
  date
  # TO DO: Step two: extract term date from treaty text
}
