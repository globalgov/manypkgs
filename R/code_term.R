#' Code treaty termination type
#'
#' Creates a column with the date and type of termination
#' of the treaty.
#' @param title title column variable.
#'
#' @return a character vector with the term date of the
#' treaty and type of termination
#' @importFrom stringr str_remove_all str_extract_all
#' @examples
#' GNEVAR$Term <- code_term(GNEVAR$Title)
code_term <- function(title, text = NULL) {
  # Step one: extract term date if present in treaty title
  title <- as.character(title)
  # All the treaties having the term date in the title had the word "For the Period"
  date <- ifelse(grepl("For The Period", title), stringr::str_extract_all(title, "\\d{1,2}.\\w{3,}.\\d{4}"), NA)
  # Extract only the term date
  date <- stringr::str_remove_all(date, "^c|\\(|\\)|\"")
  date <- stringr::str_extract_all(date, "\\d{1,2}.\\w{3,}.\\d{4}$")
  # Standardise the date format
  date <- as.Date(as.character(date), format = "%d %B %Y")
  date <- ifelse(!is.na(date), paste0(date, " Sunset/Expiry"), date)
  date
}
