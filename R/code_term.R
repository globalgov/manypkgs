#' Code treaty termination date
#'
#' Creates a column with the date of termination
#' of the treaty.
#' @param title title column variable.
#' @param text treaty text column variable
#'
#' @return a character vector with the term date of the
#' treaty
#' @importFrom stringr str_remove_all str_extract_all
#' @examples
#' GNEVAR$Term_Date <- code_term_date(GNEVAR$Title)
code_term_date <- function(title, text = NULL) {
  # Step one: extract term date if present in treaty title
  title <- as.character(title)
  # All the treaties having the term date in the title had the word "For the Period"
  date <- ifelse(grepl("For The Period", title), stringr::str_extract_all(title, "\\d{1,2}.\\w{3,}.\\d{4}"), NA)
  # Extract only the term date
  date <- suppressWarnings(stringr::str_remove_all(date, "^c|\\(|\\)|\""))
  date <- stringr::str_extract_all(date, "\\d{1,2}.\\w{3,}.\\d{4}$")
  # Standardise the date format
  date <- as.Date(as.character(date), format = "%d %B %Y")
  date
  # TO DO: Step two: extract term date from treaty text
}


#' Code treaty termination type
#'
#' Creates a column with the type of termination
#' of the treaty.
#' @param title title column variable
#' @param text treaty text column variable
#'
#' @return a character vector with the termination clause type
#' of the treaty
#' @examples
#' GNEVAR$Term_Type <- code_term_type(GNEVAR$Title)
code_term_type <- function(title, text = NULL){
  # Step one: if the term date is mentioned in the treaty title,
  # it should be classified as "Sunset/Expiry"
  title <- as.character(title)
  # All the treaties having the term date in the title had the
  # word "For the Period"
  type <- ifelse(grepl("For The Period", title) & grepl("\\d{1,2}.\\w{3,}.\\d{4} To \\d{1,2}.\\w{3,}.\\d{4}", title, ignore.case = T), paste0("Sunset/Expiry"), NA)
  type
  # TO DO: Step two: use treaty text to identify which termination
  # clause type the treaty has.
}
