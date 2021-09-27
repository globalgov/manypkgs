#' Code Treaty Termination Type
#'
#' Creates a column with the type of termination
#' of the treaty.
#' @param title title column variable
#' @param text treaty text column variable
#' @return a character vector with the termination clause type
#' of the treaty
#' @examples
#' GNEVAR <- dplyr::slice_sample(qEnviron::agreements$GNEVAR, n = 200)
#' GNEVAR$Term_Type <- code_grounds(GNEVAR$Title)
#' @export
code_grounds <- function(title, text = NULL) {

  if (missing(title)) {
    # If missing argument, function returns list of termination clause types
    type <- as_tibble(termination_type)
    type$meaning[7] <- paste(substr(type$meaning[7], 0, 100), "...")
    type$meaning[10] <- paste(substr(type$meaning[10], 0, 100), "...")
    type$meaning[11] <- paste(substr(type$meaning[11], 0, 100), "...")
    type$meaning[12] <- paste(substr(type$meaning[12], 0, 100), "...")
    type <- knitr::kable(type, "simple",
                         caption = "Treaty Termination Type, source: https://www.srdlawnotes.com/2017/08/termination-of-treaties.html")
    type
  }  else {
    # Step one: if the term date is mentioned in the treaty title,
    # it should be classified as "Sunset/Expiry"
    title <- as.character(title)
    # All the treaties having the term date in the title had the
    # word "For the Period"
    type <- ifelse(grepl("For The Period", title) &
                     grepl("\\d{1,2}.\\w{3,}.\\d{4} To \\d{1,2}.\\w{3,}.\\d{4}", 
                           title, ignore.case = T), paste0("EXP"), NA)
    type
  }
}
