#' Linking metadata of original dataset when adding datasets to the qPackage
#'
#' This function saves original variable names and various other elements as
#' attributes to the imported objects to power the data_evolution() function
#' in qData. To be used in every data preparation script right after loading
#' the dataset in the environment.
#' 
#' @param dataset Name of the imported dataset.
#' @details This function returns the original dataset with the additional
#' "metadata_orig" attribute that will be used later by `data_evolution()`
#' to highlight changes that the dataset underwent during our import process.
#' @return This function returns the original dataset with the additional
#' "metadata_orig" attribute.
#' @examples
#' \dontrun{
#' link_metadata(COW)
#' @export

link_metadata <- function(dataset){
  colnames <- colnames(dataset)
  nobs <- nrow(dataset)
  ncol <- ncol(dataset)
  missdata <- paste0(round(sum(is.na(dataset)) * 100 / prod(dim(dataset)), digits = 2), " %")
  metadata_orig <- list(colnames, nobs, ncol, missdata)
  attr(dataset, "metadata_orig") <- metadata_orig
  return(dataset)
}
