#' Get memberships' list
#'
#' @param country A country variable
#' @param dataset_id A treaty ID variable
#'
#' @return A list of countries part of the treaty
#' @import dplyr
#' @examples
#' IEADB_MEM$Memberships <- get_memberships(IEADB_MEM$CountryID, IEADB_MEM$IEADB_ID)
#' @export
get_memberships <- function(country, dataset_id){
  country <- as.character(country)
  dataset_id <- as.character(dataset_id)
  s <- cbind(country, dataset_id)
  s <- as.data.frame(s)
  k <- s %>% 
    dplyr::group_by(dataset_id) %>% dplyr::summarise(Memberships = toString(country)) %>% 
    dplyr::ungroup()
  s <- dplyr::left_join(s, k, by = "dataset_id")
  s <- s$Memberships
  s
}
