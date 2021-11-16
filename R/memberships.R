#' Get memberships' list
#'
#' Memberships database have countries column and 
#' treaty title column but information on the other
#' countries that are part to the treaties is often
#' missing.
#' The function allows to generate a dataframe of 
#' treaty IDs and the countries part to the treaty.
#' @param actor A country variable
#' @param id A treaty ID variable
#'
#' @return A dataframe of treaty IDs and countries
#' part of the treaty
#' @import dplyr
#' @examples
#' sample <- qEnviron::memberships$IEADB_MEM
#' get_memberships(actor = sample$CountryID, id = sample$qID_ref)
#' get_memberships(qEnviron::memberships)
#' @export
get_memberships <- function(database, actor, id){
  if (!missing(database)) {
    id <- unname(unlist(purrr::map(database, "qID_ref")))
    actor <- unname(unlist(purrr::map(database, "CountryID")))
    s <- cbind(actor, id)
    s <- as.data.frame(s)
    k <- s %>% 
      dplyr::group_by(id) %>% dplyr::summarise(Memberships = toString(actor)) %>% 
      dplyr::ungroup()
    s <- dplyr::left_join(s, k, by = "id") %>% 
      dplyr::select(id, Memberships) %>% unique()
    s
  }else {
  actor <- as.character(actor)
  id <- as.character(id)
  s <- cbind(actor, id)
  s <- as.data.frame(s)
  k <- s %>% 
    dplyr::group_by(id) %>% dplyr::summarise(Memberships = toString(actor)) %>% 
    dplyr::ungroup()
  s <- dplyr::left_join(s, k, by = "id") %>% 
    dplyr::select(id, Memberships)
  s
  }
}
