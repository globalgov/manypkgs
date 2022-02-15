#' Get conditions and processes to accede memberships
#'
#' The function allows to get information on the conditions
#' for parties to enter a treaty.
#' It could be limited to a entity area,
#' a certain domain,
#' or on State party nominations for new members.
#' The function also allows to get information on the
#' process by which a new member has to go through in
#' order to accede memberships
#' @param t A text variable
#' @param title A title variable.
#' If not declared, NULL by default.
#' @param memberships The "condition" or "process"
#' for parties to accede a treaty.
#' If not declared, returns the "process".
#' @return Either the conditions to be part of the treaty
#' or the different process steps to become a member.
#' @importFrom dplyr case_when
#' @importFrom stringr str_remove_all str_trim
#' @examples
#' \donttest{
#' m <- manyenviron::texts$AGR_TXT[100:300,]
#' code_memberships(m$Text, m$Title, memberships = "condition")
#' code_memberships(m$Text, memberships = "process")
#' code_memberships()
#' }
#' @export
code_memberships <- function(t, title = NULL, memberships = NULL) {
  if (missing(t)) {
    me <- member
    me <- knitr::kable(me, "simple", caption = "Memberships process steps and criterion")
    me
  } else {
  # First step: select all the articles concerning memberships
  memb <- get_articles(t, article = "membership")
  if (isTRUE(memberships == "condition")) {
    # Second step: match terms to identify memberships conditions
    condition_1 <- dplyr::case_when(
      grepl("any government|any", memb, ignore.case = T) ~ "open",
      )
    condition_2 <- dplyr::case_when(
      grepl("nomination", memb, ignore.case = T) ~ "Semi-open",
      )
    condition_3 <- manypkgs::code_entity(title)
    condition_3 <- ifelse(!stringr::str_detect(condition_3, "NA"), paste0("entity: ", condition_3), NA)
    condition_4 <- manypkgs::code_domain(title)
    condition_4 <- ifelse(!stringr::str_detect(condition_4, "NA"), paste0("domain: ", condition_4), NA)
    condition <- paste0(condition_1, " + ", condition_2, " + ", condition_3, " + ", condition_4)
    condition <- stringr::str_remove_all(condition, "NA\\s\\+\\s|\\s\\+\\sNA")
    condition
  } else {
    # Third step: when the user select "process" instead of "condition",
    # the terms to detect specific processes to accede memberships match are used here
    # to create categories
    process_1 <- dplyr::case_when(
      grepl("open for signature", memb, ignore.case = T) ~ "signature",
      )
    process_2 <- dplyr::case_when(
      grepl("ratification", memb, ignore.case = T) ~ "ratification",
    )
    process_3 <- dplyr::case_when(
      grepl("accession shall be notified|notified|any notification|receipt of any notice", memb, ignore.case = T) ~ "notification",
      )
    process_4 <- dplyr::case_when(
      grepl("two-thirds majority of its membership", memb, ignore.case = T) ~ "majority vote",
      )
    process_5 <- dplyr::case_when(
      grepl("unanimity", memb, ignore.case = T) ~ "unanimity",
      )
    process <- paste0(process_1, " + ", process_2, " + ", process_3, " + ", process_4, " + ", process_5)
    process <- stringr::str_remove_all(process, "\\+ NA|NA \\+")
    process <- stringr::str_trim(process)
    process
  }
  }
}

#' Get memberships' list
#'
#' Memberships database have actor column(s) and
#' treaty column(s) but information on the other
#' countries that are part to the treaties is often
#' hard to read.
#' The function allows to generate a dataframe of
#' treaty IDs and actors part to the treaty.
#' @param database A database
#' @param actor A actor variable (e.g. country)
#' @param id A treaty ID variable
#' @return A dataframe of treaty IDs and actors
#' part of the treaty
#' @import dplyr
#' @examples
#' \dontrun{
#' sample <- manyenviron::memberships$IEADB_MEM
#' get_memberships(actor = sample$CountryID, id = sample$manyID)
#' get_memberships(qEnviron::memberships)
#' }
#' @export
get_memberships <- function(database, actor, id) {
  Memberships <- NULL
  if (!missing(database)) {
    id <- unname(unlist(purrr::map(database, "manyID")))
    actor <- unname(unlist(purrr::map(database, "CountryID")))
    s <- cbind(actor, id)
    s <- as.data.frame(s)
    k <- s %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(Memberships = toString(actor)) %>%
      dplyr::ungroup()
    s <- dplyr::left_join(s, k, by = "id") %>%
      dplyr::select(id, Memberships) %>% unique()
    s
  } else {
    actor <- as.character(actor)
    id <- as.character(id)
    s <- cbind(actor, id)
    s <- as.data.frame(s)
    k <- s %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(Memberships = toString(actor)) %>%
      dplyr::ungroup()
    s <- dplyr::left_join(s, k, by = "id") %>%
      dplyr::select(id, Memberships)
    s
  }
}
