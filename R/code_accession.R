#' Get conditions and processes to accede memberships
#'
#' The function extracts information on the conditions for parties to become
#' part of a treaty or the different processes to become a treaty member.
#' @param textvar A text variable
#' @param title A title variable.
#' NULL by default.
#' @param accession The "condition" or "process" for parties to accede a treaty.
#' "process" by default.
#' @return A list of the accession conditions or processes.
#' @details The function helps provide metadata on accession to
#' treaties allowing researchers to connect and compare this information.
#' The processes relate to how new members accede treaty
#' and include treaty signature, treaty ratification,
#' notification of consent, or majority vote.
#' The conditions for treaty accession include entity type or area,
#' a certain issue-domain, open membership for all,
#' or nomination of new members.
#' @importFrom dplyr case_when
#' @importFrom stringr str_remove_all str_trim
#' @examples
#' \dontrun{
#' m <- manyenviron::agreements$HUGGO[200:300,] %>%
#'      dplyr::select(Title, Beg, MainText, AppendixText, AnnexText)
#' code_accession_terms(m$MainText, m$Title, accession = "condition")
#' code_accession_terms(m$MainText, accession = "process")
#' code_accession_terms()
#' }
#' @export
code_accession_terms <- function(textvar, title = NULL, accession = NULL) {
  if (missing(textvar)) {
    return(member)
  } else {
    # First step: select all the articles concerning accession
    memb <- read_clauses(standardise_treaty_text(textvar),
                         article = "accession")
    if (isTRUE(accession == "condition")) {
      # Second step: match terms to identify accession conditions
      condition_1 <- dplyr::case_when(grepl("a government|any government|
                                            |all governments|all states|
                                            |any state",
                                            memb, ignore.case = T) ~ "open", )
      condition_2 <- dplyr::case_when(grepl("nomination", memb,
                                            ignore.case = T) ~ "Semi-open", )
      condition_3 <- manypkgs::code_entity(title)
      condition_3 <- ifelse(!stringr::str_detect(condition_3, "NA"),
                          paste0("entity: ", condition_3), NA)
      condition_4 <- manypkgs::code_domain(title)
      condition_4 <- ifelse(!stringr::str_detect(condition_4, "NA"),
                          paste0("domain: ", condition_4), NA)
      condition <- paste0(condition_1, " + ", condition_2, " + ",
                        condition_3, " + ", condition_4)
      condition <- stringr::str_remove_all(condition, "NA\\s\\+\\s|\\s\\+\\sNA")
      condition
  } else {
    # Third step: when the user select "process" instead of "condition",
    # the terms to detect specific processes to accede accession match are
    # used here to create categories
    process_1 <- dplyr::case_when(grepl("open for signature", memb,
                                        ignore.case = T) ~ "signature", )
    process_2 <- dplyr::case_when(grepl("ratification|ratified", memb,
                                        ignore.case = T) ~ "ratification", )
    process_3 <- dplyr::case_when(grepl("accession shall be notified|any notification|
                                        |receipt of any notice|notified.*application|
                                        |shall notify", memb,
                                        ignore.case = T) ~ "notification", )
    process_4 <- dplyr::case_when(grepl("two[-]?thirds majority", memb,
                                        ignore.case = T) ~ "majority vote", )
    process_5 <- dplyr::case_when(grepl("unanimity|unanimous|unanimously", memb,
                                        ignore.case = T) ~ "unanimity", )
    process <- paste0(process_1, " + ", process_2, " + ", process_3, " + ",
                      process_4, " + ", process_5)
    process <- stringr::str_remove_all(process, "\\+ NA|NA \\+")
    process <- stringr::str_trim(process)
    process
  }
  }
}
