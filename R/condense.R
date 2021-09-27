#' Condense similar qIDs
#'
#' Different qIDs generated for different datasets
#' might have minor differences in terms of acronym or linkage.
#' Some minor differences in qIDs could mean different qIDS
#' in different datasets actually refer to the same agreement.
#' The function finds these occurences and returns the
#' first qID argument entered as a replacement.
#' @param database A qPackage database
#' @param var Two or more qID variables
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stringr str_detect str_trim
#' @return A dataframe of qID and qID references
#' @examples
#' data1 <- data.frame(qID = c("CPV-PRT[FSD]_1980A",
#' "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data2 <- data.frame(qID = c("TD06LJ_1981A", "RAMSA_1971A",
#' "WIIEWH_1982P:RAMSA_1971A",
#' "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
#' qID_ref <- condense_qID(var = c(data1$qID, data2$qID))
#' data1 <- merge(data1, qID_ref)
#' data2 <- merge(data2, qID_ref)
#' @export
condense_qID <- function(database = NULL, var = NULL) {

  # Step one: identify if database is present
  if (is.null(var)) {
    qID <- lapply(database, function(x) x[["qID"]])
    qID <- unname(unlist(purrr::map(qID, as.character)))
  } else {
    qID <- unlist(var)
  }

  # Step two: rbind variables and remove duplicates
  qID <- data.frame(qID = qID)
  qID <- qID %>%
    dplyr::distinct(qID) %>%
    dplyr::mutate(qID = stringr::str_trim(qID, "both"))
  # Initialize variables to avoid CMD notes/issues
  ID <- linkage <- ID1 <- year_type <- qID_ref <- match_bt <- match_yt <- NULL

  # step two: split qID and organize data
  similar <- qID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", qID), gsub(".*:", "", qID), NA),
                  ID1 = gsub("\\:.*", "", qID),
                  acronym = as.character(gsub("\\_.*", "", ID1)),
                  parties = as.character(ifelse(stringr::str_detect(qID, "\\["),
                                                gsub("\\[.*", "", ID1), 0)),
                  year_type = gsub(".*_", "", ID1))

  # Step three: identify very similar acronyms, for multilateral treaties
  fuzzy <- fuzzy_agreements_multilateral(qID$qID)
  # Join data
  similar <- dplyr::full_join(similar, fuzzy, by = "acronym")
  # Tranform match NAs into 0
  similar$match <- ifelse(is.na(similar$match), 0, similar$match)

  # Step four: repeat same operations for bilateral treaties
  bt <- fuzzy_agreements_bilateral(qID$qID)
  # Join data
  similar <- dplyr::full_join(similar, bt, by = "acronym")
  # Tranform match NAs into 0
  similar$match_bt <- ifelse(is.na(similar$match_bt), 0, similar$match_bt)

  # Step five: re-organize data and assign fuzzy matches to observation
  similar <- similar %>%
    dplyr::distinct(qID, .keep_all = TRUE) %>% # join can add duplication
    dplyr::mutate(fuzzy = gsub("\\_.*", "", match),
                  match_yt = gsub(".*_", "", match),
                  fuzzy_bi = gsub("\\_.*", "", match_bt),
                  match_yt_bi = gsub(".*_", "", match_bt),
                  match_pt = gsub("\\[.*", "", match_bt)) %>%
    dplyr::mutate(ID = ifelse(fuzzy != 0 & match_yt == year_type,
                              paste0(fuzzy, "_", year_type), ID1))
  # Assign fuzzy matches to bilaterals
  similar$ID <- ifelse(similar$fuzzy_bi != 0 &
                         similar$match_yt_bi == similar$year_type &
                         similar$match_pt == similar$parties,
                       paste0(similar$fuzzy_bi, "_",
                              similar$year_type), similar$ID)

  # Step six: Get linkages standardized and return only pertinent columns
  similar <- similar %>%
    dplyr::group_by(ID) %>%
    tidyr::fill(linkage, .direction = "updown") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(qID_ref = stringr::str_trim((
      ifelse(is.na(linkage), ID, paste0(ID, ":", linkage))), "both")) %>%
    dplyr::select(qID, qID_ref) %>%
    dplyr::distinct()

  similar
}

#' Helper function for fuzzy matching multilateral agreements
#'
#' Fuzzy match agreements' acronyms for multilateral treaties
#' @details Extracts agreement acronyms from qIDs and fuzzy
#' match their similarities.
#' See `qCreate::code_acronym()` for more details on acronyms.
#' @param qID qID variable created with `qCreate::code_agreements()`
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringdist stringsimmatrix
#' @return A data frame with acronyms and qID matches without linkages
fuzzy_agreements_multilateral <- function(qID) {

  # Split qID
  ID <- as.character(gsub("\\:.*", "", qID))
  acronym <- as.character(gsub("\\_.*", "", qID))

  # Fuzzy match acronyms
  fuzzy <- stringdist::stringsimmatrix(acronym, acronym, method = "jaccard")
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  rownames(fuzzy) <- acronym
  colnames(fuzzy) <- ID
  # Add names for the very similar acronyms (1 letter change)
  fuzzy <- ifelse(fuzzy > 0.7, rownames(fuzzy), 0)
  # Tranform matrix into data frame
  fuzzy <- data.frame(match = colnames(fuzzy)[row(fuzzy)],
                      acronym = as.character(c(t(fuzzy))),
                      stringsAsFactors = FALSE)
  # Keep only named obs
  fuzzy <- dplyr::filter(fuzzy, acronym != 0)
  # Delete first match and keep only additional matches
  fuzzy <- fuzzy[as.character(fuzzy$match) < fuzzy$acronym, ]
  # Remove bilateral treaties
  fuzzy$acronym <- ifelse(stringr::str_detect(fuzzy$acronym, "\\-"),
                          0, fuzzy$acronym)
  fuzzy <- dplyr::filter(fuzzy, acronym != 0)
  # make sure returned acronym is character
  fuzzy$acronym <- as.character(fuzzy$acronym)
  fuzzy
}

#' Helper function for fuzzy matching bilateral agreements
#'
#' Fuzzy match agreements' acronyms for bilateral treaties
#' @details Extracts agreement acronyms from qIDs and fuzzy
#' match their similarities.
#' See `qCreate::code_acronym()` for more details on acronyms.
#' @param qID qID variable created with `qCreate::code_agreements()`
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringdist stringsimmatrix
#' @return A data frame with acronyms and qID matches without linkages
fuzzy_agreements_bilateral <- function(qID) {

  # Get acronyms and IDs from qIDs
  ID <- as.character(gsub("\\:.*", "", qID))
  acronym <- as.character(gsub("\\_.*", "", qID))

  # Fuzzy match acronyms
  fuzzy <- stringdist::stringsimmatrix(acronym, acronym)
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  rownames(fuzzy) <- acronym
  colnames(fuzzy) <- ID
  # Add names for the very similar acronyms (1 letter change)
  fuzzy <- ifelse(fuzzy > 0.8, rownames(fuzzy), 0)
  # Tranform matrix into data frame
  fuzzy <- data.frame(match_bt = colnames(fuzzy)[row(fuzzy)],
                      acronym = as.character(c(t(fuzzy))),
                      stringsAsFactors = FALSE)
  # Keep only named obs
  fuzzy <- dplyr::filter(fuzzy, acronym != 0)
  # Delete first match and keep only additional matches
  fuzzy <- fuzzy[as.character(fuzzy$match_bt) < fuzzy$acronym, ]
  # Remove multilateral treaties
  fuzzy$acronym <- ifelse(stringr::str_detect(fuzzy$acronym, "\\-",
                                              negate = TRUE),
                          0, fuzzy$acronym)
  fuzzy <- dplyr::filter(fuzzy, acronym != 0)
  # Make sure returns are character
  fuzzy$acronym <- as.character(fuzzy$acronym)
  fuzzy
}
