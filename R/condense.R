#' Condense similar treaty_IDs
#'
#' Different treaty_IDs generated for different datasets
#' might have minor differences in terms of acronym or linkage.
#' Some minor differences in treaty_IDs could mean different treaty_IDS
#' in different datasets actually refer to the same agreement.
#' The function finds these occurences and returns the
#' first treaty_ID argument entered as a replacement.
#' @param database A "many" package database
#' @param var Two or more treaty_ID variables
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stringr str_detect str_trim
#' @importFrom tidyr fill
#' @return A dataframe of treaty_ID and treaty_ID references
#' @examples
#' data1 <- data.frame(treaty_ID = c("CPV-PRT[FSD]_1980A",
#' "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data2 <- data.frame(treaty_ID = c("TD06LJ_1981A", "RAMSA_1971A",
#' "WIIEWH_1982P:RAMSA_1971A",
#' "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
#' many_ID <- condense_agreements(var = c(data1$treaty_ID, data2$treaty_ID))
#' data1 <- merge(data1, many_ID)
#' data2 <- merge(data2, many_ID)
#' @export
condense_agreements <- function(database = NULL, var = NULL) {

  # Step one: identify if database is present
  if (is.null(var)) {
    treaty_ID <- lapply(database, function(x) x[["treaty_ID"]])
    treaty_ID <- unname(unlist(purrr::map(treaty_ID, as.character)))
  } else {
    treaty_ID <- unlist(var)
  }

  # Step two: rbind variables and remove duplicates
  treaty_ID <- data.frame(treaty_ID = treaty_ID)
  treaty_ID <- treaty_ID %>%
    dplyr::distinct(treaty_ID) %>%
    dplyr::mutate(treaty_ID = stringr::str_trim(treaty_ID, "both"))
  # Initialize variables to avoid CMD notes/issues
  ID <- linkage <- ID1 <- year_type <- many_ID <- match_bt <- match_yt <- NULL

  # step two: split treaty_ID and organize data
  similar <- treaty_ID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", treaty_ID), gsub(".*:", "", treaty_ID), NA),
                  ID1 = gsub("\\:.*", "", treaty_ID),
                  acronym = as.character(gsub("\\_.*", "", ID1)),
                  parties = as.character(ifelse(stringr::str_detect(treaty_ID, "\\["),
                                                gsub("\\[.*", "", ID1), 0)),
                  year_type = gsub(".*_", "", ID1))

  # Step three: identify very similar acronyms, for multilateral treaties
  fuzzy <- fuzzy_agreements_multilateral(treaty_ID$treaty_ID)
  # Join data
  similar <- dplyr::full_join(similar, fuzzy, by = "acronym")
  # Tranform match NAs into 0
  similar$match <- ifelse(is.na(similar$match), 0, similar$match)

  # Step four: repeat same operations for bilateral treaties
  bt <- fuzzy_agreements_bilateral(treaty_ID$treaty_ID)
  # Join data
  similar <- dplyr::full_join(similar, bt, by = "acronym")
  # Tranform match NAs into 0
  similar$match_bt <- ifelse(is.na(similar$match_bt), 0, similar$match_bt)

  # Step five: re-organize data and assign fuzzy matches to observation
  similar <- similar %>%
    dplyr::distinct(treaty_ID, .keep_all = TRUE) %>% # join can add duplication
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
    dplyr::mutate(many_ID = stringr::str_trim((
      ifelse(is.na(linkage), ID, paste0(ID, ":", linkage))), "both")) %>%
    dplyr::select(treaty_ID, many_ID) %>%
    dplyr::distinct()

  similar
}

#' Helper function for fuzzy matching multilateral agreements
#'
#' Fuzzy match agreements' acronyms for multilateral treaties
#' @details Extracts agreement acronyms from treaty_IDs and fuzzy
#' match their similarities.
#' See `manypkgs::code_acronym()` for more details on acronyms.
#' @param treaty_ID treaty_ID variable created with `manypkgs::code_agreements()`
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringdist stringsimmatrix
#' @return A data frame with acronyms and treaty_ID matches without linkages
fuzzy_agreements_multilateral <- function(treaty_ID) {

  # Split treaty_ID
  ID <- as.character(gsub("\\:.*", "", treaty_ID))
  acronym <- as.character(gsub("\\_.*", "", treaty_ID))

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
#' @details Extracts agreement acronyms from treaty_IDs and fuzzy
#' match their similarities.
#' See `manypkgs::code_acronym()` for more details on acronyms.
#' @param treaty_ID treaty_ID variable created with `manypkgs::code_agreements()`
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringdist stringsimmatrix
#' @return A data frame with acronyms and treaty_ID matches without linkages
fuzzy_agreements_bilateral <- function(treaty_ID) {

  # Get acronyms and IDs from treaty_IDs
  ID <- as.character(gsub("\\:.*", "", treaty_ID))
  acronym <- as.character(gsub("\\_.*", "", treaty_ID))

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
