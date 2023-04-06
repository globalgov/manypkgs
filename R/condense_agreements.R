#' Condense similar treatyIDs
#'
#' Different treatyIDs generated for different datasets
#' might have minor differences in terms of acronym or linkage.
#' Some minor differences in treatyIDs could mean different treatyIDs
#' in different datasets actually refer to the same agreement.
#' The function finds these occurrences and returns the
#' first treatyID argument entered as a replacement.
#' @param database A "many" package database
#' @param idvar Two or more treatyID variables
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stringr str_detect str_trim str_remove_all str_extract_all
#' @importFrom tidyr fill
#' @return A dataframe of treatyID and treatyID references
#' @examples
#' data1 <- data.frame(treatyID = c("CPV-PRT[FSD]_1980A",
#' "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data2 <- data.frame(treatyID = c("TD06LJ_1981A", "RAMSA_1971A",
#' "WIIEWH_1982P:RAMSA_1971A",
#' "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
#' condense_agreements(idvar = c(data1$treatyID, data2$treatyID))
#' @export
condense_agreements <- function(database = NULL, idvar = NULL) {
  # Initialize variables to avoid CMD notes/issues
  ID <- linkage <- ID1 <- year_type <- manyID <- match_bt <- match_yt <- NULL
  # Step one: identify if database is present
  if (is.null(idvar)) {
    treatyID <- lapply(database, function(x) x[["treatyID"]])
    treatyID <- unname(unlist(purrr::map(treatyID, as.character)))
  } else {
    treatyID <- unlist(idvar)
  }
  # Step two: rbind variables and remove duplicates
  treatyID <- data.frame(treatyID = treatyID) %>% 
    dplyr::distinct(treatyID) %>%
    dplyr::mutate(treatyID = stringr::str_trim(treatyID, "both"))
  # Step three: split treatyID and organize data
  similar <- treatyID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", treatyID), gsub(".*:", "",
                                                              treatyID), NA),
                  ID1 = gsub("\\:.*", "", treatyID),
                  acronym = as.character(ifelse(stringr::str_detect(treatyID, "\\["),
                                                NA, gsub("\\_.*", "", ID1))),
                  parties = as.character(ifelse(stringr::str_detect(treatyID,
                                                                    "\\["),
                                                gsub("*\\[.*", "", ID1), NA)),
                  year_type = gsub(".*_", "", ID1),
                  activity = stringr::str_remove_all(ifelse(
                    grepl("\\[", treatyID),
                    stringr::str_extract_all(treatyID, "\\[[^()]+\\]"),
                    NA_character_), "\\[|\\]"))
  # Step four: identify very similar acronyms and activities
  if(all(is.na(similar$parties))) {
    fuzzy <- fuzzy_agreements_multilateral(similar)
  } else if (all(is.na(similar$acronym))) {
    fuzzy <- fuzzy_agreements_bilateral(similar)
  } else {
    fuzzy <- dplyr::full_join(fuzzy_agreements_multilateral(similar),
                              fuzzy_agreements_bilateral(similar)) 
  }
  similar <- dplyr::full_join(similar, fuzzy, multiple = "all") # Join data
  # Transform match NAs into 0
  similar$match <- ifelse(is.na(similar$match), 0, similar$match)
  # Step five:assign fuzzy matches to observations and standardize linkages
  similar <- similar %>%
    dplyr::distinct(treatyID, .keep_all = TRUE) %>% # remove added duplication
    dplyr::mutate(fuzzy = gsub("\\_.*", "", match),
                  # separated matched IDs to check
                  match_yt = gsub(".*_", "", match),
                  match_pt = ifelse(grepl("\\-", match),
                                    gsub("\\[.*", "", match), 0),
                  ID = dplyr::case_when(
                    match_pt == 0 & fuzzy != 0 & match_yt == year_type ~
                      paste0(fuzzy, "_", year_type),
                    match_pt == parties & fuzzy != 0 & match_yt == year_type ~
                      paste0(fuzzy, "_", year_type), .default = ID1)) %>%
    dplyr::group_by(ID) %>%
    tidyr::fill(linkage, .direction = "updown") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(manyID = stringr::str_trim((
      ifelse(is.na(linkage), ID, paste0(ID, ":", linkage))), "both")) %>%
    dplyr::select(treatyID, manyID) %>%
    dplyr::distinct()
  similar
}

fuzzy_agreements_multilateral <- function(dat) {
  # Set variables
  dat <- tidyr::drop_na(dat, acronym)
  # Fuzzy match acronyms
  fuzzy <- stringdist::stringsimmatrix(dat$acronym, dat$acronym,
                                       method = "jaccard")
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  rownames(fuzzy) <- dat$acronym
  colnames(fuzzy) <- dat$ID1
  # Add names for the very similar acronyms (1 letter change)
  fuzzy <- ifelse(fuzzy > 0.75, rownames(fuzzy), 0)
  # Tranform matrix into data frame and keep only named obs
  fuzzy <- data.frame(match = colnames(fuzzy)[row(fuzzy)],
                      acronym = as.character(c(t(fuzzy))),
                      stringsAsFactors = FALSE) %>%
    dplyr::filter(acronym != 0)
  # Delete first match and keep only additional matches
  fuzzy <- fuzzy[as.character(fuzzy$match) < fuzzy$acronym, ]
  # Make sure returned acronym is character
  fuzzy$acronym <- as.character(fuzzy$acronym)
  fuzzy
}

fuzzy_agreements_bilateral <- function(dat) {
  # Set variables
  dat <- tidyr::drop_na(dat, activity)
  # Fuzzy match acronyms
  fuzzy <- stringdist::stringsimmatrix(dat$activity, dat$activity,
                                       method = "lv")
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  rownames(fuzzy) <- dat$activity
  colnames(fuzzy) <- dat$ID1
  # Add names for the very similar acronyms (1 letter change)
  fuzzy <- ifelse(fuzzy > 0.66, rownames(fuzzy), 0)
  # Transform matrix into data frame and keep only named obs
  fuzzy <- data.frame(match = colnames(fuzzy)[row(fuzzy)],
                      activity = as.character(c(t(fuzzy))),
                      stringsAsFactors = FALSE) %>%
    dplyr::filter(activity != 0)
  # Delete first match and keep only additional matches
  fuzzy <- fuzzy[as.character(fuzzy$match) < fuzzy$activity, ]
  # Make sure returns are character
  fuzzy$activity <- as.character(fuzzy$activity)
  fuzzy
}
