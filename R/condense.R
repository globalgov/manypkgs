#' Condense similar qIDs
#'
#' Different qIDs generated for different datasets
#' might have minor differences in terms of acronym or linkage.
#' Some minor differences in qIDs could mean different qIDS
#' in different datasets actually refer to the same agreement.
#' The function finds these occurences and returns the
#' first qID argument entered as a replacement.
#' @param ... Two or more qID variables
#' @import dplyr
#' @importFrom purrr map
#' @importFrom stringr str_detect str_trim
#' @importFrom stringdist stringsimmatrix
#' @return A dataframe of similar qIDs
#' @examples
#' data1 <- data.frame(qID = c("CPV-PRT[FSD]_1980A",
#' "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data2 <- data.frame(qID = c("TD06LJ_1981A", "RAMSA_1971A",
#' "WIIEWH_1982P:RAMSA_1971A",
#' "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
#' qID_ref <- condense_qID(data1$qID, data2$qID)
#' data1 <- merge(data1, qID_ref)
#' data2 <- merge(data2, qID_ref)
#' @export
condense_qID <- function(...) {

  # Step one: rbind variables and remove duplicates
  qID <- unlist(c(...))
  qID <- data.frame(qID = qID)
  qID <- qID %>%
    dplyr::distinct(qID) %>%
    dplyr::mutate(qID = stringr::str_trim(qID, "both"))

  # Initialize variables to avoid CMD notes
  ID <- linkage <- ID1 <- dup <- year_type <- qID_ref <- NULL

  # step two: split qID and organize data
  similar <- qID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", qID), gsub(".*:", "", qID), NA),
                  ID1 = gsub("\\:.*", "", qID),
                  acronym = gsub("\\_.*", "", ID1),
                  year_type = gsub(".*_", "", ID1))

  # Step three: identify very similar acronyms
  # Get similar qIDs using the Levenshtein distance
  fuzzy <- stringdist::stringsimmatrix(similar$acronym,
                                       similar$acronym, method = "lv")
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  rownames(fuzzy) <- similar$acronym
  colnames(fuzzy) <- similar$acronym
  # Add names for the very similar acronyms (1 letter change)
  fuzzy <- ifelse(fuzzy > 0.83, rownames(fuzzy), 0)
  # Tranform matrix into data frame
  fuzzy <- data.frame(match = colnames(fuzzy)[row(fuzzy)],
                      acronym = c(t(fuzzy)), stringsAsFactors = FALSE)
  # Keep only named obs
  fuzzy <- filter(fuzzy, acronym != 0)
  # Delete first match and keep only additional matches
  fuzzy <- fuzzy[as.character(fuzzy$match) < as.character(fuzzy$acronym), ]
  fuzzy$acronym <- as.character(fuzzy$acronym)
  # Join data
  similar <- dplyr::full_join(similar, fuzzy, by = "acronym")
  # Tranform NAs into 0
  similar$fuzzy <- ifelse(is.na(similar$match), 0, similar$match)
  # Remove bilaterals (works only for multilateral at the moment)
  similar$fuzzy <- ifelse(stringr::str_detect(similar$fuzzy, "\\-"),
                          0, similar$fuzzy)

  # Step five: assign same acronyms to very similar observation
  similar <- similar %>%
    dplyr::distinct(qID, .keep_all = TRUE) %>% # join can add duplication
    dplyr::group_by_at(dplyr::vars(year_type)) %>%
    dplyr::mutate(dup = dplyr::row_number() > 1,
                  ID = ifelse(fuzzy != 0 & dup == TRUE,
                              paste0(fuzzy, "_", year_type), ID1))

  # Step six: Get linkages standardized and return only pertinent columns
  similar <- similar %>%
    dplyr::group_by(ID) %>%
    tidyr::fill(linkage, .direction = "updown") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(qID_ref = stringr::str_trim((ifelse(is.na(linkage), ID,
                                                      paste0(ID, ":", linkage))),
                                              "both")) %>%
    dplyr::select(qID, qID_ref) %>%
    dplyr::distinct()

  similar
}
