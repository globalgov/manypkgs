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
#' @return A dataframe of similar qIDs
#' @examples
#' data1 <- data.frame(qID = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data2 <- data.frame(qID = c("TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A",
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
  ID <- linkage <- ID1 <- ID2 <- dup <- year_type <- qID_ref <- NULL

  # step two: organize data
  similar <- qID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", qID), gsub(".*:", "", qID), NA),
                  ID1 = gsub("\\:.*", "", qID),
                  acronym = gsub("\\_.*", "", ID1),
                  year_type = gsub(".*_", "", ID1))

  # Step three: identify very similar acronyms
  fuzzy <- stringdist::stringsimmatrix(similar$acronym, similar$acronym, method = "lv")
  fuzzy <- ifelse(fuzzy == 1, 0, fuzzy)
  colnames(fuzzy) <- similar$acronym
  fuzzy <- ifelse(fuzzy > 0.83, colnames(fuzzy), 0)
  fuzzy <- data.frame(fuzzy = apply(fuzzy, 2, max),
                      acronym = similar$acronym)
  fuzzy <- fuzzy[as.character(fuzzy$fuzzy) < as.character(fuzzy$acronym),]
  similar <- dplyr::full_join(similar, fuzzy, by = "acronym")
  similar$fuzzy <- ifelse(is.na(similar$fuzzy), 0, similar$fuzzy)
  # Works only for multilateral at the moment
  similar$fuzzy <- ifelse(stringr::str_detect(similar$fuzzy, "\\-"), 0, similar$fuzzy)
  
  # Step five: assign same acronyms to very similar observation
  similar <- similar %>%
    dplyr::group_by_at(dplyr::vars(year_type)) %>%
    dplyr::mutate(dup = dplyr::row_number() > 1,
                  ID = ifelse(fuzzy != 0 & dup == TRUE, paste0(fuzzy, "_", year_type), ID1))
  
  # Step six: Get linkages standardized and return only pertinent columns 
  similar <- similar %>% 
    dplyr::group_by(ID) %>% 
    tidyr::fill(linkage, .direction = "updown") %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(qID_ref = stringr::str_trim((ifelse(is.na(linkage), ID, paste0(ID, ":", linkage))), "both")) %>%
    dplyr::select(qID, qID_ref) %>%
    dplyr::distinct()

  similar
}

