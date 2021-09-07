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
#' @importFrom stringr str_detect
#' @importFrom stringdist stringsimmatrix
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
  qID <- qID %>% dplyr::distinct(qID)
  
  # Initialize variables to avoid CMD notes
  ID <- linkage <- ID1 <- ID2 <- dup <- year_type <- qID_ref <- NULL

  # step two: organize data
  similar <- qID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", qID), gsub(".*:", "", qID), NA),
                  ID1 = gsub("\\:.*", "", qID),
                  acronym = gsub("\\_.*", "", ID1),
                  year_type = gsub(".*_", "", ID1))
  
  # Step three: identify very similar acronyms
  fuzzy <- stringdist::stringsimmatrix(similar$acronym, similar$acronym)
  diag(fuzzy) <- 0
  similar$fuzzy <- apply(fuzzy, 1, max)
  similar$fuzzy <- ifelse(similar$fuzzy == 1, 0, similar$fuzzy)
  similar$fuzzy <- ifelse(stringr::str_detect(similar$acronym, "\\-"), 0, similar$fuzzy) 
  # works only for multilateral treaties at the moment.
  
  similar <- similar %>%
    dplyr::group_by_at(dplyr::vars(year_type)) %>%
    dplyr::mutate(
      dup = dplyr::row_number() > 1,
      ID2 = ifelse(dup, paste0(dplyr::first(ID1)), NA)) %>% 
    dplyr::group_by(ID2) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(line = dplyr::case_when(n != 1 ~ paste(ID2), n == 1 ~ "1"))
  
  # Step five: assign same acronyms to very similar observation qith the same year and type
  similar$ID <- ifelse(similar$fuzzy > 0.79 & similar$dup == TRUE, paste0(similar$ID2), paste0(similar$ID1))
  
  # Step four: Get linkages standardized and return only pertinent columns 
  similar <- similar %>% 
    dplyr::group_by(ID) %>% 
    tidyr::fill(linkage, .direction = "updown") %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(qID_ref = ifelse(is.na(linkage), ID, paste0(ID, ":", linkage))) %>%
    dplyr::select(qID, qID_ref)

  similar
}
