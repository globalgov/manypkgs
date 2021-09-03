#' Condense similar qIDs
#'
#' Different qIDs generated for different datasets
#' might have minor differences in terms of acronym or linkage. 
#' Some minor differences in qIDs could mean different qIDS
#' in different datasets actually refer to the same agreement.
#' The function finds these occurences and returns a
#' standardized qID replacement.
#' @param ... Two or more qID variables
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom purrr map
#' @return A dataframe of similar qIDs
#' @examples
#' data <- data.frame(qID = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A",
#' "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
#' data1 <- data.frame(qID = c("TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A",
#' "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
#' condense_qID(data$qID, data1$qID)
#' @export
condense_qID <- function(...) {

  # Step one: rbind variables and remove duplicates
  qID <- unlist(c(...))
  qID <- data.frame(qID = qID)
  qID <- qID %>% dplyr::distinct(qID)
  
  # Initialize variables to avoid CMD notes
  ID <- NULL
  linkage <- NULL

  # step two: Split qIDs and get linkages standardized
  similar <- qID %>%
    dplyr::mutate(linkage = ifelse(grepl(":", qID), gsub(".*:", "", qID), NA),
                  ID = gsub("\\:.*", "", qID)) %>% 
   dplyr::group_by(ID) %>% 
   tidyr::fill(linkage, .direction = "updown") %>% 
   ungroup() %>%
    dplyr::mutate(ref = ifelse(is.na(linkage), ID, paste0(ID, ":", linkage)))

  similar
}
