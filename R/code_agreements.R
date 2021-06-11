#' Code Agreement Titles
#'
#' Creates an ID that contains information on the
#' parties to an agreement, the type of agreement, the date
#' and the linkage to other agreements in the dataset.
#' @param dataset dataset name.
#' If provided without a title and date column,
#' the function finds title and date conforming columns
#' in the dataset automatically if available.
#' @param title title column variable.
#' Ideally, title variable should come from a qPackage database/dataset
#' for which strings were standardised with `standardise_titles()`
#' @param date date column variable
#' Ideally, date variable should come from a qPackage database/dataset
#' for which dates were standardised with `standardise_dates()`
#' @return a character vector with the qIDs
#' @importFrom usethis ui_done
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr map
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_agreements(dataset = IEADB)
#' code_agreements(title = IEADB$Title, date = IEADB$Signature)
#' @export
code_agreements <- function(dataset = NULL, title, date) {
  
  if (is.null(dataset) & missing(title) & missing(date)) {
    stop("Please declare a dataset or title and date columns.")
  }
  
  if (!is.null(dataset) & missing(title) & missing(date)) {
    if (exists("Title", dataset) & exists("Signature", dataset)) {
      title <- dataset$Title
      date <- dataset$Signature
      usethis::ui_done("Title and date conforming columns in dataset automatically found")
    } else if (!exists("Title", dataset) | !exists("Signature", dataset)) {
      stop("Unable to find both 'Title' and 'Signature' columns in dataset. 
         Please declare the name of these columns or rename them.")
    }
  }
  
  # Step one: create a new qID column
  qID <- purrr::map(title, as.character)
  # Eventually this will connect to a centralized GitHub repo or SQL file for
  # smarter, interactive and more consistent coding of agreement titles.
  
  # Step two: code parties if present
  parties <- code_parties(qID)
  usethis::ui_done("Coded agreement parties")
  
  # Step three: code agreement type
  type <- code_type(qID)
  usethis::ui_done("Coded agreement type")
  
  # Step four: code known agreements
  abbrev <- code_known_agreements(qID)
  usethis::ui_done("Coded known agreements")
  
  # Step five: give the observation a unique ID and acronym
  uID <- code_dates(date)
  action <- code_action(title)
  usethis::ui_done("Coded agreement dates")
  
  #Step six: code acronyms from titles
  acronym <- code_acronym(title)
  usethis::ui_done("Coded acronyms for agreements")
  
  # Step seven: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  usethis::ui_done("Coded agreement linkages")
  
  # Step eight: add items together correctly
  out <- vector(mode = "character", length = length(title)) # initialize vector
  # for agreements (A) where abrreviation is known
  qID <- ifelse(!is.na(abbrev) & (type == "A"), paste0(abbrev, type), out)
  # when abbreviation is known but treaty type is not agreement
  qID <- ifelse(!is.na(abbrev) & (type != "A"), paste0(acronym, "_", uID, type, ":", line), qID)
  # when parties were not identified and treaty type is agreement (A)
  qID <- ifelse(is.na(parties) & (type == "A") & is.na(abbrev), paste0(acronym, "_", uID, type), qID)
  # when parties were not identified and type is not agreement
  qID <- ifelse(is.na(parties) & (type != "A"), paste0(acronym, "_", uID, type, ":", line), qID)
  # when parties were identified and type is agreement (A)
  qID <- ifelse(!is.na(parties) & (type == "A"), paste0(parties, "_", uID, type, action), qID)
  # when parties were identified and type is not agreement
  qID <- ifelse(!is.na(parties) & (type != "A"), paste0(parties, "_", uID, type, action, ":", line), qID)
  # deletes empty line or linkage
  qID <- stringr::str_remove_all(qID, "_$")
  qID <- stringr::str_remove_all(qID, ":$")
  # makes sure NAs are standard
  qID <- stringr::str_replace_all(qID, "NA_", NA_character_)
  
  cat(sum(is.na(qID)), "entries were not matched at all.\n")
  cat("There were", sum(duplicated(qID, incomparables = NA)), "duplicated IDs.\n")
  usethis::ui_done("Please run `vignette('agreements')` for more information.")
  
  qID
  
}

#' Code Agreement Parties
#'
#' Identify the countries that are part of an agreement.
#' @param title A character vector of treaty titles
#' @importFrom qStates code_states
#' @importFrom stringr str_replace_all
#' @return A character vector of parties that are mentioned in the treaty title
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_parties(IEADB$Title)
#' @export
code_parties <- function(title) {
  
  parties <- qStates::code_states(title)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  
  # Only considers bilateral agreements where two parties have been identified
  parties <- ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"), parties,
                    ifelse(stringr::str_detect(parties, "^[:alpha:]{2}-[:alpha:]{3}$"), parties,
                           ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{2}$"), parties, NA)))
  
  parties
  
}

#' Code Agreement Type
#'
#' Identify the type of international agreement from titles.
#' Agreements can be, for example, multilateral treaties or coventions (A),
#' protocols (P) or amendments (E), if they contain words in title.
#' @param title A character vector of treaty title
#' @return A character vector of the treaty type
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract str_replace_na word
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_type(IEADB$Title)
#' @export
code_type <- function(title) {
  
  # Get types from title
  type <- gsub("protocol|additional|subsidiary|supplementary|
               complementaire|complementar|complementario|annex |annexes",
               "PROTO", title, ignore.case = TRUE)
  type <- gsub("amendment|modify|extend|proces-verbal|amend", "AMEND",
               type, ignore.case = T)
  type <- gsub("agreement|arrangement|accord|acuerdo|bilateral co|
               technical co|treat|trait|tratado|convention|convencion|
               convenio|constitution|charte|instrument|statute|estatuto|
               provisional understanding|provisions relating|ubereinkunft|
               Act|Covenant|Scheme|Government Of|Law",
               "AGREE", type, ignore.case = T)
  type <- gsub("amendment|modify|extend|proces-verbal|amend", "AMEND",
               type, ignore.case = T)
  type <- gsub("Exchange|Letters|Notas|Minute|Adjustment|First Session Of|
               First Meeting Of|Commission|Committee|Center", "NOTES",
               type, ignore.case = T)
  type <- gsub("Memorandum|memorando|Principles of Conduct|Code of Conduct|Strategy|
               Plan|Program|Improvement|Project|Study|Working Party|Working Group", "STRAT",
               type, ignore.case = T)
  type <- gsub("Agreed Measures|Agreed Record|Consensus|Conclusions|
                Decision|Directive|Regulation|Reglamento|Resolution|
                Rules|Recommendation|Statement|Communiq|Comminiq|
                Joint Declaration|Declaration|Proclamation|Administrative Order", "RESOL",
               type, ignore.case = T)
  
  # EXtract only first type
  type <- stringr::str_extract_all(type, "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
  type <- gsub("c\\(", "", type)
  type <- stringr::word(type, 1)
  
  # Assign type abbreviations
  type <- dplyr::case_when(
    grepl("PROTO", type) ~ "P", # protocol
    grepl("AMEND", type) ~ "E", # amendment
    grepl("AGREE", type) ~ "A", # agreement
    grepl("NOTES", type) ~ "N", # notes
    grepl("STRAT", type) ~ "S", # strategy
    grepl("RESOL", type) ~ "R", # resolution
  )
  
  # Extracts meaningful ordering numbers for protocols and amendments
  number <- order_agreements(title)
  
  # When no type is found
  type <- stringr::str_replace_na(type, "O")
  
  type <- paste0(type, number)
  
  type
  
}

#' Code Actions for Titles
#'
#' Identifies actions performed by agreements
#' so that bidevtleteral treaties can be better distinguished.
#' @param title A character vector of treaty title
#' @importFrom stringr str_remove_all
#' @importFrom purrr map
#' @importFrom dplyr group_by mutate
#' @details Actions of agreements help differentiate date duplicates
#' in the same dataset as different treaties.
#' For the complete list of action and their 2 letter abbreviations
#' please refer to the actions list available in sysdata.
#' @return A character vector with 2 letter action abbreviations for date duplicates
#' @export
code_action <- function(title) {
  
  # Get issues list
  action <- purrr::map(action, as.character)
  # Assign the specific issue abbreviation to agreements
  iss <- sapply(action$words, function(x) grepl(x, title, ignore.case = T, perl = T)*1)
  colnames(iss) <- paste0(action$action)
  rownames(iss) <- paste0(title)
  out <- apply(iss, 1, function(x) paste(names(x[x==1])))
  out[out=="character(0)"] <- NA_character_
  out <- unname(out)
  out <- as.character(out)
  # Extracts only the first action detected
  out <- ifelse(grepl("c\\(", out), substr(out, 4, 5), out)
  
  # If output is a list with no values, returns an empty list of the same length as title variable
  lt <- as.numeric(length(title))
  ifelse(length(out) == 0, out <- rep(NA_character_, lt), out)
  
  out
  
  action <- ifelse(is.na(out), "", out)
  action <- ifelse(action == "", action, paste0("[", action, "]"))

  action
  
}

#' Creates Numerical IDs from Signature Dates
#'
#' Agreements should have a unique identification number that is meaningful,
#' we condense their signature dates to produce this number.
#' @param date A date variable
#' @return A character vector with condensed dates
#' @import stringr
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_dates(IEADB$Title)
#' @export
code_dates <- function(date) {

  uID <- stringr::str_remove_all(date, "-")
  
  # For treaties without signature date, 9999 is assigned as a year and along
  # 4 ramdom to facilitate identification of missing dates without
  # making observations duplicates.
  # uID[is.na(uID)] <- paste0("9999",
  #                           sample(1000:9999, sum(is.na(uID)), replace = TRUE))
  # In some cases all dates are incomplete (e.g. year only) and become a range, 
  # a lot of false duplicates might be created. In this case, we assign
  # some specific letters from the titles to differentiate treaties.
  # A <- suppressWarnings(stringr::str_extract_all(title, "^[:alpha:]"))
  # A <- suppressWarnings(stringr::str_to_upper(A))
  # B <- stringr::str_sub(title, start = 19, end = 19)
  # B <- suppressWarnings(ifelse(stringr::str_detect(B, "\\s"), "L", B))
  # B <- stringr::str_to_upper(B)
  # C <- stringr::str_extract_all(title, "[:alpha:]$")
  # C <- suppressWarnings(ifelse(!stringr::str_detect(C, "^[:alpha:]$"), "O", C))
  # C <- stringr::str_to_upper(C)

  # NA dates will appear as far future dates to facilitate identification
  uID[is.na(uID)] <- paste0(sample(5000:9999, 1), "NULL")
  # Ranges are removed first year is taken
  uID <- stringr::str_replace_all(uID, "\\:[:digit:]{8}$", "")
  # keep year only
  uID <- ifelse(nchar(uID) > 4, substr(uID, 1, nchar(uID) - 4), uID)

  uID

}

#' Known agreements abbreviation
#'
#' Some agreements have known abbreviations that facilitate their identification.
#' @param title A character vector of treaty title
#' @return A character vector of abbreviation of known treaties
#' @importFrom dplyr case_when
#' @importFrom purrr map
#' @details The function identifies agreements that match
#' the list of known agreements with their titles, abbreviations
#' and signature dates and substitutes the known titles for abbreviations.
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_known_agreements(IEADB$Title)
#' @export
code_known_agreements <- function(title) {
  
  abbreviations <- purrr::map(abbreviations, as.character)
  # Assign the specific abbreviation to the "known" treaties
  ab <- sapply(abbreviations$title, function(x) grepl(x, title, ignore.case = T, perl = T)*1)
  colnames(ab) <- paste0(abbreviations$abbreviation, as.character(stringr::str_remove_all(abbreviations$signature, "-")))
  rownames(ab) <- title
  out <- apply(ab, 1, function(x) paste(names(x[x==1])))
  out[out=="character(0)"] <- NA_character_
  out <- unname(out)
  out <- as.character(out)
  # keep year only
  out <- ifelse(is.na(out), out, substr(out, 1, nchar(out) - 4))
  
  # If output is a list with no values, returns an empty list of the same length as title variable
  lt <- as.numeric(length(title))
  ifelse(length(out) == 0, out <- rep(NA_character_, lt), out)
  
  out
  
}

#' Code Acronym for Titles
#'
#' Codes an acronym for agreement titles to facilitate identification
#' and comparuison across datasets.
#' @param title A character vector of treaty title
#' @import stringr
#' @importFrom purrr map_chr
#' @importFrom tm stopwords
#' @details Codes acronyms that are 4 to 6 digits long.
#' For shorter treaty titles, six words or less, acronym
#' includes first letter of each word.
#' For longer treaty titles, seven words or more, acronym includes
#' first letter of first word followed by the number of words in
#' and title first letter of last word in title.
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_acronym(IEADB$Title)
#' }
#' @export
code_acronym <- function(title){
  
  # Step one: standardise titles
  x <- standardise_titles(tm::removeWords(tolower(title), tm::stopwords("en")))
  
  # Step two: remove some agreement types, numbers and parenthesis from titles
  x <- gsub("protocol|protocols|amendment|amendments|amend|Agreement|agreements|convention|Exchange|Exchanges|Notes|Strategy|strategies|Resolution|resolutions",
            "", x, ignore.case = TRUE)
  x <- stringr::str_remove_all(x, "[0-9]")
  x <- stringr::str_remove_all(x, "\\(|\\)")

  # Step three: get abbreviations for words left
  x <- abbreviate(x, minlength = 4, method = 'both.sides')
  x <- toupper(x)
  
  # step four: cut longer abreviations into four digits
  x <- purrr::map_chr(x, function(y){
    if(nchar(y)<=6){
      y
    } else {
      y <- paste0(substr(y, 1, 1), stringr::str_pad(nchar(y)-2, 2, pad = "0"), substr(y, nchar(y), nchar(y)))
    }
  })

  x <- as.character(x)
  x

}

#' Code Agreement Linkages
#'
#' Identify the linkage between amendments and protocols to a main agreement.
#' @param title A character vector of treaty title
#' @param date A date variable
#' @importFrom textclean add_comma_space
#' @importFrom stringr str_replace_all str_squish str_remove_all
#' @importFrom purrr map
#' @import dplyr
#' @return A character vector of the agreements that are linked
#' @details The function identifies duplicates by excluding
#' "predictable" words from strings, this maintains key words then used
#' to identify and link duplicates.
#' This is a choice that considers errors should lie on the side of false
#' negatives rather than false positives.
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_linkage(IEADB$Title, IEADB$Signature)
#' @export
code_linkage <- function(title, date) {
  
  s <-  purrr::map(title, as.character)
  
  # Step one: find type, parties, known agreements and acronyms
  type <- code_type(s)
  abbrev <- code_known_agreements(s)
  parties <- code_parties(s)
  uID <- code_dates(date)
  acronym <- code_acronym(s)
  action <- code_action(s)
  
  out <- standardise_titles(as.character(title))
  
  # Step two: remove 'predictable words' in agreements
  predictable_words <- predictable_words$predictable_words
  predictable_words <- paste(predictable_words, collapse = '\\>|\\<')
  predictable_words <- paste0("\\<", predictable_words, "\\>")
  out <- gsub(predictable_words, "", out, ignore.case = TRUE)
  out <- stringr::str_replace_all(out, predictable_words, "")
  
  #Setep three: remove numbers, signs and parentheses
  out <- gsub("\\s*\\([^\\)]+\\)", "", out, ignore.case = FALSE)
  out <- gsub("-", "", out, ignore.case = FALSE)
  out <- stringr::str_replace_all(out, ",|-", "")
  out <- stringr::str_remove_all(out, "[0-9]")
  out <- trimws(out)
  out <- stringr::str_squish(out)
  out <- textclean::add_comma_space(out)
  out <- as.data.frame(out)
  
  # Step four: assign ID to observations
  id <- ifelse((!is.na(abbrev)), paste0(abbrev, type),
               (ifelse((is.na(parties)), paste0(acronym, "_", uID, type),
                       (ifelse((!is.na(parties)), paste0(parties, "_", uID, type, action), NA)))))
  
  out <- cbind(out, id)
  
  # Initialize variables to suppress CMD notes
  ref <- NULL
  
  # Step five: find duplicates and original values
  out <- out %>%
    dplyr::group_by_at(dplyr::vars(out)) %>%
    dplyr::mutate(
      dup = dplyr::row_number() > 1,
      ref = ifelse(dup, paste0(dplyr::first(id)), as.character(id)))
  
  # step six: assign same id to duplicates
  out <- out %>%
    dplyr::group_by(ref) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(line = dplyr::case_when(n != 1 ~ paste(ref), n == 1 ~ "1"))

  # Step seven: keep only linkages
  line <- out$line
  line <- stringr::str_replace_all(line, "^1$", "")
  
  # Step eight: remove linkages that are not agreements
  line <- stringr::str_replace_all(line, "[:digit:]{4}E", "")
  line <- stringr::str_replace_all(line, "[:digit:]{4}P", "")
  line <- stringr::str_replace_all(line, "[:digit:]{4}s", "")
  line <- stringr::str_replace_all(line, "[:digit:]{4}N", "")
  line <- stringr::str_replace_all(line, "[:digit:]{4}R", "")
  line <- ifelse(nchar(as.character(line)) < 8, "", line)
  
  line
  
}

#' Extracts Ordering Numbers from Titles
#'
#' Identifies and extracts meaningful numbers from agreements titles that
#' are important information about the order of the agreement.
#' Dates can cause an issue when trying to extract meaningful
#' numbers from titles for ordering purposes, this function also removes dates.
#' @param title A character vector of treaty title
#' @import stringr
#' @return A character vector with meangniful numbers from titles
order_agreements <- function(title) {
  
  # Step one: remove dates from title
  rd <- stringr::str_remove(title, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd,  "[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{9}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{4}")
  
  # Step two: standardises ordinal numbers and ordering text into digits
  oa <- gsub("\\<one\\>|\\<first\\>", "1", rd)
  oa <- gsub("\\<two\\>|\\<second\\>", "2", oa)
  oa <- gsub("\\<three\\>|\\<third\\>", "3", oa)
  oa <- gsub("\\<four\\>|\\<fourth\\>", "4", oa)
  oa <- gsub("\\<five\\>|\\<fifth\\>", "5", oa)
  oa <- gsub("\\<six\\>|\\<sixth\\>", "6", oa)
  oa <- gsub("\\<seven\\>|\\<seventh\\>", "7", oa)
  oa <- gsub("\\<eight\\>|\\<eighth\\>", "8", oa)
  oa <- gsub("\\<nine\\>|\\<ninth\\>", "9", oa)
  oa <- gsub("\\<ten\\>|\\<tenth\\>", "10", oa)
  oa <- gsub("\\<eleven\\>|\\<eleventh\\>", "11", oa)
  oa <- gsub("\\<twelve\\>|\\<twelfth\\>", "12", oa)
  oa <- gsub("\\<thirteen\\>|\\<thirteenth\\>", "13", oa)
  oa <- gsub("\\<fourteen\\>|\\<fourteenth\\>", "14", oa)
  oa <- gsub("\\<fifteen\\>|\\<fifteenth\\>", "15", oa)
  oa <- gsub("\\<sixteen\\>|\\<sixteenth\\>", "16", oa)
  oa <- gsub("\\<seventeen\\>|\\<seventeenth\\>", "17", oa)
  oa <- gsub("\\<eighteen\\>|\\<eighteenth\\>", "18", oa)
  oa <- gsub("\\<nineteen\\>|\\<nineteenth\\>", "19", oa)
  oa <- gsub("\\<twenty\\>|\\<twentieth\\>", "20", oa)
  
  # Step three: make sure meaningful numbers extracted correctly
  oa <- stringr::str_extract(oa, "\\s[:digit:]{1}\\s|\\s[:digit:]{2}\\s|\\s[:digit:]{3}\\s|\\s[:digit:]{1}|\\s[:digit:]{2}|\\s[:digit:]{3}")
  oa <- stringr::str_replace_all(oa, "\\s", "")
  oa <- stringr::str_replace_na(oa)
  oa <- stringr::str_remove_all(oa, "NA")
  
  oa
  
}
