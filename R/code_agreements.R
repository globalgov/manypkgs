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
  usethis::ui_done("Coded agreement dates")
  
  #Step six: code acronyms from titles
  acronym <- code_acronym(title)
  usethis::ui_done("Coded acronyms for agreements")
  
  # Step seven: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  usethis::ui_done("Coded agreement linkages")
  
  # Step eight: add items together correctly
  out <- vector(mode = "character", length = length(title)) # initialize vector
  # for agreements (A) where abbreviation is known and
  # bilateral agreement is made subsequently
  qID <- ifelse(!is.na(abbrev) & (type == "A") & !is.na(parties), paste0(parties, "_", uID, type, ":", abbrev), out)
  qID <- ifelse(!is.na(abbrev) & (type != "A") & !is.na(parties), paste0(parties, "_", uID, type, ":", line), qID)
  # for agreements (A) where abbreviation is known
  qID <- ifelse(!is.na(abbrev) & (type == "A") & is.na(parties), paste0(abbrev, type), qID)
  # when abbreviation is known but treaty type is not agreement
  qID <- ifelse(!is.na(abbrev) & (type != "A") & is.na(parties), paste0(acronym, "_", uID, type, ":", line), qID)
  # when parties were not identified and treaty type is agreement (A)
  qID <- ifelse(is.na(parties) & (type == "A") & is.na(abbrev), paste0(acronym, "_", uID, type), qID)
  # when parties were not identified and type is not agreement
  qID <- ifelse(is.na(parties) & (type != "A") & is.na(abbrev), paste0(acronym, "_", uID, type, ":", line), qID)
  # when parties were identified and type is agreement (A)
  qID <- ifelse(!is.na(parties) & (type == "A") & is.na(abbrev), paste0(parties, "_", uID, type), qID)
  # when parties were identified and type is not agreement
  qID <- ifelse(!is.na(parties) & (type != "A") & is.na(abbrev), paste0(parties, "_", uID, type, ":", line), qID)
  # deletes empty line or linkage
  qID <- stringr::str_remove_all(qID, "_$")
  qID <- stringr::str_remove_all(qID, ":$")
  
  # step nine: inform users about observations not matched and duplicates
  cat(sum(is.na(qID)), "entries were not matched at all.\n")
  cat("There were", sum(duplicated(qID, incomparables = NA)), "duplicated IDs.\n")
  usethis::ui_done("Please run `vignette('agreements')` for more information.")
  qID
}

#' Code Agreement Parties
#'
#' Identify the countries that are part of an agreement.
#' @param title A character vector of treaty titles
#' @importFrom stringr str_replace_all
#' @importFrom tm removeWords stopwords
#' @return A character vector of parties that are mentioned in the treaty title
#' @details The function codes states in treaties alongside, returning only
#' parties for bileteral treaties (i.e. 2 parties coded).
#' The number of meaningful words (removes articles, numbers and other common
#' words)in title for bilateral tretaies is returned next to parties.
#' For the complete list of words removed from title to identify
#' duplicates please run `pred_words()`.
#' This is done to avoid false duplicates as often bileteral treaties between
#' the same two countries are signed in the same day.
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_parties(IEADB$Title)
#' @export
code_parties <- function(title) {
  
  # Step one: get ISO country codes countryregex and match in title variable
  title <- as.character(title)
  title <- ifelse(grepl("\\s*\\([^\\)]+\\)", title), gsub("\\s*\\([^\\)]+\\)", "", title), title)
  coment <- sapply(countryregex[,3], function(x) grepl(x, title, ignore.case = T, perl = T)*1)
  colnames(coment) <- countryregex[,1]
  rownames(coment) <- title
  out <- apply(coment, 1, function(x) paste(names(x[x==1]), collapse = "_"))
  out[out==""] <- NA
  parties <- unname(out)
  parties <- stringr::str_replace_all(parties, "_", "-")
  
  # Step two: add NAs to observations not matched
  parties[!grepl("-", parties)] <- NA
  
  # Step three:: get bilateral agreements where two parties have been identified
  parties <- ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"), parties,
                    ifelse(stringr::str_detect(parties, "^[:alpha:]{2}-[:alpha:]{3}$"), parties,
                           ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{2}$"), parties, NA)))
  
  # Step four: get activity in title to reduce number of false duplicates
  out <- code_activity(title)
  parties <- ifelse(is.na(parties), parties, paste0(parties, "[", out, "]"))
  parties
}

#' Code Abbreviations for Activity
#'
#' Code abbreviations for activity in bilateral treaty titles
#' @param title A character vector of treaty titles
#' @details Bilateral agreements usully detail their activity and specify area
#' in the last words of the titles.
#' These last words are abbreviated by the function to differentiate between
#' bilateral treaties and avoid false positives being generated since
#' multiple, different, bileteral treaties are often signed in the same day.
#' @importFrom stringr str_squish word
#' @importFrom tm removeWords stopwords
#' @return A character vector of abbreviations of last words in treaty title
#' @export
code_activity <- function(title) {

  if (missing(title)) {
    # If missing argument, function returns list of states and abbreviations
    out <- as.data.frame(countryregex)
    out$Regex[56] <- paste(substr(out$Regex[56], 0, 100), "...")
    out <- knitr::kable(out, "simple")
  } else {

  # Step one: remove states' names and agreements' type
  out <- as.character(title)
  states <- countryregex$Label
  states <- paste(states, collapse = '|')
  words <- agreement_type$words
  words <- paste(words, collapse = '|')
  out <- gsub(states, "", out, ignore.case = TRUE)
  out <- gsub(words, "", out, ignore.case = TRUE)
  # Some states and abbreviations are missed
  out <- gsub("Soviet Socialist Republics|\\<USSR\\>|\\<UK\\>|\\<US\\>||\\<united\\>|\\<america\\>", "", out, ignore.case = TRUE)
  
  # Step two: remove stop words, numbers and parenthesis
  out <- tm::removeWords(tolower(out), tm::stopwords('SMART'))
  out <- gsub("[0-9]", "", out)
  out <- gsub("\\(|\\)|\U00AC|\U00F1 ", "", out)
  out <- gsub("-", " ", out)
  
  # Step three: remove months and unimportant words
  out <- gsub("january|february|march|april|may|june|july|august|september|october|november|december",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<text\\>|\\<signed\\>|\\<government\\>|\\<federal\\>|\\<republic\\>|\\<states\\>|
              \\<confederation\\>|\\<federative\\>|\\<kingdom\\>|\\<republics\\>",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<coast\\>|\\<ocean\\>|\\<eastern\\>|\\<western\\>|\\<north\\>|\\<south\\>|\\<west\\>|\\<east\\>|
              \\<southern\\>|\\<northern\\>|\\<middle\\>|\\<atlantic\\>|\\<pacific\\>|\\<columbia\\>|\\<danube\\>",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<between\\>|\\<cooperation\\>|\\<cooperative\\>|\\<scientific\\>|\\<technical\\>|\\<basic\\>|\\<border\\>|\\<pollution\\>|\\<river\\>|\\<basin\\>|\\<water\\>|\\<resources\\>|\\<aim\\>|\\<reducing\\>|\\<cross\\>|\\<relating\\>|\\<iron\\>|\\<gates\\>|\\<power\\>|\\<navigation\\>|\\<system\\>|\\<sphere\\>|\\<field\\>|\\<partnership\\>|\\<science\\>|\\<matters\\>",
              "", out, ignore.case = TRUE)
  
  # Step four: get abbreviations for last three words and counting of words
  out <- stringr::str_squish(out)
  # lt <- lengths(gregexpr("\\W+", out))
  out <- suppressWarnings(abbreviate(out, minlength = 3, method = 'both.sides', strict = TRUE))
  out <- stringr::str_extract(out, ".{3}$")
  out <- toupper(out)
  # out <- paste0(lt,out)
  }
  out
}

#' Code Agreement Type
#'
#' Identify the type of international agreement from titles.
#' Agreements can be, for example, multilateral treaties or coventions (A),
#' protocols (P) or amendments (E), if they contain words in title.
#' @param title A character vector of treaty title
#' @return A character vector of the treaty type
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract str_replace_na
#' @importFrom purrr map
#' @importFrom knitr kable
#' @details Types of agreements differentiate agreements from protocols or
#' amendments, for example.
#' For the complete list of words coded for type and their type
#' abbreviations please run the function without and argument
#' (i.e. `code_action()`).
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_type(IEADB$Title)
#' @export
code_type <- function(title) {

  if (missing(title)) {
    # If missing argument, function returns list of types and words coded
    type <- as.data.frame(agreement_type)
    type$words[3] <- paste(substr(type$words[3], 0, 120), "...")
    type$words[5] <- paste(substr(type$words[5], 0, 120), "...")
    type$words[6] <- paste(substr(type$words[6], 0, 120), "...")
    type <- knitr::kable(type, "simple")
  }  else {
    # Step one: get type codes
    out <- purrr::map(title, as.character)
    type <- as.data.frame(agreement_type)
    
    # Step two: substitute matching words for categories
    for (k in seq_len(nrow(type))) {
      out <- gsub(paste0(type$word[[k]]), paste0(type$category[[k]]), out, ignore.case = TRUE, perl = T)
    }
    
    # Step three: eXtract only first category identified
    type <- stringr::str_extract(out, "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
    
    # Step four: assign type abbreviations
    type <- dplyr::case_when(
      grepl("PROTO", type) ~ "P", # protocol
      grepl("AMEND", type) ~ "E", # amendment
      grepl("AGREE", type) ~ "A", # agreement
      grepl("NOTES", type) ~ "N", # notes
      grepl("STRAT", type) ~ "S", # strategy
      grepl("RESOL", type) ~ "R", # resolution
    )
    
    # step 5: extracts meaningful ordering numbers for protocols and amendments
    number <- order_agreements(title)
    # Assign other (O) no type is found
    type <- stringr::str_replace_na(type, "O")
    # Add type and number if available
    type <- paste0(type, number)
  }
  type
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

  # Step one: collapse dates
  uID <- stringr::str_remove_all(date, "-")

  # Step two: get NA dates to appear as far future dates to facilitate
  # identification
  uID[is.na(uID)] <- paste0(sample(5000:9999, 1), "NULL")

  # Step three: remove ranges, first date is taken
  uID <- stringr::str_replace_all(uID, "\\:[:digit:]{8}$", "")

  # Step four: keep year only
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
#' @details The function identifies agreements that match the list of known
#' agreements with their titles, abbreviations and signature dates and
#' substitutes the known titles for abbreviations.For the complete list of
#' known agreements coded for and their respective abbreviations please run
#' the function without and argument
#' (i.e. `code_known_agreements()`).
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_known_agreements(IEADB$Title)
#' @export
code_known_agreements <- function(title) {
  
  if (missing(title)) {
    # If missing argument, function returns list of known agreements coded
    ka <- as.data.frame(abbreviations)
    ka$title[15] <- paste(substr( ka$title[15] , 0, 90), "...")
    ka$title[17] <- paste(substr( ka$title[17] , 0, 90), "...")
    out <- knitr::kable(ka, "simple")
  } else {
    # Step one: get abbreviations dataset
    abbreviations <- purrr::map(abbreviations, as.character)

    # Step two: assign the specific abbreviation to the "known" treaties
    # when they match
    ab <- sapply(abbreviations$title, function(x) grepl(x, title, ignore.case = T, perl = T)*1)
    colnames(ab) <- paste0(abbreviations$abbreviation, "_", as.character(stringr::str_remove_all(abbreviations$signature, "-")))
    rownames(ab) <- title
    out <- apply(ab, 1, function(x) paste(names(x[x==1])))
    # Assign NA when observation is not matched
    out[out=="character(0)"] <- NA_character_
    out <- unname(out)
    out <- as.character(out)
    out <- ifelse(grepl("c\\(", out), "PARIS_20151212", out)

    # Step three: keep year only for IDs
    out <- ifelse(is.na(out), out, substr(out, 1, nchar(out) - 4))

    # Step four: if output is a list with no values, returns an 
    # empty list of the same length as argument
    lt <- as.numeric(length(title))
    ifelse(length(out) == 0, out <- rep(NA_character_, lt), out)
  }
  out
}

#' Code Acronym for Titles
#'
#' Codes an acronym for agreement titles to facilitate identification
#' and comparuison across datasets.
#' @param title A character vector of treaty title
#' @import stringr
#' @importFrom tm removeWords stopwords
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
  
  # Step two: remove agreement types, numbers, punctuations marks, and
  # short abbreviations within parenthesis from titles
  x <- gsub("protocol|protocols|amendment|amendments|amend|amending|Agreement|agreements|convention|Exchange|Exchanges|Notes|Strategy|strategies|Resolution|resolutions",
            "", x, ignore.case = TRUE)
  x <- stringr::str_remove_all(x, "\\s\\([:alpha:]{3,9}\\)")
  x <- stringr::str_remove_all(x, "\\s\\(.{3,20}\\)")
  x <- stringr::str_remove_all(x, "[0-9]")
  x <- stringr::str_remove_all(x, "\\(|\\)")
  x <- gsub("-", " ", x)
  
  # Step three: remove known agreement cities or short titles
  # (these often appear inconsistently across datasets)
  x <- gsub("\\<Nairobi\\>|\\<Basel\\>|\\<Bamako\\>|\\<Lusaka\\>|\\<Stockholm\\>|\\<Kyoto\\>|\\<Hong Kong\\>", "", x)
  x <- ifelse(grepl("^Fisheries", x), gsub("Fisheries", "", x), x)
  
  # Step four: remove unimportant but differentiating words
  x <- gsub("\\<populations\\>|\\<basin\\>|\\<resources\\>|\\<stock\\>|\\<concerning\\>|\\<priority\\>|\\<revised\\>|\\<version\\>|\\<national\\>|\\<trilateral\\>|\\<multilateral\\>|\\<between\\>|\\<marine\\>|\\<Fao\\>|\\<field\\>|\\<sphere\\>|\\<adjustment\\>|\\<activities\\>",
            "", x, ignore.case = TRUE)

  # Step five: get abbreviations for words left
  x <- suppressWarnings(abbreviate(x, minlength = 6, method = 'both.sides', strict = TRUE))
  x <- toupper(x)
  
  # step six: cut longer abbreviations into four digits
  x <- ifelse(stringr::str_detect(x, "[:upper:]{7}"), paste0(substr(x, 1, 2), stringr::str_pad(nchar(x)-3, 2, pad = "0"), substr(x, nchar(x)-1, nchar(x))), x)
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
#' For the complete list of words removed from title to identify
#' duplicates please run `pred_words()`.
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_linkage(IEADB$Title, IEADB$Signature)
#' @export
code_linkage <- function(title, date) {

  # Step one: find type, parties, known agreements and acronyms
  s <-  purrr::map(title, as.character)
  type <- code_type(s)
  abbrev <- code_known_agreements(s)
  parties <- code_parties(s)
  uID <- code_dates(date)
  acronym <- code_acronym(s)
  activity <- code_activity(s)
  
  # Step two: standardise titles to improve accuracy
  out <- standardise_titles(as.character(title))
  
  # Step three: remove 'predictable words' in agreements
  predictable_words <- predictable_words$predictable_words
  predictable_words <- paste(predictable_words, collapse = '\\>|\\<')
  predictable_words <- paste0("\\<", predictable_words, "\\>")
  out <- gsub(predictable_words, "", out, ignore.case = TRUE)
  
  #Setep four: remove numbers, signs and parentheses
  out <- gsub("\\s*\\([^\\)]+\\)", "", out, ignore.case = FALSE)
  out <- gsub("-", " ", out, ignore.case = FALSE)
  out <- stringr::str_replace_all(out, ",|-", "")
  out <- stringr::str_remove_all(out, "[0-9]")
  out <- stringr::str_squish(out)
  out <- textclean::add_comma_space(out)
  out <- as.data.frame(out)
  
  # Step five: assign ID to observations
  id <- ifelse((!is.na(abbrev)), paste0(abbrev, "A"),
               (ifelse((is.na(parties)), paste0(acronym, "_", uID, type),
                       (ifelse((!is.na(parties)), paste0(activity, "_", uID, type), NA)))))
  
  # Step six: bind data
  out <- cbind(out, id)
  # Initialize variables to suppress CMD notes
  ref <- NULL
  dup <- NULL
  
  # Step seven: find duplicates and original values
  out <- out %>%
    dplyr::group_by_at(dplyr::vars(out)) %>%
    dplyr::mutate(
      dup = dplyr::row_number() > 1,
      ref = ifelse(dup, paste0(dplyr::first(id)), as.character(id)))
  
  # step eight: assign same id to duplicates
  out <- out %>%
    dplyr::group_by(ref) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(line = dplyr::case_when(n != 1 ~ paste(ref), n == 1 ~ "1"))

  # Step nine: keep only linkages
  line <- out$line
  line <- stringr::str_replace_all(line, "^1$", "")
  
  # Step ten: removes all linkages that are not agreements
  line <- gsub("[0-9]{4}E|[0-9]{4}P|[0-9]{4}S|[0-9]{4}N|[0-9]{4}R", "xxxxxxxxxxxxxxxxxxxxXx", line)
  line <- ifelse(nchar(as.character(line)) > 20, "", line)
  line
}

#' Predictable words list
#'
#' @return A list of predicatble words removed
#' in code_linkages() and code_parties().
#' @importFrom knitr kable
#' @export
pred_words <- function() {
  pred <- as.data.frame(predictable_words)
  pred_words <- knitr::kable(pred, "simple")
  pred_words
}

#' Extracts Ordering Numbers from Titles
#'
#' Identifies and extracts meaningful numbers from agreements titles
#' that are important information about the order of the agreement.
#' Dates can cause an issue when trying to extract meaningful numbers
#' from titles for ordering purposes, this function also removes dates.
#' @param title A character vector of treaty title
#' @import stringr
#' @return A character vector with meangniful numbers from titles
order_agreements <- function(title) {
  
  # Step one: remove dates from title
  rd <- stringr::str_remove_all(title, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd,  "[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{9}\\s[:digit:]{4}")
  rd <- stringr::str_remove_all(rd, "[:digit:]{4}| [:digit:]{2}\\s[:digit:]{4}")
  # remove also numbers in parenthesis
  rd <- stringr::str_remove_all(rd, "\\s\\(No\\s.{3,7}\\)")
  
  # Step two: standardises ordinal numbers and ordering text into digits
  oa <- gsub("\\<one\\>|\\<first\\>|  I ", "1", rd)
  oa <- gsub("\\<two\\>|\\<second\\>| Ii ", "2", oa)
  oa <- gsub("\\<three\\>|\\<third\\>| Iii ", "3", oa)
  oa <- gsub("\\<four\\>|\\<fourth\\>| Iv ", "4", oa)
  oa <- gsub("\\<five\\>|\\<fifth\\>| V |No5", "5", oa)
  oa <- gsub("\\<six\\>|\\<sixth\\>|No6", "6", oa)
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
  oa <- stringr::str_extract(oa, "\\s[:digit:]{1}\\s|^[:digit:]{1}\\s|\\s[:digit:]{2}\\s|\\s[:digit:]{3}\\s|\\s[:digit:]{1}|\\s[:digit:]{2}|\\s[:digit:]{3}")
  oa <- stringr::str_replace_all(oa, "\\s", "")
  oa <- stringr::str_replace_na(oa)
  oa <- stringr::str_remove_all(oa, "NA")
  oa
}
