#' Standardise titles
#'
#' Standardises words in a character title variable to improve readability,
#' facilitate string matching and enable more accurate comparisons
#' for variables in different datatsets.
#' @param s A string
#' @param strict By default FALSE
#' @param api_key If google API key is provided, the function will
#' translate and return strings in english using google translator.
#' @details The function capitalises words in the strings passed to it.
#' It trims white spaces from the start, middle and end of the strings.
#' Removes ambiguous punctions and symbols from strings.
#' All the strings are transformed into to ASCII character encoding.
#' Written numbers in ordinal form are transformed into numerical form.
#' If a google API key is provided as an argument, the function detects
#' strings in other languages and translates them to English.
#' @return A capitalised, trimmed and standardised string
#' @importFrom textclean add_comma_space mgsub
#' @importFrom english ordinal words
#' @importFrom stringr str_count str_squish str_to_title
#' @importFrom utils as.roman
#' @importFrom stringi stri_trans_general
#' @import dplyr
#' @examples
#' e <- standardise_titles("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' @export
standardise_titles <- standardize_titles <- function(s,
                                                     strict = FALSE,
                                                     api_key = NULL) {

  # Step one: capitalises first letter in words
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
    if (strict) tolower(s) else s
  }
  , sep = "", collapse = " ")
  out <- vapply(strsplit(s, split = " "), cap, "",
                USE.NAMES = !is.null(names(s)))

  # Step two: translate strings if API is provided
  if (!is.null(api_key)) {
    out <- lingua(out, api_key == api_key)
  }

  # Step three: standardise strings returned
  # Transforms strings to ASCII character encoding
  out <- suppressWarnings(stringi::stri_trans_general(out, id = "Latin-ASCII"))
  # standardises NAs
  out[out == "NANA"] <- NA
  out <- gsub("\\.(?=\\.*$)", "", out, perl = TRUE)
  # standardises spaces before and after apostrophes and comma spaces
  out <- gsub(" '|' ", "'", out)
  # Delete hyphens when separating two parts of the title
  # (when there is a space before and after)
  out <- gsub(" - ", " ", out)
  # Delete special character found in some treaty titles
  out <- gsub("\U00AC[[:alpha:]]{1}\\s|\U00AC\\s", "", out)
  # Add space after a comma
  out <- textclean::add_comma_space(out)
  # Change number symbol into word
  out <- gsub("\\#", "Number ", out)

  # standardise some country abbreviations and specific words
  out <- purrr::map(out, as.character)
  out <- correct_words(out)

  # Step four: Standardises how ordinal numbers are returned
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", as.roman(1:100), "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE)
  ords <- english::ordinal(1:100)
  ords <- paste0(ords,
                 dplyr::if_else(stringr::str_count(ords, "\\S+") == 2,
                         paste0("|", gsub(" ", "-", as.character(ords))), ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  num <- english::words(1:100)
  num <- paste0(num,
                 dplyr::if_else(stringr::str_count(num, "\\S+") == 2,
                                paste0("|", gsub(" ", "-",  as.character(num))), ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", num, "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)

  # Step five: make sure most punctuations are removed
  # and whitespaces trimmed
  out <- gsub("(?!\\-|\\(|\\))[[:punct:]]", "", out, perl = TRUE)
  # removes all punctuations but hyphen and parentheses,
  # which may contain important information for distinguishing
  # treaties
  out <- stringr::str_squish(out)
  out
}

#' Translate Strings
#'
#' Tranlates strings in data and returns object of the same lenght as original string.
#' The function automacally identifies language from strings by rows
#' so that tranlations to target language are more accurate and allows
#' for text in multiple languages to be present in string.
#' @param s A character string
#' @param api_key Google API key.
#' For more information please go to: https://cloud.google.com/translate/docs/setup
#' @param target_lang Which language would you like this translated to?
#' Please provide a two letter language abbreviation (e.g. "en" or "pt").
#' By default english.
#' @param translate Do you want strings to be translated?
#' By default TRUE.
#' If FALSE returns source language for each string. 
#' @importFrom purrr map map_chr
#' @return A character vector of the same length of original.
#' @export
lingua <- function(s, api_key, target_lang = "en", translate = TRUE) {

  depends(c("translateR", "cld2"))

  # Check if API key is declared
  if(missing(api_key) & translate == TRUE) {
    stop("Please declare a Google API key.
         For more information please go to: https://cloud.google.com/translate/docs/setup")
  }
  
  # Get strings as character and initialize varibles
  out <- data.frame(out = as.character(s))
  s <- purrr::map(s, as.character)
  . <- NULL

  # Find source language
  source_lang <- s %>%
    vapply(., purrr::map_chr, "", cld2::detect_language) %>%
    data.frame(check.names = FALSE)
  
  # Return source language if translate is false, else translate string
  if (missing(api_key) & translate == FALSE) {
    out <- source_lang
  } else {
    out <- cbind(out, source_lang)
    for (k in seq_len(nrow(out))) {
      if (is.na(out$.[k])) {
        out$out[k] == out$out[k]
        # print(paste0("Could not translate ", [k], ", langauge not detect."))
      } else if (out$.[k] == target_lang) {
        out$out[k] == out$out[k]
        } else {
          out$out[k] <- suppressWarnings(translateR::translate(content.vec = out$out[k],
                                                               google.api.key = api_key,
                                                               source.lang = out$.[k],
                                                               target.lang = target_lang))
        }
      }
    out <- out$out
  }
  out
}

#' Helper function for standardising words spelling
#'
#' Change some words spelling,
#' specifically those that can vary
#' from one text to another.
#' The function uses Britsh English spellings.
#' @param s A list of character vector
#' @return A list of character vector with the words changed
#' @importFrom knitr kable
correct_words <- function(s) {
  # If no arguments, the list of corrected words appears
  if (missing(s)) {
    corrected_words <- as.data.frame(corrected_words)
    corrected_words <- knitr::kable(corrected_words, "simple")
    corrected_words
  } else {
    # Substitute matching words for corrected words
    corrected_words <- as.data.frame(corrected_words)
    for (k in seq_len(nrow(corrected_words))) {
      s <- gsub(paste0(corrected_words$words[[k]]),
                paste0(corrected_words$corr_words[[k]]),
                s, ignore.case = TRUE, perl = T)
      }
    s
  }
}
