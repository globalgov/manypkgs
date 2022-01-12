#' Get treaty articles by number or match
#'
#' The function splits treaty texts into lists that reflect a structure
#' based on having a preamble and several articles.
#' Once articles are split,
#' you can access all the "preambles",
#' "membership" or "termination" clauses,
#' or articles that contain certain word.
#' @param textvar A text variable
#' @param article Would you like to get a specific article?
#' Null by default.
#' Other options include the "preamble",
#' "termination" clause, "membership" clause, "annex",
#' or an article number.
#' The specified portion or number
#' across all treaties will be returned.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param treaty_type What types of treaty do you want to look at?
#' By default, "all".
#' Other treaty types include:
#' "agreements", "protocols", "amendments",
#' "notes", "memorandum", and "resolutions".
#' @details If no article or match are declared, only text,
#' a structured list for each agreement based on articles is returned.
#' @importFrom purrr map_chr map
#' @importFrom stringr str_extract
#' @importFrom dplyr na_if
#' @importFrom stringi stri_trans_general
#' @return A list of treaty sections of the same length
#' @examples
#' \donttest{
#' t <- sample(manyenviron::texts$AGR_TXT$Text, 30)
#' get_articles(t)
#' get_articles(t, article = "preamble")
#' get_articles(t, article = "memberships")
#' get_articles(t, article = "termination")
#' get_articles(t, article = "annex")
#' get_articles(t, article = 1)
#' get_articles(t, match = "constitution")
#' get_articles(t, article = "preamble", match = "unofficial")
#' get_articles(t, treaty_type = "agreements")
#' get_articles(t, treaty_type = "protocols")
#' get_articles(t, treaty_type = "amendments")
#' }
#' @export
get_articles <- function(textvar, article = NULL, match = NULL, treaty_type = "all") {
  # Get textvar standardized
  t <- stringi::stri_trans_general(tolower(as.character(textvar)), id = "Latin-ASCII")
  # Get treaty type if declared (adapted from code_type)
  if (treaty_type != "all") {
    out <- purrr::map(t, as.character)
    type <- as.data.frame(agreement_type)
    for (k in seq_len(nrow(type))) {
      out <- gsub(paste0(type$word[[k]]),
                  paste0(type$category[[k]]),
                  out, ignore.case = TRUE,
                  perl = T)
    }
    type <- stringr::str_extract(out, "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
    if (treaty_type == "agreements") {
      t <- ifelse(type == "AGREE", t, "character(0)") 
    } 
    if (treaty_type == "protocols") {
      t <- ifelse(type == "PROTO", t, "character(0)") 
    }
    if (treaty_type == "amendments") {
      t <- ifelse(type == "AMEND", t, "character(0)") 
    }
    if (treaty_type == "notes") {
      t <- ifelse(type == "NOTES", t, "character(0)") 
    }
    if (treaty_type == "memorandum") {
      t <- ifelse(type == "STRAT", t, "character(0)") 
    }
    if (treaty_type == "resolution") {
      t <- ifelse(type == "RESOL", t, "character(0)") 
    }
    t
  }
  # Split treaty texts into articles
  t <- split_treaty(t)
  # Get articles if declared
  if (is.numeric(article)) {
    for (k in seq_len(length(t))) {
      a <- list(rep("NA",  as.numeric(article) + 1))
      t[k] <- ifelse(lengths(t[k]) < as.numeric(article) + 1, t[k] <- a, t[k])
      }
    t <- purrr::map_chr(t, c(as.numeric(article) + 1))
  }
  if (isTRUE(article == "preamble")) {
    t <- ifelse(lengths(t) > 0, purrr::map_chr(t, 1), NA)
  }
  if (isTRUE(article == "memberships")) {
    t <- lapply(t, function(x) grep("open for accession|accession shall be|can accede to|may join|open for joining|open for signature|shall be open|may accede|to accede to|may become a member|accession shall bind|accede thereto|become parties|request accession|may be admitted|any notification|receipt of any notice",
                                    x, ignore.case = TRUE, value = TRUE))
  }
  if (isTRUE(article == "termination")) {
    t <- lapply(t, function(x) grep("shall terminate|shall remain in force|will expire on|concluded for a period|shall apply for|période de|shall be terminated|expiration of the period|denunciation|terminated|shall supersede|shall.*supplant|shall be extended through|have denounced this convention|shall be dissolved|may decide.*to dissolve|may be dissolved|renounce its membership|may withdraw|extraordinary events|
                                    |failure of obligation|nonperformance of obligations|conflict with.*jus cogens|state party existence.*come to.*end|incompatibility between.*agreement and UN charter|incompatibility between.*agreement and United Nations charter|in the case of war.*end|party injurious.*end.*obligations|may.*denounce|any member.*may.*withdraw|injured party.*end.*obligations|party.*may withraw|renunciation.*by.*party",
                                    x, ignore.case = TRUE, value = TRUE))
  }
  if (isTRUE(article == "annex")) {
    t <- lapply(t, function(x) grep("^annex",
                                    x, ignore.case = TRUE, value = TRUE))
  }
  if(!is.null(match)) {
    t <- lapply(t, function(x) grep(match, x, ignore.case = TRUE, value = TRUE))
  }
  t <- dplyr::na_if(t, "character(0)")
  t
}

#' Split Treaty Texts
#' 
#' Helper function for spliting treaty texts into lists
#' that reflect a structure
#' based on having a preamble and several articles.
#' @param textvar A text variable
#' @importFrom stringr str_detect str_replace str_trim
#' @return A structured list for each agreement
split_treaty <- function(textvar) {
  # Detect annexes
  t <- purrr::map(textvar, as.character)
  articles <- ifelse(stringr::str_detect(t, "\nannex"),
                     stringr::str_replace(t, "\nannex", "\nannex annex"), t)
  # Split list
  articles <- ifelse(stringr::str_detect(articles, "\n"),
                     strsplit(as.character(articles), "\nannex|\narticle|\nart\\.", perl = TRUE),
                     strsplit(as.character(articles), "\\.\\sarticle\\s|\nnote [a-z]{1,4}\n|\n[a-z]{1,4}\n|\n[a-z]{1,4} paragraph|\n[1-9]{1,2}) paragraph"))
  # Add attributes
  for(i in seq_len(length(articles))) attr(articles[[i]], "Treaty") <- paste0("Treaty_", i)
  for(i in seq_len(length(articles))) {
    attr(articles[[i]], "Article") <- paste0("Articles = ", lengths(articles[i]))
  }
  articles <- lapply(articles, stringr::str_trim)
  articles
}
