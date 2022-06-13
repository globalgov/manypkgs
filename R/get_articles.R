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
#' "termination" clause, "membership" clause, or "annex".
#' The specified portion for all treaties will be returned.
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
#' @importFrom stringr str_extract str_replace_all str_trim str_split
#' @importFrom stringi stri_trans_general
#' @return A list of treaty sections of the same length.
#' @examples
#' \dontrun{
#' t <- standardise_texts(sample(manyenviron::texts$AGR_TXT$Text, 30))
#' get_articles(t)
#' get_articles(t, article = "preamble")
#' get_articles(t, article = "memberships")
#' get_articles(t, article = "termination")
#' get_articles(t, article = "annex")
#' get_articles(t, match = "constitution")
#' get_articles(t, article = "preamble", match = "amend")
#' get_articles(t, treaty_type = "agreements")
#' get_articles(t, treaty_type = "protocols")
#' get_articles(t, treaty_type = "amendments")
#' }
#' @export
get_articles <- function(textvar, article = NULL,
                         match = NULL, treaty_type = "all") {
  usethis::ui_info("Please make sure treaty texts have been standardised first
                   using `standardise_texts()`")
  t <- textvar
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
      t <- ifelse(type == "AGREE", t, NA_character_)
    } else if (treaty_type == "protocols") {
      t <- ifelse(type == "PROTO", t, NA_character_)
    } else if (treaty_type == "amendments") {
      t <- ifelse(type == "AMEND", t, NA_character_)
    } else if (treaty_type == "notes") {
      t <- ifelse(type == "NOTES", t, NA_character_)
    } else if (treaty_type == "memorandum") {
      t <- ifelse(type == "STRAT", t, NA_character_)
    } else if (treaty_type == "resolution") {
      t <- ifelse(type == "RESOL", t, NA_character_)
    }
    t
  }
  # Split list (if already not split by paragraph marks)
  t <- ifelse(lengths(t) < 10, stringr::str_split(as.character(t), "((?=ARTICLE)|(?=ANNEX))"), t)
  # Get articles if declared
  if (isTRUE(article == "preamble")) {
    p <- lapply(t, function(x) grep("^preface|^preamble", x,
                                    ignore.case = TRUE, value = TRUE))
    t <- ifelse(lengths(p) == 0, purrr::map_chr(t, 1), p)
  } else if (isTRUE(article == "memberships")) {
    t <- lapply(t, function(x) {
    grep("open for accession|accession shall be|can accede to|may join|open for joining|open for signature|shall be open|may accede|to accede to|may become a member|accession shall bind|accede thereto|
         |become parties|request accession|may be admitted|any notification|receipt of any notice", x, ignore.case = TRUE, value = TRUE)
      })
  } else if (isTRUE(article == "termination")) {
    t <- lapply(t, function(x) grep("shall terminate|shall remain in force|will expire on|
                                    |concluded for a period|shall apply for|
                                    |periode de|shall be terminated|expiration of the period|
                                    |denunciation|terminated|shall supersede|shall.*supplant|
                                    |shall be extended through|have denounced this convention|
                                    |shall be dissolved|may decide.*to dissolve|may be dissolved|
                                    |renounce its membership|may withdraw|extraordinary events|
                                    |failure of obligation|nonperformance of obligations|
                                    |conflict with.*jus cogens|state party existence.*come to.*end|
                                    |incompatibility between.*agreement and UN charter|
                                    |incompatibility between.*agreement and United Nations charter|
                                    |in the case of war.*end|party injurious.*end.*obligations|
                                    |may.*denounce|any member.*may.*withdraw|injured party.*end.*obligations|
                                    |party.*may withraw|renunciation.*by.*party",
                                    x, ignore.case = TRUE, value = TRUE))
  } else if (isTRUE(article == "annex")) {
    t <- lapply(t, function(x) grep("^annex", x, ignore.case = TRUE,
                                    value = TRUE))
  }
  if (!is.null(match)) {
    t <- lapply(t, function(x) grep(match, x, ignore.case = TRUE, value = TRUE))
  }
  t <- ifelse(lengths(t) == 0, NA_character_, t)
  t
}
