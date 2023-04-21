#' Read treaty articles by number or match
#'
#' The function splits treaty texts into lists that reflect a structure
#' based on having a preamble and several articles.
#' Once articles are split, users can access the "preambles",
#' "accession" and "termination" clauses, or "annexes"
#' within a collection of treaty texts.
#' The selection of each of these articles relies on regex matching
#' certain expressions unique to each of them.
#' Alternatively, users can get all the articles that contain a certain
#' 'desired' word within a collection of treaty texts.
#' As well, users can select only certain types of treaties for which
#' specific articles are to be returned (e.g. agreements).
#' @param textvar A text variable
#' @param article Would you like to get a specific article?
#' Null by default.
#' Other options include the "preamble", the "termination" clauses,
#' "membership" clauses, or the "annex".
#' The specified portion for all treaties will be returned.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param treaty_type Would you like to get certain types of treaty only?
#' Null by default.
#' Other treaty types include: "agreements", "protocols", "amendments",
#' "notes", "memorandum", and "resolutions".
#' @details Please make sure treaty texts have been standardised first
#' using `standardise_texts()` for best results.
#' @importFrom purrr map
#' @importFrom stringr str_extract str_split
#' @return A list of treaty sections of the same length.
#' @examples
#' \dontrun{
#' t <- standardise_treaty_text(sample(manyenviron::agreements$HUGGO$MainText, 30))
#' read_clauses(t)
#' read_clauses(t, article = "preamble")
#' read_clauses(t, article = "accession")
#' read_clauses(t, article = "termination")
#' read_clauses(t, article = "annex")
#' read_clauses(t, match = "constitution")
#' read_clauses(t, article = "preamble", match = "amend")
#' read_clauses(t, treaty_type = "agreements")
#' read_clauses(t, treaty_type = "protocols")
#' read_clauses(t, treaty_type = "amendments")
#' }
#' @export
read_clauses <- function(textvar, article = NULL,
                         match = NULL, treaty_type = NULL) {
  # Check if text variable was standardised first
  if (any(grepl("<.*?>|\\\r|\\\t", textvar))) {
    message("Please make sure treaty texts have been standardised first
              using `standardise_texts()`")
  }
  # Get treaty type if declared (adapted from code_type)
  if (!is.null(treaty_type)) {
    out <- purrr::map(textvar, as.character)
    type <- as.data.frame(agreement_type)
    for (k in seq_len(nrow(type))) {
      out <- gsub(paste0(type$word[[k]]),
                  paste0(type$category[[k]]),
                  out, ignore.case = TRUE,
                  perl = TRUE)
    }
    type <- stringr::str_extract(out, "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
    if (treaty_type == "agreements") {
      textvar <- ifelse(type == "AGREE", textvar, NA_character_)
    } else if (treaty_type == "protocols") {
      textvar <- ifelse(type == "PROTO", textvar, NA_character_)
    } else if (treaty_type == "amendments") {
      textvar <- ifelse(type == "AMEND", textvar, NA_character_)
    } else if (treaty_type == "notes") {
      textvar <- ifelse(type == "NOTES", textvar, NA_character_)
    } else if (treaty_type == "memorandum") {
      textvar <- ifelse(type == "STRAT", textvar, NA_character_)
    } else if (treaty_type == "resolution") {
      textvar <- ifelse(type == "RESOL", textvar, NA_character_)
    }
    textvar
  }
  # Split list (if already not split by paragraph marks)
  t <- ifelse(lengths(textvar) < 10,
              stringr::str_split(as.character(textvar),
                                 "((?=ARTICLE)|(?=ANNEX))"), textvar)
  # Get articles if declared
  if (!is.null(article)) {
    t <- get_articles(t, article)
  }
  if (!is.null(match)) {
    t <- lapply(t, function(x) grep(match, x, ignore.case = TRUE, value = TRUE))
  }
  t <- ifelse(lengths(t) == 0, NA_character_, t)
  t
}

# Helper function
get_articles <- function(textvar, article) {
  if (isTRUE(article == "preamble")) {
    p <- lapply(textvar, function(x) grep("^preface|^preamble", x,
                                          ignore.case = TRUE, value = TRUE))
    t <- ifelse(lengths(p) == 0, purrr::map_chr(textvar, 1), p)
  } else if (isTRUE(article == "accession")) {
    t <- lapply(textvar, function(x) {
      grep("([^\\s]+\\s+){0,20}open for accession([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}accession shall be([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}accede to([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}may join([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}open for joining([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}open for signature([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}may become a member([^\\s]+\\s+){0,20}}|
          |([^\\s]+\\s+){0,20}accede thereto([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}become parties([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}become a party([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}request accession([^\\s]+\\s+){0,20}|
          |([^\\s]+\\s+){0,20}may be admitted([^\\s]+\\s+){0,20}",
           x, ignore.case = TRUE, perl = TRUE, value = TRUE)
    })
  } else if (isTRUE(article == "termination")) {
    t <- lapply(textvar, function(x) grep("shall terminate|shall remain in force|will expire on|
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
    t <- lapply(textvar, function(x) grep("^annex", x, ignore.case = TRUE,
                                          value = TRUE))
  }
  t
}
