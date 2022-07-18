#' Get treaty articles by number or match
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
#' Other options include the "preamble",
#' "termination" clause, "membership" clause, or "annex".
#' The specified portion for all treaties will be returned.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param treaty_type What types of treaty do you want to look at?
#' By default, "all".
#' Other treaty types include: "agreements", "protocols", "amendments",
#' "notes", "memorandum", and "resolutions".
#' @details Please make sure treaty texts have been standardised first
#' using `standardise_texts()` for best results.
#' @importFrom purrr map_chr map
#' @importFrom stringr str_extract str_replace_all str_trim str_split
#' @importFrom stringi stri_trans_general
#' @return A list of treaty sections of the same length.
#' @examples
#' \dontrun{
#' t <- standardise_texts(sample(manyenviron::texts$AGR_TXT$Text, 30))
#' get_articles(t)
#' get_articles(t, article = "preamble")
#' get_articles(t, article = "accession")
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
  t <- ifelse(lengths(t) < 10, stringr::str_split(as.character(t),
                                                  "((?=ARTICLE)|(?=ANNEX))"), t)
  # Get articles if declared
  if (isTRUE(article == "preamble")) {
    p <- lapply(t, function(x) grep("^preface|^preamble", x,
                                    ignore.case = TRUE, value = TRUE))
    t <- ifelse(lengths(p) == 0, purrr::map_chr(t, 1), p)
  } else if (isTRUE(article == "accession")) {
    t <- lapply(t, function(x) {
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

#' Get links from manyID
#'
#' Treaties that modify, amend, or expand other treaties
#' usually specify so in the title.
#' We use the manyIDs generated by `code_agreements()` and
#' `condense_agreements()` to return a data frame of treaty
#' links for a database.
#' @param database A Package database
#' @param dataset A Package dataset
#' @param treaty_type The type of treaties to be returned.
#' By default, all treaties are returned.
#' Other options include bilateral or multilateral treaties.
#' @return A dataframe of agreements' treatyID and their linkages.
#' @importFrom purrr map map_chr
#' @examples
#' \dontrun{
#' get_links(database = manyenviron::agreements)
#' get_links(database = manyenviron::agreements, treaty_type = "multilateral")
#' get_links(dataset = manyenviron::agreements$IEADB, treaty_type = "bilateral")
#' samples <- lapply(manyenviron::agreements,
#' function(x) x[x$Beg > "1991-12-31" & x$Beg < "1993-01-01", ])
#' migraph::gglineage(get_links(samples))
#' }
#' @export
get_links <- function(database, dataset, treaty_type = "all") {
  # Get manyID
  if (!missing(database)) {
    treatyID <- unname(unlist(purrr::map(database, "manyID")))
  }
  if (!missing(dataset)) {
    treatyID <- dataset$manyID
  }
  usethis::ui_done("treatyID column in dataset automatically found")
  # Filter by links
  treatyID <- grep(":", treatyID, value = TRUE)
  # Filter by treaty_type
  if (treaty_type == "bilateral") {
    treatyID <- grep("-", treatyID, value = TRUE)
  }
  if (treaty_type == "multilateral") {
    treatyID <- grep("-", treatyID, value = TRUE, invert = TRUE)
  }
  # Split treatyID
  link <- purrr::map_chr(strsplit(treatyID, ":"), 2)
  agreement <- purrr::map_chr(strsplit(treatyID, ":"), 1)
  # Return dataset
  out <- data.frame(agreement, link)
  out
}

#' Get memberships' list
#'
#' Memberships database have actor column(s) and
#' treaty column(s) but information on the other
#' countries that are party to the treaties is often
#' hard to read.
#' The function allows to generate a dataframe of
#' treaty IDs and actors part to the treaty.
#' @param database A database
#' @param actor A actor variable (e.g. country)
#' @param id A treaty ID variable
#' @return A dataframe of treaty IDs and actors
#' part of the treaty
#' @import dplyr
#' @examples
#' \dontrun{
#' sample <- manyenviron::memberships$IEADB_MEM
#' get_memberships(actor = sample$CountryID, id = sample$manyID)
#' get_memberships(manyenviron::memberships)
#' }
#' @export
get_memberships <- function(database, actor, id) {
  memberships <- NULL
  if (!missing(database)) {
    id <- unname(unlist(purrr::map(database, "manyID")))
    actor <- unname(unlist(purrr::map(database, "CountryID")))
    s <- cbind(actor, id)
    s <- as.data.frame(s)
    k <- s %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(memberships = toString(actor)) %>%
      dplyr::ungroup()
    s <- dplyr::left_join(s, k, by = "id") %>%
      dplyr::select(id, memberships) %>%
      unique()
    s
  } else {
    actor <- as.character(actor)
    id <- as.character(id)
    s <- cbind(actor, id)
    s <- as.data.frame(s)
    k <- s %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(memberships = toString(actor)) %>%
      dplyr::ungroup()
    s <- dplyr::left_join(s, k, by = "id") %>%
      dplyr::select(id, memberships)
    s
  }
}
