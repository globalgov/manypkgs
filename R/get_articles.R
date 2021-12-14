#' Get treaty articles by number or match
#'
#' The function splits treaty texts into lists that reflect a structure
#' based on having a preamble and several articles.
#' Once articles are split,
#' you can access all the "preambles",
#' "membership" or "termination" clauses,
#' or articles that contain certain word.
#' @param textvar A text variable
#' @param article Either the "preamble" or an article number
#' across all treaties to be returned.
#' It can be left NULL if user intends to look for a word in
#' all treaties.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @details If no article or match are declared, only text,
#' a structured list for each agreement based on articles is returned.
#' @importFrom purrr map_chr
#' @importFrom dplyr na_if
#' @return A list of treaty sections of the same length
#' @examples
#' \donttest{
#' t <- sample(manyenviron::texts$AGR_TXT$Text, 30)
#' get_articles(t)
#' get_articles(t, article = "preamble")
#' get_articles(t, article = "memberships")
#' get_articles(t, article = "termination")
#' get_articles(t, article = 1)
#' get_articles(t, match = "constitution")
#' get_articles(t, article = "preamble", match = "unofficial")
#' }
#' @export
get_articles <- function(textvar, article = NULL, match = NULL) {
  # Split treaty texts into articles
  t <- split_treaty(textvar)
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
    t <- lapply(t, function(x) grep("open for accession|accession shall be|can accede to|may join|open for joining|open for signature|shall be open|may accede|to accede to|may become a member|accession shall bind|accede thereto|become parties|request accession|may be admitted",
                                    x, ignore.case = TRUE, value = TRUE))
  }
  if (isTRUE(article == "termination")) {
    t <- lapply(t, function(x) grep("shall terminate|shall remain in force|will expire on|concluded for a period|shall apply for|période de|shall be terminated|expiration of the period|denunciation|terminated|shall supersede|shall.*supplant|shall be extended through",
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
#' @importFrom stringr str_detect
#' @importFrom stringi stri_trans_general
#' @return A structured list for each agreement
split_treaty <- function(textvar) {
  # Lower case and standardizes all, just in case
  t <- stringi::stri_trans_general(tolower(as.character(textvar)), id = "Latin-ASCII")
  # Split list
  articles <- ifelse(stringr::str_detect(t, "\n"), strsplit(t, "\narticle|\nart\\."),
                     strsplit(t, "\\.\\sarticle\\s"))
  # Add attributes
  for(i in seq_len(length(articles))) attr(articles[[i]], "Treaty") <- paste0("Treaty_", i)
  for(i in seq_len(length(articles))) {
    attr(articles[[i]], "Article") <- paste0("Articles = ", lengths(articles[i]))
  }
  articles
}
