#' Split Treaty Texts
#' 
#' Split treaty texts into lists that reflect a structure
#' based on having a preamble and several articles.
#' @param textvar text variable
#' @importFrom stringr str_detect
#' @importFrom stringi stri_trans_general
#' @return A structured list for each agreement
#' @examples
#' \dontrun{
#' t <- head(qEnviron::texts$IEADB_TXT$Text)
#' split_treaty(t)
#' }
#' @export
split_treaty <- function(textvar) {

  # Lower case and standardizes all, just in case
  t <- stringi::stri_trans_general(tolower(as.character(textvar)), id = "Latin-ASCII")
  # Split list
  articles <- ifelse(stringr::str_detect(t, "\n"), strsplit(t, "\narticle|\nart\\."),
                     strsplit(t, "\\. article"))
  # Add attributes
  for(i in seq_len(length(articles))) attr(articles[[i]], "Treaty") <- paste0("Treaty_", i)
  for(i in seq_len(length(articles))) {
    attr(articles[[i]], "Article") <- paste0("Articles = ", lengths(articles[i]))
  }
  articles
}

#' Get treaty articles by number or match
#'
#' Once articles were split, you can access all the "preambles",
#' or first articles, or articles that contain certain words
#' with this function.
#' @param t A list of treaty texts that has been "splitted" with
#' `manypkgs::split_treaty()`
#' @param article Either the "preamble" or an article number
#' across all treties to be returned.
#' It can be left NULL if user intends to look for a word in
#' all treaties.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @importFrom purrr map_chr
#' @return A list of treaty sections of the same length
#' @examples
#' \dontrun{
#' t <- head(qEnviron::texts$IEADB_TXT$Text)
#' t <- split_treaty(t)
#' get_treaty(t, article = "preamble")
#' get_treaty(t, article = 1)
#' get_treaty(t, match = "constitution")
#' get_treaty(t, article = "preamble", match = "unofficial")
#' }
#' @export
get_treaty <- function(t, article = NULL, match = NULL) {

  if (is.null(article) & is.null(match)) {
    stop("Please declare either an article to be returned or a word match to be found")
  }
  if (is.numeric(article)) {
    for (k in seq_len(length(t))) {
      a <- list(rep("NA",  as.numeric(article) + 1))
      t[k] <- ifelse(lengths(t[k]) < as.numeric(article) + 1, t[k] <- a, t[k])
    }
    t <- purrr::map_chr(t, c(as.numeric(article) + 1))
  }
  if (isTRUE(article == "preamble")) {
    t <- purrr::map_chr(t, 1)
  }
  if(!is.null(match)) {
    t <- lapply(t, function(x) grep(match, x, value = TRUE))
  }
  t
}
