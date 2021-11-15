#' Standardises a wide range of date inputs
#'
#' The function is wrapper for `messydates::as_messydate()`.
#' The function standardises a wide range of date inputs parsed through it,
#' and convert it into a messydt class
#' It accepts date inputs in different formats, incomplete dates,
#' historical dates and future dates. It also creates nested
#' vectors of dates for vague date inputs, ambiguous and ranged dates,
#' into a range of dates.
#' @param ... One (ymd) or three (yyyy, mm, dd) variables
#' @details The function seeks to convert a wide range of dates into
#' dates so that these can be meaningfully used for analysis.
#' There are several limitations of other date wrangling packages
#' and/or functions for dealing with incomplete dates, dates with
#' different or inconsistent formats or historical dates for which
#' `standardise_dates()`, and `{messydates}` more broadly, can be used.
#' It also converts  ambiguous and ranged dates into a
#' range of dates.
#' @return Nested vector of dates under messydt class
#' @importFrom messydates make_messydate as_messydate
#' @examples
#' dates_comparison <- tibble::tribble(~Example, ~OriginalDate,
#' "A normal date", "2010-01-01",
#' "A historical date", "1712-01-01",
#' "A really historical date", "712-01-01",
#' "A very historical date", "012-01-01",
#' "A clearly future date", "9999-12-31",
#' "A not so clearly future date", "2599-12-31")
#' dates_comparison %>% dplyr::mutate(
#' lubridate = suppressWarnings(lubridate::as_date(OriginalDate)),
#' anytime = anytime::anydate(OriginalDate),
#' manypkgs = manypkgs::standardise_dates(OriginalDate)
#' ) %>% print(n = 25)
#' @export
standardise_dates <- standardize_dates <- function(...) {
  dots <- list(...)
  if (length(dots) == 1) {
    x <- messydates::as_messydate(...)
  } else if (length(dots) == 3) {
    x <- messydates::make_messydate(...)
  }
}

#' Interleaving two vectors by position
#'
#' Insert elements in different positions for vectors
#' @param vect Main vector
#' @param pos Positions to be inserted
#' @param elems Elements to be inserted at those positions.
#' By default, these are NAs (missing values).
#' @return A vector the length of the sum of \code{vect}
#' and \code{pos}.
#' @examples
#' interleave(1:5, c(2,4))
#' @export
interleave <- function(vect, pos, elems = NA) {

  j <- 0
  for (k in seq_len(length(pos))) {
    if (pos[k] == 1)
      vect <- c(elems[j + 1], vect)
    else if (pos[k] == length(vect) + 1)
      vect <- c(vect, elems[j + 1])
    else
      vect <- c(vect[1:(pos[k] - 1)],
                elems[j + 1],
                vect[(pos[k]):length(vect)])
    j <- j + 1
  }
  return(vect)
}
