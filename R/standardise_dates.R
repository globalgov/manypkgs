#' Standardises a wide range of date inputs
#'
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
#' qCreate = qCreate::standardise_dates(OriginalDate)
#' ) %>% print(n = 25)
#' @export
standardise_dates <- standardize_dates <- function(...) {
  dots <- list(...)
  if (length(dots) == 1) {
    x <- messydates::as_messydate(...)
  } else if (length(dots) == 3) {
    x <- messydates::make_messydate(...)
    x <- messydates::as_messydate(x)
  } 
}

#' Resetting century of future events
#'
#' Resets the century of (unlikely) future events
#' @param dates First variable to be used, required.
#' Can be a character or date variable.
#' @param sep Year from which to make a cut-off and return the last century.
#' By default this is the system date, but can be specified.
#' @return A vector the same length as \code{dates}
#' @details This function will use the system date as the cut-off
#' for identifying past or future events.
#' It is geared towards numeric dates (i.e. "12") not named dates (i.e. "Dec"),
#' though these dates should be correctly parsed regardless of order or separator.
#' It will return any datestamps of "9999-12-31",
#' which may be interpreted as unknown/future date, as is.
#' Please note that different types of separators in the same vector,
#' for example c("2004-12-12","12/12/2004", "12.12.2004"),
#' may confuse the algorithm.
#' In other words, this function is built for internally consistent codes.
#' @examples
#' \dontrun{
#' recent(head(tfd_agree$Sign))
#' }
#' @export
recent <- function(dates, sep = NULL) {
  .Deprecated("qCreate::standardise_dates")
}

#' Resorting and filtering dates
#'
#' Resorting and filtering dates
#' @param data a dataframe
#' @param vars a character vector identifying columns in the dataframe to sequence
#' @param unity a string identifying how multiple entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA),
#'                    Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01"))
#' resequence(data, c("Sign", "Force"))
#' }
#' @export
resequence <- function(data, vars, unity = "_") {

  len <- length(vars)

  out <- apply(data[,vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)),unity)))

    if (length(dates) < len) {
      dates <- qCreate::interleave(dates, which(is.na(x)))
    }

    if (length(dates) > len) {
      if (sum((!grepl("-01-01", dates)) * 1) >= len) dates <- dates[!grepl("-01-01", dates)]
      if (sum((!grepl("9999", dates)) * 1) >= len) dates <- dates[!grepl("9999", dates)]

      dmax <- max(lubridate::as.duration(interval(dates[1 : (length(dates)-1)],
                                                  dates[2 : (length(dates))])))
      dmax <- which(lubridate::as.duration(interval(dates[1 : (length(dates)-1)],
                                                    dates[2 : (length(dates))])) == as.duration(dmax))
      dates <- dates[c(1,dmax + 1)]
    }

    dates
  })

  t(out)

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
  
  l <- length(vect)
  j <- 0
  for (i in 1:length(pos)) {
    if (pos[i] == 1)
      vect <- c(elems[j + 1], vect)
    else if (pos[i] == length(vect) + 1)
      vect <- c(vect, elems[j + 1])
    else
      vect <- c(vect[1:(pos[i] - 1)], elems[j + 1], vect[(pos[i]):length(vect)])
    j <- j + 1
  }
  return(vect)
}
