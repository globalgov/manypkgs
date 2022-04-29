#' Standardises a wide range of date inputs
#'
#' The function is wrapper for `messydates::as_messydate()`.
#' The function standardises a wide range of date inputs parsed through it,
#' and convert it into a messydt class
#' It accepts date inputs in different formats, incomplete dates,
#' historical dates and future dates. It also creates nested
#' vectors of dates for vague date inputs, ambiguous and ranged dates,
#' into a range of dates.
#' @name standardise_dates
#' @param ... One (ymd) or three (yyyy, mm, dd) variables
#' @param interactive By default FALSE.
#' If TRUE, it allows users to choose the correct order for
#' ambiguous 6 digit date in a messydt vector.
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
#' @importFrom stringr str_detect
#' @importFrom utils menu
#' @examples
#' library(tibble)
#' dates_comparison <- tibble::tribble(~Example, ~OriginalDate,
#' "A normal date", "2010-01-01",
#' "A historical date", "1712-01-01",
#' "A really historical date", "33 AD",
#' "A very historical date", "300 BC",
#' "A clearly future date", "9999-12-31",
#' "A not so clearly future date", "2599-12-31")
#' dates_comparison %>% dplyr::mutate(
#' lubridate = suppressWarnings(lubridate::as_date(OriginalDate)),
#' anytime = anytime::anydate(OriginalDate),
#' manypkgs = manypkgs::standardise_dates(OriginalDate)
#' ) %>% print(n = 25)
#' @export
standardise_dates <- function(..., interactive = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    x <- messydates::as_messydate(...)
  } else if (length(dots) == 3) {
    x <- messydates::make_messydate(...)
  }
  if (isTRUE(interactive)) {
    x <- ifelse(stringr::str_detect(x, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2}$)") &
                  as.numeric(gsub("-", "", stringr::str_extract(x, "^[:digit:]{2}-"))) < 32 &
                  as.numeric(gsub("-", "", stringr::str_extract(x, "-[:digit:]{2}-"))) < 32 &
                  as.numeric(gsub("-", "", stringr::str_extract(x, "[:digit:]{2}$"))) < 32, ask_user(x), x)
    x <- messydates::as_messydate(x)
    }
    x
}

#' @rdname standardise_dates
#' @export
standardize_dates <- standardise_dates

ask_user <- function(d) {
  input <- menu(c("Year-Month-Day", "Day-Month-Year", "Month-Day-Year"),
                title = paste0("What format is this date ", d," ?"))
  if (input == 1) {
    out = d
    message("Dates already in standard YMD format")
  }
  if (input == 2) {
    out <- stringr::str_replace_all(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$",
                                    "\\3-\\2-\\1")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  if (input == 3) {
    out <- stringr::str_replace_all(d, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$",
                                    "\\3-\\1-\\2")
    message("Ambiguous 6 digit dates have been changed to standard YMD format")
  }
  out
}
