#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2022-05-17.
#' This wrapper function is deprecated and its functionality is included in the
#' original `make_messydates()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export

standardise_dates <- function(..., from_text = FALSE, interactive = FALSE) {
  .Defunct(msg = "This function has been removed in {manypkgs} v.0.2.2,
              please use messydates::make_messydate() directly instead.")
  dots <- list(...)
  if (length(dots) == 1) {
    x <- messydates::as_messydate(..., from_text, interactive)
  } else if (length(dots) == 3) {
    x <- messydates::make_messydate(..., from_text, interactive)
  }
  x
}

#' @describeIn defunct Deprecated on 2022-05-17.
#' This wrapper function is deprecated and its functionality is included in the
#' original `make_messydates()` function. Please refer to its documentation
#' for more details about the new implementation.
#' @export
standardize_dates <- standardise_dates
