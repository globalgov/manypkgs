# Code Agreement Titles

Creates an ID that contains information on the parties to an agreement,
the type of agreement, the date, and the linkage to other agreements.

## Usage

``` r
code_agreements(dataset = NULL, title, date)
```

## Arguments

- dataset:

  A 'many' dataset. If provided without a title and date variables, the
  function finds title and date conforming columns in the dataset. The
  function "expects" that there are variables named `Title` and `Begin`
  that they have been standardised using `standardise_titles()` and
  [`messydates::as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.html),
  respectively.

- title:

  A title variable. The function "expects" that the variable has been
  standardised using `standardise_titles()`.

- date:

  A date variable. The function "expects" that the variable has been
  standardised using
  [`messydates::as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.html).

## Value

a character vector with the treatyIDs

## Examples

``` r
if (FALSE) { # \dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_agreements(dataset = IEADB)
code_agreements(title = IEADB$Title, date = IEADB$Begin)
} # }
```
