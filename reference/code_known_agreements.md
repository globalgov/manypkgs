# Code Known Agreements Abbreviation

Some agreements have known abbreviations that facilitate their
identification.

## Usage

``` r
code_known_agreements(title)
```

## Arguments

- title:

  A character vector of treaty title

## Value

A character vector of abbreviation of known treaties

## Details

The function identifies agreements that match the list of known
agreements with their titles, abbreviations and signature dates and
substitutes the known titles for abbreviations.For the complete list of
known agreements coded for and their respective abbreviations please run
the function without an argument (i.e. `code_known_agreements()`).

## Examples

``` r
if (FALSE) { # \dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_known_agreements(IEADB$Title)
} # }
```
