# Code Agreement Linkages

Identify the linkage between amendments and protocols to a main
agreement.

## Usage

``` r
code_linkage(title, date, return_all = FALSE)
```

## Arguments

- title:

  A character vector of treaty title

- date:

  A date variable

- return_all:

  Do you want all the variables to be returned in a list? By default,
  FALSE.

## Value

A character vector of the agreements that are linked

## Details

The function identifies duplicates by excluding "predictable" words from
strings, this maintains key words then used to identify and link
duplicates. This is a choice that considers errors should lie on the
side of false negatives rather than false positives. For the complete
list of words removed from title to identify duplicates please run the
function without arguments (i.e. `code_linkage()`).

## Examples

``` r
if (FALSE) { # \dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_linkage(IEADB$Title, IEADB$Begin)
} # }
```
