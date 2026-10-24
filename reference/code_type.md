# Code Agreement Type

Identify the type of international agreement from titles. Agreements can
be, for example, multilateral treaties or coventions (A), protocols (P)
or amendments (E), if they contain words in title.

## Usage

``` r
code_type(title)
```

## Arguments

- title:

  A character vector of treaty title

## Value

A character vector of the treaty type

## Details

Types of agreements differentiate agreements from protocols or
amendments, for example. For the complete list of types of agreements
coded please run the function without an argument (i.e. `code_type()`).

## Examples

``` r
if (FALSE) { # \dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_type(IEADB$Title)
} # }
```
