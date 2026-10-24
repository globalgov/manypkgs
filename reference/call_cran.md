# Calling CRAN for the packages of an author

`call_cran()` lists the packages on CRAN whose authors include `author`,
with the number of days since each was published, and the first date on
which a new version can be submitted. CRAN asks that a package is not
updated more often than every 10 days or so.

## Usage

``` r
call_cran(author = "Hollway")
```

## Arguments

- author:

  A character string that matches the name of an author. By default
  "Hollway".

## Value

A tibble with the variables `Package`, `Version`, `SincePub` (days since
the package was published), `Updateable` (whether 10 days have passed),
and `CanCRAN` (the first date on which a new version can be submitted).

## Examples

``` r
if (FALSE) { # \dontrun{
call_cran("Hollway")
} # }
```
