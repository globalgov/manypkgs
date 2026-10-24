# Loading packages

Function for loading and, if necessary, installing packages

## Usage

``` r
depends(packages)
```

## Arguments

- packages:

  Character vector of packages to install from CRAN or GitHub

## Value

Loads and, if necessary, first installs packages

## Details

The function looks up required packages and loads the ones already
installed, while installing and loading for packages not installed. For
CRAN packages the package name is required as argument, for GitHub
username/repo are required as argument.

## Examples

``` r
if (FALSE) { # \dontrun{
depends("manypkgs")
} # }
```
