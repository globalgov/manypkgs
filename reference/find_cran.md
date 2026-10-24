# Finding what CRAN asks to fix

These functions find problems that `R CMD check --as-cran` reports and
that CRAN asks to be fixed before it accepts a package:

- `find_undocumented_values()` lists the help files of a package that
  have no `\value{}` section. CRAN asks for every help file of a
  function to say what the function returns. Help files for data and for
  the package itself are left out.

- `find_nonascii()` looks through the datasets of a package for
  character variables or node attributes that contain characters that
  are not ASCII. CRAN asks for the data in a package to be ASCII, or to
  declare an encoding.

## Usage

``` r
find_undocumented_values(package = NULL)

find_nonascii(pkg)
```

## Arguments

- package:

  The path to the root of a package, or the name of an installed
  package. By default `NULL`, which uses the working directory.

- pkg:

  The name of an installed package.

## Value

`find_undocumented_values()` returns a character vector of the names of
the help files that have no `\value{}` section.

`find_nonascii()` returns a character vector of the datasets and
variables, in the form "dataset\$variable", that contain non-ASCII
characters, invisibly. Each is also printed.

## Examples

``` r
find_undocumented_values("manypkgs")
#> character(0)
find_nonascii("datasets")
```
