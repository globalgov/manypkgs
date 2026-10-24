# Grouping the functions of a package

`group_functions()` lists the exported functions of a package that match
a pattern, leaves out the defunct and deprecated functions, and groups
the rest by the prefix before the first underscore.

## Usage

``` r
group_functions(pattern = "", package = "manynet")
```

## Arguments

- pattern:

  A regular expression that the function names must match. By default
  "", which matches all of them.

- package:

  The name of an installed package. By default "manynet".

## Value

A named list of character vectors of function names, with one element
for each prefix.

## Examples

``` r
group_functions("^find", package = "manypkgs")
#> $find
#> [1] "find_installed_pkg_sizes" "find_nonascii"           
#> [3] "find_required_r"          "find_transitive_minR"    
#> [5] "find_undocumented_values"
#> 
```
