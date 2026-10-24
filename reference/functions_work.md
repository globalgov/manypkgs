# Checking whether functions work across different types of objects

`functions_work()` calls each of the functions on each of the objects,
and reports which calls run without an error.

## Usage

``` r
functions_work(functions, objects)
```

## Arguments

- functions:

  A character vector of the names of functions. Each function must take
  the object as its first argument.

- objects:

  A character vector of the names of objects.

## Value

A logical matrix with a row for each function and a column for each
object. An element is `TRUE` where the function runs on the object
without an error.

## Examples

``` r
 library(manynet)
 functions_work(c("net_nodes", "net_ties", "net_density"),
                c("ison_adolescents", "ison_southern_women"))
#> ℹ `net_density()` was moved since manynet 2.0.0. Please use
#>   `netrics::net_by_density()`
#> ℹ `net_density()` was moved since manynet 2.0.0. Please use
#>   `netrics::net_by_density()`
#>             ison_adolescents ison_southern_women
#> net_nodes               TRUE                TRUE
#> net_ties                TRUE                TRUE
#> net_density            FALSE               FALSE
```
