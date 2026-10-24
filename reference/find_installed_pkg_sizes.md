# Finding the sizes of the installed packages

`find_installed_pkg_sizes()` lists the installed packages in all the
library paths, from the largest to the smallest. It uses the `du`
command, so it works on macOS and Linux only.

## Usage

``` r
find_installed_pkg_sizes()
```

## Value

A tibble with the size of each package in kilobytes (`size`) and its
name (`name`).

## Examples

``` r
if (FALSE) { # \dontrun{
find_installed_pkg_sizes()
} # }
```
