# Finding the minimum R version that a package needs

`find_transitive_minR()` finds the highest minimum version of R declared
by the packages on CRAN that a package depends upon, directly or
indirectly. `find_required_r()` does the same from the installed
packages, so it also works for packages that are not on CRAN, and it
also says which packages need that version.

## Usage

``` r
find_transitive_minR(package)

find_required_r(pkg)
```

## Source

https://blog.r-hub.io/2022/09/12/r-dependency/

## Arguments

- package, pkg:

  The name of a package.

## Value

`find_transitive_minR()` returns the minimum version of R as a
`package_version`. `find_required_r()` returns a list with the version
of R that the package itself declares (`current`), the highest version
that it or its dependencies declare (`max_required_R`), the packages
that declare that version (`responsible_packages`), and the version that
each package declares (`all_requirements`).

## Examples

``` r
if (FALSE) { # \dontrun{
find_transitive_minR("manynet")
find_required_r("manypkgs")
} # }
```
