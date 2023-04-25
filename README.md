
# manypkgs <img src="man/figures/manypkgslogo.png" align="right" width="220"/>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/manypkgs)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/manypkgs)
![GitHub
issues](https://img.shields.io/github/issues-raw/globalgov/manypkgs)
<!-- [![HitCount](http://hits.dwyl.com/globalgov/manydata.svg)](http://hits.dwyl.com/globalgov/manydata) -->
[![Codecov test
coverage](https://codecov.io/gh/globalgov/manypkgs/branch/main/graph/badge.svg)](https://codecov.io/gh/globalgov/manypkgs?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/manypkgs/badge)](https://www.codefactor.io/repository/github/globalgov/manypkgs)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4867/badge)](https://bestpractices.coreinfrastructure.org/projects/4867)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/jhollway/roctopus/total) -->
<!-- badges: end -->

`{manypkgs}` is a package from the [many](https://github.com/globalgov)
packages universe. It provides contributors the necessary tools to put
their data in the hands of users. The package includes functions to make
this easier to set up a ‘many’ package, import their existing data, and
export them in structures consistent with other packages in the many
packages universe.

For more details, please see the
[vignette](https://globalgov.github.io/manypkgs/articles/developer.html).

## Downloading and installing manypkgs

The development version of `{manypkgs}` can be downloaded from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("globalgov/manypkgs")
```

## Cheat Sheet

<a href="https://github.com/globalgov/manypkgs/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manypkgs/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## The many packages universe

The many packages universe aims at making it easier to collect, correct,
and and connect network data across issue-domains of global governance.
[manydata](https://github.com/globalgov/manydata) is the core package
ofnthe many packages universe. The package makes it easy to find,
download, and analyze all the data included in the various ‘many
packages’. For example, the `manydata::get_packages()` function can be
used to discover the packages currently available in the many packages
universe.

``` r
manydata::get_packages()
```

    ## # A tibble: 7 × 6
    ##   Name        Repository            Installed Latest Updated    Description     
    ##   <chr>       <chr>                 <chr>     <chr>  <date>     <chr>           
    ## 1 manydata    globalgov/manydata    0.8.2     0.8.2  2022-11-17 An R portal for…
    ## 2 manyenviron globalgov/manyenviron 0.3.0     0.3.0  2023-03-22 Ensembled data …
    ## 3 manyhealth  globalgov/manyhealth  0.1.1     0.2.0  2023-02-14 Ensembled data …
    ## 4 manypkgs    globalgov/manypkgs    0.3.0     0.3.0  2023-04-21 Support for cre…
    ## 5 manystates  globalgov/manystates  0.1.0     0.2.0  2022-10-17 Ensembled data …
    ## 6 manytrade   globalgov/manytrade   0.3.0     0.3.0  2023-03-22 Ensembled data …
    ## 7 messydates  globalgov/messydates  0.3.5     0.3.5  2023-01-20 An R package fo…

Please see [the website](https://globalgov.github.io/manypkgs/) for more
information about how to use `{manypkgs}` as a developer or as a data
contributor. For more information on `{manydata}`, please see [the
website](https://globalgov.github.io/manydata/).
