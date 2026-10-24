# manypkgs ![](reference/figures/manypkgslogo.png)

[manypkgs](https://github.com/globalgov/manypkgs) is a package from the
[many](https://github.com/globalgov) packages universe. It provides
contributors the necessary tools to put their data in the hands of
users. The package includes functions to make this easier to set up a
‘many’ package, import their existing data, and export them in
structures consistent with other packages in the many packages universe.

For more details, please see the
[vignette](https://globalgov.github.io/manypkgs/articles/contributor.html).

## Downloading and installing manypkgs

The development version of
[manypkgs](https://github.com/globalgov/manypkgs) can be downloaded from
GitHub.

``` r

# install.packages("remotes")
remotes::install_github("globalgov/manypkgs")
```

## Cheat Sheet

[![The manypkgs
cheatsheet](https://raw.githubusercontent.com/globalgov/manypkgs/main/man/figures/cheatsheet.png)](https://github.com/globalgov/manypkgs/blob/main/man/figures/cheatsheet.pdf)

## The many packages universe

The many packages universe aims at making it easier to collect, correct,
and and connect network data across issue-domains of global governance.
[manydata](https://github.com/globalgov/manydata) is the core package
ofnthe many packages universe. The package makes it easy to find,
download, and analyze all the data included in the various ‘many
packages’. For example, the
[`manydata::call_packages()`](https://www.manydata.ch/reference/call_packages.html)
function can be used to discover the packages currently available in the
many packages universe.

``` r

manydata::call_packages()
```

``` R
## # A tibble: 5 × 4
##   Name         Description                                    Installed Latest 
##   <chr>        <chr>                                          <chr>     <chr>  
## 1 manydata     The portal for global governance data          0.9.3     "0.9.3"
## 2 manypkgs     Support for creating new manyverse packages    0.3.0     "0.3.0"
## 3 manystates   Many data on sovereign states                  0.2.0     "0.2.0"
## 4 manytreaties Many data on international agreements          0.0.1     ""     
## 5 messydates   R package for Extended Date/Time Format (EDTF) 0.5.1     "0.5.1"
## Would you like to update manytreaties ? (Yes/no/cancel)
```

Please see [the website](https://globalgov.github.io/manypkgs/) for more
information about how to use
[manypkgs](https://github.com/globalgov/manypkgs) as a developer or as a
data contributor. For more information on
[manydata](https://www.manydata.ch/), please see [the
website](https://www.manydata.ch/).
