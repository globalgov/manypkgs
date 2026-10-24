# Getting the GitHub issues of a package

`evaluate_bugs()` reads the `BugReports` field in the DESCRIPTION file
of an installed package, and gets the issues of its GitHub repository.

## Usage

``` r
evaluate_bugs(pkg)
```

## Arguments

- pkg:

  The name of an installed package.

## Value

A data frame of the issues, as returned by
[`githubr::get_issues()`](https://rdrr.io/pkg/githubr/man/get_issues.html).

## Examples

``` r
if (FALSE) { # \dontrun{
evaluate_bugs("manypkgs")
} # }
```
