# Keep objects from the global environment

Function for keeping objects from the global environment. It removes all
variables of a dataset in the global environment except those which are
specified in the given character vector, regular expression or both.

## Usage

``` r
retain(
  keep = NULL,
  envir = .GlobalEnv,
  keep_functions = TRUE,
  gc_limit = 100,
  regex = "auto"
)
```

## Source

https://bitbucket.org/mehrad_mahmoudian/varhandle/src/master/R/rm.all.but.R

## Arguments

- keep:

  A vector containing the name of variables which you wish to keep and
  not remove or a regular expression to match variables you want to
  keep. Variable names and regular expressions can be used combined if
  the argument 'regex' is set to "auto". (Mandatory)

- envir:

  The environment that this function should be functional in, search in
  and act in. (Optional)

- keep_functions:

  A logical vector of length 1 indicating exclusion of function
  variables from removal. (optional)

- gc_limit:

  A numeric vector of length 1 indicating the threshold for garbage
  collection in Megabyte (MB) scale. (Optional)

- regex:

  A vector with length 1 to define whether the function use regular
  expression in keep (TRUE or FALSE) or auto detect ("auto")

## Value

Clears the environment except for the stated objects.

## Details

The function is is suitable for Small to semi-large projects with
massive amount of data as it performs variable operations in a fast and
efficient way. The function is also useful for developers preparing and
assembling a pipeline

## Examples

``` r
origins <- c("New Zealand", "Brazil", "Switzerland")
members <- c(1, 1, 2)
team <- data.frame(origins, members)
team
#>       origins members
#> 1 New Zealand       1
#> 2      Brazil       1
#> 3 Switzerland       2
retain("origins")
#> Error in retain("origins"): All the items in the keep should be a real existing
#>                    variable or valid regular expressions.
#> The following
#>                    is/are not among variables of selected environment or
#>                    patterns that match anything!
#>  origins
origins
#> [1] "New Zealand" "Brazil"      "Switzerland"
```
