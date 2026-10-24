# Code Abbreviations for Activity

Code in abbreviated form the activity of bilateral treaties' titles.

## Usage

``` r
code_activity(title)
```

## Arguments

- title:

  A character vector of treaty titles

## Value

A character vector of abbreviations of last words in treaty title.

## Details

Bilateral agreements usually detail their activity and specify area in
the last words of the titles. These last words are abbreviated by the
function to differentiate between bilateral treaties and avoid false
positives being generated since multiple, different, bilateral treaties
are often signed in the same day.
