# Building and editing the tutorials of a package

`build_tutorials()` rebuilds every learnr tutorial in `inst/tutorials/`
of the package that you work in, for example `{manynet}`, `{autograph}`,
or `{netrics}`. For each tutorial it builds:

- the prerendered HTML file that learnr serves, next to the source
  `.Rmd`, and the folder of figures that this file links to, if the
  tutorial draws any

- a static article in `vignettes/articles/` for the pkgdown website

The articles are a preview of the tutorial and not a replacement for it.
The transform shows the demonstration code but does not run it, drops
blank exercise chunks and their hints and solutions, and replaces each
quiz with a callout that points back to `run_tute()`. The only chunks it
runs are those with a label that starts with `explain-`. These chunks
draw the figures that explain the text, so the article shows their
output and hides their code.

`edit_tutorial()` opens in RStudio the learnr tutorial in
`inst/tutorials/` of the package in the working directory whose name is
nearest to `tute`.

## Usage

``` r
build_tutorials(path = ".", tutorials = NULL, html = TRUE, articles = TRUE)

edit_tutorial(tute)
```

## Arguments

- path:

  Path to the root of the package. By default this is the working
  directory.

- tutorials:

  A character vector that selects the tutorials to build. Each element
  matches the tutorial folder name or the file name, for example
  "netrics1" or "centrality". By default all the tutorials are built.

- html:

  Logical. By default `TRUE`, which rebuilds the prerendered HTML of
  each tutorial. Set this to `FALSE` to build the articles only.

- articles:

  Logical. By default `TRUE`, which rebuilds the static article of each
  tutorial. Set this to `FALSE` to build the HTML only.

- tute:

  The name of a tutorial, or of its folder. A name that starts with
  "tut" is matched to the folder names, and any other name to the file
  names.

## Value

`build_tutorials()` returns a character vector of the paths of the files
and figure folders that are built, returned invisibly.

`edit_tutorial()` returns the path to the tutorial, invisibly.

## Details

The name of the article file comes from the article in
`vignettes/articles/` that already holds the title of the tutorial. If
there is no such article, the name comes from the title itself. For
example the title "Making Network Data" gives
`vignettes/articles/making-network-data.Rmd`.

A tutorial that draws figures outside its exercises, such as in the
`explain-` chunks, links to them in a `<tutorial>_files/` folder next to
the HTML file. The tutorial does not show these figures without the
folder, so commit the folder together with the HTML file. Do not embed
such figures in the HTML file as base64 data: learnr replaces an image
with a video player where the data of the image contains the text "v/".

## Examples

``` r
if (FALSE) { # \dontrun{
build_tutorials()
build_tutorials(tutorials = "centrality", html = FALSE)
edit_tutorial("centrality")
} # }
```
