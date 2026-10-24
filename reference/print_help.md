# Printing a help page to the console

`print_help()` prints the help page of a function to the console, or
into a knitr document, as text, HTML, LaTeX, or Rd.

## Usage

``` r
print_help(
  topic,
  format = c("text", "html", "latex", "Rd"),
  lines = NULL,
  before = NULL,
  after = NULL
)
```

## Source

https://www.r-bloggers.com/2013/06/printing-r-help-files-in-the-console-or-in-knitr-documents/

## Arguments

- topic:

  The name of the function or help topic, as a character string or
  unquoted.

- format:

  The format to print the help page in, one of "text" (the default),
  "html", "latex", or "Rd".

- lines:

  An optional integer vector that selects the lines to print.

- before, after:

  Optional character vectors to print before or after the help page.

## Value

The lines of the help page, invisibly.

## Examples

``` r
print_help("depends", lines = 1:5)
#> _L_o_a_d_i_n_g _p_a_c_k_a_g_e_s
#> 
#> _D_e_s_c_r_i_p_t_i_o_n:
#> 
#>      Function for loading and, if necessary, installing packages
```
