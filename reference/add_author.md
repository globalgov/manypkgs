# Add an author to the current package

Adds an author to the description file of the current package.

## Usage

``` r
add_author(
  orcid = NULL,
  name = NULL,
  role = NULL,
  email = NULL,
  affiliation = NULL
)
```

## Arguments

- orcid:

  Character string of the author's ORCID number. If this is null, then
  the function switches to manual entry.

- name:

  A vector giving the package author(s)' name(s). Authors(s)last name(s)
  and first name(s) are separated by a comma.

- role:

  Character vector of role(s) the author has in the project. Contributor
  by default. For example "c(aut, cre, ctb)".

- email:

  Character string of the author's email

- affiliation:

  Character vector of the author's miscellaneous information such as
  his/her institution.

## Value

Adds a new author to the description file of the package

## Details

This function adds an author to the description file of the current
package. This can be done in two ways. First you can specify the ORCID
number of the author you want to add. This will leverage the excellent
`rorcid` package and scrape the information from the ORCID API and fill
out the description file automatically. Second, you can specify the
arguments manually if the author does not have an ORCID number. Finally,
note that by default the role of the new author is set to contributor.

## Examples

``` r
if (FALSE) { # \dontrun{
add_author(orcid = "0000-0002-8361-9647", role = list(c("aut", "cre", "ctb")))
add_author(name = "Smith, John",
affiliation = "University of Somewhere")
} # }
```
