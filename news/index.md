# Changelog

## manypkgs 0.4.0

### Package

- Updated GitHub workflows, issue templates, and the pull request
  template
- Updated CONTRIBUTING with a table of contents and `NEWS.md`
  conventions
- Updated the website reference index to the functions now in the
  package
- Dropped `{english}` and
  [textclean](https://github.com/trinker/textclean) from Imports
- Updated CITATION to use
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html)

### Coding

- Removed `code_dates()` in favour of
  [`messydates::year()`](https://lubridate.tidyverse.org/reference/year.html)
- Removed `code_acronym()` in favour of
  [`manytreaties::code_acronyms()`](https://globalgov.github.io/manytreaties/reference/code_acronyms.html)
- Moved `code_states()` to
  [manystates](https://globalgov.github.io/manystates/)
- Moved `standardise_titles()` and `standardise_treaty_text()` to
  [manytreaties](https://globalgov.github.io/manytreaties/)
- Moved `read_clauses()` to
  [manytreaties](https://globalgov.github.io/manytreaties/)
- Moved `code_lineage()`, `code_domain()`, and `code_entity()` to
  [manytreaties](https://globalgov.github.io/manytreaties/)
- Moved the accession and termination functions to
  [manytreaties](https://globalgov.github.io/manytreaties/)
  - `code_accession_terms()`, `code_term()`, and `code_term_date()`
- Improved
  [`code_linkage()`](https://globalgov.github.io/manypkgs/reference/code_linkage.md)
  to code parties with
  [`manystates::code_states()`](https://globalgov.github.io/manystates/reference/code_states.html)
  - Keeps parties only where two are found, with their activity

### Constructing

- Improved
  [`export_data()`](https://globalgov.github.io/manypkgs/reference/export_data.md)
  - Dropped the prompt to recode the IDs and titles of the other
    datasets
  - Dropped the documentation and test scripts that it added

### Developing

- Added
  [`build_tutorials()`](https://globalgov.github.io/manypkgs/reference/tutorials.md)
  to build the HTML and articles of learnr tutorials
- Added
  [`bigo_net()`](https://globalgov.github.io/manypkgs/reference/bigo_net.md)
  to time a network function on networks of growing size
- Added
  [`group_functions()`](https://globalgov.github.io/manypkgs/reference/group_functions.md)
  to list the exported functions of a package by prefix
- Added
  [`find_required_r()`](https://globalgov.github.io/manypkgs/reference/find_r.md)
  to find the R version that installed packages need
- Added
  [`find_undocumented_values()`](https://globalgov.github.io/manypkgs/reference/find_cran.md)
  to list the help files without a `\value{}`
- Added
  [`find_installed_pkg_sizes()`](https://globalgov.github.io/manypkgs/reference/find_installed_pkg_sizes.md)
  to list the installed packages by size
- Added
  [`evaluate_bugs()`](https://globalgov.github.io/manypkgs/reference/evaluate_bugs.md)
  to get the GitHub issues of a package
- Improved
  [`functions_work()`](https://globalgov.github.io/manypkgs/reference/functions_work.md)
  to catch errors without
  [berryFunctions](https://github.com/brry/berryFunctions)
- Improved
  [`print_help()`](https://globalgov.github.io/manypkgs/reference/print_help.md)
  to use only exported functions from `{tools}`
- Improved
  [`find_nonascii()`](https://globalgov.github.io/manypkgs/reference/find_cran.md)
  to check data frames as well as networks
- Fixed
  [`call_cran()`](https://globalgov.github.io/manypkgs/reference/call_cran.md)
  to use its `author` argument
- Fixed
  [`evaluate_bugs()`](https://globalgov.github.io/manypkgs/reference/evaluate_bugs.md)
  to pass the repository and not the URL to
  [githubr](https://github.com/fhdsl/githubr)

## manypkgs 0.3.1

### Package

- Updated GitHub checks and release actions
- Closed [\#99](https://github.com/globalgov/manypkgs/issues/99) and
  [\#98](https://github.com/globalgov/manypkgs/issues/98) with updated
  regex matches in countryregex to reduce double matching
- Updated templates for preparation and test scripts to use standardized
  variables and refer to ‘many package’.
- Closed [\#95](https://github.com/globalgov/manypkgs/issues/95) with
  fixes and updates to ‘agreements’ vignette.
- Added package startup message to
  [`setup_package()`](https://globalgov.github.io/manypkgs/reference/setup_package.md)
  and
  [`update_package()`](https://globalgov.github.io/manypkgs/reference/update_package.md)
  functions.
- Amended agreement_type matches in sysdata to correctly identify types
  of agreements.
- Updated documentation and functions to refer to datacubes rather than
  databases.
- Updated test templates to refer to `Begin` rather than `Beg` variable.

### Connection

- Added
  [`functions_work()`](https://globalgov.github.io/manypkgs/reference/functions_work.md)
  to see which functions work on which objects
- Added
  [`print_help()`](https://globalgov.github.io/manypkgs/reference/print_help.md)
  for printing a function’s help page to the console
- Added
  [`edit_tutorial()`](https://globalgov.github.io/manypkgs/reference/tutorials.md)
  for opening a tutorial for editing
- Added
  [`call_cran()`](https://globalgov.github.io/manypkgs/reference/call_cran.md)
  for listing an author’s CRAN packages and when each can be updated
- Added
  [`find_transitive_minR()`](https://globalgov.github.io/manypkgs/reference/find_r.md)
  for finding the minimum R version a package needs
- Added
  [`find_nonascii()`](https://globalgov.github.io/manypkgs/reference/find_cran.md)
  for finding non-ASCII characters in a package’s data

### Correction

- Closed [\#97](https://github.com/globalgov/manypkgs/issues/97) by
  updating
  [`code_agreements()`](https://globalgov.github.io/manypkgs/reference/code_agreements.md)
  and
  [`export_data()`](https://globalgov.github.io/manypkgs/reference/export_data.md)
  functions to expect ‘Begin’ rather than ‘Beg’ variable
- Expanded `code_domain()` to code topics for health agreements.

## manypkgs 0.3.0

### Package

- Closed [\#87](https://github.com/globalgov/manypkgs/issues/87) by
  updating data templates for new changes with HUGGO and AIGGO new
  structure
- Renamed `retrieve_clauses()` to `read_clauses()`

### Correction

- Closed [\#89](https://github.com/globalgov/manypkgs/issues/89) by
  updating
  [`code_agreements()`](https://globalgov.github.io/manypkgs/reference/code_agreements.md)
  and
  [`condense_agreements()`](https://globalgov.github.io/manypkgs/reference/condense_agreements.md)to
  improve duplicated matching and methods “fuzzy” matching of treatyIDs
  for the creation of manyIDs
- Closed [\#90](https://github.com/globalgov/manypkgs/issues/90) by
  updating `code_states()`, previously called `code_parties()`, to work
  similarly as it did in
  [manystates](https://globalgov.github.io/manystates/) to avoid
  unnecessary overlaps
- Updated country regex data to improve states’ ID matching
- Closed [\#91](https://github.com/globalgov/manypkgs/issues/91) by
  updating `standardising_treaty_titles()` to be faster, more concise,
  and consistent
- Closed [\#92](https://github.com/globalgov/manypkgs/issues/92) by
  updating
  [`export_data()`](https://globalgov.github.io/manypkgs/reference/export_data.md)
  to ask users if treatyIDs, manyIDs, and treaty titles in datasets in a
  database need to also be updated

## manypkgs 0.2.2

### Package

- Closed [\#55](https://github.com/globalgov/manypkgs/issues/55) by
  updating README and workflow files to download
  [manydata](https://www.manydata.ch/) from CRAN
- Closed [\#74](https://github.com/globalgov/manypkgs/issues/74) by
  increasing package logo size
- Updated workflow action files to run faster by implementing package
  caching
- Closed [\#78](https://github.com/globalgov/manypkgs/issues/78) by
  defining consistent ways of dealing with code errors in ‘many’ data
  packages
- Closed [\#85](https://github.com/globalgov/manypkgs/issues/85) by
  adding guidelines for writing in markdown documents to contributing
  files

### Correction

- Closed [\#18](https://github.com/globalgov/manypkgs/issues/18) by
  adding
  [`convert_pdf()`](https://globalgov.github.io/manypkgs/reference/convert_pdf.md)
  function that extract texts from PDF files
- Added `retrieve_clauses()` function
  - Divided `get_articles()` into `retrieve_clauses()` for retrieving
    treaty clauses and `standardise_treaty_text()` for standardising
    texts  
  - Closed [\#75](https://github.com/globalgov/manypkgs/issues/75) by
    fixing memory usage issues with function
- Updated accession and termination functions
  - Renamed `code_memebership()` function `code_accession_terms()` for
    clarity
  - Closed [\#76](https://github.com/globalgov/manypkgs/issues/76) by
    improving dictionary matching for these functions
- Closed [\#77](https://github.com/globalgov/manypkgs/issues/77) by
  making
  [`standardise_dates()`](https://globalgov.github.io/manypkgs/reference/defunct.md)
  defunct and favour calling
  [messydates](https://globalgov.github.io/messydates/) directly for
  dates conversion
- Closed [\#80](https://github.com/globalgov/manypkgs/issues/80) by
  updating `standardise_titles()` to use
  [deeplr](https://www.deepl.com/translator) package for translating
  titles
- Closed [\#84](https://github.com/globalgov/manypkgs/issues/84) by
  adding `standardise_texts()` function to standardise treaty texts in
  data preparation scripts
- Renamed `get_memberships()` and `get_links()` to
  `retrieve_memberships()` and
  [`retrieve_links()`](https://www.manydata.ch/reference/defunct.html),
  respectively, for clarity

## manypkgs 0.2.1

### Package

- Closed [\#11](https://github.com/globalgov/manypkgs/issues/11) by
  adding more informative [skimr](https://docs.ropensci.org/skimr/)
  output in documentation
- Closed [\#52](https://github.com/globalgov/manypkgs/issues/52) by
  moving `interleave()` to the
  [messydates](https://globalgov.github.io/messydates/) package
- Closed [\#61](https://github.com/globalgov/manypkgs/issues/61) and
  [\#63](https://github.com/globalgov/manypkgs/issues/63) by changing
  references from “qPackages” into “manyPackages”
- Closed [\#71](https://github.com/globalgov/manypkgs/issues/71) by
  changing references from many_ID and treaty_ID to manyID and treatyID

### Correction

- Updated termination and membership functions
  - Renamed `code_grounds()` function `code_memberships()` for clarity
    and consistency
  - Added `code_term_date()` to code treaty termination date
  - Added `get_memberships()` to get memberships’ lists as a data frame
  - Closed [\#51](https://github.com/globalgov/manypkgs/issues/51) by
    integrating texts into `code_term()` and `code_memberships()`
  - Closed [\#64](https://github.com/globalgov/manypkgs/issues/64) by
    improving how `code_term()` and `code_memberships()` extract
    information from texts
- Added `code_lineage()` function to code lineage from agreement titles
  - Added `code_entity()` function to extract entities from treaty
    titles
  - Added `code_domain()` function to code domains from agreement titles
  - Closed [\#65](https://github.com/globalgov/manypkgs/issues/65) and
    [\#70](https://github.com/globalgov/manypkgs/issues/70) by updating
    dictionary of words used to identify domains from treaty titles
  - Added `get_links()` to get links from manyID facilitate plotting
    with [migraph](https://stocnet.github.io/migraph/)
  - Closed [\#68](https://github.com/globalgov/manypkgs/issues/68) by
    writing tests for `code_lineage()` and related functions

### Connection

- Closed [\#17](https://github.com/globalgov/manypkgs/issues/17) by
  updating translate argument in `standardise_titles()`
- Closed [\#66](https://github.com/globalgov/manypkgs/issues/66) by
  fixing bugs with `lingua()` function
- Updated functions to structure texts function
  - Renamed `get_treaty()` function `get_articles()` for clarity
  - Updated `split_treaty()` to become a helper function to
    `get_articles()`
  - Closed [\#69](https://github.com/globalgov/manypkgs/issues/69) by
    making `get_articles()` and `split_treaty()` work on various types
    of treaty texts

## manypkgs 0.2.0

### Package

- Closed [\#58](https://github.com/globalgov/manypkgs/issues/58) by
  renaming package from `qCreate` to `manypkgs`
- Closed [\#28](https://github.com/globalgov/manypkgs/issues/28) by
  adding cheatsheet to README

### Correction

- Closed [\#57](https://github.com/globalgov/manypkgs/issues/57) by
  updating `code agreements()` and `condense_qID()` to make code more
  concise
- Closed [\#48](https://github.com/globalgov/manypkgs/issues/48) by
  updating data preparation template in
  [`export_data()`](https://globalgov.github.io/manypkgs/reference/export_data.md)
- Updated the countryregex table used in `code_parties()` so that
  countries are detected more accurately

### Connection

- Updated `standardise_titles()`
  - Closed [\#9](https://github.com/globalgov/manypkgs/issues/9) by
    adding `lingua()` function for translating text to English
- Closed [\#47](https://github.com/globalgov/manypkgs/issues/47) by
  adding functions to structure treaty text
  - Added `split_treaty()` to split treaty text into preamble and
    articles
  - Added `get_treaty()` to get all matching articles by number or word
    match
