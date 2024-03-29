# manypkgs 0.3.0

## Package

- Closed #87 by updating data templates for new changes with HUGGO and AIGGO new structure
- Renamed `retrieve_clauses()` to `read_clauses()`

## Correction

- Closed #89 by updating `code_agreements()` and `condense_agreements()`to improve duplicated matching and methods "fuzzy" matching of treatyIDs for the creation of manyIDs
- Closed #90 by updating `code_states()`, previously called `code_parties()`, to work similarly as it did in `{manystates}` to avoid unnecessary overlaps
- Updated country regex data to improve states' ID matching
- Closed #91 by updating `standardising_treaty_titles()` to be faster, more concise, and consistent
- Closed #92 by updating `export_data()` to ask users if treatyIDs, manyIDs, and treaty titles in datasets in a database need to also be updated

# manypkgs 0.2.2

## Package

* Closed #55 by updating README and workflow files to download `{manydata}` from CRAN
* Closed #74 by increasing package logo size
* Updated workflow action files to run faster by implementing package caching
* Closed #78 by defining consistent ways of dealing with code errors in 'many' data packages
* Closed #85 by adding guidelines for writing in markdown documents to contributing files

## Correction

* Closed #18 by adding `convert_pdf()` function that extract texts from PDF files
* Added `retrieve_clauses()` function
  * Divided `get_articles()` into `retrieve_clauses()` for retrieving treaty clauses and `standardise_treaty_text()` for standardising texts  
  * Closed #75 by fixing memory usage issues with function
* Updated accession and termination functions
  * Renamed `code_memebership()` function `code_accession_terms()` for clarity
  * Closed #76 by improving dictionary matching for these functions
* Closed #77 by making `standardise_dates()` defunct and favour calling `{messydates}` directly for dates conversion
* Closed #80 by updating `standardise_titles()` to use `{deeplr}` package for translating titles
* Closed #84 by adding `standardise_texts()` function to standardise treaty texts in data preparation scripts
* Renamed `get_memberships()` and `get_links()` to `retrieve_memberships()` and `retrieve_links()`, respectively, for clarity

# manypkgs 0.2.1

## Package

* Closed #11 by adding more informative `{skimr}` output in documentation
* Closed #52 by moving `interleave()` to the `{messydates}` package
* Closed #61 and #63 by changing references from "qPackages" into "manyPackages"
* Closed #71 by changing references from many_ID and treaty_ID to manyID and treatyID

## Correction

* Updated termination and membership functions
  * Renamed `code_grounds()` function `code_memberships()` for clarity and consistency
  * Added `code_term_date()` to code treaty termination date
  * Added `get_memberships()` to get memberships' lists as a data frame
  * Closed #51 by integrating texts into `code_term()` and `code_memberships()`
  * Closed #64 by improving how `code_term()` and `code_memberships()` extract information from texts
* Added `code_lineage()` function to code lineage from agreement titles
  * Added `code_entity()` function to extract entities from treaty titles
  * Added `code_domain()` function to code domains from agreement titles
  * Closed #65 and #70 by updating dictionary of words used to identify domains from treaty titles
  * Added `get_links()` to get links from manyID facilitate plotting with `{migraph}` 
  * Closed #68 by writing tests for `code_lineage()` and related functions

## Connection

* Closed #17 by updating translate argument in `standardise_titles()`
* Closed #66 by fixing bugs with `lingua()` function
* Updated functions to structure texts function
  * Renamed `get_treaty()` function `get_articles()` for clarity
  * Updated `split_treaty()` to become a helper function to `get_articles()`
  * Closed #69 by making `get_articles()` and `split_treaty()` work on various types of treaty texts

# manypkgs 0.2.0

## Package
* Closed #58 by renaming package from `qCreate` to `manypkgs`
* Closed #28 by adding cheatsheet to README

## Correction
* Closed #57 by updating `code agreements()` and `condense_qID()` to make code more concise
* Closed #48 by updating data preparation template in `export_data()`
* Updated the countryregex table used in `code_parties()` so that countries are detected more accurately

## Connection
* Updated `standardise_titles()`
  * Closed #9 by adding `lingua()` function for translating text to English
* Closed #47 by adding functions to structure treaty text
  * Added `split_treaty()` to split treaty text into preamble and articles
  * Added `get_treaty()` to get all matching articles by number or word match

# qCreate 0.1.1

## Package

* Updated `setup_package()` function
  * Added a package citation template to be exported to new qPackage
  * Updated exported workflow templates to remove unnecessary PR checks for data packages
* Updated `import_data()` function
  * Closed #44 by adding argument to import codebook if available
  * Closed #45 by allowing text function to accept text delimited files
* Updated `export_data()` function
  * Closed #43 by adding qPackage name as an argument of `export_data()` 
  * Closed #41 by adding test templates for actor databases and memberships databases
* Closed #42 by adding `add_bib()` function to help developers cite datasets in the expected formats
* Updated agreements vignette by simplifying code chunks and expanding hand-coded sample

## Correction

* Closed #40 by adding the helper function `correct_words()` into `standardise_titles()`

## Connection

* Added `code_term()` which extracts the date term from the treaty title
* Added `code_grounds()` which indicates the type of termination clause the treaty has
* Added tests for `code_term()` and `code_grounds()` functions
* Updated the countryregex table with more regex options for certain countries
* Closed #50 and #23 by adding `condense_qID()` function to match similar, but not identical, qIDs for a database and return a qID_ref vector
* Added tests for `condense_qID()`

# qCreate 0.1.0

## Package

* Added `update_package()` function which updates packages created with `setup_package()`
* Moved `interleave()` from `{qData}` to `{qCreate}` to avoid dependency knots among q Packages
  * Added tests for `interleave()`
* Updated templates and scripts to correctly call `{qCreate}` instead of `{qData}` where necessary
* Updated agreements vignette to reflect new changes with `code_agreements()` function

## Correction

* Updated `standardise_titles()` 
  * Updated `standardise_titles()` to correct some words often misspelled in agreement titles
  * Updated `standardise_titles()` to allow for translation of treaty titles in other languages to English if API is provided
* Updated `standardise_dates()` function
  * Made `standardise_dates()` a wrapper function for `as_messydate()` from `{messydates}`
  * Updated tests for `standardise_dates()` to reflect new changes
  * Updates test templates used to setup q Packages to ensure dates are in messydt class

## Connection

* Updated code_agreements() and added more functions
  * Updated the structure of the qIDs generated by `code_agreements()`
  * Added `code_activity()` function which generates for bilateral treaties titles
  * Added `code_acronym()` function which generates unique abbreviations from treaty titles
  * Updated how `code_linkages()` to remove certain words and to not return non-agreement linkages 
  * Updated `code_types()` to better capture meaningful order for agreements
  * Updated `code_type()` and `code_known_agreements()`  to return coding lists when function is called without arguments
  * Updated test for `code_agreements()` to reflect the changes made to function

# qCreate 0.0.1

## Package

* Setup the `{qCreate}`  package for developers and data contributors 
  * Moved `setup_package()` and related functions from `{qData}`
  * Moved `import_data()` and related functions from `{qData}`
  * Moved `export_data()` and related functions from `{qData}`
* Added agreements vignette for working with `code_agreements()`

## Correction

* Moved `standardise_titles()` from `{qData}` to `{qCreate}` 
  * Updated `standardise_titles()` to translate strings to English when API key is provided as an argument
* Moved `standardise_dates()` from `{qData}` to `{qCreate}`

## Connection

* Added `code_agreements()` that extracts meaningful information from agreements titles and dates
  * Added `code_parties()` to extract parties from agreement titles
  * Added `code_type()` to identify agreement types
  * Added `code_dates()` to assign a numerical ID based on the signature date
  * Added `code_known_agreements()` to facilitate identification of agreements with known abbreviations
  * Added `code_linkage()` to identify the links between amendments and protocols to a main agreement
  * Added tests for `code_agreements()` 
