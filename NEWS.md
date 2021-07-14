# qCreate 0.1.0

## Package
* Added `link_metadata()`
* Added `update_package()`
* Moved `interleave()` from `{qData}` to `{qCreate}`
* Updated code_agreements() with the addition of these helper functions: 
  * Added `code_activity()`
  * Added `code_acronym()`

## Connection
* Added `update_package()` to update qPackage when the functions or templates are changed 
* Added `link_metadata()` to link metadata of the original dataset when dataset are exported
* Made `code_agreements()` to differentiate treaties from same parties and year with `code_activity()` that generates abbreviation of the activity described in the treaty title
* Made `code_acronym()` generates a 6 letters acronym for multilateral to avoid false duplicates
* Updated test for `code_agreements()` to reflect the changes made
* Updates test templates for when datasets are exported into database in order to ensure date columns are under messydt class
* Added tests for the function `interleave()`
* Updated the agreements vignette to be in line with the new function qID
* Changed where `{qData}` was still mentioned in the function or files to `{qCreate}`

## Collection
* Fixes #37, #32, #34, #30, #24 by adding `code_actvity()` which generate activity abbreviation in the qID, and `code_acronym()` which create a title acronym for qID to `code_agreements()` function
* Fixes #35 by adding `code_activity()` in `code_agreements()`
* Fixes #31 by creating a list that appears when helper functions are called without argument.
* Fixes #29 by adding `code_acronym()` in `code_agreements()`
* Fixes #6 by creating the `update_package()` function
* Fixes #26 by adding some new the words used to detect the treaty type
* Fixes #25 by removing the linkage part when it links to a protocol or amendment
* Fixes #27 by adding types of number writings like roman numerals or "No5"
* Fixes #4 by adding translation for treaty titles other than english

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
