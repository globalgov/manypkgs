# qCreate 0.1.0

## Package

* Setup the `{qCreate}`  package for developers and data contributors 
  * Moved the `setup_package()` function from `{qData}` to `{qCreate}`
  * Moved the `import_data()` function from `{qData}` to `{qCreate}`
  * Moved the `export_data()` function from `{qData}` to `{qCreate}`
  * Moved the `standardise_titles()` function from `{qData}` to `{qCreate}` 
  * Moved the `standardise_dates()` function from `{qData}` to `{qCreate}`
* Added agreements vignette for working with `code_agreements()`

## Correction

* Updated `standardise_titles()` to translate strings to English when API key is provided as an argument

## Connection

* Added `code_agreements()` function
  * Added `code_agreements()` that extracts meaningful information from agreements titles and dates
  * Added `code_parties()` to extract parties from agreement titles
  * Added `code_type()` to identify agreement types
  * Added `code_dates()` to assign a numerical ID based on the signature date
  * Added `code_known_agreements()` to facilitate identification of agreements with known abbreviations
  * Added `code_linkage()` to identify the links between amendments and protocols to a main agreement
  * Added tests for `code_agreements()` 
