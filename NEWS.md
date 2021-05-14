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
