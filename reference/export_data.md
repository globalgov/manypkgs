# Adding datasets to the many packages universe

Save a cleaned data object, consistent with the manydata universe, ready
to be lazy-loaded and create scripts for documenting and testing that
object within the new package.

## Usage

``` r
export_data(..., datacube, URL = NULL)
```

## Arguments

- ...:

  Unquoted name of the dataset object to save.

- datacube:

  Quoted name of any existing datacube or of the datacube to be created.

- URL:

  Website URL to the source of a dataset. Null by default. Please make
  sure to fill in documentation file by hand if not provided.

## Value

This function saves the dataset to the named datacube, silently creates
a set of tests for this dataset, and creates and opens documentation for
the dataset.

## Details

The function creates a data directory, if nonexistent, and saves cleaned
data. The functions also creates a script for testing the cleaned data
and make sure it complies with manydata requirements. As well, it
creates a documentation script to help documenting data sources and
describing variables. Note that users need to provide a `.bib` file for
citation purposes alongside their dataset in the corresponding
`data-raw` subfolder.

## Examples

``` r
if (FALSE) { # \dontrun{
export_data(COW, datacube = "states",
URL = "https://correlatesofwar.org/data-sets/state-system-membership")
} # }
```
