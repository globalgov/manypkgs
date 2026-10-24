# Updates packages of our data universe

As our packages evolve, expectations about package structures as well as
templates might also change. This function updates files for packages
setup with
[`setup_package()`](https://globalgov.github.io/manypkgs/reference/setup_package.md).

## Usage

``` r
update_package(package = NULL, name = NULL, path = getwd())
```

## Arguments

- package:

  The name of the package to be updated, optional. When not declared
  package name is extracted from description.

- name:

  A list of vectors giving the package author(s)' name(s). Optional.
  Authors(s)last name(s) and first name(s) are separated by a comma.

- path:

  The file path, optional. If not specified, function gets the current
  working directory.

## Value

Updated files in correct structure for packages.

## Details

Some of the files in the .github folder, such as workflow actions, pull
request templates, and issue templates, are automatically updated. The
function also asks developers if other files such as the package
license, contributing file, and code of conduct should be updated.

## Examples

``` r
if (FALSE) { # \dontrun{
update_package("manystates")
} # }
```
