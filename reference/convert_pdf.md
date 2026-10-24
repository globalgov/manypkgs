# Get and convert PDFs to text

Get and convert PDFs to text

## Usage

``` r
convert_pdf(path)
```

## Source

https://stackoverflow.com/questions/71064939/how-to-check-if-pdf-is-scanned-image-or-contains-text-in-r

## Arguments

- path:

  The file(s) path. The path can also be specified to a folder with
  multiple PDF files, in that case all PDF files in the folder will be
  converted.

## Value

A list with the converted texts.

## Details

If PDF file is not in "readable" format, function attempts to apply
optical character recognition (OCR) to convert these to text.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_pdf(path = getwd())
} # }
```
