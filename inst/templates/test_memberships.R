# Test if {{{dataset}}} meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\s$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("N\\.A\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("n\\.a\\.$", {{{dab}}}[["{{{dat}}}"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Country))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]]))
  expect_false(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]]))
  expect_false(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]]))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Beg), "messydt")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         {{{dab}}}[["{{{dat}}}"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Optional dates columns are standardized", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Force)) {
    expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Force), "messydt")
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Signature)) {
    expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Signature), "messydt")
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$End)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Rat)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Term)) {
    expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Term), "messydt")
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by the `Beg` variable", {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[1] < {{{dab}}}[["{{{dat}}}"]]$Beg[10])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[50] < {{{dab}}}[["{{{dat}}}"]]$Beg[75])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[100] < {{{dab}}}[["{{{dat}}}"]]$Beg[120])
})
