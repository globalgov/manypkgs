# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\s$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("N\\.A\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("n\\.a\\.$", {{{dab}}}[["{{{dat}}}"]])))
})

# Contains the required variables
test_that("object has the correct variables", {
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(ID))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Begin))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Actor))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(StateName))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]])))
})

# Column Beg is in mdate class
test_that("Beg column is in mdate class and standardized", {
  expect_s3_class({{{dab}}}[["{{{dat}}}"]]$Begin, "mdate")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         {{{dab}}}[["{{{dat}}}"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         {{{dab}}}[["{{{dat}}}"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         {{{dab}}}[["{{{dat}}}"]]$Begin)))
})
