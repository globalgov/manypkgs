# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\s$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("N\\.A\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("n\\.a\\.$", {{{dab}}}[["{{{dat}}}"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Begin), "mdate")
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

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[1] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[100])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[120] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[220])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[250] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[350])
})
