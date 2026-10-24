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

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
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

test_that("Column `Signature` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Signature), "mdate")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         {{{dab}}}[["{{{dat}}}"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[1] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[10])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[50] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[75])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Begin[100] <
                {{{dab}}}[["{{{dat}}}"]]$Begin[120])
})
