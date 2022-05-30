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

# Uniformity tests (agreements have a countryID and Beg columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(CountryID))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Beg))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Beg), "mdate")
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

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by the `Beg` variable", {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[1] <
                {{{dab}}}[["{{{dat}}}"]]$Beg[100])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[120] <
                {{{dab}}}[["{{{dat}}}"]]$Beg[220])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[250] <
                {{{dab}}}[["{{{dat}}}"]]$Beg[350])
})
