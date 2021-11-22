# Test if {{{dataset}}} meets the many packages universe requirements

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

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]])))
})

# Contains the required variables
test_that("object has the correct variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(ID))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(End))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Label))
})

# Variables with dates are standardized
test_that("dates are standardised", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Beg), "messydt")
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$End), "messydt")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$End)))
})

# Labels are standardized
test_that("labels are standardised", {
  expect_false(any(grepl("U\\.S\\.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("U\\.K\\.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
})
