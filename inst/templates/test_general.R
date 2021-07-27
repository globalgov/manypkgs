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

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Label)) {
  expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
  }
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]]))
  expect_false(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]]))
  expect_false(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]]))
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Beg)) {
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
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null({{{dab}}}[["{{{dat}}}"]]$Beg)) {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[25] < {{{dab}}}[["{{{dat}}}"]]$Beg[50])
    }
})
