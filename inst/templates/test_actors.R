# Test if {{{dataset}}} meets the q ecosystem requirements

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
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(ID))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Actor))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Country))
})

# Column Beg is in messydt class
test_that("Beg column is in messydt class", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Beg), "messydt")
})

# Country column is standardized
test_that("Country column is standardised", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Country)) {
    expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]]$Country)))
    expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]]$Country)))
    expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]]$Country)))
    expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]]$Country)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
test_that("dataset is arranged by date variable", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Beg)) {
    expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[1] < {{{dab}}}[["{{{dat}}}"]]$Beg[10])
    expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[50] < {{{dab}}}[["{{{dat}}}"]]$Beg[75])
    expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[100] < {{{dab}}}[["{{{dat}}}"]]$Beg[120])
  }
})