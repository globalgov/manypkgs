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

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
})

# Country column is standardized
test_that("labels are standardised", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Country)) {
    expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]])))
    expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]])))
    expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]])))
    expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
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