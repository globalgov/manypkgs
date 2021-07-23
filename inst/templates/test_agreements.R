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

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Title))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_true(any(grepl("ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Signature))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Force))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Beg), "messydt")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Beg)))
  #                        {{{dab}}}[["{{{dat}}}"]]$Beg)))
  # expect_false(any(grepl("^[:digit:]{3}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Beg)))
  # expect_false(any(grepl("^[:digit:]{1}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Signature), "messydt")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Signature)))
  # expect_false(any(grepl("^[:digit:]{2}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Signature)))
  # expect_false(any(grepl("^[:digit:]{3}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Sighature)))
  # expect_false(any(grepl("^[:digit:]{1}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Signature)))
})

test_that("Column `Force` has standardised dates", {
  expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Force), "messydt")
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Force)))
  expect_false(any(grepl("^[:alpha:]$",
                         {{{dab}}}[["{{{dat}}}"]]$Force)))
  # expect_false(any(grepl("^[:digit:]{2}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Force)))
  # expect_false(any(grepl("^[:digit:]{3}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Force)))
  # expect_false(any(grepl("^[:digit:]{1}$",
  #                        {{{dab}}}[["{{{dat}}}"]]$Force)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$End)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    # expect_false(any(grepl("^[:digit:]{2}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$End)))
    # expect_false(any(grepl("^[:digit:]{3}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$End)))
    # expect_false(any(grepl("^[:digit:]{1}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$End)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Rat)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    # expect_false(any(grepl("^[:digit:]{2}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Rat)))
    # expect_false(any(grepl("^[:digit:]{3}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Rat)))
    # expect_false(any(grepl("^[:digit:]{1}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Rat)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Term)) {
    expect_equal(class({{{dab}}}[["{{{dat}}}"]]$Term), "messydt")
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    # expect_false(any(grepl("^[:digit:]{2}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Term)))
    # expect_false(any(grepl("^[:digit:]{3}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Term)))
    # expect_false(any(grepl("^[:digit:]{1}$",
    #                        {{{dab}}}[["{{{dat}}}"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[1] < {{{dab}}}[["{{{dat}}}"]]$Beg[10])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[50] < {{{dab}}}[["{{{dat}}}"]]$Beg[75])
  expect_true({{{dab}}}[["{{{dat}}}"]]$Beg[100] < {{{dab}}}[["{{{dat}}}"]]$Beg[120])
})
