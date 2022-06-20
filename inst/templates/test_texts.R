# Test if the dataset meets the many packages universe requirements
# Test that certain columns exist
test_that("datasets have the required variables", {
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(Beg))
  expect_true(any(grepl("ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
  pointblank::expect_col_exists({{{dab}}}[["{{{dat}}}"]],
                                pointblank::vars(TreatyText))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date({{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(lubridate::is.POSIXct({{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(lubridate::is.POSIXlt({{{dab}}}[["{{{dat}}}"]]$Beg)))
})
