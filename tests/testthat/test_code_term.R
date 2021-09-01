# Test term date is correctly identified from treaty title
test_that("treaty term date is identified from treaty title", {
  # Add title to the code_dates function arguments
  expect_equal(code_term(data$title),
               as.Date(c("1998-04-18", "1998-04-18", NA, "1999-04-18", NA, "2009-03-12", "1998-09-18")))
})
