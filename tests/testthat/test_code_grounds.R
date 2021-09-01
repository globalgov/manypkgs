# Test term type is detected correctly from treaty title
test_that("treaty type is identified from treaty title", {
  # Add title to the code_dates function arguments
  expect_equal(code_grounds(data$title), 
               c("EXP", "EXP", NA, "EXP", NA, "EXP", "EXP"))
})

# Test that table appears if no argument is mentioned
test_that("function returns information when no argument is mentioned", {
  expect_type(code_grounds(), "character")
})
