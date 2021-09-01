data <- data.frame(title = c("Treaty For The Period From 1 September 1992 To 18 April 1998 Between France And Spain",
                             "Treaty Between France And Italy For The Period Between The 1 September 1992 To 18 April 1998",
                             "Treaty Signed in Berlin 8 August 1998 Between Germany and Bulgaria",
                             "Treaty Concerning Geneva Lake For The Period 1 September 1992 To 18 April 1999",
                             "Protocol To The 17 May 1998 Convention on Fisheries",
                             "Treaty For The Period Between 22 February 2002 To 12 March 2009 Concerning Jan Mayen Seal Fishery",
                             "Amendments For The Period 19 May 1992 To 18 September 1998 Convention For The Prevention Of Pollution From Ships"),
                   date = c("1783-10-03", "1783-10-03", "1876-10-28", "1876-10-28", "1982-12-10", "1876-10-28", "1876-10-28"))

# Test term type is detected correctly from treaty title
test_that("treaty type is identified from treaty title", {
  # Add title to the code_dates function arguments
  expect_equal(code_term_type(data$title), 
               c("EXP", "EXP", NA, "EXP", NA, "EXP", "EXP"))
})

# Test term date is correctly identified from treaty title
test_that("treaty term date is identified from treaty title", {
  # Add title to the code_dates function arguments
  expect_equal(code_term_date(data$title),
               as.Date(c("1998-04-18", "1998-04-18", NA, "1999-04-18", NA, "2009-03-12", "1998-09-18")))
})

# Test that table appears if no argument is mentioned
test_that("function returns information when no argument is mentioned", {
  expect_type(code_term_type(), "character")
})
