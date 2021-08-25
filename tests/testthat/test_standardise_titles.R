test_that("capitalisation works",{
  expect_match(standardise_titles("A treaty to do things"), "A Treaty To Do Things")
})

test_that("white space is stripped",{
  expect_match(standardise_titles("A treaty to do things  "), "A Treaty To Do Things")
})

test_that("end of a sentence points are taken off",{
  expect_match(standardise_titles("A treaty to do things."), "A Treaty To Do Things")
})

test_that("special character like ¬í are deleted",{
  expect_equal(standardise_titles("Management Of Ships¬í Ballast Water And Sediments"), "Management Of Ships Ballast Water And Sediments")
})

test_that("Most of special charactes are deleted",{
  expect_equal(standardise_titles("Agreement - on specific ? topic (FAO)"), "Agreement On Specific Topic (FAO)")
})

test_that("Regions are spelled correctly",{
  expect_equal(standardise_titles(c("A treaty for the South-Eastern region", "The Convention for Northwest states")),
               c("A Treaty For The South Eastern Region", "The Convention For North West States"))
})

test_that("words are correctly standardised",{
  expect_equal(standardise_words(c("Treaty between U.K. and U.R.S.S.", "Deep-Sea and Land-Based Treaty")), 
               c("Treaty between UK and U.R.S.S.", "Deep Sea and Land Based Treaty"))
})
