treaty <- "preamble: this is the preamble \narticle 1: this is the article 1 \narticle 2: this is the article 2"

test_that("Treaty text is splitted correctly", {
  t <- split_treaty(treaty)
  expect_length(t, 1)
  expect_equal(lengths(t), 3)
  expect_equal(get_treaty(t, article = "preamble"), "preamble: this is the preamble ")
  expect_equal(get_treaty(t, article = 1), " 1: this is the article 1 ")
  expect_equal(unlist(get_treaty(t, match = "preamble")), "preamble: this is the preamble ")
})
