treaty <- "preamble: this is the preamble \narticle 1: this is the article 1 
\narticle 2: this is the article 2 \narticle 3: open for accession \narticle 4: shall terminate as \n shall be dissolved"

test_that("Treaty text is splitted correctly", {
  t <- get_articles(treaty)
  expect_length(t, 1)
  expect_equal(lengths(t), 5)
  expect_equal(get_articles(treaty, article = "preamble"), "preamble: this is the preamble ")
  expect_equal(get_articles(treaty, article = "memberships"), list(" 3: open for accession "))
  expect_equal(get_articles(treaty, article = "termination"), list(" 4: shall terminate as \n shall be dissolved"))
  expect_equal(get_articles(treaty, article = 1), " 1: this is the article 1 \n")
  expect_equal(get_articles(treaty, match = "preamble"), list("preamble: this is the preamble "))
})

# Test that the helper function split_treaty() works on annexes
treaty2 <- "preamble: this is the preamble \narticle 1: this is the article 1 
\narticle 2: this is the article 2 \narticle 3: open for accession \nannex I: supplementary informations"

test_that("Treaty are divided by articles and annex", {
  expect_equal(get_articles(treaty2, "annex"), as.list("∑ i: supplementary informations"))
})
