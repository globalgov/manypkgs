treaty <- list("preamble: this is the preamble \narticle 1: this is the article 1 
\narticle 2: this is the article 2 \narticle 3: open for accession
\narticle 4: shall terminate as \n shall be dissolved
\nannex: this is an annex", "preamble: this is the preamble \narticle 1: this is the article 1 
\narticle 2: this is the article of an agreement")

test_that("Treaty text is splitted correctly", {
  t <- get_articles(treaty)
  expect_length(t, 2)
  expect_equal(lengths(t), c(6, 3))
  expect_equal(get_articles(treaty, article = "preamble"), c("preamble: this is the preamble",
                                                             "preamble: this is the preamble"))
  expect_equal(get_articles(treaty, article = "memberships"), list("3: open for accession", NA))
  expect_equal(get_articles(treaty, article = "termination"),
               list("4: shall terminate as \n shall be dissolved", NA))
  expect_equal(get_articles(treaty, article = "annex"), list("annex: this is an annex", NA))
  expect_equal(get_articles(treaty, article = 1), c("1: this is the article 1",
                                                    "1: this is the article 1"))
  expect_equal(get_articles(treaty, match = "preamble"), list("preamble: this is the preamble",
                                                              "preamble: this is the preamble"))
  expect_equal(get_articles(treaty, treaty_type = "agreements"),
               ist("na",c("preamble: this is the preamble",
                          "1: this is the article 1",
                          "2: this is the article of an agreement")))
  expect_equal(get_articles(treaty, treaty_type = "amendments"), list("NA", NA))
})
