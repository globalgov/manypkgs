treaty_text <- list("preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article 2 \narticle 3: open for accession
\narticle 4: shall terminate as \n shall be dissolved
\nannex: this is an annex", "preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article of an agreement")

test_that("Treaty text is splitted correctly", {
  treaty <- standardise_texts(treaty_text)
  expect_length(treaty, 2)
  expect_length(get_articles(treaty), 2)
  expect_equal(get_articles(treaty, article = "preamble"), list("preamble: this is the preamble ",
                                                                "preamble: this is the preamble "))
  expect_equal(get_articles(treaty, article = "accession"), list("ARTICLE 3: open for accession ", "NA"))
  expect_equal(get_articles(treaty, article = "termination"),
               list("ARTICLE 4: shall terminate as shall be dissolved ", "NA"))
  expect_equal(get_articles(treaty, article = "annex"), list("ANNEX : this is an annex", "NA"))
  expect_equal(get_articles(treaty, match = "preamble"), list("preamble: this is the preamble ",
                                                              "preamble: this is the preamble "))
  expect_equal(as.character(get_articles(treaty, treaty_type = "agreements")),
               c("NA", "c(\"preamble: this is the preamble \", \"ARTICLE 1: this is the article 1 \", \"ARTICLE 2: this is the article of an agreement\")"))
  expect_equal(as.character(get_articles(treaty, treaty_type = "amendments")), c("NA", "NA"))
})
