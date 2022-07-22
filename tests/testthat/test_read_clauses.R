treaty_text <- list("preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article 2 \narticle 3: open for accession
\narticle 4: shall terminate as \n shall be dissolved
\nannex: this is an annex", "preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article of an agreement")

test_that("Treaty text is splitted correctly", {
  treaty <- standardise_treaty_text(treaty_text)
  expect_length(read_clauses(treaty), 2)
  expect_equal(read_clauses(treaty, article = "preamble"), list("preamble: this is the preamble ",
                                                                "preamble: this is the preamble "))
  expect_equal(read_clauses(treaty, article = "accession"), list("ARTICLE 3: open for accession ", "NA"))
  expect_equal(read_clauses(treaty, article = "termination"),
               list("ARTICLE 4: shall terminate as shall be dissolved ", "NA"))
  expect_equal(read_clauses(treaty, article = "annex"), list("ANNEX : this is an annex", "NA"))
  expect_equal(read_clauses(treaty, match = "preamble"), list("preamble: this is the preamble ",
                                                              "preamble: this is the preamble "))
  expect_equal(as.character(read_clauses(treaty, treaty_type = "agreements")),
               c("NA", "c(\"preamble: this is the preamble \", \"ARTICLE 1: this is the article 1 \", \"ARTICLE 2: this is the article of an agreement\")"))
  expect_equal(as.character(read_clauses(treaty, treaty_type = "amendments")), c("NA", "NA"))
})
