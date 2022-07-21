treaty_text <- list("preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article 2 \narticle 3: open for accession
\narticle 4: shall terminate as \n shall be dissolved
\nannex: this is an annex", "preamble: this is the preamble \narticle 1: this is the article 1
\narticle 2: this is the article of an agreement",
                    "agreement between states <!-- if(window.location==) this page uses javascript//-->",
                    "<[document{the form}")

test_that("Treaty text is standardised correctly", {
  treaty <- standardise_treaty_text(treaty_text)
  treaty1 <- standardize_treaty_text(treaty_text)
  expect_false(any(stringr::str_detect(treaty, "<.*?>|\r|\t|\n")))
  expect_length(treaty, 2)
  expect_equal(treaty, treaty1)
})