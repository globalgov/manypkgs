test_that("url is valid", {
  expect_error(export_data("COW", databse = "states", URL = 5),
               "Please provide the URL argument to the source of your dataset as a character string.")
})

a <- data.frame(Title = c("A treaty about things", "treaty about thing"),
                Beg = c("2010-01-01", "2010-01-01"),
                treatyID = c("TRTYTH_2010A", "TRTYTH_2010A"),
                manyID = c("TRTYTH_2010A", "TRTYTH_2010A"))
b <- data.frame(Title = c("A treaty about things", "treaty about thing"),
                Beg = c("2010-01-01", "2010-01-01"),
                treatyID = c("a", "a"),
                manyID = c("a", "a"))
ab <- tibble::lst(a, b)
cd <- tibble::lst(a, a)
names(cd) <- c("a" ,"b")

test_that("titles, treatyIDs, and manyIDs are correctly updated", {
  expect_equal(update_ids(ab), update_ids(cd))
})
