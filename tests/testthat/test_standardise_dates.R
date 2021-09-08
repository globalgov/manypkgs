test_that("standardise_dates() treats typical dates correctly", {
  expect_equal(standardise_dates("2010-01-01"), messydates::as_messydate("2010-01-01"))
})

test_that("standardise_dates() takes three variables as input",{
  expect_equal(standardise_dates("2010", "01", "01"), messydates::as_messydate("2010-01-01"))
})

test_that("standardise_dates() treats reverse ordered dates correctly", {
  expect_equal(standardise_dates(lubridate::dmy("30.1.1874")), messydates::as_messydate("1874-01-30"))
  expect_equal(standardise_dates(lubridate::dmy("6/10/2012")), messydates::as_messydate("2012-10-06"))
  expect_equal(standardise_dates(lubridate::dmy("9.4.1939")), messydates::as_messydate("1939-04-09"))
  expect_equal(standardise_dates(lubridate::dmy("12/5/1993")), messydates::as_messydate("1993-05-12"))
  expect_equal(standardise_dates(lubridate::mdy("10.30.93")), messydates::as_messydate("1993-10-30"))
})

test_that("standardise_dates() treats future dates correctly",{
  expect_match(standardise_dates("9999-12-31"), messydates::as_messydate("9999-12-31"))
})


test_that("standardise_dates() treats multiple inconsistent dates correctly",{
  expect_equal(as.character(standardise_dates(c("2010-10-12", "2010-11-13",
                                                "2010-12-14"))),
               c(messydates::as_messydate("2010-10-12"), messydates::as_messydate("2010-11-13"), messydates::as_messydate("2010-12-14")))
  dat1 <- data.frame(date = as.Date(c("1351-08-01", "1353-10-20",
                                      "1403-06-27", "1407-03-10",
                                      "1656-07-17", "NA", "1867-04-29")))
  # # Example of errors from datasets in qEnviron
  expect_equal(as.character(standardise_dates(dat1$date)),
               c(messydates::as_messydate("1351-08-01"), messydates::as_messydate("1353-10-20"), messydates::as_messydate("1403-06-27"),
                 messydates::as_messydate("1407-03-10"), messydates::as_messydate("1656-07-17"), NA, messydates::as_messydate("1867-04-29")))
  dat2 <- data.frame(date = c("Sep 12, 2009", "Oct 1, 2019",
                              "Nov 13, 1998", "May 13, 2003"))
  expect_equal(as.character(standardise_dates(lubridate::mdy(dat2$date))),
               c("2009-09-12", "2019-10-01", "1998-11-13", "2003-05-13"))
})
