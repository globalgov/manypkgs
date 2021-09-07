data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Protocol to the Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean"),
                   date = c("1980-05-08", "1990-12-31", "1981-01-30", "1971-02-02", "1982-12-03", "1976-12-03", "1983-04-29", "1973-02-21", "1973-02-21"))


test_that("Code_agreements helper functions work properly", {
  expect_equal(code_parties(data$title), c("CPV-PRT[FSD]", "CPV-PRT[FSD]", NA, NA, NA, NA, NA, "RUS-USA[KTC]", "RUS-USA[FON]"))
  expect_equal(code_type(data$title), c("A", "P", "A", "A", "P", "A", "E1", "A", "A"))
  expect_equal(code_dates(data$date), c("1980", "1990",
                                        "1981", "1971",
                                        "1982", "1976",
                                        "1983", "1973", "1973"))
  expect_equal(code_known_agreements(data$title), c(NA, NA, NA, "RAMSA_1971",
                                                    "RAMSA_1971", NA, NA, NA, NA))
  expect_equal(code_acronym(data$title), c("CPVPFD", "CPVPFD", "TD06LJ", "WIIEWH", "WIIEWH", "PRTRPC",
                                           "PRTRPC", "GU11TC", "GU13PO"))
  expect_equal(code_linkage(data$title, data$date), c("FSD_1980A", "FSD_1980A", "",
                                                      "RAMSA_1971A", "RAMSA_1971A", "PRTRPC_1976A", 
                                                      "PRTRPC_1976A" , "", ""))
})

# Test for datasets that have ranged dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 19819"),
                    date = c("1980-01-01:1980-12-31", "1981-01-01:1981-12-31"))

test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments
  expect_equal(code_dates(data2$date), c("1980", "1981"))
})

# Test linkages to mother treaties
data3 <- data.frame(title = c("Protocol On Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999",
                              "Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999"),
                    date = c("2015-10-30", "2019-02-01"))

test_that("code_agreements() links only agreements as mather treaties", {
  expect_equal(code_agreements(data3, data3$title, data3$date), c("CPREMJ_2015P", "CPREMJ_2019E"))
})

# Test that treaty types are assigned correctly
data4 <- data.frame(title = c("Declaration Modifying Agreement on the River",
                              "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                              "Strategy On The Agreement On The River Basin"),
                    date = c("1999-07-12", "1912-09-02", "2019-03-15"))

test_that("code_agreements() recognizes the correct type of treaty", {
  expect_equal(code_agreements(data4, data4$title, data4$date), c( "DCLRMR_1999R", "CEBRIP_1912N", "RIVER_2019S"))
})

# Test numbers assigned to procotol/amendment
data5 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                              "Amendments Of The Limitation Amounts In The 1992 Convention (Annex 4)"),
                    date = c("1899-10-02", "2000-10-18", "2010-10-10"))

test_that("code_agreements() identify correct number of protocol or amendment", {
  expect_equal(code_agreements(data5, data5$title, data5$date), c("TRCSNR_1899E18", "LMTTNA_2000E34", "LMTTNA_2010E4"))
})

# Test that some functions return coding information when argument is missing
test_that("certain functions return coding information when argument is missing", {
  expect_type(code_type(), "character")
  expect_type(code_known_agreements(), "character")
  expect_type(code_activity(), "character")
})

# Test that the punctuation marks are not in the qID
test_that("Punctation marks are not in the qID", {
  qID <- code_agreements(data5, data5$title, data5$date)
  expect_false(any(grepl("\\(|\\)", qID)))
})
