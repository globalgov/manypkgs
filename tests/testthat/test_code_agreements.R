data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Amendment 1 To The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Declaration Modifying Agreement on the River",
                             "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                             "Strategy On The Agreement On The River Basin",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean"),
                   date = c("1980-05-08", "1990-12-31", "1981-01-30", "1982-12-03", "1976-12-03", "1983-04-29", "1971-02-02", "1973-02-21", "1973-02-21"))

test_that("Code_agreements helper functions work properly", {
  expect_equal(code_type(data$title), c("A", "E1", "A", "R",
                                        "N", "S", "A", "A", "A"))
  expect_equal(code_dates(data$date), c("1980", "1990",
                                        "1981", "1982",
                                        "1976", "1983",
                                        "1971", "1973", "1973"))
  expect_equal(code_known_agreements(data$title), c(NA, "RAMSA_1971",
                                                    NA, NA, NA, NA,
                                                    "RAMSA_1971", NA, NA))
  expect_equal(code_acronym(data$title), c("CPVPFD", "WIIEWH",
                                           "TD06LJ", "DCLRMR",
                                           "CEBRIP", "RIVER",
                                           "WIIEWH", "GU11TC",
                                           "GU13PO"))
  expect_equal(code_linkage(data$title, data$date), c("", "RAMSA_1971A",
                                                      "", "", "", "",
                                                      "RAMSA_1971A",
                                                      "", ""))
})

# Test for datasets that have ranged dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 19819"),
                    date = c("1980-01-01:1980-12-31", "1981-01-01:1981-12-31"))

test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments
  expect_equal(code_dates(data2$date), c("1980", "1981"))
})

# Test numbers assigned to procotol/amendment
data5 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                              "Amendments Of The Limitation Amounts In The 1992 Convention (Annex 4)"),
                    date = c("1899-10-02", "2000-10-18", "2010-10-10"))

test_that("code_agreements() identify correct number of protocol or amendment", {
  expect_equal(code_agreements(data5, data5$title, data5$date),
               c("TRCSNR_1899E18", "LMTTNA_2000E34", "LMTTNA_2010E4"))
})

# Test that some functions return coding information when argument is missing
test_that("certain functions return coding information when argument is missing", {
  expect_type(code_type(), "character")
  expect_type(code_known_agreements(), "character")
})

# Test that the punctuation marks are not in the treatyID
test_that("Punctation marks are not in the treatyID", {
  treatyID <- code_agreements(data5, data5$title, data5$date)
  expect_false(any(grepl("\\(|\\)", treatyID)))
})

# Test that code_states works
test_that("Code_states codes parties from treaty titles properly", {
  expect_equal(code_states(data$title, parties = TRUE, activity = TRUE),
               c("CPV-PRT[FSD]",NA, NA, NA, NA, NA, NA,
                 "RUS-USA[KTC]", "RUS-USA[FON]"))
  expect_equal(code_states(data$title, parties = TRUE),
               c("CPV-PRT",NA, NA, NA, NA, NA, NA,
                 "RUS-USA", "RUS-USA"))
})

test <- data.frame(title = c("Korea and Democratic People's Republic of Korea",
                             "Guinea Equatorial",
                             "Guinea",
                             "Republic germany",
                             "German Democratic Republic",
                             "Austria-Hungary and Hungary",
                             "Hungary and Austria",
                             "East Pakistan",
                             "Pakistan",
                             "UK and US",
                             "USA and U.K.",
                             "UK/Uganda Treaty",
                             "UAE treaty",
                             "U.A.E.",
                             "Great Colombia",
                             "Gran Colombia",
                             "The US and EU",
                             "The US and E.U.",
                             "South Georgia",
                             "British east africa",
                             "Luxembourg",
                             "s. africa",
                             "south-africa",
                             "Northern rhodesia"))

test_that("states are given the correct abbreviation", {
  expect_equal(code_states(test$title, abbrev = TRUE), c("KOR_PRK", "GNQ",
                                                         "GIN", "DEU", "DDR",
                                                         "AUH_HUN", "AUT_HUN",
                                                         "BGD", "PAK",
                                                         "GBR_USA", "GBR_USA",
                                                         "GBR_UGA",
                                                         "ARE", "ARE",
                                                         "GCL", "GCL",
                                                         "EUE_USA", "EUE_USA",
                                                         NA, "KEN",
                                                         "LUX", "ZAF",
                                                         "ZAF", "ZMB"))
})

test_that("states are given the correct label", {
  expect_equal(code_states(test$title),
               c("Republic of Korea_Democratic People's Republic of Korea",
                 "Equatorial Guinea", "Guinea", "Germany (Prussia)",
                 "German Democratic Republic", "Austria-Hungary_Hungary",
                 "Austria_Hungary", "Bangladesh", "Pakistan",
                 "United Kingdom_United States of America",
                 "United Kingdom_United States of America",
                 "United Kingdom_Buganda", "United Arab Emirates",
                 "United Arab Emirates", "Gran Colombia", "Gran Colombia",
                 "European Union_United States of America",
                 "European Union_United States of America",
                 "South Georgia", "Kenya", "Luxembourg", "South Africa",
                 "South Africa", "Zambia"))
})
