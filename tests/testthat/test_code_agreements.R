data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Protocol to the Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                   date = c("1980-05-08", "1990-12-31", "1981-01-30", "1971-02-02", "1982-12-03", "1976-12-03", "1983-04-29"))

test_that("Code_agreements() properly returns qIDs", {
  expect_equal(code_agreements(data, data$title, data$date), c("19800508_CPV-PRT", "19901231P_19800508_CPV-PRT",
                                                         "19810130A",  "RAMSA19710202",
                                                         "19821203P_RAMSA19710202",
                                                         "19761203A", "19830429E1_19761203A"))
})

test_that("Code_agreements helper functions work properly", {
  expect_equal(code_parties(data$title), c("CPV-PRT", "CPV-PRT", NA, NA, NA, NA, NA))
  expect_equal(code_type(data$title), c("A", "P", "A", "A", "P", "A", "E1"))
  expect_equal(code_dates(data$title, data$date), c("19800508", "19901231",
                                                    "19810130", "19710202",
                                                    "19821203", "19761203",
                                                    "19830429"))
  expect_equal(code_known_agreements(data$title), c(NA, NA, NA, "RAMSA19710202",
                                                    "RAMSA19710202", NA, NA))
  expect_equal(code_linkage(data$title, data$date), c("19800508_CPV-PRT",
                                                      "19800508_CPV-PRT", "",
                                                      "RAMSA19710202",
                                                      "RAMSA19710202",
                                                      "19761203A", "19761203A"))
})

# Test for datasets that have ranged dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 19819"),
                    date = c("1980-01-01:1980-12-31", "1981-01-01:1981-12-31"))
test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments
  expect_equal(code_dates(data2$title, data2$date), c("1980ACT01", "1981TTO01"))
})

# Test that date duplicates are returned correctly
data3 <- data.frame(title = c("Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean",
                             "Agreement Between The Government Of Kazakhstan And The Government Of Mongolia On Cooperation In The Field Of Environmental Protection",
                             "Agreement Between The Government Of Kazakhstan And The Government Of Mongolia On Cooperation In The Field Of Plant Quarantine"),
                   date = c("1973-02-21", "1973-02-21", "1998-03-12", "1998-03-12"))


test_that("code_agreements() differentiates treaties signed the same day", {
  expect_equal(code_agreements(data3, data3$title, data3$date), c("19730221_RUS-USA", "19730221[OP]_RUS-USA", 
                                                                  "19980312_KAZ-MNG", "19980312[CR]_KAZ-MNG"))
})

data4 <- data.frame(title = c("Protocol On Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999",
                              "Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999",
                              "International Convention For The Regulation Of Whaling",
                              "Amendments To The Schedule To The International Convention For The Regulation Of Whaling, 20-1 Meeting"),
                    date = c("2015-10-30", "2019-02-01", "1960-02-05", "1969-06-27"))

test_that("code_agreements() link treaties correctly", {
  expect_equal(code_agreements(data4, data4$title, data4$date), c("20151030P", "20190201E", "19600205A", "19690627E_19600205A"))
})

# Test that treaty types are assigned correctly
data5 <- data.frame(title = c("Declaration Modifying Agreement on the River",
                              "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                              "Strategy On The Agreement On The River Basin"),
                    date = c("1999-07-12", "1912-09-02", "2019-03-15"))

test_that("code_agreements() recognizes the correct type of treaty", {
  expect_equal(code_agreements(data5, data5$title, data5$date), c("19990712R", "19120902N", "20190315S"))
})

# Test on number assign to procotol/amendment
data6 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention"),
                    date = c("1899-10-02", "2000-10-18"))

test_that("code_agreements() identify correct number of protocol or amendment", {
  expect_equal(code_agreements(data6, data6$title, data6$date), c("18991002E18", "20001018E34"))
})
