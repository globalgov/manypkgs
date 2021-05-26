data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Protocol to the Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                   date = c("1980-05-08", "1990-12-31", "1981-01-30", "1971-02-02", "1982-12-03", "1976-12-03", "1983-04-29"))

test_that("Code_agreements() properly returns qIDs", {
  expect_equal(code_agreements(data$title, data$date), c("19800508_CPV-PRT", "19901231P_19800508_CPV-PRT",
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

# Add one test for dataset that have range as dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                              "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                              "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                              "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                              "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                    date = c("1980-01-01:1980-12-31", "1981-01-01:1981-12-31", "1971-01-01:1971-12-31",
                             "1982-01-01:1982-12-31", "1976-01-01:1976-12-31", "1983-01-01:1983-12-31"))
test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments
  expect_equal(code_dates(data2$title, data2$date), c("1980ACT01", "1981TTO01",
                                                      "1971CAT01", "1982PTT01", 
                                                      "1976CPS01", "1983ALS01"))
})

#Test to be added one issue on false duplicates is solved!
# data3 <- data.frame(title = c("Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
#                              "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean",
#                              "Agreement On Cooperation In The Field Of Environmental Protection",
#                              "Agreement On Cooperation In The Field Of Protection, Regulation, And Recreation Living Water Resources In The Boundary Waters Of Amur And Ussuri Rivers",
#                              "Agreement On The Lake Hanka Sanctuary",
#                              "Agreement On Cooperation In The Field Of Peaceful Uses Of Atomic Energy",
#                              "Agreement Between The Government Of Kazakhstan And The Government Of Mongolia On Cooperation In The Field Of Environmental Protection",
#                              "Agreement Between The Government Of Kazakhstan And The Government Of Mongolia On Cooperation In The Field Of Plant Quarantine"),
#                    date = c("1973-02-21", "1973-02-21", "1994-05-27", "1994-05-27", "1996-04-25", "1996-04-25", "1998-03-12", "1998-03-12"))
# 
# 
# test_that("code_agreements() differentiates treaties signed the same day", {
#   expect_equal(code_agreements(data3$title, data3$date), c("19730221_RUS-USA", "19730221_RUS-USA",
#                                                            "19940527A", "19940527A", "19960425A",
#                                                            "19960425A", "19980312_KAZ-MNG", "19980312_KAZ-MNG"))
# })

data4 <- data.frame(title = c("Protocol On Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999",
                              "Amendments To The Agreement On Cooperation In The Field Of Environmental Monitoring Of 13 January 1999",
                              "Subsidiary Agreement On Fisheries Between The Government Of Australia And The Government Of Japan Concerning Japanese Tuna Long Line Fishing",
                              "Protocol To The Agreement On Fisheries Between The Government Of Australia And The Government Of Japan Concerning Japanese Tuna Long Line Fishing",
                              "International Convention For The Regulation Of Whaling",
                              "Amendments To The Schedule To The International Convention For The Regulation Of Whaling, 20-1 Meeting"),
                    date = c("2015-10-30", "2019-02-01", "1984-10-30", "1955-10-14", "1960-02-05", "1969-06-27"))

test_that("code_agreements() link treaties correctly", {
  expect_equal(code_agreements(data4$title, data4$date), c("20151030P", "20190201E", "19841030P", "19551014P", "19600205A", "19690627E_19600205A"))
})


data5 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments Of The Limitation Amounts In The 1992 Protocol To Amend The International Convention On Civil Liability For Oil Pollution Damage",
                              "International Convention On Civil Liability For Oil Pollution Damage",
                              "Exchange Of Note Between The Government Of The United State Of America And The Government Of Canada Constituting An Agreement Regarding The Development Of Certain Portions Of The Great Lakes-St. Lawrence Basin Project",
                              "Protocol To The Amendement Of The Convention On Water"),
                    date = c("1899-10-02", "2000-10-18", "1969-11-29", "1940-11-07", "1999-03-02"))

test_that("code_agreements() recognizes the correct type of treaty", {
  expect_equal(code_agreements(data5$title, data5$date), c("18991002E18", "20001018E_CLC19691129", "CLC19691129", "19401107A", "19990302P"))
})

data6 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                              "Protocol 2 Between The Government Of The United State Of America And The Government Of Canada Constituting An Agreement Regarding The Development Of Certain Portions Of The Great Lakes-St. Lawrence Basin Project",
                              "Protocol To The Amendement 300 Of The Convention On Water"),
                    date = c("1899-10-02", "2000-10-18", "1940-11-07", "1999-03-02"))

test_that("code_agreements() identify correct number of protocol or amendment", {
  expect_equal(code_agreements(data6$title, data6$date), c("18991002E18", "20001018E34", "19401107P2", "19990302P300"))
})



