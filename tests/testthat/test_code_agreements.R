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
  expect_equal(code_states(data$title), c("CPV-PRT[FSD]", NA, NA, NA, NA, NA,
                                           NA, "RUS-USA[KTC]", "RUS-USA[FON]"))
  expect_equal(code_type(data$title), c("A", "E1", "A", "R",
                                        "N", "S", "A", "A", "A"))
  expect_equal(code_dates(data$date), c("1980", "1990",
                                        "1981", "1982",
                                        "1976", "1983",
                                        "1971", "1973", "1973"))
  expect_equal(code_known_agreements(data$title), c(NA, "RAMSA_1971",
                                                    NA, NA, NA, NA,
                                                    "RAMSA_1971", NA, NA))
  expect_equal(code_acronym(data$title), c("CPVPFD", "WTIIWH",
                                           "TD06LJ", "DCLRMR",
                                           "CEBRIP", "RIVER",
                                           "WTIIWH", "GU11TC",
                                           "GU13PO"))
  expect_equal(code_linkage(data$title, data$date), c("", "RAMSA_1971A", "",
                                                      "", "", "", "", "", ""))
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

# Test that linkages are not duplicates
mem <- tibble::tibble(
  Title = c("3 ACP-EEC Convention",
            "2 Arrangement Implementing The Nauru Agreement Setting Forth Additional Terms And Conditions Of Access To The Fisheries Zones Of The Parties",
            "Agreement Between Sri Lanka And India On The Maritime Boundary Between The 2 Countries In The Gulf Of Mannar And The Bay Of Bengal And Related Matters",
            "European Outline Convention On Transfrontier Cooperation Between Territorial Communities Or Authorities (CETS No 106)",
            "Convention On International Civil Aviation Annex 16 Aircraft Noise",
            "1 Arrangement Implementing The Nauru Agreement Setting Forth Minimum Terms And Conditions Of Access To The Fisheries Zones Of The Parties",
            "2 Arrangement Implementing The Nauru Agreement Setting Forth Additional Terms And Conditions Of Access To The Fisheries Zones Of The Parties",
            "Convention On The Game Hunting Formalities Applicable To Tourists Entering Countries In The Conseil De 50Entente",
            "Agreement On The Protection Of The Scheldt 50Escaut", "Technical Arrangement Between The United Kingdom Of Great Britain And Northern Ireland The French Republic And Belgium Made Under Article 6 (4) Of The Agreement For Cooperation In Dealing With Pollution Of The North Sea By Oil",
            "International Convention For The Prevention Of Pollution Of The Sea By Oil 1954 As Amended In 1962 And 1969",
            "Application Of Safeguards On Implementation Of Article 3 (1) And (4) Of The Treaty On The Non-Proliferation Of Nuclear Weapons",
            "International Convention For The Prevention Of Pollution Of The Sea By Oil 1954 As Amended In 1962 And 1969",
            "Agreement Between Sri Lanka India And The Maldives On The Determination Of Trijunction Point Between The 3 Countries In The Gulf Of Mannar",
            "Agreement 2 Between Chad Egypt Libya And Sudan For Monitoring And Sharing Data For The Sustainable Development And Proper Management Of The Nubian Sandstone Aquifer System",
            "Agreement Between Sri Lanka And India On The Boundary In Historic Waters Between The 2 Countries And Related Matters",
            "Agreement Between The Government Of The Kingdom Of Thailand And The Socialist Republic Of Viet Nam On The Delimitation Of The Maritime Boundary Between The 2 Countries In The Gulf Of Thailand",
            "Maritime Agreement Between The Government Of The French Republic And The Government Of The Republic Of Latvia Signed In Riga December 5 1997",
            "3 ACP-EEC Convention",
            "Convencin Para El Establecimiento De La RED De Acuicultura De Las Amricas (RAA)"),
  Beg = messydates::as_messydate(
    c("1984-12-08", "1990-09-19", "1976-03-23", "1980-05-21", "1944-12-07",
      "1982-02-11", "1990-09-19", "1976-02-26", "1994-04-26", "1972-07-28",
      "1954-05-12", "1973-04-05", "1954-05-12", "1976-07-31", "2000-10-05",
      "1974-06-28", "1997-08-09", "1997-12-05", "1984-12-08", "2012-04-18"))) %>%
  mutate(treatyID = code_agreements(title = Title, date = Beg),
         a = ifelse(grepl(":", treatyID), sub(":.*$", "", treatyID), NA_character_),
         b = ifelse(grepl(":", treatyID), sub("^.*?:", "",  treatyID), NA_character_))

test_that("linkages are not duplicates", {
  expect_true(is.na(any(mem$a == mem$b)))
})

# Test that linkages are added correctly for bilaterals treaties
data9 <- data.frame(title = c("Supplementary Protocol To The Treaty Relating To The Utilization Of The Waters Of The Colorado And Tijuana Rivers And Of The Rio Grande (Rio Bravo) From Fort Quitman Texas To The Gulf Of Mexico",
                             "Treaty Relating To The Utilization Of The Waters Of The Colorado And Tijuana Rivers And Of The Rio Grande (Rio Bravo) From Fort Quitman Texas To The Gulf Of Mexico"),
                    date = c("1944-11-14", "1944-11-14"))
treatyID <- c("MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A", "MEX-TEX[FQG]_1944A")
cd <- code_agreements(title = data9$title, date = data9$date)

test_that("linkages are added correctly for bilaterals treaties", {
  expect_equal(cd, treatyID)
})
