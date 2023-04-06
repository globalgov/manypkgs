test_that("url is valid", {
  expect_error(export_data("COW", databse = "states", URL = 5),
               "Please provide the URL argument to the source of your dataset as a character string.")
})

a <- data.frame(Title = c("A treaty about things", "treaty about thing",
                          "Agreement Between Cape Verde And Portugal On Fisheries Development",
                          "Amendment 1 To The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                          "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                          "Declaration Modifying Agreement on the River",
                          "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                          "Strategy On The Agreement On The River Basin",
                          "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                          "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                          "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean"),
                Beg = c("2010-01-01", "2010-01-01", "1980-05-08", "1990-12-31",
                        "1981-01-30", "1982-12-03", "1976-12-03", "1983-04-29",
                        "1971-02-02", "1973-02-21", "1973-02-21"),
                treatyID = c("TRTYTH_2010A", "TRTYTH_2010A", "CPV-PRT[FSD]_1980A",
                             "WTIIWH_1990E1:RAMSA_1971A", "TD06LJ_1981A",
                             "DCLRMR_1982R", "CEBRIP_1976N", "RIVER_1983S",
                             "RAMSA_1971A", "RUS-USA[KTC]_1973A", 
                             "RUS-USA[UFO]_1973A"),
                manyID = c("TRTYTH_2010A", "TRTYTH_2010A", "CPV-PRT[FSD]_1980A",
                           "WTIIWH_1990E1:RAMSA_1971A", "TD06LJ_1981A",
                           "DCLRMR_1982R", "CEBRIP_1976N", "RIVER_1983S",
                           "RAMSA_1971A", "RUS-USA[KTC]_1973A", 
                           "RUS-USA[UFO]_1973A"))
b <- data.frame(Title = c("A treaty about things", "treaty about thing",
                          "Agreement Between Cape Verde And Portugal On Fisheries Development",
                          "Amendment 1 To The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                          "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                          "Declaration Modifying Agreement on the River",
                          "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                          "Strategy On The Agreement On The River Basin",
                          "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                          "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                          "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean"),
                Beg = c("2010-01-01", "2010-01-01", "1980-05-08", "1990-12-31",
                        "1981-01-30", "1982-12-03", "1976-12-03", "1983-04-29",
                        "1971-02-02", "1973-02-21", "1973-02-21"),
                treatyID = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"),
                manyID = c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"))
ab <- update_ids(tibble::lst(a, b))

test_that("titles, treatyIDs, and manyIDs are correctly updated", {
  expect_equal(ab$a$manyID, ab$b$manyID)
  expect_equal(ab$a$treatyID, ab$b$treatyID)
})
