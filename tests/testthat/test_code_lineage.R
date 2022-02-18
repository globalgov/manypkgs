data <- tibble::tibble(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development",
                             "Amendment 1 To The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Declaration Modifying Agreement on the River",
                             "Exchange Of Notes Constituting An Agreement On The Exploitation Of Border Rivers For Industrial Purposes",
                             "Strategy On The Agreement On The River Basin",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing For King And Tanner Crab",
                             "Agreement Between The Government Of The United States Of America And The Government Of The Union Of Soviet Socialist Republics Relating To Fishing Operations In The Northeastern Pacific Ocean"),
                       manyID = c("CPV-PRT[FSD]_1980A", "WIIEWH_1990E1:RAMSA_1971A", "TD06LJ_1981A",
                               "DCLRMR_1982R", "CEBRIP_1976N", "RIVER_1983S", "RAMSA_1971A",
                               "RUS-USA[KTC]_1973A", "RUS-USA[UFO]_1973A"))

test_that("code_lineage function works", {
  expect_equal(code_lineage(title = data$title), c("CPV-PRT[FSD] - fishing", "NA - environment",
                                                   "NA - territorial boundaries", "River",
                                                   "NA - trade",
                                                   "River Basin", "NA - environment",
                                                   "RUS-USA[KTC] - fishing", "Pacific Ocean - fishing"))
})

links <- data.frame(agreement = "WIIEWH_1990E1", link = "RAMSA_1971A")

test_that("get_links function works", {
  expect_equal(get_links(dataset = data), links)
})
