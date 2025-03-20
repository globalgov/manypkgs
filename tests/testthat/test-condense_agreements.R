data1 <- data.frame(treatyID = c("CPV-PRT[FSD]_1980A",
                            "CPV-PRT[FSD]_1990P:FSD_1980A",
                           "TD06LJ_1981A", "RAMSAI_1971A",
                           "WIIEWH_1982P"))

data2 <- data.frame(treatyID = c("TD06LJ_1981A", "RAMSEI_1971A",
                            "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A",
                            "PRTRPC_1983E1:PRTRPC_1976A"))

data3 <- data.frame(treatyID = c("CPV-PRT[FSD]_1980A",
                            "CPV-PRT[FSD]_1990P:FSD_1980A",
                            "RAMSAI_1971A",
                           "TD06LJ_1981A", "WIIEWH_1982P"),
                   manyID = c("CPV-PRT[FSD]_1980A",
                               "CPV-PRT[FSD]_1990P:FSD_1980A",
                               "RAMSAI_1971A",
                               "TD06LJ_1981A",
                               "WIIEWH_1982P:RAMSA_1971A"))

data4 <- data.frame(treatyID = c("PRTRPC_1976A",
                            "PRTRPC_1983E1:PRTRPC_1976A",
                            "RAMSEI_1971A",
                            "TD06LJ_1981A",
                            "WIIEWH_1982P:RAMSA_1971A"),
                    manyID = c("PRTRPC_1976A",
                                "PRTRPC_1983E1:PRTRPC_1976A",
                                "RAMSAI_1971A",
                                "TD06LJ_1981A",
                                "WIIEWH_1982P:RAMSA_1971A"))

test_that("Linkages are added correctly", {
  a <- condense_agreements(idvar = c(data1$treatyID, data2$treatyID))
  expect_equal(a$manyID, c("CPV-PRT[FSD]_1980A",
                            "CPV-PRT[FSD]_1990P:FSD_1980A",
                            "TD06LJ_1981A",
                            "RAMSAI_1971A",
                            "WIIEWH_1982P:RAMSA_1971A",
                            "RAMSAI_1971A",
                            "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A",
                            "PRTRPC_1983E1:PRTRPC_1976A"))
  m1 <- merge(data1, a)
  expect_equal(m1, data3)
  m2 <- merge(data2, a)
  expect_equal(m2, data4)
})

data5 <- data.frame(treatyID = "DNK-SWE[FSC]_1899A",
                    manyID = "DNK-SWE[FSC]_1899A")

data6 <- data.frame(treatyID = "DNK-SWE[FSS]_1899A",
                    manyID = "DNK-SWE[FSS]_1899A")

data7 <- data.frame(treatyID = c("DNK-SWE[FSC]_1899A",
                                 "DNK-SWE[FSC]_1902E:DNK-SWE[FSC]_1899A"),
                    manyID = c("DNK-SWE[FSC]_1899A",
                               "DNK-SWE[FSC]_1902E:DNK-SWE[FSC]_1899A"))

test_that("activity is generated correctly", {
  a <- condense_agreements(idvar = c(data5$treatyID, data6$treatyID))
  b <- condense_agreements(idvar = c(data5$treatyID, data7$treatyID))
  j1 <- dplyr::bind_rows(data5, data6)
  #expect_equal(a$manyID, j1$manyID)
  j2 <- dplyr::bind_rows(data5, data7)
  expect_equal(b$manyID, unique(j2$manyID))
})

data8 <- data.frame(treatyID = c("MEX-TEX[FQG]_1944P",
                                 "MEX-TEX[FQG]_1944A",
                                 "MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A"),
                    manyID = c("MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A",
                               "MEX-TEX[FQG]_1944A",
                               "MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A"))

data9 <- data.frame(treatyID = c("MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A",
                                 "MEX-TEX[FQG]_1944A"),
                    manyID = c("MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A",
                               "MEX-TEX[FQG]_1944A"))

data10 <- data.frame(treatyID = c("MEX-TEX[FQG]_1944P",
                                  "MEX-TEX[FQG]_1944A"))

set1 <- data.frame(title = c("Agreement On Regional Cooperation In Combating Pollution Of The South East Pacific By Oil And Other Harmful Substances In Cases Of Emergency",
                             "Supplementary Protocol To The Agreement On Regional Cooperation In Combating Pollution Of The South East Pacific By Oil And Other Harmful Substances In Cases Of Emergency"),
                   date = c("1981-11-12", "1983-07-22"),
                   treatyID = c("RC09CE_1981A",
                                "SR10CE_1983P:RC09CE_1981A"),
                   manyID = c("RC09CE_1981A",
                              "SR10CE_1983P:RC09CE_1981A"))

set2 <- data.frame(title = c("Agreement On Regional Cooperation In Combating Pollution Of The South East Pacific By Hydrocarbons And Other Harmful Substances In Cases Of Emergency",
                             "Supplementary Protocol To The Agreement On Regional Cooperation In Combating Pollution Of The South East Pacific By Oil And Other Harmful Substances In Cases Of Emergency"),
                   date = c("1981-11-12", "1983-07-22"),
                   treatyID = c("RC09CE_1981A",
                                "SR10CE_1983P"),
                   manyID = c("RC09CE_1981A",
                              "SR10CE_1983P:RC09CE_1981A"))

set3 <- data.frame(title = "Agreement On Regional Cooperation In Combating Pollution Of The South East Pacific By Hydrocarbons And Other Harmful Substances In Cases Of Emergency",
                   date = "1981-11-12",
                   treatyID = "RC09CE_1981A",
                   manyID = "RC09CE_1981A")

test_that("linkage generated from agreements in same dataset", {
  # scenario 1: manyID generated with linkage when one of the datasets
  # in the datacube contains both agreements so that treatyID in one dataset
  # is generated with linkage
  c <- condense_agreements(idvar = c(data8$treatyID, data9$treatyID))
  d <- condense_agreements(idvar = c(data9$treatyID, data10$treatyID))
  expect_equal(data8$manyID, c$manyID)
  expect_equal(data9$manyID, unique(d$manyID))
  # scenario 2: treatyID not generated with linkage due to different titles,
  # but linkage is generated in manyID due to similar treatyIDs
  idtest1 <- code_agreements(title = set1$title, date = set1$date)
  idtest2 <- code_agreements(title = set2$title, date = set2$date)
  expect_equal(idtest1, set1$treatyID)
  expect_equal(idtest2, set2$treatyID)
  f <- condense_agreements(idvar = c(set1$treatyID, set2$treatyID))
  expect_equal(unique(f$manyID), set1$manyID)
  g <- condense_agreements(idvar = set2$treatyID, set3$treatyID)
  #expect_equal(g$manyID, set2$manyID)
})
