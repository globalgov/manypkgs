data1 <- data.frame(treaty_ID = c("CPV-PRT[FSD]_1980A",
                            "CPV-PRT[FSD]_1990P:FSD_1980A",
                           "TD06LJ_1981A", "RAMSAI_1971A",
                           "WIIEWH_1982P"))
data2 <- data.frame(treaty_ID = c("TD06LJ_1981A", "RAMSEI_1971A",
                            "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A",
                            "PRTRPC_1983E1:PRTRPC_1976A"))
data3 <- data.frame(treaty_ID = c("CPV-PRT[FSD]_1980A",
                            "CPV-PRT[FSD]_1990P:FSD_1980A",
                            "RAMSAI_1971A",
                           "TD06LJ_1981A", "WIIEWH_1982P"),
                   many_ID = c("CPV-PRT[FSD]_1980A",
                               "CPV-PRT[FSD]_1990P:FSD_1980A",
                               "RAMSAI_1971A",
                               "TD06LJ_1981A",
                               "WIIEWH_1982P:RAMSA_1971A"))
data4 <- data.frame(treaty_ID = c("PRTRPC_1976A",
                            "PRTRPC_1983E1:PRTRPC_1976A",
                            "RAMSEI_1971A",
                            "TD06LJ_1981A",
                            "WIIEWH_1982P:RAMSA_1971A"),
                    many_ID = c("PRTRPC_1976A",
                                "PRTRPC_1983E1:PRTRPC_1976A",
                                "RAMSAI_1971A",
                                "TD06LJ_1981A",
                                "WIIEWH_1982P:RAMSA_1971A"))

test_that("Linkages are added correctly", {
  a <- condense_agreements(var = c(data1$treaty_ID, data2$treaty_ID))
  expect_equal(a$many_ID, c("CPV-PRT[FSD]_1980A",
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
