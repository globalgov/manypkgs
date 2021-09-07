data1 <- data.frame(qID = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A",
                           "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
data2 <- data.frame(qID = c("TD06LJ_1981A", "RAMSE_1971A", "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))
data3 <- data.frame(qID = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A", "RAMSA_1971A",
                           "TD06LJ_1981A", "WIIEWH_1982P"),
                   qID_ref = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A", "RAMSA_1971A",
                               "TD06LJ_1981A", "WIIEWH_1982P:RAMSA_1971A"))
data4 <- data.frame(qID = c("PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A", "RAMSE_1971A",
                            "TD06LJ_1981A", "WIIEWH_1982P:RAMSA_1971A" ),
                    qID_ref = c("PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A", "RAMSA_1971A",
                                "TD06LJ_1981A", "WIIEWH_1982P:RAMSA_1971A" ))

test_that("Linkages are added correctly", {
  a <- condense_qID(data1$qID, data2$qID)
  expect_equal(a$qID_ref, c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A", "TD06LJ_1981A",
                            "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A", "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A" ))
  m1 <- merge(data1, a)
  expect_equal(m1, data3)
  m2 <- merge(data2, a)
  expect_equal(m2, data4)
})
