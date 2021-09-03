data <- data.frame(qID = c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A",
                           "TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P"))
data1 <- data.frame(qID = c("TD06LJ_1981A", "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A",
                            "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A"))

test_that("Linkages are added correctly", {
  a <- condense_qID(data$qID, data1$qID)
  expect_equal(a$ref, c("CPV-PRT[FSD]_1980A", "CPV-PRT[FSD]_1990P:FSD_1980A", "TD06LJ_1981A",
                        "RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A", "WIIEWH_1982P:RAMSA_1971A",
                        "PRTRPC_1976A", "PRTRPC_1983E1:PRTRPC_1976A" ))
})
