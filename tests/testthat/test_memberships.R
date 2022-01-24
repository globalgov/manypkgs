data <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                              "Amendments Of The Limitation Amounts In The 1992 Convention (Annex 4)"),
                    text = c("Article 21 - Memberships: his protocol shall remain open for signature by all states 
                             who become parties to any convention on the law of the sea adopted by the united nations 
                             conference on the law of the sea and is subject to ratification, where necessary, 
                             according to the constitutional requirements of the signatory states.",
                             "xii ratification and adherence 1. this convention shall be open for signature by all governments 
                             until 1 may 1952 and shall be ratified at the earliest possible date. the instruments of 
                             ratification shall be deposited with the director-general of fao, who shall give notice of the 
                             date of deposit to each of the signatory governments",
                             "x 1. states interested in co-operating in the regulation of hunting operations and the 
                             conservation of stocks of the species of animals to which this agreement applies may, with 
                             the consent of the contracting parties, become parties to the agreement."))


test_that("code_memberships works properly", {
  expect_equal(code_memberships(data$text, data$title, memberships = "condition"), c("open + domain: waste", NA, NA))
  expect_equal(code_memberships(data$text, data$title, memberships = "process"), c("signature + ratification",
                                                                                   "signature + ratification", NA))
  expect_type(code_memberships(), "character")
})

