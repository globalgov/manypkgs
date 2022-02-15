# Test for code_term()
data <- data.frame(title = c("Treaty For The Period From 1 September 1992 To 18 April 1998 Between France And Spain",
                             "Treaty Between France And Italy For The Period Between The 1 September 1992 To 18 April 1998",
                             "Treaty Signed in Berlin 8 August 1998 Between Germany and Bulgaria",
                             "Treaty Concerning Geneva Lake For The Period 1 September 1992 To 18 April 1999",
                             "Protocol To The 17 May 1998 Convention on Fisheries",
                             "Treaty For The Period Between 22 February 2002 To 12 March 2009 Concerning Jan Mayen Seal Fishery",
                             "Amendments For The Period 19 May 1992 To 18 September 1998 Convention For The Prevention Of Pollution From Ships"),
                   date = c("1783-10-03", "1783-10-03", "1876-10-28", "1876-10-28", "1982-12-10", "1876-10-28", "1876-10-28"))

# Test term_date type is detected correctly from treaty title
test_that("treaty type is identified from treaty title", {
  expect_equal(code_term(data$title),
               c("EXP", "EXP", NA, "EXP", NA, "EXP", "EXP"))
})

data2 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                             "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                             "Amendments Of The Limitation Amounts In The 1992 Convention (Annex 4)",
                             "Convention of the Lake",
                             "Treaty on the environment",
                             "Protocol on the Convention of the Sea",
                             "Amendment on Convention on biodiversity",
                             "Convention on the delimitation of the border between x and y",
                             "Convention between switzerland and france",
                             "Treaty to preserve the lake of Geneva"),
                   text = c("the present convention shall remain in force for a period of five years and thereafter until two years 
                            from the date when either of the high contracting parties shall give notice to the other of its desire to
                            term_dateinate it",
                            "this convention shall, from the date of the exchange of ratifications be deemed to supplant the 
                            convention between his britannic majesty and the united states of america for the preservation of the halibut
                            fishery of the northern pacific ocean including bering sea, concluded march 2, 1923",
                            " 9. the duration of the present convention shall be for four years in accordance with the provisions of article
                            14; denunciation of the convention may only take place within six months from the end of this period and shall be
                            communicated to the ministry of foreign relations of el salvador. the duration shall be prolonged automatically and
                            tacitly so long as the convention continues in effect for four or more of the signatory countries.",
                            "Any member states may withdraw from this present convention",
                            "This agreement shall term_dateinate upon the complete completion of the project",
                            "Member state can renounce its memberships at any time after ratification",
                            "In the cases of extraordinary events, members states can withdrawal",
                            "In the case of war, the parties to the treaties will end this agreement",
                            "If one become party injurious, the treaty come to an end and parties are liberate from their obligations",
                            "Nonperformance of obligations will generate the end of the treaty"))

# term_dateination types identified through treaty texts
test_that("treaty term is identified from treaty text", {
  expect_equal(code_term(data2$title, data2$text),
               c("SUN", "SUB", "SUN", "WTH", "NA", "REN", "REB", "WAR", "INJ", "NON"))
})

# Test that table appears if no argument is mentioned
test_that("function returns information when no argument is mentioned", {
  expect_type(code_term(), "character")
})

# Test for code_term_date()
data <- data.frame(title = c("Treaty For The Period From 1 September 1992 To 18 April 1998 Between France And Spain",
                             "Treaty Between France And Italy For The Period Between The 1 September 1992 To 18 April 1998",
                             "Treaty Signed in Berlin 8 August 1998 Between Germany and Bulgaria",
                             "Treaty Concerning Geneva Lake For The Period 1 September 1992 To 18 April 1999",
                             "Protocol To The 17 May 1998 Convention on Fisheries",
                             "Treaty For The Period Between 22 February 2002 To 12 March 2009 Concerning Jan Mayen Seal Fishery",
                             "Amendments For The Period 19 May 1992 To 18 September 1998 Convention For The Prevention Of Pollution From Ships"),
                   date = c("1783-10-03", "1783-10-03", "1876-10-28", "1876-10-28", "1982-12-10", "1876-10-28", "1876-10-28"))

# Test term date is correctly identified from treaty title
test_that("treaty term_date date is identified from treaty title", {
  expect_equal(code_term_date(data$title),
               as.Date(c("1998-04-18",
                         "1998-04-18",
                         NA, "1999-04-18",
                         NA, "2009-03-12",
                         "1998-09-18")))
})
