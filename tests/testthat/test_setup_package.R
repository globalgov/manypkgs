test_that("package name is required", {
  expect_error(setup_package(), "Please declare a package name")
})

test_that("author names must be declared", {
  expect_error(setup_package("qtest"), "Please declare one author")
})

test_that("package name must start with the letter q", {
  expect_error(setup_package("test"), "Package name must start with a 'q'")
})

test_that("additional authors are added correctly", {
  expect_error(add_author(),
               "Either a correct ORCID number or name in the format 'Surname, Given Names' must be provided.")
})