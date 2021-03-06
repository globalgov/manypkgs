#' Adding datasets to the many packages universe
#'
#' Save a cleaned data object, consistent with the manydata universe,
#' ready to be lazy-loaded and create scripts for documenting and
#' testing that object within the new package.
#' @param ... Unquoted name of the dataset object to save.
#' @param database Quoted name of any existing database or of the database to
#' be created.
#' @param URL website URL to the source of a dataset.
#' @details The function creates a data directory, if nonexistent, and
#' saves cleaned data. The functions also creates a script for testing
#' the cleaned data and make sure it complies with manydata requirements.
#' As well, it creates a documentation script to help documenting data
#' sources and describing variables. Note that users need to provide a `.bib`
#' file for citation purposes alongside their dataset in the corresponding
#' `data-raw` subfolder.
#' @return This function saves the dataset to the named database,
#' silently creates a set of tests for this dataset,
#' and creates and opens documentation for the dataset.
#' @importFrom fs path
#' @importFrom usethis ui_info ui_done
#' @examples
#' \dontrun{
#' export_data(COW, database = "states",
#' URL = "https://correlatesofwar.org/data-sets/state-system-membership")
#' }
#' @export
export_data <- function(..., database, URL) {

  # Step one: checks and set up directory
  # Check if URL is present
  if (missing(URL)) {
    stop("Please use the URL argument to provide a direct webURL to the source of your dataset.")
  }
  if (!is.character(URL)) {
    stop("Please provide a valid URL argument.")
  }
  dataset_name <- deparse(substitute(...))
  # Check if bibliography file exists
  if (!file.exists(paste0("data-raw/", database, "/", dataset_name, "/", dataset_name, ".bib"))) {
    stop("Bibliography file not found.
         Please run `manypkgs::add_bib()` to add a .bib file to the data-raw
         folder before proceding.")
  }
  usethis::use_directory("data", ignore = FALSE)

  # Step two: join dataset to any related in a database
  if (file.exists(paste0("data/", database, ".rda"))) {
    usethis::ui_info("Found an existing {usethis::ui_value(database)} database.
                     Imported it ready to update.")
    env <- new.env()
    load(paste0("data/", database, ".rda"), envir = env)
    dataset_exists <- exists(dataset_name, envir = env)
    if (dataset_exists) {
      usethis::ui_info("Found an existing {usethis::ui_value(dataset_name)} dataset.
                       This will be overwritten.")
    } else {
      usethis::ui_info("The {usethis::ui_value(dataset_name)} dataset does not
                       yet exist in {usethis::ui_value(database)}. It will be added.")
    }
    env[[database]][[dataset_name]] <- get(dataset_name)

    #Adding static source attributes to each dataset
    attr(env[[database]][[dataset_name]], "source_URL") <- URL
    attr(env[[database]][[dataset_name]],
         "source_bib") <- RefManageR::ReadBib(file = paste0("data-raw/",
                                                            database, "/",
                                                            dataset_name, "/",
                                                            dataset_name, ".bib"))
    save(list = database, envir = env,
         file = fs::path("data", database, ext = "rda"), compress = "bzip2")
    if (dataset_exists) {
      usethis::ui_info("Saved a new version of the {usethis::ui_value(database)} database
                       with an updated version of the {usethis::ui_value(dataset_name)} dataset.")
    } else {
      usethis::ui_info("Saved a new version of the {usethis::ui_value(database)}
                       database that includes the {usethis::ui_value(dataset_name)} dataset.")
    }
  } else {
    usethis::ui_info("Didn't find an existing {usethis::ui_value(database)} database.")
    env <- new.env()
    env[[database]] <- tibble::lst(...)
    attr(env[[database]][[dataset_name]], "source_URL") <- URL
    attr(env[[database]][[dataset_name]],
         "source_bib") <- RefManageR::ReadBib(file = paste0("data-raw/",
                                                            database, "/",
                                                            dataset_name, "/",
                                                            dataset_name, ".bib"))
    save(list = database, envir = env,
         file = fs::path("data", database, ext = "rda"), compress = "bzip2")
    usethis::ui_done("Saved a {usethis::ui_value(database)} database that
                     includes the {usethis::ui_value(deparse(substitute(...)))} dataset.")
  }

  # Step three: create and open a documentation script
  # Create a succinct database documentation.
  db <- get(load(paste0("data/", database, ".rda")))
  dblen <- length(db)
  dsnames <- names(db)
  strdsnames <- str_c(names(db), collapse = ", ")
  dsobs <- lapply(db, nrow)
  dsnvar <- lapply(db, ncol)
  dsvarstr <- lapply(lapply(db, colnames), str_c, collapse = ", ")
  describe <- paste0("#'\\describe{\n",
                     paste0("#' \\item{", dsnames, ": }",
                            "{A dataset with ", dsobs,
                            " observations and the following\n",
                            "#' ", dsnvar, " variables: ", dsvarstr,
                            ".}\n", collapse = ""), "#' }")
  sourceelem <- paste0("#' @source \\url{", URL, "}", collapse = "")
  #Output
  package <- get_package_name()
  manytemplate("Package-DBDoc.R",
               save_as = fs::path("R", paste0(package, "-", database, ".R")),
               data = list(dat = dataset_name, nd = dblen,
                           strdsnames = strdsnames, dsvarstr = dsvarstr,
                           database = database, describe = describe,
                           source = sourceelem),
               ignore = FALSE, path = getwd())

  # Step four: create the right kind of test script for the type of object
  if (database == "states") {
    manytemplate("test_states.R",
                 save_as = fs::path("tests", "testthat",
                                 paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  } else if (database == "agreements") {
    manytemplate("test_agreements.R",
                 save_as = fs::path("tests", "testthat",
                                 paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  } else if (database == "memberships") {
    manytemplate("test_memberships.R",
                 save_as = fs::path("tests", "testthat",
                                 paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  } else if (database == "actors") {
    manytemplate("test_actors.R",
                 save_as = fs::path("tests", "testthat",
                                 paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  } else if (database == "texts") {
    manytemplate("test_texts.R",
                 save_as = fs::path("tests", "testthat",
                                    paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  } else {
    manytemplate("test_general.R",
                 save_as = fs::path("tests", "testthat",
                                 paste0("test_", dataset_name, ".R")),
                 data = list(dat = dataset_name, dab = database),
                 open = FALSE, ignore = FALSE, path = getwd())
  }
  ui_done("A test script has been created for this data.")
  ui_todo("Press Cmd/Ctrl-Shift-T to run all tests or run devtools::test().")
}

get_package_name <- function(path = getwd()) {
  file.exists(paste0(path, "/DESCRIPTION"))
  package <- read.dcf(paste0(path, "/DESCRIPTION"))[[1]]
  package
}
