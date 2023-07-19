#' Adding datasets to the many packages universe
#'
#' Save a cleaned data object, consistent with the manydata universe,
#' ready to be lazy-loaded and create scripts for documenting and
#' testing that object within the new package.
#' @param ... Unquoted name of the dataset object to save.
#' @param database Quoted name of any existing database or of the database to
#' be created.
#' @param URL Website URL to the source of a dataset.
#' Null by default.
#' Please make sure to fill in documentation file by hand if not provided.
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
#' @importFrom dplyr mutate select left_join distinct %>%
#' @examples
#' \dontrun{
#' export_data(COW, database = "states",
#' URL = "https://correlatesofwar.org/data-sets/state-system-membership")
#' }
#' @export
export_data <- function(..., database, URL = NULL) {
  # Step 1: check if bibliography file exists
  dataset_name <- deparse(substitute(...))
  if (!file.exists(paste0("data-raw/", database, "/", dataset_name, "/",
                          dataset_name, ".bib"))) {
    stop("Bibliography file not found.
         Please run `manypkgs::add_bib()` to add a .bib file to the data-raw
         folder before proceding.")
  }
  usethis::use_directory("data", ignore = FALSE)
  # Step 2: add datasets to related database, if exists, or create one
  if (file.exists(paste0("data/", database, ".rda"))) {
    usethis::ui_info("Found an existing {usethis::ui_value(database)} database.")
    env <- new.env()
    load(paste0("data/", database, ".rda"), envir = env)
    dataset_exists <- exists(dataset_name, envir = env)
    if (dataset_exists) {
      usethis::ui_info("Found an existing {usethis::ui_value(dataset_name)} dataset.
                       Saving a new version of the {usethis::ui_value(database)} database
                       with an updated version of the {usethis::ui_value(dataset_name)} dataset.")
    } else {
      usethis::ui_info("The {usethis::ui_value(dataset_name)} dataset will be
                       added to {usethis::ui_value(database)}.")
    }
    # ask users if they want to update titles and IDs in database
    if (utils::askYesNo("Would like to update IDs and titles in
                        the other datasets in this database?") == TRUE) {
      usethis::ui_info("Standardising titles and (re)coding agreements and manyIDs.
                       This might take a few minutes...")
      env[[database]] <- update_ids(database = database)
    } else {
      env[[database]][[dataset_name]] <- get(dataset_name)
    }
    } else {
    usethis::ui_info("Didn't find an existing {usethis::ui_value(database)} database.
                     Creating a {usethis::ui_value(database)} database that includes
                     the {usethis::ui_value(deparse(substitute(...)))} dataset.")
      env <- new.env()
      env[[database]] <- tibble::lst(...)
      ui_done("A documentation script has been created for the database.")
    }
  # Step 3: get attributes and URL to dataset
  if (is.null(URL)) {
    if (is.null(attributes(env[[database]][[dataset_name]])$source_URL)) {
      message("Please make sure to fill the URL in documentation file by hand")
      attr(env[[database]][[dataset_name]], "source_URL") <- "Please add URL here"
    } else {
      usethis::ui_info("Found an existing URL for the dataset.")
    }
  } else {
    attr(env[[database]][[dataset_name]], "source_URL") <- URL
  }
  attr(env[[database]][[dataset_name]], "source_bib") <-
    RefManageR::ReadBib(file = paste0("data-raw/", database, "/",
                                      dataset_name, "/", dataset_name, ".bib"))
  save(list = database, envir = env,
       file = fs::path("data", database, ext = "rda"), compress = "bzip2")
  # Step 4: create and open a documentation script
  add_docs(database = database, dataset_name = dataset_name)
  # Step 5: create the right kind of test script for the type of object
  add_tests(database = database, dataset_name = dataset_name,
            dataset_exists = dataset_exists)
  usethis::ui_info("Please make sure to manually fill the variable mapping
                   section in database documentation.")
}

# Helper functions to get package name
get_package_name <- function(path = getwd()) {
  file.exists(paste0(path, "/DESCRIPTION"))
  package <- read.dcf(paste0(path, "/DESCRIPTION"))[[1]]
  package
}

# Helper functions to update titles and IDs
update_ids <- function(database) {
  Title <- Begin <- manyID <- treatyID <- NULL
  db_up <- if (is.list(database)) database else get(database)
  for (x in names(db_up)) {
    db_up[[x]] <- dplyr::mutate(db_up[[x]],
                                Title = standardise_titles(Title),
                                treatyID = code_agreements(title = Title,
                                                           date = Begin))
  }
  mID <- condense_agreements(db_up)
  for (x in names(db_up)) {
    db_up[[x]] <-  db_up[[x]][, !names( db_up[[x]]) %in% "manyID"]
    db_up[[x]] <- dplyr::left_join(db_up[[x]], mID, by = "treatyID") %>%
      dplyr::distinct() %>%
      dplyr::relocate(manyID, treatyID, Title, Begin)
  }
  db_up
}

# Helper functions to add documentation for dataset/database
add_docs <- function(database, dataset_name) {
  db <- get(load(paste0("data/", database, ".rda")))
  dblen <- length(db)
  dsnames <- names(db)
  strdsnames <- stringr::str_c(names(db), collapse = ", ")
  dsobs <- lapply(db, nrow)
  dsnvar <- lapply(db, ncol)
  dsvarstr <- lapply(lapply(db, colnames), stringr::str_c, collapse = ", ")
  dsource <- lapply(dsnames, function(x) {
    paste0(unlist(utils::capture.output(attributes(db[[x]])$source_bib)),
                  collapse = "")
  })
  dURL <- lapply(dsnames, function(x) attributes(db[[x]])$source_URL)
  describe <- paste0("#'\\describe{\n",
                     paste0("#' \\item{", dsnames, ": }",
                            "{A dataset with ", dsobs,
                            " observations and the following\n",
                            "#' ", dsnvar, " variables: ", dsvarstr,
                            ".}\n", collapse = ""), "#' }")
  source <- paste0("#'\\itemize{\n", paste0("#' \\item{", dsnames,
                                            ": }{\n", "#' ", dsource,
                                             "}\n", collapse = ""), "#' }")
  sourceURL <- paste0("#'\\itemize{\n",
                      paste0("#' \\item{", dsnames, ": }{ \\url ",  dURL, "}\n",
                             collapse = ""), "#' }")
  vmapping <- paste0("#'\\itemize{\n",
                     paste0("#' \\item{", dsnames, ": }{\n",
                            "#' Variable Mapping\n", "#'\n",
                            "#' |  *from*  | *to*\n",
                            "#' |:------------:|:------------:|\n",
                            "#' | original_name | new_name |\n",
                            "#' Please fill in variable mapping here as above.",
                            "}\n", collapse = ""), "#' }")
  package <- get_package_name()
  manytemplate("Package-DBDoc.R",
               save_as = fs::path("R", paste0(package, "-", database, ".R")),
               data = list(dat = dataset_name, nd = dblen,
                           strdsnames = strdsnames, dsvarstr = dsvarstr,
                           database = database, describe = describe,
                           sourceURL = sourceURL, vmapping = vmapping,
                           source = source),
               ignore = FALSE, path = getwd())
}

# Helper functions to add tests for databases
add_tests <- function(database, dataset_name, dataset_exists) {
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
  if (dataset_exists == FALSE) {
    ui_done("A test script has been created for this dataset.")
  }
}
