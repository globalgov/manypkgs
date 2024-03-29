#' Updates packages of our data universe
#'
#' As our packages evolve, expectations about package structures
#' as well as templates might also change.
#' This function updates files for packages setup with `setup_package()`.
#' @details Some of the files in the .github folder, such as workflow actions,
#' pull request templates, and issue templates, are automatically updated.
#' The function also asks developers if other files such as the package license,
#' contributing file, and code of conduct should be updated.
#' @param package The name of the package to be updated, optional.
#' When not declared package name is extracted from description.
#' @param path The file path, optional.
#' If not specified, function gets the current working directory.
#' @param name A list of vectors giving the package author(s)' name(s).
#' Optional.
#' Authors(s)last name(s) and first name(s) are separated by a comma.
#' @import usethis
#' @importFrom stringr str_detect str_replace
#' @examples
#' \dontrun{
#' update_package("manystates")
#' }
#' @return Updated files in correct structure for packages.
#' @export
update_package <- function(package = NULL, name = NULL, path = getwd()) {

  # Step one: get package name if missing
  if (is.null(package)) {
    if (file.exists(paste0(path, "/DESCRIPTION"))) {
      package <- read.dcf(paste0(path, "/DESCRIPTION"))[[1]]
      usethis::ui_done("Obtained package name from existing DESCRIPTION file.")
    } else {
      stop("Please declare a package name")
    }
  }

  if (!is.null(package) & path == getwd()) {
    if (stringr::str_detect(path, package)) {
      path <- getwd()
    } else {
      stop("Package path differs from current working directory.
           Please either declare a path to package directory
           or use setwd() to setup directory beforehand.")
    }
  }

  depends("desc")

  # Step two: Get author's name
  if (!is.null(name)) {
    if (length(name) > 1) {
      author <- paste(name, collapse = " and ")
    } else {
      author <- paste(name)
    }
  }

  if (is.null(name)) {
    if (file.exists(paste0(path, "/DESCRIPTION"))) {
      author <- read.dcf(paste0(path, "/DESCRIPTION"))[[4]]
      author <- stringr::str_replace_all(author, "\",\nfamily = \"", " ")
      author <- stringr::str_replace_all(author, "c\\(", "")
      author <- stringr::str_replace_all(author, "person\\(given = \"", "")
      author <- stringr::str_replace_all(author, "\\n", "")
      author <- stringr::str_replace_all(author, "\".*", "")
      usethis::ui_done(
        "Obtained lead author name from existing DESCRIPTION file.")
    } else {
      stop("Please declare author(s) name(s)")
    }
  }

  # Step three: update License
  if (utils::askYesNo("Would you like to update package LICENSE file?") == TRUE) {
    manytemplate("LICENSE.md",
                 ignore = TRUE,
                 path = path,
                 open = FALSE)
    desc::desc_set("License", "CC BY 4.0")
    usethis::ui_done("Updated License file.")
  }

  # Step four: update Code of Conduct and Contributing files
  if (utils::askYesNo("Would you like to update package Code of Conduct and Contributing files?") == TRUE) {
    manytemplate("Package-COC.md",
                 fs::path(".github", "CODE_OF_CONDUCT", ext = "md"),
                 data = list(package = package,
                          author = author),
                 path = path,
                 open = FALSE)
    usethis::ui_done("Updated CODE_OF_CONDUCT.")

    manytemplate("Package-CONTRIB.md",
                 fs::path(".github", "CONTRIBUTING.md"),
                 data = list(package = package,
                          author = author),
                 path = path,
                 open = FALSE)
    usethis::ui_done("Updated CONTRIBUTING.")
  }

  # Step five: update templates
  manytemplate("Package-PR.md",
               fs::path(".github", "pull_request_template.md"),
               data = list(package = package,
                        author = author),
               path = path,
               open = FALSE)
  usethis::ui_done("Updated PR template.")

  manytemplate("Package-Bugs.md",
               fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
               data = list(package = package,
                        author = author),
               path = path,
               open = FALSE)
  usethis::ui_done("Updated bug report issue template.")

  manytemplate("Package-Features.md",
               fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
               data = list(package = package,
                        author = author),
               path = path,
               open = FALSE)
  usethis::ui_done("Updated feature request issue template.")

  if (interactive()) {
    file.copy(fs::path_package(package = "manypkgs",
                               "templates", "Package-Check.yml"),
              fs::path(".github", "workflows", "prchecks.yml"),
              overwrite = TRUE)
    usethis::ui_done("Updated workflow checks for push releases.")
    file.copy(fs::path_package(package = "manypkgs",
                               "templates", "Package-Commands.yml"),
              fs::path(".github", "workflows", "prcommands.yml"),
              overwrite = TRUE)
    usethis::ui_done("Updated commands workflow for push releases.")
    file.copy(fs::path_package(package = "manypkgs",
                               "templates", "Package-Release.yml"),
              fs::path(".github", "workflows", "pushrelease.yml"),
              overwrite = TRUE)
    usethis::ui_done("Updated release workflow for merging push releases.")
  }
}
