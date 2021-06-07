#' Updates qPackages
#'
#' Updates files for qPackages setup with setup_package()
#' @param package The name of the package to be updated, optional.
#' When not declared package name is extracted from description.
#' @param path The file path, optional.
#' If not specified, function get's the current working directory.
#' @import usethis
#' @import desc
#' @importFrom stringr str_detect
#' @examples
#' \dontrun{
#' update_package("qStates")
#' }
#' @return Updated files in correct structure for qPackages.
#' @export
update_package <- function(package = NULL, path = getwd()) {

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
    if(stringr::str_detect(path, package)) {
      path = getwd()
    } else {
      stop("Package path differs from current working directory.
           Please either declare a path to package directory
           or use setwd() to setup directory beforehand.")
    }
  }

  # Step two: update License
  if(askYesNo("Would you like to update package LICENSE file?") == TRUE) {
    qtemplate("LICENSE.md",
              ignore = TRUE,
              path = path,
              open = FALSE)
    # desc::desc_set("License", "CC BY 4.0")
    usethis::ui_done("Updated License file.")
  }
  
  # Step three: update Code of Conduct and Contributing files
  if(askYesNo("Would you like to update package Code of Conduct and Contributing files?") == TRUE) {
    qtemplate("qPackage-COC.md",
              fs::path(".github", "CODE_OF_CONDUCT", ext = "md"),
              data = list(package = package,
                          author = author),
              path = path,
              open = FALSE)
    usethis::ui_done("Updated CODE_OF_CONDUCT.")
    
    qtemplate("qPackage-CONTRIB.md",
              fs::path(".github", "CONTRIBUTING.md"),
              data = list(package = package,
                          author = author),
              path = path,
              open = FALSE)
    usethis::ui_done("Updated CONTRIBUTING.")
  }

  # Step four: update templates
  qtemplate("qPackage-PR.md",
            fs::path(".github", "pull_request_template.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Updated PR template.")

  qtemplate("qPackage-Bugs.md",
            fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Updated bug report issue template.")

  qtemplate("qPackage-Features.md",
            fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Updated feature request issue template.")

  if (interactive()) {
    file.copy(fs::path_package(package = "qCreate",
                               "templates", "qPackage-Check.yml"),
              fs::path(".github", "workflows", "prchecks.yml"))
    usethis::ui_done("Updated workflow checks for push releases.")
    file.copy(fs::path_package(package = "qCreate",
                               "templates", "qPackage-Commands.yml"),
              fs::path(".github", "workflows", "prcommands.yml"))
    usethis::ui_done("Updated commands workflow for push releases.")
    file.copy(fs::path_package(package = "qCreate",
                               "templates", "qPackage-Release.yml"),
              fs::path(".github", "workflows", "pushrelease.yml"))
    usethis::ui_done("Updated release workflow for merging push releases.")
  }

  # step five: update Description file
  # desc::desc_set_dep() 
  desc::desc_add_remotes("globalgov/qCreate")
  usethis::ui_done("Updated description file.")
  
}