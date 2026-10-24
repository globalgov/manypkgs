#' Getting the GitHub issues of a package
#'
#' @description
#'   `evaluate_bugs()` reads the `BugReports` field in the DESCRIPTION file
#'   of an installed package, and gets the issues of its GitHub repository.
#' @param pkg The name of an installed package.
#' @return A data frame of the issues, as returned by `githubr::get_issues()`.
#' @examples
#' \dontrun{
#' evaluate_bugs("manypkgs")
#' }
#' @export
evaluate_bugs <- function(pkg){
  rlang::check_installed("githubr", reason = "to get issues from GitHub")
  bug_url <- utils::packageDescription(pkg, fields = "BugReports")
  if (is.na(bug_url) || bug_url == "") {
    stop("No BugReports field found in the DESCRIPTION file of ", pkg, ".",
         call. = FALSE)
  }
  repo <- regmatches(bug_url, regexpr("(?<=github\\.com/)[^/]+/[^/]+",
                                      bug_url, perl = TRUE))
  if (length(repo) == 0) {
    stop("The BugReports field of ", pkg, " is not a GitHub URL: ", bug_url,
         call. = FALSE)
  }
  githubr::get_issues(repo)
}

# Not exported until it does something.
evaluate_contributors <- function(pkg){

}
