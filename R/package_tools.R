#' Loading packages
#'
#' Function for loading and, if necessary, installing packages
#' @param packages Character vector of packages to install from CRAN or GitHub
#' @importFrom utils install.packages
#' @importFrom remotes install_github
#' @importFrom stringr str_split
#' @return Loads and, if necessary, first installs packages
#' @details The function looks up required packages and loads the ones
#' already installed, while installing and loading for packages not installed.
#' For CRAN packages the package name is required as argument,
#' for GitHub username/repo are required as argument.
#' @examples
#' \dontrun{
#' depends("manypkgs")
#' }
#' @export
depends <- function(packages) {
  lapply(packages,
         function(x) {
           if (stringr::str_detect(x, "/")) {
             remotes::install_github(x, dependencies = TRUE)
             x <- stringr::str_split(x, "/")[[1]][2]
             library(x, character.only = TRUE)
           } else if (!require(x, character.only = TRUE)) {
             utils::install.packages(x, dependencies = TRUE)
             library(x, character.only = TRUE)
           } else {
             library(x, character.only = TRUE)
           }
         })
}

#' Keep objects from the global environment
#'
#' Function for keeping objects from the global environment.
#' It removes all variables of a dataset in the global environment
#' except those which are specified in the given character vector,
#' regular expression or both.
#' @param keep A vector containing the name of variables which you wish to keep
#' and not remove or a regular expression to match variables you want to keep.
#' Variable names and regular expressions can be used combined
#' if the argument 'regex' is set to "auto". (Mandatory)
#' @param envir The environment that this function should be functional in,
#' search in and act in. (Optional)
#' @param keep_functions A logical vector of length 1 indicating exclusion
#' of function variables from removal. (optional)
#' @param gc_limit A numeric vector of length 1 indicating the threshold
#' for garbage collection in Megabyte (MB) scale. (Optional)
#' @param regex A vector with length 1 to define whether the function use
#' regular expression in keep (TRUE or FALSE) or auto detect ("auto")
#' @importFrom utils lsf.str object.size
#' @return Clears the environment except for the stated objects.
#' @details The function is is suitable for Small to semi-large projects
#' with massive amount of data as it performs variable operations in a fast
#' and efficient way. The function is also useful for developers
#' preparing and assembling a pipeline
#' @source https://bitbucket.org/mehrad_mahmoudian/varhandle/src/master/R/rm.all.but.R
#' @examples
#' origins <- c("New Zealand", "Brazil", "Switzerland")
#' members <- c(1, 1, 2)
#' team <- data.frame(origins, members)
#' team
#' retain("origins")
#' origins
#' @export
retain <- function(keep = NULL, envir = .GlobalEnv, keep_functions = TRUE,
                   gc_limit = 100, regex = "auto") {

  { ## Check the envir attribute
    if (!is.environment(envir)) {
      stop("You should specify an existing environment")
    }
    ## ckeck if keep is defined
    if (is.null(keep)) {
      stop("The parameter `keep` is not defined.
           It should be a chacter vector with length 1 or more.")
      # if the provided object for keep is not a character vector
    } else if (!inherits(keep, "character")) {
      stop("The parameter `keep` should be a chacter vector with length 1 or more.")
    }
    ## Check if the keep is a character vector
    if ((!inherits(keep, "character")) | typeof(keep) != "character") {
      stop("The value of `keep` parameter should be a chacter vector with length 1 or more.")
    }
    ## check if the length of keep is more than or equal to 1
    if (!length(keep)) {
      stop("The value of `keep` parameter should be a chacter vector with length 1 or more.")
    }
    ## if the keepFunctions is not a logical vector
    if (!is.logical(keep_functions) | length(keep_functions) != 1) {
      stop("The value of the `keepFunctions` should ve either TRUE or FLASE.")
    }
    ## check if the gc_limit has a valid value
    if (!is.numeric(gc_limit) | length(gc_limit) != 1) {
      stop("The value of `gc_limit` parameter should be numeric with length 1. It's unit is MB.")
    }
    ## check if the regex has a valid value
    if (!is.element(regex, c(TRUE, FALSE, "auto", 0, 1))) {
      stop("The value of `regex` should be either TRUE, FALSE or \"auto\".")
    }
  }

  { # if user wants to automatically detect regex or manually force it
    if (is.element(regex, c(TRUE, "auto", 1))) {
      # get the index of which items in keep
      # possibly contain regular expression
      regex_index <- grep(x = keep,
                          pattern = "[\\|\\(\\)\\[\\{\\^\\$\\*\\+\\?\\]")
      # if regex_index has one or more indices
      if (length(regex_index)) {
        regex <- TRUE
        # remove regular expression pettern(s) from
        # keep and put them in separate variable
        regex_patterns <- keep[regex_index]
        keep <- keep[-regex_index]
      } else {
        regex <- FALSE
      }
      # if user manually force the code not to concider regular expression
    } else {
      regex <- FALSE
    }
    # in case user decides to only operates on non-function variables
    if (keep_functions) {
      # only put non-functions in the list
      var_list <- setdiff(ls(envir = as.environment(envir)),
                          utils::lsf.str(envir = as.environment(envir)))
      # in case user wants to consider functions as well and remove them too
    } else {
      # put everything in the list
      var_list <- ls(envir = as.environment(envir))
    }

    { # create an empty vector to store names
      # and patterns that does not match to anything
      bad_input <- vector()
      {
        # If the keep has a name that is not in ls(),
        # show error with the location of bad variable name
        if (any(!is.element(keep, var_list))) {
          # find which input item is not a real variable
          bad_input <- keep[which(is.element(keep, var_list) == FALSE)]
        }
      }

      { # check if there is any regex_patterns that does not
        # match to anything if there is any regex
        if (regex) {
          # iterate through the patterns
          for (i in regex_patterns) {
            # if the count of found match for each pattern is zero
            if (length(grep(pattern = i, x = var_list)) == 0) {
              # add it's index in keep to bad_input
              bad_input <- c(bad_input, i)
            }
          }
        }
      }
      # if there is any bad input
      if (length(bad_input) > 0) {
        # complain to user
        stop(paste("All the items in the keep should be a real existing
                   variable or valid regular expressions.\nThe following
                   is/are not among variables of selected environment or
                   patterns that match anything!\n", bad_input, sep = " "))
      }
    }
    # initialise a variable that contains all the possible variables
    removables <- var_list
    # if user have used a regular expression
    if (regex) {
      ## apply the regex in the following lines
      # iterate through the regex patterns
      for (i in regex_patterns) {
        # get indices of items in removables that match the ith pattern
        tmp_pattern_remove_list <- grep(pattern = i, x = removables)
        # if there was any matches based on the pattern
        if (length(tmp_pattern_remove_list)) {
          # remove the variables from the removables vector
          removables <- removables[-tmp_pattern_remove_list]
        }
      }
    }
    # if there is anything left in keep variable
    if (length(keep)) {
      # list the name of variables that should be removed
      removables <- removables[!(removables %in% keep)]
      # avoid any duplicates which might appear
      # by having regexp and having normal variable names
      removables <- unique(removables)
    }
    # if anything has left to be removedf
    if (length(removables)) {
      # get to total sum of the variables that are going
      # to be removed in bytes
      total_size <- sum(vapply(removables,
                               function(x) {
                                 utils::object.size(
                                   get(x, envir = as.environment(envir)))
                               }, numeric(1)))
      # remove the variables
      remove(list = removables, envir = as.environment(envir))
      # if the total size of removed varibale exceeded the threshold
      if (total_size > (gc_limit * 1024 ^ 2)) {
        # call garbage collection
        gc()
      }
    } else {
      warning("Nothing is left to be removed!")
    }
  }
}

#' Calling CRAN for the packages of an author
#'
#' @description
#'   `call_cran()` lists the packages on CRAN whose authors include `author`,
#'   with the number of days since each was published,
#'   and the first date on which a new version can be submitted.
#'   CRAN asks that a package is not updated more often than every 10 days or so.
#' @param author A character string that matches the name of an author.
#'   By default "Hollway".
#' @return A tibble with the variables `Package`, `Version`, `SincePub`
#'   (days since the package was published), `Updateable`
#'   (whether 10 days have passed), and `CanCRAN` (the first date on which
#'   a new version can be submitted).
#' @examples
#' \dontrun{
#' call_cran("Hollway")
#' }
#' @export
call_cran <- function(author = "Hollway"){
  cran_db <- tools::CRAN_package_db()
  cran_db <- cran_db[grepl(author, cran_db$Author), ]
  since <- as.numeric(Sys.Date() - as.Date(cran_db$Published))
  when <- Sys.Date() + pmax(0, 10 - since)
  out <- dplyr::tibble(Package = cran_db$Package,
                       Version = cran_db$Version,
                       SincePub = since,
                       Updateable = since >= 10,
                       CanCRAN = format(when, "%A %d %B %Y"))
  out[order(out$SincePub), ]
}

#' @source https://blog.r-hub.io/2022/09/12/r-dependency/
#' @export
find_transitive_minR <- function(package) {
  
  db <- tools::CRAN_package_db()
  
  recursive_deps <- tools::package_dependencies(
    package, 
    recursive = TRUE, 
    db = db
  )[[1]]
  
  # These code chunks are detailed below in the 'Minimum R dependencies in CRAN 
  # packages' section
  r_deps <- db |> 
    dplyr::filter(Package %in% recursive_deps) |> 
    # We exclude recommended pkgs as they're always shown as depending on R-devel
    dplyr::filter(is.na(Priority) | Priority != "recommended") |>  
    dplyr::pull(Depends) |> 
    strsplit(split = ",") |> 
    purrr::map(~ grep("^R ", .x, value = TRUE)) |> 
    unlist()
  
  r_vers <- trimws(gsub("^R \\(>=?\\s(.+)\\)", "\\1", r_deps))
  
  return(max(package_version(r_vers)))
}

#' @importFrom rstudioapi navigateToFile
#' @export
edit_tutorial <- function(tute){
  tutes <- list.files("inst/tutorials", recursive = TRUE)
  tutes <- tutes[grepl(".Rmd$", tutes)]
  if(grepl("^tut", tute)){
    opts <- vapply(strsplit(tutes,"/"), `[`, 1, FUN.VALUE=character(1))
  } else {
    opts <- vapply(strsplit(tutes,"/"), `[`, 2, FUN.VALUE=character(1))
  }
  opt <- tutes[which.min(utils::adist(tute, opts, ignore.case = TRUE,
                                      costs = list(ins=0, del=1, sub=1)))]
  pth <- paste0("inst/tutorials/", opt)
  if(rstudioapi::isAvailable())
    rstudioapi::navigateToFile(pth)

#' Grouping the functions of a package
#'
#' @description
#'   `group_functions()` lists the exported functions of a package
#'   that match a pattern, leaves out the defunct and deprecated functions,
#'   and groups the rest by the prefix before the first underscore.
#' @param pattern A regular expression that the function names must match.
#'   By default "", which matches all of them.
#' @param package The name of an installed package.
#'   By default "manynet".
#' @return A named list of character vectors of function names,
#'   with one element for each prefix.
#' @examples
#' group_functions("^find", package = "manypkgs")
#' @export
group_functions <- function(pattern = "", package = "manynet"){
  funs <- sort(collect_functions(pattern, package))
  funs <- funs[!funs %in% find_defunct(package)]
  split(funs, sub("_.*", "", funs))
}

#' Finding the sizes of the installed packages
#'
#' @description
#'   `find_installed_pkg_sizes()` lists the installed packages
#'   in all the library paths, from the largest to the smallest.
#'   It uses the `du` command, so it works on macOS and Linux only.
#' @return A tibble with the size of each package in kilobytes (`size`)
#'   and its name (`name`).
#' @examples
#' \dontrun{
#' find_installed_pkg_sizes()
#' }
#' @export
find_installed_pkg_sizes <- function() {
  out <- do.call(rbind, lapply(.libPaths(), function(d) {
    sizes <- utils::read.table(pipe(paste("du -k -d 1", shQuote(d))),
                               col.names = c("size", "name"))
    sizes[sizes$name != d, ]
  }))
  out$name <- basename(out$name)
  dplyr::tibble(out[order(-out$size), ])
}
