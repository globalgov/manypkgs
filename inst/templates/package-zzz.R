.onAttach <- function(lib, pkg) {
  msg <- c(paste0("{{{package}}} ", utils::packageVersion("{{{package}}}")),
           "\nFor more information about many packages please visit https://manydata.ch/",
           "\nType 'citation(\"{{{package}}}\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
