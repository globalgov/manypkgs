.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manypkgs ", utils::packageVersion("{{{package}}}")),
           "\nFor more information about many packages pleae visit https://manydata.ch/",
           "\nType 'citation(\"{{{package}}}\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
