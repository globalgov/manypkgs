.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manypkgs ", utils::packageVersion("manypkgs")),
           "\nFor more information about the package please visit https://globalgov.github.io/manypkgs/",
           "\nType 'citation(\"manypkgs\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
