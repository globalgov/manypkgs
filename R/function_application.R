#' Checking whether functions work across different types of objects
#'
#' @param functions blah
#' @param objects blah
#' @examples
#'  library(manynet)
#'  functions <- c("node_in_optimal", "node_in_partition", "node_in_infomap",
#'                "node_in_spinglass", "node_in_fluid", "node_in_louvain", 
#'                "node_in_leiden", "node_in_betweenness", "node_in_greedy", 
#'                "node_in_eigen", "node_in_walktrap")
#'  objects <- table_data() %>% 
#'                 dplyr::distinct(directed, weighted, twomode, 
#'                 labelled, signed, .keep_all = TRUE) %>% 
#'                 dplyr::pull(dataset)
#'  functions_work(functions, objects)
#' @export
functions_work <- function(functions, objects){
  opts <- expand.grid(functions, objects)
  out <- apply(opts, 1, function(x) !berryFunctions::is.error(get(x[1])(get(x[2]))))
  out <- matrix(out, nrow = length(functions))
  rownames(out) <- functions
  colnames(out) <- objects
  out
}

#' @source https://www.r-bloggers.com/2013/06/printing-r-help-files-in-the-console-or-in-knitr-documents/
#' @export
print_help <- function(topic, format=c("text", "html", "latex", "Rd"),
                         lines=NULL, before=NULL, after=NULL) {  
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))
  hs <- capture.output(switch(format, 
                              text=tools:::Rd2txt(helpfile),
                              html=tools:::Rd2HTML(helpfile),
                              latex=tools:::Rd2latex(helpfile),
                              Rd=tools:::prepare_Rd(helpfile)
  )
  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}

#' @source https://www.r-bloggers.com/2013/06/printing-r-help-files-in-the-console-or-in-knitr-documents/
#' @export
find_nonascii <- function(pkg) {  
  pkgs_data <- data(package = pkg)$results[,3]
  lapply(pkgs_data, function(x){
    attribs <- net_node_attributes(get(x))
    print(x)
    if(length(attribs)>0)
      lapply(attribs, function(y){
        print(paste(" ", y))
        a <- node_attribute(get(x), y)
        if(is.character(a))
          if(!all(stringi::stri_enc_isascii(a), na.rm = TRUE))
            print(paste(x, y))
      })
  })
}
