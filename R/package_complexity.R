#' Analyze Big-O algorithmic complexity of a network function
#'
#' This function times a network function on generated networks
#' of different sizes and densities.
#' It returns an object of class \code{bigo}, which contains a
#' tibble of runtime results, the name of the function being measured, and
#' the number of runs over which the results are averaged.
#'
#' @param fun The function to be investigated.
#'   It must take a network as its first argument.
#' @param gen A character vector of the kinds of network to generate,
#'   each of which names a `{manynet}` function `generate_*()`,
#'   for example "random", "smallworld", or "scalefree".
#'   By default "random".
#' @param n A numeric vector of the numbers of nodes in the generated networks.
#' @param p A numeric vector of the values of the `p` argument
#'   of the generating function, for example the density for "random".
#' @param num_runs The number of runs over which each runtime is averaged.
#'   By default 1.
#' @return An object of class \code{bigo} containing the results of the
#'   runtime experiments: a tibble of runtimes, the function name, and the
#'   number of runs that the results are averaged over.
#' @examplesIf rlang::is_installed("manynet")
#' bigo_net(fun = manynet::net_nodes, gen = "random",
#'          n = c(10, 20), p = 0.1)
#' @export
bigo_net <- function(fun, gen = "random", n, p, num_runs = 1) {
  rlang::check_installed("manynet", reason = "to generate networks")
  f_char <- deparse(substitute(fun))
  if (length(f_char) > 1) f_char <- "anonymous"
  fun <- match.fun(fun)
  ns <- asNamespace("manynet")
  gens <- paste0("generate_", gen)
  missing_gens <- gens[!vapply(gens, exists, logical(1), envir = ns,
                               inherits = FALSE)]
  if (length(missing_gens) > 0)
    stop("These generating functions are not in {manynet}: ",
         paste(missing_gens, collapse = ", "), call. = FALSE)
  opts <- expand.grid(gen = gen, n = n, p = p, stringsAsFactors = FALSE)
  opts$elapsed <- vapply(seq_len(nrow(opts)), function(i) {
    generator <- get(paste0("generate_", opts$gen[i]), envir = ns)
    net <- generator(n = opts$n[i], p = opts$p[i])
    mean(vapply(seq_len(num_runs), function(r) .time(fun(net)), numeric(1)))
  }, numeric(1))
  results <- list(runtimes = dplyr::tibble(opts),
                  function_name = f_char,
                  num_runs = num_runs)
  class(results) <- "bigo"
  results
}

.time <- function(.expr) {
  elapsed <- system.time(.expr)
  elapsed[["elapsed"]]
}
