# Analyze Big-O algorithmic complexity of a network function

This function times a network function on generated networks of
different sizes and densities. It returns an object of class `bigo`,
which contains a tibble of runtime results, the name of the function
being measured, and the number of runs over which the results are
averaged.

## Usage

``` r
bigo_net(fun, gen = "random", n, p, num_runs = 1)
```

## Arguments

- fun:

  The function to be investigated. It must take a network as its first
  argument.

- gen:

  A character vector of the kinds of network to generate, each of which
  names a `{manynet}` function `generate_*()`, for example "random",
  "smallworld", or "scalefree". By default "random".

- n:

  A numeric vector of the numbers of nodes in the generated networks.

- p:

  A numeric vector of the values of the `p` argument of the generating
  function, for example the density for "random".

- num_runs:

  The number of runs over which each runtime is averaged. By default 1.

## Value

An object of class `bigo` containing the results of the runtime
experiments: a tibble of runtimes, the function name, and the number of
runs that the results are averaged over.

## Examples

``` r
bigo_net(fun = manynet::net_nodes, gen = "random",
         n = c(10, 20), p = 0.1)
#> $runtimes
#> # A tibble: 2 × 4
#>   gen        n     p elapsed
#>   <chr>  <dbl> <dbl>   <dbl>
#> 1 random    10   0.1 0.00200
#> 2 random    20   0.1 0      
#> 
#> $function_name
#> [1] "manynet::net_nodes"
#> 
#> $num_runs
#> [1] 1
#> 
#> attr(,"class")
#> [1] "bigo"
```
