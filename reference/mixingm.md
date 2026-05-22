# Network mixing matrix

Creating network mixing matrices (`mixingm()`) and data frames
(`mixingdf()`).

## Usage

``` r
mixingm(object, ...)

# S3 method for class 'igraph'
mixingm(
  object,
  rattr,
  cattr = rattr,
  full = FALSE,
  directed = is_directed(object),
  loops = any(which_loop(object)),
  ...
)

mixingdf(object, ...)

# S3 method for class 'table'
mixingdf(object, ...)

# S3 method for class 'igraph'
mixingdf(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from other methods

- rattr:

  name of the vertex attribute or an attribute itself as a vector. If
  `cattr` is not `NULL`, `rattr` is used for rows of the resulting
  mixing matrix.

- cattr:

  name of the vertex attribute or an attribute itself as a vector. If
  supplied, used for columns in the mixing matrix.

- full:

  logical, whether two- or three-dimensional mixing matrix should be
  returned.

- directed:

  logical, whether the network is directed. By default, directedness of
  the network is determined with
  [`igraph::is_directed()`](https://r.igraph.org/reference/is_directed.html).

- loops:

  logical, whether loops are allowed. By default it is `TRUE` whenever
  there is at least one loop in `object`.

## Value

Function `mixingm()`, depending on `full` argument, a two- or
three-dimensional array crossclassifying connected or all dyads in
`object`. For undirected network and if `foldit` is `TRUE` (default),
the matrix is folded onto the upper triangle (entries in lower triangle
are 0).

Function `mixingdf()` returns non-zero entries of a mixing matrix (as
returned by `mixingm()`), but organized in a data frame with columns:

- `ego`, `alter` – group membership of ego an alter

- `tie` – present only if `full=TRUE`, with `TRUE` or `FALSE` for
  connected and disconnected dyads respectively

- `n` – counts

## Details

Network mixing matrix is, traditionally, a two-dimensional
cross-classification of edges depending on the values of a specified
vertex attribute for tie sender and tie receiver. It is an important
tool for assessing network homophily or segregation.

Let \\G\\ be the number of distinct values of the vertex attribute in
question. We may say that we have \\G\\ mutually exclusive groups in the
network. The mixing matrix is a \\G \times G\\ matrix such that
\\m\_{ij}\\ is the number of ties send by vertices in group \\i\\ to
vertices in group \\j\\. The diagonal of that matrix is of special
interest as, say, \\m\_{ii}\\ is the number of ties *within* group
\\i\\.

A full mixing matrix is a three-dimensional array that cross-classifies
*all* network *dyads* depending on:

1.  the value of the vertex attribute for tie sender

2.  the value of the vertex attribute for tie receiver

3.  the status of the dyad, i.e. whether it is connected or not

The two-dimensional version is a so-called "contact layer" of the
three-dimensional version.

If `object` is of class "igraph," mixing matrix is created for the
network in `object` based on vertex attributes supplied in arguments
`rattr` and optionally `cattr`.

If only `rattr` is specified (or, equivalently, `rattr` and `cattr` are
identical), the result will be a mixing matrix \\G \times G\\ if `full`
is `FALSE` or \\G \times G \times 2\\ if `full` is `TRUE`. Where \\G\\
is the number of categories of vertex attribute specified by `rattr`.

If `rattr` and `cattr` can be used to specify different vertex
attributes for tie sender and tie receiver.

## Examples

``` r
if(requireNamespace("igraph", quietly = TRUE)) {
  # some directed network
  net <- igraph::make_graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
  igraph::V(net)$type <- c(1,1,1, 2,2)
  mixingm(net, "type")
  mixingm(net, "type", full=TRUE)
  # as undirected
  mixingm( igraph::as_undirected(net), "type")
  mixingm(net, "type")
  mixingm(net, "type", full=TRUE)
}
#> , , tie = FALSE
#> 
#>    alter
#> ego 1 2
#>   1 3 4
#>   2 4 1
#> 
#> , , tie = TRUE
#> 
#>    alter
#> ego 1 2
#>   1 3 2
#>   2 2 1
#> 
```
