# Assortativity Coefficient

Assortativity coefficient is a measure of segregation for social
networks due to Newman & Girvan (2002).

## Usage

``` r
assort(object, ...)

# S3 method for class 'table'
assort(object, ...)

# S3 method for class 'igraph'
assort(object, vattr, ...)

# Default S3 method
assort(object, ...)
```

## Arguments

- object:

  R object, see available methods

- ...:

  other arguments to/from other methods

- vattr:

  character, name of the vertex attribute for which the measure is to be
  calculated

## Value

Numeric value of the index.

## Details

The measure evaluates the relative prevalence of within-group ties. It
is based on the contact layer of the mixing matrix.

Assortativity coefficient is 1 if all ties are within-group. The minimum
can be negative, but not less than -1, and depends on the relative
number of ties of nodes in different groups. If the network conforms to
"proportionate mixing", the coefficient is 0.

If `object` is a table it is interpreted as a mixing matrix.
Two-dimensional table is interpreted as a contact layer.
Three-dimensional table is interpreted as a full mixing matrix
\\m\_{ghy}\\ cross-classyfying all dyads, in which \\g\\ and \\h\\
correspond to group membership of ego and alter respectively. Layers
\\y=1\\ and \\y=2\\ are assumed to be non-contact and contact layers
respectively.

If `object` is of class "igraph" it is required to supply `vattr` with
the name of the vertex attribute to calculate intermediate mixing
matrix.

For any other classes, `object` is coerced to a table and the table
method is called.

## References

Newman, M. J. and Girvan, M. (2002) "Mixing patterns and community
structure in networks", arXiv:cond-mat/0210146v1

Newman, M. J. (2003) "Mixing patterns in networks"
arXiv:cond-mat/0209450v2

## See also

Mixing matrices:
[`mixingm()`](https://mbojan.github.io/netseg/reference/mixingm.md)

Other segregation measures:
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
assort(WhiteKinship, "gender")
#> [1] 0.1801242
assort(EF3, "type")
#> [1] 0

# Values of `assort()` for full networks of different sizes
if( requireNamespace("igraph", quietly = TRUE) ) {
  f <- function(n) {
    gfull <- igraph::make_full_graph(n, directed=FALSE)
    igraph::V(gfull)$type <- rep(1:2, length = igraph::vcount(gfull))
    assort(gfull, "type")
  }
  set.seed(1)
  x <- sort(sample(5:100, 25) * 2)
  y <- sapply(x, f)
  plot(x, y, type="o",
       xlab="Network size", ylab="Assortativity coefficient",
       main="Assortativity coef. for full networks of different sizes")
}
```
