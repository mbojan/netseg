# Krackhard and Stern's E-I index

An index proposed by Krackhard and Stern (1988) to capture relative
prevalence of between- and within-group ties. From that perspective it
can be interpreted as a measure of network segregation.

## Usage

``` r
ei(object, ...)

# S3 method for class 'table'
ei(object, ...)

# S3 method for class 'igraph'
ei(
  object,
  vattr,
  directed = is_directed(object),
  loops = any(which_loop(object)),
  ...
)

# Default S3 method
ei(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from other methods

- vattr:

  character scalar or vector of length equal to the size of `object`,
  vertex attribute for which mixing matrix is to be computed

- directed:

  logical whether the network is directed

- loops:

  logical, whether loops are allowed

## Value

Numerical value of the E-I index.

## Details

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

## References

Krackhardt, D., & Stern, R. N. (1988). Informal networks and
organizational crises: An experimental simulation. *Social Psychology
Quarterly*, 123-140.

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
ei(WhiteKinship, "gender")
#> [1] -0.1818182
```
