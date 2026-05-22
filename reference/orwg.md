# Odds ratio of existence of within-group ties

Odds ratio for connected, as opposed to disconnected, dyads depending
whether it is between- or within-group, i.e. how much more likely the
dyad will be connected if it is within-group.

## Usage

``` r
orwg(object, ...)

# S3 method for class 'table'
orwg(object, ...)

# S3 method for class 'igraph'
orwg(object, vattr, ...)

# Default S3 method
orwg(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from other methods

- vattr:

  character scalar or any vector, name of the vertex attribute or the
  attribute itself (as a vector)

## Value

Numeric value of the measure.

## Details

The measure takes values, like all odds ratios, from (0; Inf).

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

Moody, Jim (2001) "Race, school integration, and friendship segregation
in America", American Journal of Sociology, 107(3):679–377

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
orwg(WhiteKinship, "gender")
#> [1] 3.301587
```
