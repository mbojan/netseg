# Gupta-Anderson-May measure of within-group mixing

Measure of within-group mixing in networks proposed in Gupta, Anderson
and May (1989).

## Usage

``` r
gamix(object, ...)

# S3 method for class 'table'
gamix(object, ...)

# S3 method for class 'igraph'
gamix(object, vattr, ...)

# Default S3 method
gamix(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other objects passed to/from other methods

- vattr:

  character, name of vertex attribute

## Value

Numerical value of the measure.

## Details

The measure varies between `-1/vcount(g)` for dissassortative mixing and
1 for perfect within-group mixing. It takes a value of 0 for
proportionate mixing.

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

Gupta, S., Anderson, R., May, R. (1989) "Networks of sexual contacts:
implications for the pattern of spread of HIV", AIDS 3:807–817

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
gamix(WhiteKinship, "gender")
#> [1] 0.1801242
gamix(EF3, "race")
#> [1] 0.261852
```
