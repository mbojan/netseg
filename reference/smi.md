# Segregation Matrix Index

Segregation Matrix Index due to Freshtman (1997). A measure of network
segregation. Currently (and originally) supports only two groups.

## Usage

``` r
smi(object, ...)

# S3 method for class 'table'
smi(object, normalize = TRUE, ...)

# S3 method for class 'igraph'
smi(object, vattr, ...)

# Default S3 method
smi(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from other methods

- normalize:

  logical, whether normalized values should be returned, defaults to
  `TRUE`

- vattr:

  character, name of the node attribute designating groups

## Value

Numeric vector of length equal to the number of groups in `g` according
to `vattr` with the values of SMI for the groups.

## Details

The Segregation Matrix Index (SMI) is calculated for every group
separately. It compares the density within group to the density of
between group ties of nodes belonging to that group.

Non-normalized version is the ratio of the within-group density to the
between-group density, so vary between 0 and infinity. The normalized
version varies between 0 and 1.

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

Freshtman, M. (1997) "Cohesive Group Segregation Detection in a Social
Network by the Segregation Matrix Index", Social Networks, 19:193–207

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
# smi() needs a directed network
smi( igraph::as_directed(WhiteKinship, "mutual"), "gender")
#>    female      male 
#> 0.2500000 0.3207547 
```
