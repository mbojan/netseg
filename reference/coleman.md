# Coleman's homophily index

Colemans's homphily index for directed networks.

## Usage

``` r
coleman(object, ...)

# S3 method for class 'table'
coleman(object, gsizes = NULL, loops = FALSE, ...)

# S3 method for class 'igraph'
coleman(object, vattr, ...)

# Default S3 method
coleman(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from methods

- gsizes:

  numeric vector of group sizes

- loops:

  logical, whether loops are allowed

- vattr:

  character, vertex attribute

## Value

Vector of numeric values of the index for each group

## Details

Coleman's homophily index computes homophily scores for each group
defined by a vertex attribute.

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

Coleman, J. (1958) "Relational analysis: The study of social
organizations with survey methods", *Human Organization* 17:28–36.

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`freeman()`](https://mbojan.github.io/netseg/reference/freeman.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r
if(requireNamespace("igraph", quietly = TRUE)) {
  # Converting networks to directed
  coleman(igraph::as_directed(WhiteKinship, "mutual"), "gender")
  coleman(igraph::as_directed(EF3, "mutual"), "race")
}
#>         1         2 
#> 0.3337838 0.1918033 
```
