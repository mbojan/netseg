# Generalized Freeman's segregation index

Calculate Freeman's segregation index for undirected networks with
arbitrary number of groups.

## Usage

``` r
freeman(object, ...)

# S3 method for class 'table'
freeman(object, gsizes = NULL, loops = FALSE, ...)

# S3 method for class 'igraph'
freeman(object, vattr, gsizes = NULL, loops = any(which_loop(object)), ...)

# Default S3 method
freeman(object, ...)
```

## Arguments

- object:

  R object, see Details for available methods

- ...:

  other arguments passed to/from other methods

- gsizes:

  numeric, optional true distribution of types, see Details

- loops:

  logical, whether loops are allowed

- vattr:

  character scalar or any vector of length equal to `vcount(object)`,
  name of the vertex attribute in `object` designating the groups or a
  vector with the attribute itself

## Value

The value of the Freeman's index.

## Details

Freeman's segregation index (Freeman, 1978) is designed to capture the
extent to which the defined groups of vertices tend to have more edges
with vertices from the same group than with other groups. Formally, the
index compares the observed number of between-group ties with the number
of between-group ties that would be expected if ties would be created
randomly.

Originally the index has a discontinuity for network and group size
configurations that are characterized by the higher number of
between-group ties that is expected under a random graph, for which it
returns 0 (as originally described by Freeman (1978)). We removed that
truncation such that it returns values betweem -1 and 1.

The original Freeman's formulation involves two groups of vertices. Here
it is extended to the arbitrary number of groups. The generalization
affects the way in which the expected number of between-group edges
under pure random graph is calculated, see Bojanowski & Corten (2014)
for details.

The function internally calculates the sizes of groups of vertices in
the supplied attribute `vattr`. However, it is possible to override this
by specifying "true" type distribution with the `gsizes` argument. It is
assumed to be a table (as returned by
[`table()`](https://rdrr.io/r/base/table.html)) or a numeric vector with
the group sizes. This may be especially usefull when dealing with large
graphs and/or with large number of isolates.

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

Method for mixing matrices

Method for "igraph"s

## References

Freeman, Linton C. (1978) Segregation in Social Networks, *Sociological
Methods & Research* **6**(4):411–429

Bojanowski, Michał, and Rense Corten. 2014. "Measuring Segregation in
Social Networks." *Social Networks* 39: 14–32.
[doi:10.1016/j.socnet.2014.04.001](https://doi.org/10.1016/j.socnet.2014.04.001)

## See also

Other segregation measures:
[`assort()`](https://mbojan.github.io/netseg/reference/assort.md),
[`coleman()`](https://mbojan.github.io/netseg/reference/coleman.md),
[`ei()`](https://mbojan.github.io/netseg/reference/ei.md),
[`gamix()`](https://mbojan.github.io/netseg/reference/gamix.md),
[`orwg()`](https://mbojan.github.io/netseg/reference/orwg.md),
[`smi()`](https://mbojan.github.io/netseg/reference/smi.md),
[`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md)

## Examples

``` r

## White's data from Freeman's article segregation level
freeman(WhiteKinship, "gender")
#> [1] 0.2636364

# using 'more' argument
freeman(WhiteKinship, "gender")
#> [1] 0.2636364
```
