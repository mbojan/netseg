# Computing group sizes from square mixing matrices

`group_sizes()` recomputes group sizes from a full mixing matrix. This
is only limited to square (single-attribute) mixing matrices.

## Usage

``` r
group_sizes(mm, directed = TRUE, loops = FALSE)
```

## Arguments

- mm:

  numeric array with `dim` of \\(k, k, 2)\\ for some \\k\\

- directed:

  logical, whether network is directed

- loops:

  logical, whether loops are allowed

## Value

A numeric vector of group sizes
