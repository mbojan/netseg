# Symmetrize square numeric matrix

Symmetrize square binary matrix in various ways.

## Usage

``` r
symmetrize(mat, rule = c("upper", "lower", "div", "intdiv"))
```

## Arguments

- mat:

  square numeric matrix

- rule:

  character, direction of copying, see Details

## Value

A matrix: symmetrized version of `mat`.

## Details

Argument `mat` is to be a square numeric matrix. The way it is made
symmetric, or asymetric, depends on the value of the `rule` argument.

If `rule` is "upper" or "lower" then `mat` is made symmetric by copying,
respectively, upper triangle onto lower, or lower onto upper. The value
of `rule` specifies values of which triangle will stay in the returned
value.

If `rule` is "intdiv" then the off-diagonal values are distributed
approximately equally between the lower/upper triangles. If `r` is the
computed result, then `r[i,j]` will be equal to
`(x[i,j] + x[j,i]) \%/\% 2` if `r[i,j]` is in the lower triangle. It
will be equal to `(x[i,j] + x[j,i]) \%/\% 2 + 1` if in the upper
triangle.

If `rule` is "div" then the off-diagonal values are distributed equally
between the lower/upper triangles: as with "intdiv" but using normal `/`
division.

## See also

[`fold()`](https://mbojan.github.io/netseg/reference/fold.md),
`sna::symmetrize()`

## Examples

``` r
m <- matrix(1:16, 4, 4)

# copy upper triangle onto lower symmetrically
symmetrize(m, "upper")
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    5    6   10   14
#> [3,]    9   10   11   15
#> [4,]   13   14   15   16

# copy lower triangle onto upper symmetrically
symmetrize(m, "lower")
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    2    3    4
#> [2,]    2    6    7    8
#> [3,]    3    7   11   12
#> [4,]    4    8   12   16

# distribute off-diagonal values exactly
# r[i,j] = (m[i,j] + m[j,i]) / 2
r1 <- symmetrize(m, "div")
r1
#>      [,1] [,2] [,3] [,4]
#> [1,]  1.0  3.5  6.0  8.5
#> [2,]  3.5  6.0  8.5 11.0
#> [3,]  6.0  8.5 11.0 13.5
#> [4,]  8.5 11.0 13.5 16.0
all.equal(sum(m), sum(r1))
#> [1] TRUE

# distribute off-diagonal values using integer division
r2 <- symmetrize(m, "intdiv")
r2
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    6    9
#> [2,]    3    6    9   11
#> [3,]    6    8   11   14
#> [4,]    8   11   13   16
all.equal(sum(m), sum(r2))
#> [1] TRUE
```
