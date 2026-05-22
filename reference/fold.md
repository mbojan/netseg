# Folding square matrices around the diagonal

Fold a square matrix by collapsing lower triangle on upper triangle, or
vice versa, through addition.

## Usage

``` r
fold(x, direction = c("upper", "lower"))
```

## Arguments

- x:

  square numeric matrix

- direction:

  character, one of `"upper"` or `"lower"`, direction of folding

## Value

Square matrix of the same `dim` as `x` with the lower (upper) triangle
folded onto the upper (lower) triangle.

## Details

By default, for `direction=="upper"`, the function takes the values in
the lower triangle of `x` and adds them symetrically to the values in
the upper triangle. The values on the diagonal remain unchanged. The
lower triangle is filled with 0s. If `direction=="lower"` the upper
triangle is collapsed on to the lower triangle.

## See also

[`upper.tri()`](https://rdrr.io/r/base/lower.tri.html),
[`lower.tri()`](https://rdrr.io/r/base/lower.tri.html),
[`symmetrize()`](https://mbojan.github.io/netseg/reference/symmetrize.md)

## Examples

``` r
(m <- matrix(1:4, 2, 2))
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
(f1 <- fold(m))
#>      [,1] [,2]
#> [1,]    1    5
#> [2,]    0    4
(f2 <- fold(m, "lower"))
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    5    4

stopifnot( all.equal(diag(m), diag(f1)) )
stopifnot( all.equal(diag(m), diag(f2)) )
stopifnot( all.equal(f1[1,2], m[2,1] + m[1,2]) )
stopifnot( all.equal(f2[2,1], m[2,1] + m[1,2]) )
```
