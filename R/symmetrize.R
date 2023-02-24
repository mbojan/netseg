#' Symmetrize square numeric matrix
#'
#' Symmetrize square binary matrix in various ways.
#'
#' Argument `mat` is to be a square numeric matrix. The way it is made
#' symmetric, or asymetric, depends on the value of the `rule` argument.
#'
#' If `rule` is "upper" or "lower" then `mat` is made symmetric by copying,
#' respectively, upper triangle onto lower, or lower onto upper. The value of
#' `rule` specifies values of which triangle will stay in the returned value.
#'
#' If `rule` is "intdiv" then the off-diagonal values are distributed
#' approximately equally between the lower/upper triangles. If `r` is the
#' computed result, then `r[i,j]` will be equal to
#' `(x[i,j] + x[j,i]) \%/\% 2` if `r[i,j]` is in the lower triangle.
#' It will be equal to
#' `(x[i,j] + x[j,i]) \%/\% 2 + 1` if in the upper triangle.
#'
#' If `rule` is "div" then the off-diagonal values are distributed equally
#' between the lower/upper triangles: as with "intdiv" but using normal
#' `/` division.
#'
#' @param mat square numeric matrix
#' @param rule character, direction of copying, see Details
#'
#' @return A matrix: symmetrized version of `mat`.
#'
#' @seealso [fold()], [sna::symmetrize()]
#'
#' @export
#'
#' @examples
#' m <- matrix(1:16, 4, 4)
#'
#' # copy upper triangle onto lower symmetrically
#' symmetrize(m, "upper")
#'
#' # copy lower triangle onto upper symmetrically
#' symmetrize(m, "lower")
#'
#' # distribute off-diagonal values exactly
#' # r[i,j] = (m[i,j] + m[j,i]) / 2
#' r1 <- symmetrize(m, "div")
#' r1
#' all.equal(sum(m), sum(r1))
#'
#' # distribute off-diagonal values using integer division
#' r2 <- symmetrize(m, "intdiv")
#' r2
#' all.equal(sum(m), sum(r2))

symmetrize <- function(mat, rule=c("upper", "lower", "div", "intdiv"))
{
    stopifnot(is.matrix(mat))
    rul <- match.arg(rule)
    rval <- mat
    switch( rul,
        upper = {
          rval[ lower.tri(mat) ] <- t(mat)[lower.tri(mat)]
          rval
        },

        lower = {
          rval[ upper.tri(mat) ] <- t(mat)[upper.tri(mat)]
          rval
        },

        intdiv = {
          mm <- mat + t(mat)
          rval <- mm %/% 2
          r <- mm[upper.tri(mm)] - 2 * rval[upper.tri(rval)]
          rval[upper.tri(rval)] <- rval[upper.tri(rval)] + r
          rval
        },

        div = {
          mm <- mat + t(mat)
          mm / 2
        },

        stop("unknown value for 'rule'")
        )
}
