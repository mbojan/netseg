#' Folding square matrices around the diagonal
#'
#' Fold a square matrix by collapsing lower triangle on upper triangle, or vice
#' versa, through addition.
#'
#' By default, for \code{direction=="upper"}, the function takes the values in
#' the lower triangle of \code{x} and adds them symetrically to the values in
#' the upper triangle. The values on the diagonal remain unchanged. The lower
#' triangle is filled with 0s. If \code{direction=="lower"} the upper triangle
#' is collapsed on to the lower triangle.
#'
#' @param x square numeric matrix
#'
#' @param direction character, one of \code{"upper"} or \code{"lower"},
#' direction of folding
#'
#' @return Square matrix of the same \code{dim} as \code{x} with the lower
#' (upper) triangle folded onto the upper (lower) triangle.
#'
#' @seealso \code{\link{upper.tri}}, \code{\link{lower.tri}}
#'
#' @export
#' @examples
#' (m <- matrix(1:4, 2, 2))
#' (f1 <- fold(m))
#' (f2 <- fold(m, "lower"))
#'
#' stopifnot( all.equal(diag(m), diag(f1)) )
#' stopifnot( all.equal(diag(m), diag(f2)) )
#' stopifnot( all.equal(f1[1,2], m[2,1] + m[1,2]) )
#' stopifnot( all.equal(f2[2,1], m[2,1] + m[1,2]) )

fold <- function(x, direction=c("upper", "lower"))
{
    stopifnot(is.matrix(x))
    stopifnot(dim(x)[1] == dim(x)[2])
    m <- t(x) + x
    diag(m) <- diag(x)
    d <- match.arg(direction)
    if( d == "upper" )
        m[ lower.tri(m) ] <- 0
    else
        m[ upper.tri(m) ] <- 0
    m
}
