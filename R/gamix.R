#' Gupta-Anderson-May measure of within-group mixing
#'
#' Measure of within-group mixing in networks proposed in Gupta, Anderson and
#' May (1989).
#'
#' The measure varies between `-1/vcount(g)` for dissassortative mixing
#' and 1 for perfect within-group mixing. It takes a value of 0 for
#' proportionate mixing.
#'
#' @param object R object, see Details for available methods
#' @param ... other objects passed to/from other methods
#'
#' @template mm-igraph-methods
#'
#' @return Numerical value of the measure.
#'
#' @references
#' Gupta, S., Anderson, R., May, R. (1989) "Networks of sexual contacts:
#' implications for the pattern of spread of HIV", AIDS 3:807--817
#'
#' @export
#' @family segregation measures
#'
#' @examples
#' gamix(WhiteKinship, "gender")
#' gamix(EF3, "race")

gamix <- function(object, ...) UseMethod("gamix")




#' @rdname gamix
#' @export
gamix.table <- function(object, ...)
{
  if( length(dim(object)) == 3 )
  {
    m <- symmetrize(object[,,2])
  } else
  {
    m <- symmetrize(object)
  }
  p <- sweep(m, 1, rowSums(m), "/")
  w <- eigen(p)$values
  (sum(w) - 1) / (dim(m)[1] - 1)
}



#' @rdname gamix
#' @param vattr character, name of vertex attribute
#' @export
gamix.igraph <- function (object, vattr, ...)
{
  m <- mixingm(object, rattr=vattr)
  gamix(m, ...)
}



#' @rdname gamix
#' @export
gamix.default <- function(object, ...)
{
  gamix.table( as.table(object), ... )
}
