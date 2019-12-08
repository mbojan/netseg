#' Gupta-Anderson-May measure of within-group mixing
#'
#' Measure of within-group mixing in networks proposed in Gupta, Anderson and
#' May (1989).
#'
#' The measure varies between \code{-1/vcount(g)} for dissassortative mixing
#' and 1 for perfect within-group mixing. It takes a value of 0 for
#' proportionate mixing.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other objects passed to/from other methods
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




#' @details
#' Method for mixing matrices
#'
#' @param debug logical, return some intermediate results as attributes to the
#' returned value
#'
#' @method gamix table
#' @export
#' @rdname gamix
gamix.table <- function(object, debug=FALSE, ...)
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
  rval <- (sum(w) - 1)/(dim(m)[1] - 1)
  if(debug)
    structure(rval, mix=m, mixp=p, e=w)
  else
    rval
}



#' @details
#' Method for igraphs
#'
#' @param vattr character, name of vertex attribute
#'
#' @method gamix igraph
#' @export
#' @rdname gamix
gamix.igraph <- function (object, vattr, ...)
{
  m <- mixingm(object, rattr=vattr)
  gamix(m, ...)
}



#' @method gamix default
#' @export
#' @rdname gamix
gamix.default <- function(object, ...)
{
  gamix.table( as.table(object), ... )
}
