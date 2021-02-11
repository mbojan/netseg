#' Odds ratio of existence of within-group ties
#'
#' Odds ratio for connected, as opposed to disconnected, dyads depending
#' whether it is between- or within-group, i.e. how much more likely the dyad
#' will be connected if it is within-group.
#'
#' The measure takes values, like all odds ratios, from (0; Inf).
#'
#' @param object R object, see Details for available methods
#' @param ... other arguments passed to/from other methods
#'
#' @template mm-igraph-methods
#'
#' @return Numeric value of the measure.
#'
#' @references
#' Moody, Jim (2001) "Race, school integration, and friendship segregation in
#' America", American Journal of Sociology, 107(3):679--377
#'
#' @export
#' @family segregation measures
#'
#' @examples
#' orwg(WhiteKinship, "gender")

orwg <- function(object, ...) UseMethod("orwg")


#' @rdname orwg
#' @export
orwg.table <- function(object, ...)
{
  # needs full mixing matrix
  stopifnot(length(dim(object)) == 3)
  # compute number of b-g and w-g dyads in both layers
  z <- apply(object, 3, function(x)
             c(between= sum(x) - sum(diag(x)),
               within=sum(diag(x)))
  )
  offdiag <- z[ col(z) != row(z) ]
  prod(diag(z)) / prod(offdiag)
}

#' @rdname orwg
#' @param vattr character scalar or any vector, name of the vertex attribute or
#' the attribute itself (as a vector)
#' @export
orwg.igraph <- function(object, vattr, ...)
{
  m <- mixingm(object, rattr=vattr, full=TRUE)
  orwg(m, ...)
}




#' @rdname orwg
#' @export
orwg.default <- function(object, ...)
{
  orwg.table( as.table(object), ... )
}
