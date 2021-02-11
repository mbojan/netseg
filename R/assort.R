#' Assortativity Coefficient
#'
#' Assortativity coefficient is a measure of segregation for social networks
#' due to Newman & Girvan (2002).
#'
#' The measure evaluates the relative prevalence of within-group ties. It is
#' based on the contact layer of the mixing matrix.
#'
#' Assortativity coefficient is 1 if all ties are within-group.
#' The minimum can be negative, but not less than -1, and depends on the
#' relative number of ties of nodes in different groups. If the network
#' conforms to "proportionate mixing", the coefficient is 0.
#'
#' @template mm-igraph-methods
#'
#' @param object R object, see available methods
#' @param ... other arguments to/from other methods
#'
#' @return Numeric value of the index.
#'
#' @family segregation measures
#' @seealso Mixing matrices: [mixingm()]
#'
#' @references Newman, M. J. and Girvan, M. (2002) "Mixing patterns and
#'   community structure in networks", arXiv:cond-mat/0210146v1
#'
#'   Newman, M. J. (2003) "Mixing patterns in networks" arXiv:cond-mat/0209450v2
#'
#' @export
#' @examples
#' assort(WhiteKinship, "gender")
#' assort(EF3, "type")
#'
#' # Values of `assort()` for full networks of different sizes
#' if( requireNamespace("igraph", quietly = TRUE) ) {
#'   f <- function(n) {
#'     gfull <- igraph::make_full_graph(n, directed=FALSE)
#'     igraph::V(gfull)$type <- rep(1:2, length = igraph::vcount(gfull))
#'     assort(gfull, "type")
#'   }
#'   set.seed(1)
#'   x <- sort(sample(5:100, 25) * 2)
#'   y <- sapply(x, f)
#'   plot(x, y, type="o",
#'        xlab="Network size", ylab="Assortativity coefficient",
#'        main="Assortativity coef. for full networks of different sizes")
#' }

assort <- function(object, ...) UseMethod("assort")



#' @rdname assort
#' @export
assort.table <- function(object, ...)
{
  stopifnot( valid_mm(object) )
  if( length(dim(object)) == 3 )
  {
    m <- object[,,2]
  } else
  {
    m <- object
  }
  m <- symmetrize(m, "div")
  p <- m / sum(m)
  s <- sum(colSums(p) * rowSums(p))
  rval <- ( sum(diag(p)) - s ) / (1 - s)
  rval
}



#' @rdname assort
#' @param vattr character, name of the vertex attribute for which the measure is
#'   to be calculated
#'
#' @export
assort.igraph <- function(object, vattr, ...)
{
  # missing matrix
  object <- mixingm(object, vattr)
  assort.table(object, ...)
}




#' @rdname assort
#' @details For any other classes, `object` is coerced to a table and the table
#'   method is called.
#'
#' @export
assort.default <- function(object, ...)
{
  m <- as.table(object)
  assort.table(m, ...)
}
