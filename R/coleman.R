#' Coleman's homophily index
#'
#' Colemans's homphily index for directed networks.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from methods
#'
#' @details
#' Coleman's homophily index computes homophily scores for each group
#' defined by a vertex attribute.
#'
#' @return Vector of numeric values of the index for each group
#'
#' @references
#' Coleman, J. (1958) "Relational analysis: The study of social organizations
#' with survey methods", Human Organization 17:28--36.
#'
#' @export
#' @family segregation measures
#'
#' @examples
#' if( require(igraph, quietly = TRUE)) {
#' # Converting network to directed
#' coleman(as.directed(WhiteKinship, "mutual"), "gender")
#' coleman(as.directed(EF3, "mutual"), "race")
#' }

coleman <- function(object, ...) UseMethod("coleman")







#' @details
#' If \code{object} is a table it is interpreted as a mixing matrix. If it is
#' only the contact layer (2-dimensional), then vector of group sizes need to
#' be supplied via \code{gsizes}.
#'
#' @param gsizes numeric vector of group sizes
#'
#' @param loops logical, whether loops are allowed
#'
#' @method coleman table
#' @export
#' @rdname coleman
coleman.table <- function(object, gsizes=NULL, loops=FALSE, ...)
{
  dims <- dim(object)
  # only contact layer
  if( length(dims) == 2 )
  {
    stopifnot( !is.null(gsizes) )
    m <- object
  } else
  {
    stopifnot( length(dims) == 3 )
    gsizes <- group_sizes(object, directed=TRUE, loops=loops)
    m <- object[,,2]
  }
  # take group sizes
  degsums <- rowSums(m)
  # expected number of within-group ties for each group
  ewg <- degsums * (gsizes - 1) / (sum(gsizes) - 1)
  # rval
  r <- (diag(m) - ewg) / (degsums - ewg)
  i <- diag(m) <= ewg
  repl <- (diag(m) - ewg) / ewg
  r[i] <- repl[i]
  structure(as.numeric(r), names=names(gsizes))
}



#' @details
#' \code{object} can be of class "igraph"
#' @param vattr character, vertex attribute
#' @method coleman igraph
#' @export
#' @rdname coleman
coleman.igraph <- function(object, vattr, ...)
{
  stopifnot(is.directed(object))
  object <- mixingm(object, vattr, full=TRUE)
  coleman(object, ...)
}



#' @details
#' Default method tries to coerce \code{object} to table and use other methods.
#' @method coleman default
#' @export
#' @rdname coleman
coleman.default <- function(object, ...)
{
  m <- as.table(object)
  coleman(m, ...)
}
