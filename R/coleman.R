#' Coleman's homophily index
#'
#' Colemans's homphily index for directed networks.
#'
#' @param object R object, see Details for available methods
#' @param ... other arguments passed to/from methods
#'
#' @details Coleman's homophily index computes homophily scores for each group
#'   defined by a vertex attribute.
#'
#' @template mm-igraph-methods
#'
#' @return Vector of numeric values of the index for each group
#'
#' @references
#' Coleman, J. (1958) "Relational analysis: The study of social organizations
#' with survey methods", *Human Organization* 17:28--36.
#'
#' @export
#' @family segregation measures
#'
#' @examples
#' if(requireNamespace("igraph", quietly = TRUE)) {
#'   # Converting networks to directed
#'   coleman(igraph::as.directed(WhiteKinship, "mutual"), "gender")
#'   coleman(igraph::as.directed(EF3, "mutual"), "race")
#' }

coleman <- function(object, ...) UseMethod("coleman")







#' @rdname coleman
#' @param gsizes numeric vector of group sizes
#' @param loops logical, whether loops are allowed
#' @export
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



#' @rdname coleman
#' @param vattr character, vertex attribute
#' @export
coleman.igraph <- function(object, vattr, ...)
{
  stopifnot(is.directed(object))
  object <- mixingm(object, vattr, full=TRUE)
  coleman(object, ...)
}



#' @rdname coleman
#' @export
coleman.default <- function(object, ...)
{
  m <- as.table(object)
  coleman(m, ...)
}
