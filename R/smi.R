#' Segregation Matrix Index
#'
#' Segregation Matrix Index due to Freshtman (1997). A measure of network
#' segregation.  Currently (and originally) supports only two groups.
#'
#' The Segregation Matrix Index (SMI) is calculated for every group separately.
#' It compares the density within group to the density of between group ties of
#' nodes belonging to that group.
#'
#' Non-normalized version is the ratio of the within-group density to the
#' between-group density, so vary between 0 and infinity. The normalized
#' version varies between 0 and 1.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return
#' Numeric vector of length equal to the number of groups in \code{g} according
#' to \code{vattr} with the values of SMI for the groups.
#'
#' @references
#' Freshtman, M. (1997) "Cohesive Group Segregation Detection in a Social
#' Network by the Segregation Matrix Index", Social Networks, 19:193--207
#'
#' @family segregation measures
#' @export
#'
#' @examples
#' # smi() needs a directed network
#' smi( as.directed(WhiteKinship, "mutual"), "gender")


smi <- function(object, ...) UseMethod("smi")


#' @details
#' Method for mixing matrices.
#'
#' @param normalize logical, whether normalized values should be returned,
#' defaults to \code{TRUE}
#'
#' @method smi table
#' @export
#' @rdname smi
smi.table <- function(object, normalize=TRUE, ...)
{
  # only directed networks
  stopifnot(length(dim(object)) == 3)
  if( dim(object)[1] != 2 )
      stop("currently 'smi' supports only two groups")
  pmm <- prop.table(object, c(1,2))
  r <- diag( pmm[,,2] ) / pmm[1,2,2]
  if( normalize )
      return( (r - 1) / (r + 1) )
  else return(r)
}



#' @details
#' Method for igraphs
#'
#' @param vattr character, name of the node attribute designating groups
#'
#' @method smi igraph
#' @export
#' @rdname smi
smi.igraph <- function(object, vattr, ...)
{
  stopifnot(is.directed(object))
  m <- mixingm(object, rattr=vattr, full=TRUE)
  smi(m, ...)
}






#' @method smi default
#' @export
#' @rdname smi
smi.default <- function(object, ...)
{
  smi.table(as.table(object), ...)
}
