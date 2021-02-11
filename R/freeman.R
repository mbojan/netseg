#' Generalized Freeman's segregation index
#'
#' Calculate Freeman's segregation index for undirected netoworks with
#' arbitrary number of groups.
#'
#' Freeman's segregation index (Freeman, 1978) is designed to capture the
#' extent to which the defined groups of vertices tend to have more edges with
#' vertices from the same group than with other groups.  Formally, the index
#' compares the observed number of between-group ties with the number of
#' between-group ties that would be expected if ties would be created randomly.
#'
#' The index has some discontinuity as there are network and group configurations
#' that are characterized by the higher number of between-group ties that is
#' expected under a random graph. The index is truncates these situations and
#' takes a value of 0 (as originally described by Freeman (1978)).
#'
#' The original Freeman's formulation involves two groups of vertices. Here
#' it is extended to the arbitrary number of groups. The generalization
#' affects the way in which the expected number of between-group edges under pure
#' random graph is calculated, see Bojanowski & Corten (2014) for details.
#'
#' The function internally calculates the sizes of groups of vertices in the
#' supplied attribute `vattr`. However, it is possible to override this by
#' specifying "true" type distribution with the `gsizes` argument. It is assumed
#' to be a table (as returned by [table()]) or a numeric vector with the group
#' sizes. This may be especially usefull when dealing with large graphs and/or
#' with large number of isolates.
#'
#' @template mm-igraph-methods
#'
#' @param object R object, see Details for available methods
#' @param ... other arguments passed to/from other methods
#'
#' @return The value of the Freeman's index.
#'
#' @references Freeman, Linton C. (1978) Segregation in Social Networks,
#'   \emph{Sociological Methods & Research} \bold{6}(4):411--429
#'
#'   Bojanowski, Michał, and Rense Corten. 2014. "Measuring Segregation in
#'   Social Networks." *Social Networks* 39: 14–32.
#'   https://doi.org/10.1016/j.socnet.2014.04.001.
#'
#' @keywords graphs
#' @export
#' @family segregation measures
#'
#' @examples
#'
#' ## White's data from Freeman's article segregation level
#' freeman(WhiteKinship, "gender")
#'
#' # using 'more' argument
#' freeman(WhiteKinship, "gender")

freeman <- function(object, ...) UseMethod("freeman")


#' @details
#' Method for mixing matrices
#'
#' @param gsizes numeric, optional true distribution of types, see Details
#' @param loops logical, whether loops are allowed
#'
#' @method freeman table
#' @export
#' @rdname freeman
freeman.table <- function(object, gsizes=NULL, loops=FALSE, ...)
{
  dims <- dim(object)
  # need group sizes if only contact layer
  if( length(dims) == 2 )
  {
    stopifnot(!is.null(gsizes))
    mat <- object
  } else
  {
    stopifnot(length(dims)==3)
    gsizes <- group_sizes(object, directed=FALSE, loops=loops)
    mat <- object[,,2]
  }
  # number of cross-group ties
  cct <- sum(mat) - sum(diag(mat))
  # group distribution
  btab <- gsizes
  n <- sum(mat)
  m <- sum(btab)
  ecct <- (n * (sum(btab)^2 - sum(btab^2))) / (m * (m - 1))
  s <- ecct - cct
  if(s < 0)  s <- 0
  s / ecct
}





#' @details
#' Method for "igraph"s
#'
#' @param vattr character scalar or any vector of length equal to
#' `vcount(object)`, name of the vertex attribute in `object`
#' designating the groups or a vector with the attribute itself
#'
#' @method freeman igraph
#' @export
#' @rdname freeman
freeman.igraph <- function(object, vattr, gsizes=NULL, loops=any(is.loop(object)), ...)
{
    stopifnot(!is.directed(object))
    m <- mixingm(object, vattr, full=TRUE, loops=loops)
    freeman(m, ...)
}




#' @method freeman default
#' @export
#' @rdname freeman
freeman.default <- function(object, ...)
{
  freeman.table( as.table(object), ... )
}
