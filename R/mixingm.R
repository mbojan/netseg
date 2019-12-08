#' Network mixing matrix
#'
#' Creating network mixing matrices.
#'
#'
#' Network mixing matrix is, traditionally, a two-dimensional
#' cross-classification of edges depending on the values of a specified vertex
#' attribute for tie sender and tie receiver. It is an important tool
#' for assessing network homophily or segregation.
#'
#' Let \eqn{G} be the number of distinct values of the vertex attribute in
#' question.  We may say that we have \eqn{G} mutually exclusive groups in the
#' network.  The mixing matrix is a \eqn{G \times G}{GxG} matrix such that
#' \eqn{m_{ij}}{m[ij]} is the number of ties send by vertices in group \eqn{i}
#' to vertices in group \eqn{j}. The diagonal of that matrix is of special
#' interest as, say, \eqn{m_{ii}}{m[ii]} is the number of ties \emph{within}
#' group \eqn{i}.
#'
#' A full mixing matrix is a three-dimensional array that cross-classifies
#' \emph{all} network \emph{dyads} depending on:
#' \enumerate{
#' \item{the value of the vertex attribute for tie sender}
#' \item{the value of the vertex attribute for tie receiver}
#' \item{the status of the dyad, i.e. whether it is connected or not}
#' }
#' The two-dimensional version is a so-called "contact layer"
#' of the three-dimensional version.
#'
#' @param object R object, see Details for available methods
#'
#' @param ... other arguments passed to/from other methods
#'
#' @return
#' Depending on \code{full} argument a two- or three-dimensional array
#' crossclassifying connected or all dyads in \code{object}.
#'
#' For undirected network and if \code{foldit} is TRUE (default), the matrix is
#' folded onto the upper triangle (entries in lower triangle are 0).
#'
#' @export
#' @examples
#' if(require(igraph, quietly = TRUE)) {
#' # some directed network
#' net <- graph(c(1,2, 1,3, 2,3,  4,5,  1,4, 1,5, 4,2, 5,3))
#' V(net)$type <- c(1,1,1, 2,2)
#' mixingm(net, "type")
#' mixingm(net, "type", full=TRUE)
#' # as undirected
#' mixingm( as.undirected(net), "type")
#' mixingm(net, "type")
#' mixingm(net, "type", full=TRUE)
#' }
mixingm <- function(object, ...) UseMethod("mixingm")






#' @details
#' If \code{object} is of class "igraph," mixing matrix is created for the
#' network in \code{object} based on vertex attributes supplied in arguments
#' \code{rattr} and optionally \code{cattr}.
#'
#' If only \code{rattr} is specified (or, equivalently, \code{rattr} and
#' \code{cattr} are identical), the result will be a mixing matrix \eqn{G
#' \times G} if \code{full} is \code{FALSE} or \eqn{G \times G \times 2}{GxGx2}
#' if \code{full} is \code{TRUE}. Where \eqn{G} is the number of categories of
#' vertex attribute specified by \code{rattr}.
#'
#' If \code{rattr} and \code{cattr} can be used to specify different vertex
#' attributes for tie sender and tie receiver.
#'
#' @param rattr name of the vertex attribute or an attribute itself as a
#' vector. If \code{cattr} is not NULL, \code{rattr} is used for rows of the
#' resulting mixing matrix.
#'
#' @param cattr name of the vertex attribute or an attribute itself as a
#' vector. If supplied, used for columns in the mixing matrix.
#'
#' @param full logical, whether two- or three-dimensional mixing matrix
#' should be returned.
#'
#' @param directed logical, whether the network is directed. By default,
#' directedness of the network is determined with
#' \code{\link[igraph]{is.directed}}.
#'
#' @param loops logical, whether loops are allowed. By default it is TRUE
#' whenever there is at least one loop in \code{object}.
#'
#' @method mixingm igraph
#' @export
#' @rdname mixingm
mixingm.igraph <- function(object, rattr, cattr=rattr, full=FALSE,
                            directed = is.directed(object),
                            loops=any(is.loop(object)), ...)
{
  # get attributes
  if( is.character(rattr) && length(rattr)==1 )
  {
    ra <- igraph::get.vertex.attribute(object, rattr)
  } else
  {
    stopifnot( length(rattr) == vcount(object))
    ra <- rattr
  }
  if( is.character(cattr) && length(cattr)==1 )
  {
    ca <- igraph::get.vertex.attribute(object, cattr)
  } else
  {
    stopifnot( length(cattr) == vcount(object))
    ca <- cattr
  }
  # compute contact layer based on edge list
  el <- igraph::get.edgelist(object, names=FALSE)
  ego <- factor( ra[ el[,1] ], levels=sort(unique(ra)))
  alter <- factor(ca[ el[,2] ], levels=sort(unique(ca)))
  con <- table(ego=ego, alter=alter)
  if(!directed) con <- fold(con, "upper")
  if(full)
  {
    return(full_mm(con, gsizes=table(ra, ca), directed=directed, loops=loops))
  } else
  {
    return(con)
  }
}


# Does it look like a valid mm
valid_mm <- function(m, square=TRUE, verbose=FALSE)
  # square = should m be square
{
  rval <- NULL
  if( !is.array(m) )
  {
    rval <- "'m' is not an array"
    stop(rval)
  }
  dims <- dim(m)
  if( !(length(dims) %in% 2:3) )
    rval <- c(rval, paste0("'m' should have 2 or 3 dimensions, has ", length(dims)))
  if( length(dims) == 3 && dims[3] != 2 )
    rval <- c(rval, paste0("third dimension should have two values, has ", dims[3]))
  if(square)
  {
    if( dims[1] != dims[2] )
      rval <- c(rval, paste0("dimensions 1 and 2 should be equal, are ", dims[1],
                             " and ", dims[2]) )
  }
  if(is.null(rval)) return(TRUE)
  if(verbose) return(rval)
  else return(FALSE)
}









# Create a 3-dimensional mixingm matrix
#
# Given contact layer of the mixing matrix compute a full 3-dimensional mixing
# matrix by rebuilding the non-contact layer from supplied group sizes and
# other arguments.
#
# @param cl numeric matrix with contact layer of the mixing matrix
# @param gsizes numeric vector or matrix with group sizes, see Details.
# @param directed logical, whether the network is directed
# @param loops logical, whether loops (self-ties) are allowed
#
# Contact layer of the mixing matrix is a cross-classification of ties
# according to the attributes of tie sender (ego) and tie receiver (alter).
# Classically, the same attribute is used for ego and alter resulting in a
# square mixing matrix. In such cases \code{cl} shoud be square with \code{cl[i,j]} being
# the number of ties from actors in group \code{i} to actors in group \code{j}
# and \code{gsizes} should be a numeric vector with number of nodes in each group.
# Consequently, it must hold that \code{length(gsizes) == nrow(cl) == ncol(cl)}.
#
# In more general case we can use different node attributes for ego and
# alter. Then \code{cl} does not have to be square. In such cases
# \code{gsizes} should be a cross-tabulation of nodes according to
# their values on both attributes. See Examples.
#
# @return
# \code{full_mm} returns a full three-dimenstional mixing matrix as an array
# with \code{dim} attribute equal to \code{c( nrow(cl), ncol(cl), 2 )}.
#
# @export
#
# @example examples/full_mm.R

full_mm <- function(cl, gsizes, directed=TRUE, loops=FALSE)
{
  # if 3d and dims ok, return cl
  if( length(dim(cl)) == 3 )
  {
    stopifnot(dim(cl)[3] == 2)
    return(cl)
  }
  ## Compute ego-alter margin
  gsizes <- as.table(gsizes)
  ndims <- length(dim(gsizes))
  stopifnot( ndims %in% 1:2 )
  if( ndims == 1)
  {
    gs <- gsizes
  } else
  {
    dtab <- as.data.frame(as.table(gsizes))
    gs <- dtab$Freq
  }
  o <- outer(gs, gs, "*")
  # Adjust 'o' according to 'loops' and 'directed'
  if(directed)
  {
    mar <- o
    if(!loops)
    {
      diag(mar) <- diag(o) - gs
    }
  } else {
    mar <- o
    mar[ lower.tri(mar) ] <- 0
    if(loops)
    {
      diag(mar) <- (diag(o) + gs) / 2
    } else {
      diag(mar) <- (diag(o) - gs) / 2
    }
  }
  # collapse if different groups
  if( ndims == 2 )
  {
    a1 <- apply(mar, 1, function(r) tapply(r, dtab[,2], sum))
    mar <- apply(a1, 1, function(k) tapply(k, dtab[,1], sum))
  }
  rval <- array(NA, dim=c( dim(cl), 2 ))
  rval[,,1] <- mar - cl
  rval[,,2] <- cl
  # set dimension names
  if(is.null(dimnames(cl)))
  {
    dimnames(rval) <- list(NULL, NULL, tie=c(FALSE, TRUE))
  } else
  {
    dimnames(rval) <- c( dimnames(cl)[1:2], list(tie=c(FALSE, TRUE)))
  }
  rval
}
