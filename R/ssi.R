#' Spectral Segregation Index for Social Networks
#'
#' These functions implement Spectral Segregation Index as proposed by
#' Echenique & Fryer (2006). This index is a node-level measure of segregation
#' in a given network.
#'
#' For a full description and axiomatization see Echenique & Fryer (2006).
#'
#' The network \code{g} is converted to adjacency matrix and normalized so that
#' all rows sum-up to 1.
#'
#' The procedure essentially consists of creating a submatrix, say, \eqn{B} of
#' the adjacency matrix, say \eqn{A}. This submatrix \eqn{B} contains only
#' vertices of the given type. It may be viewed as a type-homogeneous
#' subnetwork of \code{A}. This subnetwork is further decomposed into connected
#' components.  Then, for every component, an eigenvalue decomposition is
#' applied.  The value of the index for the component is simply the largest
#' eigenvalue, and the individual-level indices are obtained by distributing it
#' according to the corresponding eigenvector.
#'
#' @param g object of class "igraph" representing a network
#' @param vattr character, name of the vertex attribute
#'
#' @return
#' Named vector of individual level values of SSI. Names correspond to vertex
#' ids in \code{g}.
#'
#' @references
#' Echenique, F., & Fryer Jr, R. G. (2007). A measure of segregation based on
#' social interactions. The Quarterly Journal of Economics, 122(2), 441-485.
#'
#' @family segregation measures
#' @importFrom igraph V 'V<-' E 'E<-'
#' @export
#' @examples
#' if(requireNamespace("igraph", quietly = TRUE)) {
#'
#' ### artificial EF data
#' x <- ssi(EF3, "race")
#' x
#'
#'
#' # show it on picture
#' a <- igraph::V(EF3)$race
#' # rescale SSI values to use as shades of gray
#' k <- 1 - scale(x, center=min(x), scale=max(x) - min(x))
#' plot( EF3, layout=igraph::layout.fruchterman.reingold,
#' vertex.color= gray(k),
#' vertex.label.family="",
#' vertex.shape=c("circle", "square")[a],
#' vertex.label.color=gray( (1-k) > .4 )
#' )
#'
#' ### For White's kinship data
#' x <- ssi(WhiteKinship, "gender")
#' x
#'
#' # plot it
#' a <- match(igraph::V(WhiteKinship)$gender, unique(igraph::V(WhiteKinship)$gender))
#' k <- 1 - scale(x, center=min(x), scale=max(x) - min(x))
#' set.seed(1234)
#' plot( WhiteKinship, layout=igraph::layout.fruchterman.reingold,
#' main="Node segregation in White's kinship data",
#' vertex.label.family="",
#' vertex.label=igraph::V(WhiteKinship)$name,
#' vertex.color= gray(k),
#' vertex.shape=c("circle", "csquare")[a],
#' vertex.label.color="black")
#' legend( "topleft", legend=c("Men", "Women"), pch=c(0,1), col=1)
#' }

ssi <- function(g, vattr)
{
    stopifnot( !igraph::is.directed(g), !igraph::any_multiple(g) )
    # edge weights are "directed"
    gg <- igraph::as.directed(g, mode="mutual")
    V(gg)$id <- V(g)
    # add edge weights, these are "directed"
    degs <- igraph::degree(gg, mode="out")
    for(i in seq(1, igraph::vcount(gg)))
      E(gg)[.from(i)]$weight <- 1/degs[i]
    # get vertex attribute
    a <- igraph::get.vertex.attribute(gg, vattr)
    l <- unlist(lapply(unique(a), function(val) ssib(g=gg, vattr=vattr, b=val)))
    l[ order(as.numeric(names(l))) ]
}

# computation for nodes vattr=b
ssib <- function(g, vattr, b)
{
    # take subgraph of b-nodes
    ids <- igraph::get.vertex.attribute(g, vattr)
    sub <- igraph::induced.subgraph(g, which(ids == b))
    # get components
    comps <- igraph::decompose.graph(sub)
    # compute eigen-decomposition
    e <- lapply(comps, function(k) eigen( igraph::get.adjacency(k, attr="weight")))
    # component SSIs (largest eigenvalue)
    cssi <- sapply(e, function(x) max(as.double(x$values)) )
    # eigenvectors of largest eigenvalue
    ev <- lapply(e, function(x) as.double(x$vectors[, which.max(as.double(x$values))] ))
    issi <- unlist( lapply( seq(1, length(e)),
        function(i)
        {
            rval <- cssi[i] * ev[[i]] / mean(ev[[i]])
            names(rval) <- V(comps[[i]])$id
            rval
        } ) )
    issi = issi[ order(as.numeric(names(issi))) ]
}


if(getRversion() >= "2.15.1") {
  utils::globalVariables(".from", package="netseg")
}
