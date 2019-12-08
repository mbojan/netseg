#' Example data from Figure III of Echenique and Fryer (2006)
#'
#' Artificial example data from Echenique & Fryer (2006) Figure III representing
#' a city with black and white neighbourhoods.
#'
#' This data is taken from Echenique & Fryer (2006, figure III). The data
#' represent a fictional city composed of 30 neighborhoods that are either
#' black or white.
#'
#' @format Object of class "igraph". An undirected network with vertex
#' attributes
#' \itemize{
#' \item \code{type} (values 1 or 2).
#' \item \code{name}
#' }
#'
#' @source
#' Echenique, Federico and Roland G. Fryer, Jr. (2006) "A Measure of Segregation
#' Based On Social Interactions" Quarterly Journal of Economics CXXII(2):441-485
#' @keywords datasets
#' @examples
#' if( require(igraph, quietly = TRUE) ) {
#'   set.seed(1)
#'   plot(EFnet, layout=layout.fruchterman.reingold,
#'     vertex.color=V(EFnet)$type+1, vertex.label.family="",
#'     sub="Source: Echenique & Fryer (2006)",
#'     main="Neighborhood racial segregation\n in a fictional city"
#'   )
#' }
"EF3"
