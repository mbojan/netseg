#' @details If `object` is a table it is interpreted as a mixing matrix.
#'   Two-dimensional table is interpreted as a contact layer. Three-dimensional
#'   table is interpreted as a full mixing matrix \eqn{m_{ghy}}{m[ghy]}
#'   cross-classyfying all dyads, in which \eqn{g} and \eqn{h} correspond to
#'   group membership of ego and alter respectively. Layers \eqn{y=1} and
#'   \eqn{y=2} are assumed to be non-contact and contact layers respectively.
#'
#' @details If `object` is of class "igraph" it is required to supply `vattr`
#'   with the name of the vertex attribute to calculate intermediate mixing
#'   matrix.
#'
