#' Computing group sizes from square mixing matrices

#' \code{group_sizes} recomputes group sizes from a full mixing matrix. This is only
#' limited to square (single-attribute) mixing matrices.
#'
#' @param mm numeric array with \code{dim} of (k, k, 2) for some 'k'
#' @param directed logical, whether network is directed
#' @param loops logical, whether loops are allowed
#'
#' @return
#' A numeric vector of group sizes
#'
#' @export

group_sizes <- function(mm, directed=TRUE, loops=FALSE)
{
  # Works only for square full mixing matrices
  dims <- dim(mm)
  stopifnot( dims[1] == dims[2] )
  stopifnot( dims[3] == 2 )
  eamargin <- apply(mm, 1:2, sum)
  if(directed)
  {
    if(loops)
    {
      gsizes <- sqrt(diag(eamargin))
    } else
    {
      gsizes <- 0.5 * (1 + sqrt(1 + 4*diag(eamargin)))
    }
  } else
  {
    gsizes <- 0.5 * (1 + sqrt(1 + 8*diag(eamargin)))
  }
  gsizes
}
