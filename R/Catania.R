#' Pattern of sexual contacts in AMEN study
#'
#' Contact layer of the mixing matrix of men and women in US based on "Aids in
#' Multi-Ethnic  Neighborhoods" (AMEN). Based on Newman (2003).
#'
#'
#' @docType data
#'
#' @format
#' Four-by-four numeric matrix with dimnames.
#'
#' ```{r, echo=FALSE}
#' Catania
#' ```
#'
#' @source
#' Newman, M. (2003) "Mixing patterns in networks" Arxiv:cond-mat/0209450 v2
#'
#' @references
#' Catania et al. (1992) "The population-based AMEN (AIDS in Multi-Ethnic
#' Neighborhoods) study" *American Journal of Public Health* 82, 284-287
#'
#' Morris, M. (1995) "Data driven network models for the spread of infectious
#' disease". In D. Mollison (ed.) *Epidemic Models: Their Structure and
#' Relation to Data*, pp. 302-322, Cambridge University Press, Cambridge
#'
#' Newman, M. (2003) "Mixing patterns in networks" Arxiv:cond-mat/0209450 v2
#'
#' @examples
#' data(Catania)
#'
#' # assortativity
#' ep <- sum(Catania %*% Catania)
#' ( sum(diag(Catania)) - ep ) / ( 1 - ep )
"Catania"
