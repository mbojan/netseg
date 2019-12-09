
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `netseg`: Measures of Network Segregation and Homophily <img src="man/figures/logo.png" align="right" width="20%" />

<!-- badges: start -->

<!-- badges: end -->

This is an R package implementing most important descriptive measures of
homophily and segregation in social networks. See Bojanowski & Corten
(2014) for a review.

## Installation

<!--
You can install the released version of netseg from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("netseg")
```
-->

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mbojan/netseg")
```

## Example

To illustrate, consider the network below which comes from Freeman
(1978) who took it from White (1975)

``` r
library(netseg)
library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union

data("Classroom")

plot(
  Classroom,
  layout = graphlayouts::layout_with_stress,
  vertex.color = c("pink", "lightskyblue")[match(V(Classroom)$gender, c("Girl", "Boy"))],
  vertex.label = NA,
  edge.arrow.size = 0.5
)
legend(
  "bottomright",
  pch = 21,
  legend = c("Boy", "Girl"),
  pt.cex = 2,
  pt.bg = c("lightskyblue", "pink"),
  col = "black",
  bty = "n"
)
```

<img src="man/figures/README-Classroom-1.svg" width="100%" />

The extent of segregation in this network can be assessed using one of
the indices provided in the package, for example:

``` r
# network-level
orwg(Classroom, "gender") # Odds ratio of within-group tie
#> [1] 16.58071
assort(Classroom, "gender") # Assortativity coefficient
#> [1] 0.8408885

# group-level
coleman(Classroom, "gender") # Coleman's index
#>       Boy      Girl 
#> 0.9084249 0.7909699
```

## References

Bojanowski, M., & Corten, R. (2014). Measuring segregation in social
networks. *Social Networks*, 39, 14-32.
<doi:10.1016/j.socnet.2014.04.001>

Freeman, L. C. (1978). Segregation in social networks. Sociological
Methods & Research, 6(4), 411-429. <doi:10.1177/004912417800600401>

White, D. R. (1975) “Communicative Avoidance in Social Networks”.
University of California, Irvine. (mimeo)
