context("Sanity checks for 'full_mm'")


test_that("Given 3d mixing matrix the function returns the same", {
  mm <- array( 1:8, dim=c(3,3,2))
  expect_identical( mm, full_mm(mm) )
} )

test_that("Malformed 3d array triggers error", {
  mm <- array(0, dim=c(3,3,3))
  expect_error( full_mm(mm) )
} )





context("Analyzing a single undirected dyad")

test_that("There is one tie possible in a undirected same-group dyad ", {
  mm <- matrix(c(1, 0, 0, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(2,0), directed=FALSE, loops=FALSE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( eamargin[1,1] , 1 )
  expect_equivalent( fmm[1,1,1], 0 )
} )

test_that("There is one tie possible in a undirected different-group dyad ", {
  mm <- matrix(c(0, 0, 1, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(1,1), directed=FALSE, loops=FALSE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( eamargin[1,2] , 1 )
  expect_equivalent( fmm[1,2,1], 0 )
} )

test_that("In a undirected different-group dyad with loops there are two loops possible", {
  mm <- matrix(c(1, 0, 1, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(1,1), directed=FALSE, loops=TRUE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( diag(eamargin), c(1,1) )
} )




context("Analyzing a single directed dyad")

test_that("There are two ties possible in a directed same-group dyad ", {
  mm <- matrix(c(1, 0, 0, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(2,0), directed=TRUE, loops=FALSE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( eamargin[1,1] , 2 )
  expect_equivalent( fmm[1,1,1], 1 )
} )


test_that("There are two ties possible in a directed different-group dyad ", {
  mm <- matrix(c(0, 0, 1, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(1,1), directed=TRUE, loops=FALSE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( eamargin[1,2] , 1 )
  expect_equivalent( eamargin[2,1] , 1 )
  expect_equivalent( fmm[1,2,1], 0 )
  expect_equivalent( fmm[2,1,1], 1 )
} )

test_that("In a directed different-group dyad with loops", {
  mm <- matrix(c(1, 0, 1, 0), 2, 2)
  fmm <- full_mm( mm, gsizes=c(1,1), directed=TRUE, loops=TRUE)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( diag(eamargin), c(1,1) )
} )





# Example directed network
library(igraph)
g <- graph(c(1,2, 1,3, 1,4, 4,5, 5,6, 6,1))
V(g)$a <- rep(letters[1:2], each=3)
V(g)$b <- rep(LETTERS[1:3], each=2)
el <- get.edgelist(g)
el2 <- as.data.frame(apply(el, 2, function(k) V(g)$a[k] ), stringsAsFactors=FALSE)
el2$V3 <- V(g)$b[ el[,2] ]
mm <- table( el2[,1], el2[,2] )
mm2 <- table(el2[,1], el2[,3])
gsa <- table(V(g)$a)
V(g)$label <- paste( V(g)$a, V(g)$b, sep="|")
rm(el, el2)

# Example undirected network
ug <- simplify(as.undirected(g))
el <- get.edgelist(g)
el2 <- as.data.frame(apply(el, 2, function(k) V(ug)$a[k] ), stringsAsFactors=FALSE)
el2$V3 <- V(ug)$b[ el[,2] ]
umm <- fold(table( el2[,1], el2[,2] ))
umm2 <- table(el2[,1], el2[,3])
rm(el, el2)

if(FALSE)
{
  plot( g, vertex.shape=ifelse( V(g)$a == "a", "circle", "square"), vertex.color=kol[ V(g)$b ] )
  plot( ug, vertex.shape=ifelse( V(ug)$a == "a", "circle", "square"), vertex.color=kol[ V(ug)$b ] )
}










context("Reconstructing non-contact layer for square mixing matrices")

test_that("NC layer is properly reconstructed for test directed network", {
  fmm <- full_mm( mm, table(V(g)$a), loops=FALSE)
  tab <- table(V(g)$a)
  expect_equivalent( fmm[1,2,1], tab[1] * tab[2] - mm[1,2])
  expect_equivalent( fmm[2,1,1], tab[2] * tab[1] - mm[2,1])
  expect_equivalent( fmm[1,1,1], tab[1] * (tab[1] - 1) - mm[1,1])
  expect_equivalent( fmm[2,2,1], tab[2] * (tab[2] - 1) - mm[2,2])
} )

test_that("NC layer is properly reconstructed for test undirected network", {
  fmm <- full_mm( umm, table(V(ug)$a), loops=FALSE, directed=FALSE)
  tab <- table(V(ug)$a)
  eamargin <- apply(fmm, 1:2, sum)
  expect_equivalent( eamargin[1,1], (tab[1]^2 - tab[1])/2 )
  expect_equivalent( eamargin[1,2], tab[1] * tab[2] )
  expect_equivalent( eamargin[2,1], 0 )
  expect_equivalent( eamargin[2,2], (tab[2]^2 - tab[2])/2 )
  expect_equivalent( as.table(fmm[,,1]), eamargin - umm)
} )



test_that("Mixing matrix has proper number of all possible ties for directed network", {
  fmm <- full_mm( mm, table(V(g)$a) )
  vc <- vcount(g)
  expect_equal( sum(fmm), vc^2 - vc)
} )

test_that("Mixing matrix has proper number of all possible ties for undirected network", {
  fmm <- full_mm( umm, table(V(g)$a), directed=FALSE )
  vc <- vcount(g)
  expect_equal( sum(fmm), (vc^2 - vc)/2 )
} )









context("Reconstructing full mixing matrices give results with proper dims")

test_that("Example code gives proper dims", {
  ### Square example
  # Contact layer of the mixing matrix
  mm1 <- matrix( c( 20, 10, 5,
                    12, 30, 10,
                    3, 11, 25 ),
                 byrow=TRUE, ncol=3, nrow=3)
  dimnames(mm1) <- list(ego=letters[1:3], alter=letters[1:3])
  gs1 <- c(a=9, b=12, c=10)
  ### Non-square example
  # Mixing matrix
  # Now using different attributes for ego and alter
  mm2 <- cbind(mm1, c(20, 10, 5))
  colnames(mm2) <- LETTERS[1:4]
  names(dimnames(mm2)) <- c("ego", "alter")
  # Create artificial distribution of attributes
  set.seed(123)
  a1 <- sample(letters[1:3], sum(gs1), replace=TRUE, prob=gs1/sum(gs1))
  table(a1)
  a2 <- sample(LETTERS[1:4], sum(gs1), replace=TRUE)
  x <- table(a1, a2)         # Cross-tablulation
  # Full mixing matrix
  r <- full_mm( mm2, gsizes=x)
  expect_equal( dim(r), c(3, 4, 2) )
} )
















context("Full mixing matrices: group sizes as vector or matrix")

test_that("It does not matter for square mixing matrix whether group sizes is a vector or diagonal matrix (directed)", {
  fmm <- full_mm( mm, table(V(g)$a), loops=FALSE, directed=TRUE)
  fmm2 <- full_mm( mm, table(V(g)$a, V(g)$a), loops=FALSE, directed=TRUE)
  expect_equivalent(fmm, fmm2)
} )

test_that("It does not matter for square mixing matrix whether group sizes is a vector or diagonal matrix (undirected)", {
  fmm <- full_mm( umm, table(V(g)$a), loops=FALSE, directed=FALSE)
  fmm2 <- full_mm( umm, table(V(g)$a, V(g)$a), loops=FALSE, directed=FALSE)
  expect_equivalent(fmm, fmm2)
} )






context("Other mixingm() tests")

test_that("computing full mixing matrix for a multigraph fails", {
  g <- make_graph(~ A -- B -- C -- D, B -- C, simplify = FALSE)
  V(g)$group <- c(1,1,2,2)
  expect_error(
    mixingm(g, "group", full=TRUE)
  )
  expect_silent(
    mixingm(g, "group", full=FALSE)
  )
})









context("Testing mixingdf()")

test_that("mixingdf(directed, 2groups)", {
  expect_silent(
    r <- mixingdf(g, "a", full=FALSE)
  )
  expect_equivalent(r$n, c(2,1,1,2))
  expect_silent(
    r <- mixingdf(g, "a", full=TRUE)
  )
  expect_equivalent(r$n, c(4,8,8,4,2,1,1,2))
})

test_that("mixingdf(directed, 3groups)", {
  expect_silent(
    r <- mixingdf(g, "b", full=FALSE)
  )
  expect_equivalent(r$n, c(1,1,2,1,1))
  expect_silent(
    r <- mixingdf(g, "b", full=TRUE)
  )
  expect_equivalent(r$n, c(1,4,3,2,2,4,4,3,1,1,1,2,1,1))
})




test_that("mixingdf(undirected, 2groups)", {
  expect_silent(
    r <- mixingdf(ug, "a", full=FALSE)
  )
  expect_equivalent(r$n, c(2,2,2))
  expect_silent(
    r <- mixingdf(ug, "a", full=TRUE)
  )
  expect_equivalent(r$n, c(1,7,1,2,2,2))
})

test_that("mixingdf(undirected, 3groups)", {
  expect_silent(
    r <- mixingdf(ug, "b", full=FALSE)
  )
  expect_equivalent(r$n, c(1,2,1,1,1))
  expect_silent(
    r <- mixingdf(ug, "b", full=TRUE)
  )
  expect_equivalent(r$n, c(2,1,3,3,1,2,1,1,1))
})
