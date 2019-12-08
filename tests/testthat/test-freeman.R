context("Freeman segregation index")

test_that("Freeman index is 0 for full network", {
  g <- graph.full(10, directed=FALSE)
  V(g)$type <- rep(1:2, length=vcount(g))
  expect_that( freeman(g, "type"), equals(0))
} )

test_that("Freeman index is approx 0.26 for White's kinship data", {
  r <- freeman(WhiteKinship, "gender")
  expect_equal( round(r, 2), 0.26 )
} )
