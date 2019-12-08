context("Assortativity coefficient")

test_that("AC for Catania (2d mm) data is equal to the value from the paper", {
  v <- assort(Catania)
  tv <- 0.621 # value from Newman's paper
  tol <- 0.004
  expect_true( v >= tv - tol & v <= tv + tol)
} )

test_that("AC for White data (igraph) gives correct result", {
          r <- assort(WhiteKinship, "gender")
          expect_equal( round(r, 6), 0.180124 )
} )

test_that("AC for full mixing matrix gives correct results", {
          mm <- full_mm(Catania, c(500, 300, 200, 100), directed=TRUE)
          v <- assort(mm)
          tv <- 0.621
          tol <- 0.004
          expect_true( v >= tv - tol & v <= tv + tol)
} )
