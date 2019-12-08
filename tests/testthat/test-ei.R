context("Testing E-I index")


test_that("E-I index returns correct value for Catania data", {
          p <- Catania / sum(Catania)
          int <- sum(diag(p))
          ext <- 1 - int
          tv <- ext - int
          v <- ei(Catania)
          expect_equal(v, tv)
} )
