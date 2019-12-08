context("Testing ORWG")

test_that("ORWG is correctly 1 for a trivially integrated mixing matrix", {
          m <- matrix(c(1,2,2,1), 2, 2)
          mm <- full_mm(m, gsizes=c(2, 2))
          expect_equal( orwg(mm), 1 )
} )



test_that("ORWG is correctly 1 for a trivially integrated igraph", {
          g <- graph(c(1,2, 1,3, 2,4, 3,4, 3,1, 4,2), directed=TRUE)
          V(g)$a <- c(1,1, 2,2)
          expect_equal(orwg(g, "a"), 1)
} )
