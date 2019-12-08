context("Coleman homophily index")

test_that("Coleman index works for directed ring network",
          {
            library(igraph)
            s <- rep(1:6, each=2)
            g <- graph( s[ c(2:length(s), 1) ], directed=TRUE)
            V(g)$color <- rep( seq(1, vcount(g)/2), each=2)
            res <- coleman(g, vattr="color")
            eres <- structure(rep(0.375, 3), names=1:3)
            expect_equal( res, eres)
          } )

test_that("Coleman index gives correct results for his example data",
          {
            # coleman's matrix
            mat <- matrix(c(45, 20, 15, 20), 2, 2)
            # rebuild mixing matrix given group sizes
            mmat <- full_mm(mat, directed=TRUE, gsizes=c(60, 40))
            r <- coleman(mmat) # full mixing matrix
            r2 <- coleman(mat, gsizes=c(60, 40)) # contact layer + gsizes
            # NOTE In Colemans paper (1958) he uses approximation in computing
            # expected number of ties within group 'i'. The function is exact.
            # Original results were: boys=0.375 and girls=0.167
            expect_equivalent(r, c(0.38125, 0.175))
            expect_equivalent(r2, c(0.38125, 0.175))
          } )
