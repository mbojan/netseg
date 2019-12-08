context("Testing gamix")


test_that("Gamix gives correct result for Wnet data", {
          v <- gamix(Wnet, "gender")
          tv <- -0.1625
          expect_equal(v, tv)
} )
