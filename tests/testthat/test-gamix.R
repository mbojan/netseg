context("Testing gamix")


test_that("Gamix gives correct result for Wnet data", {
  expect_equal(
    gamix(WhiteKinship, "gender"),
    0.1801242,
    tolerance = 1e-7
  )
})
