context("Testing SMI")

test_that("SMI works", {
  data("WhiteKinship")
  expect_silent(
    smi(as_directed(WhiteKinship), "gender")
  )
} )
