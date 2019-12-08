context("Testing SSI()")

test_that("values of ssi for EF3 data correspond to values from the paper", {
  s <- ssi(EF3, "race")
  names(s) <- V(EF3)$name
  # results from E&F paper
  paper <- c(b1=87, b2=62, c1=126, c2=92, d1=93, d3=29, d4=10, c5=25, b5=25) / 100
  names(paper) <- toupper(names(paper))
  i <- match(names(paper), names(s))
  expect_equal( round(s[i], 2), paper )
} )
