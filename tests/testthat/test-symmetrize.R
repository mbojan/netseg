n <- 4
m <- outer(1:n, 1:n, function(x, y) x + y/10)


# upper -------------------------------------------------------------------

test_that("symmetrize(rule=upper) is silent", {
  expect_silent(
    r <<- symmetrize(m, rule = "upper")
  )
})

test_that("symmetrize(rule=upper) returns symmetric matrix", {
  expect_true(isSymmetric(unname(r)))
})

test_that("symmetrize(rule=upper) has proper upper triangle", {
  expect_identical(
    r[upper.tri(r)],
    m[upper.tri(m)]
  )
})

test_that("symmetrize(rule=upper) has proper lower triangle", {
  expect_identical(
    t(r)[upper.tri(r)],
    m[upper.tri(m)]
  )
})


# lower -------------------------------------------------------------------

test_that("symmetrize(rule=lower) is silent", {
  expect_silent(
    r <<- symmetrize(m, rule = "lower")
  )
})

test_that("symmetrize(rule=lower) returns symmetric matrix", {
  expect_true(isSymmetric(unname(r)))
})

test_that("symmetrize(rule=lower) has proper lower triangle", {
  expect_identical(
    r[lower.tri(r)],
    m[lower.tri(m)]
  )
})

test_that("symmetrize(rule=upper) has proper upper triangle", {
  expect_identical(
    t(r)[lower.tri(r)],
    m[lower.tri(m)]
  )
})
