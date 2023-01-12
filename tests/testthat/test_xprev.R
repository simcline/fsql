require(fsql)

test_that("lagging 0 is idempotent", {
  x <- 1:3
  expect_equal(xprev(x, 0), x)
})

test_that("lag 1 is working", {
  x <- 1:3
  expect_equal(xprev(x, 1), c(NA, 1, 2))
})

test_that("lag -1 is working", {
  x <- 1:3
  expect_equal(xprev(x, -1), c(2, 3, NA))
})

test_that("lag too far in past is working", {
  x <- 1:3
  expect_equal(xprev(x, 3), as.integer(c(NA, NA, NA)))
})

test_that("lag too far in future is working", {
  x <- 1:3
  expect_equal(xprev(x, -3), as.integer(c(NA, NA, NA)))
})


