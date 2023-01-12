require(fsql)

test_that("one key value", {
  x <- 1
  names(x) <- "1"
  expect_equal(dict(1, 1), x)
  expect_equal(1%!%1, x)
})

test_that("random dict", {
  x <- dict(runif(5), runif(5))
  expect_length(x, 5)
  expect_length(runif(5)%!%runif(5), 5)
})


