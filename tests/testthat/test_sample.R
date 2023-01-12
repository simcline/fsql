require(fsql)

test_that("length kept", {
  n <- 10
  x <- n %?% 1:n
  expect_length(x, n)
  expect_equal(all(x %in% 1:n), TRUE)
})
