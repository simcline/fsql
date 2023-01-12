require(fsql)

test_that("idempotent when no NA", {
  expect_equal(0 %^% 1:2, 1:2)
})

test_that("replace all NA", {
  expect_equal(0 %^% rep(NA, 2), rep(0, 2))
})

test_that("replace NA at back", {
  expect_equal(0 %^% c(1, NA), c(1, 0))
})

test_that("replace random NA", {
  x <- 10 %?% c(NA, 1)
  y <- x
  x[is.na(x)] <- 0
  expect_equal(0 %^% y, x)
})



