require(fsql)

test_that("scalar identity", {
  expect_equal(1 %#% 1, 1)
})

test_that("first element", {
  expect_equal(1 %#% 1:10, 1)
})

test_that("last element", {
  expect_equal((-1) %#% 1:10, 1:9)
})

test_that("first two", {
  expect_equal(2 %#% 1:10, 1:2)
})

