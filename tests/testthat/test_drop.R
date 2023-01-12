require(fsql)

test_that("remove from scalar is empty", {
  expect_length(1 %_% 1, 0)
})

test_that("remove first", {
  expect_equal(1 %_% 1:10, 2:10)
})

test_that("remove all but one", {
  expect_equal((-1) %_% 1:10, 10)
})

test_that("remove first two", {
  expect_equal(2 %_% 1:10, 3:10)
})
