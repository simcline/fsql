require(fsql)


test_that("pure grid operation works", {
  expect_equal(1:3 %.% 1:2, expand.grid(1:3,1:2))
})

test_that("multiple pure grid operation works", {
  expect_equal(as.matrix(1:2 %.% 1:2 %.% 1:2), as.matrix(expand.grid(1:2, 1:2, 1:2)))
})

