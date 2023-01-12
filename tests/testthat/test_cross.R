require(fsql)

test_that("cartesian product cardinal respected", {
  d <- cross(1:2, c("a", "b", "c"))
  expect_length(d, 6)
})
