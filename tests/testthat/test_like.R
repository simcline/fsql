require(fsql)

test_that("1 is 1", {
  expect_equal(1 %like% 1, TRUE)
})

test_that("1 is 1", {
  expect_equal(c("ab", "cf") %like% "a", c(TRUE, FALSE))
})
