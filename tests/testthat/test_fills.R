require(fsql)

test_that("idempotent when no NA", {
  expect_equal(1:2 %>% fills, 1:2)
})

test_that("fill forward over 2 NA works", {
  expect_equal(c(1, NA, NA, 2) %>% fills, c(1, 1, 1, 2))
})

test_that("fill forward works over dataframes", {
  d <- data.frame(x=c(1, NA, NA, 2), y = c("a", NA, NA, "d"))
  e <- data.frame(x=c(1, 1, 1, 2), y = c("a", "a", "a", "d"))
  expect_equal(d %>% fills, e)
})




