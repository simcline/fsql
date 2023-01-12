require(fsql)

test_that("keying works", {
  x <- data.frame(name=c("a", "b"), value=2:1, row.names=c("1", "2"))
  y <- x %>% xkey("a")
  expect_equal(attributes(y)$key, "a")
})
