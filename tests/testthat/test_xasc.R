require(fsql)

test_that("sort by one column works", {
  x <- data.frame(name=c("a", "b"), value=2:1, row.names=c("1", "2"))
  y <- data.frame(name=c("b", "a"), value=1:2, row.names=c("2", "1"))
  expect_equal(x %>% xasc("value"), y)
})
