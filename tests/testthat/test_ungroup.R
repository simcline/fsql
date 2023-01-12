require(fsql)

test_that("xgroup then ungroup is idempotent", {
  x <- data.frame(name=c("a", "b"), value=1:2, row.names = c("1", "2"))
  y <- x %>% xgroup("name") %>% ungroup
  rownames(y) <- c("1", "2")
  expect_equal(x, y)
})
