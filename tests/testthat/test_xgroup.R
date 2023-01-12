require(fsql)

test_that("group by one column works", {
  x <- data.frame(name=c("a", "b"), value=1:2, row.names=c("1", "2"))
  expect_length(x %>% xgroup("name"), 2)
})


test_that("no column group return a list with one element", {
  x <- data.frame(name=c("a", "b"), value=1:2, row.names=c("1", "2"))
  expect_length(x %>% xgroup(), 1)
})


test_that("xgroup on single-column data.frame works", {
  x <- data.frame(a = c(1,2,3))
  expect_true( is.data.frame(x %>% xgroup() %>% .[[1]]) )
})
