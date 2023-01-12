require(fsql)

test_that("arranging one column puts it first", {
  x <- data.frame(name=c("a", "b"), value=1:2)
  expect_equal(colnames(x %>% xcols("value"))[1], "value")
})

test_that("arranging two columns idempotent when in order", {
  x <- data.frame(name=c("a", "b"), value=1:2)
  expect_equal(colnames(x %>% xcols("name"))[1], "name")
})

