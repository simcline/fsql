require(fsql)

test_that("renaming first column works", {
  x <- data.frame(name=c("a", "b"), value=1:2)
  expect_equal(colnames(x %>% xcol("newname"))[1], "newname")
})

test_that("renaming all columns works", {
  x <- data.frame(name=c("a", "b"), value=1:2)
  expect_equal(colnames(x %>% xcol(c("c", "d"))), c("c", "d"))
})
