require(fsql)

x <- data.frame(name=c("a", "a", "b"), value= 1:3, row.names=c("1", "2", "3"))
y <- data.frame(name=c("b", "a", "a"), value=3:1, row.names=c("3", "2", "1"))
z <- data.frame(name=c("a", "a", "b"), value= c(2,1,3), row.names=c("2", "1", "3"))

test_that("sort by one column works", {
  expect_equal(x %>% orderby(-value), y)
})

test_that("sort by two column works", {
  expect_equal(x %>% orderby(name,-value), z)
})
