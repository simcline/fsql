require(fsql)

test_that("where works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(nrow(d %>% update(where = x==1)), 3)
})

test_that("computing column works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(any(grepl("z", colnames(d %>% update(z=x^2)))), TRUE)
})


ret1_def <- rnorm(10)
d <- data.frame(
  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
  ret1 = ret1_def,
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("basic update (single) works", {
  expect_equal(d %>% update(sq1=ret1^2) %>% colnames, c(colnames(d),"sq1") )
})

test_that("basic upd (multiple) works", {
  expect_equal(d %>% update(sq1 = ret1^2, sq2 = ret2^2) %>% colnames, c(colnames(d),"sq1", "sq2") )
})

test_that("by update works", {
  expect_equal(d %>% update(ret1 = mean(ret1), by = ticker) %>% nrow, 10)
})

test_that("alist update works", {
  expect_equal(d %>% update( alist(sq1=ret1^2, sq2=ret2^2)) %>% colnames ,c(colnames(d),"sq1", "sq2"))
})

test_that("mixed update works", {
  expect_equal(d %>% update(sq1 = ret1^2, alist(sq2=ret2^2) ) %>% colnames , c(colnames(d),"sq1","sq2"))
})

test_that("numerical variables outside d are masked", {
  ret1 <- 1:10
  expect_equal(d %>% update(sq1 = ret1^2) %>% .[1:10, "sq1"] , ret1_def^2)
})

test_that("string variables outside d are masked", {
  ret1 <- c("ret2", "ret3")
  expect_equal(d %>% update(sq1=ret1^2) %>% .[1:10,"sq1"] , ret1_def^2)
})

test_that("alist variables outside d are masked", {
  ret1 <- alist(ret2=ret2, ret3=ret3)
  expect_equal(d %>% update(sq1=ret1^2) %>% .[1:10,"sq1"] , ret1_def^2)
})

test_that("where condition with nested ',' works", {
  expect_equal(d %>% update(sq1 = ret1, where = c(date >= "2001-01-01", ticker %in% c("1 HK", "2 HK"))) %>% .$sq1, ret1_def)
})

test_that("update inside a function works", {

  f <- function(x,y) x %>% update(ret4 = y)

  y <- 0
  expect_equal(d %>% f(1) %>% .[1, "ret4"], 1)
})

