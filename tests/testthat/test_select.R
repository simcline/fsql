require(fsql)

test_that("where works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(nrow(d %>% select(where= x==1)), 1)
})

test_that("computing column works", {
  d <- data.frame(x=1:3, y=c("a", "b", "c"))
  expect_equal(grepl("z", colnames(d %>% select(z=x^2))), TRUE)
})

ret1_def <- rnorm(10)
d <- data.frame(
  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
  ret1 = ret1_def,
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("basic selection (single) works", {
  expect_equal(d %>% select(ret1) %>% colnames, "ret1")
})

test_that("basic selection with name change (single) works", {
  expect_equal(d %>% select(ret10 = ret1) %>% colnames, "ret10")
})

test_that("basic selection (multiple) works", {
  expect_equal(d %>% select(ret1,ret2) %>% colnames, c("ret1", "ret2"))
})

test_that("negative selection works", {
  expect_equal(d %>% select(-ret1) %>% colnames, c("date","ticker","ret2","ret3"))
})

test_that("character vector selection works", {
  expect_equal(d %>% select(c("ret2", "ret3")) %>% colnames, c("ret2","ret3"))
})

test_that("by selection works", {
  expect_equal(d %>% select(ret1 = mean(ret1), by = ticker) %>% nrow, 2)
})

test_that("alist selection works", {
  expect_equal(d %>% select( alist(ret1=ret1, ret2=ret2)) %>% colnames , c("ret1","ret2"))
})

test_that("mixed selection works", {
  expect_equal(d %>% select(ret1, alist(ret2=ret2), "ret3" ) %>% colnames , c("ret1","ret2", "ret3"))
})

test_that("numerical variables outside d are masked", {
  ret1 <- 1:10
  expect_equal(d %>% select(ret1) %>% .[1:10,] , ret1_def)
})

test_that("string variables outside d are masked", {
  ret1 <- c("ret2", "ret3")
  expect_equal(d %>% select(ret1) %>% .[1:10,] , ret1_def)
})

test_that("alist variables outside d are masked", {
  ret1 <- alist(ret2=ret2, ret3=ret3)
  expect_equal(d %>% select(ret1) %>% .[1:10,] , ret1_def)
})

test_that("where condition with nested ',' works", {
  expect_equal(d %>% select(ticker, where = c(date >= "2001-01-01", ticker %in% c("1 HK", "3 HK"))) %>% .[1,], "1 HK")
})

test_that("select inside a function works", {

  f <- function(x,y) x %>% select(ret4 = y)

  y <- 0
  expect_equal(d %>% f(1) %>% .[1, "ret4"], 1)
})
