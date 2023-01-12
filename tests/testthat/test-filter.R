require(fsql)

ret1_def <- rnorm(10)
d <- data.frame(
  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
  ret1 = ret1_def,
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("filter (single) works", {
  expect_equal(d %>% filter(ret1 >0) %>% .$ret2 , d[d$ret1 >0, "ret2"])
})


test_that("filter (multiple) works", {
  expect_equal(d %>% filter(ret1 >0, ret2 <0) %>% .$ret2 , d[d$ret1 >0 & d$ret2 <0, "ret2"])
})

test_that("filter inside a function works", {

  f <- function(x,y) x %>% filter(date == y)

  y <- "2001-01-02"
  expect_equal(d %>% f("2001-01-01") %>% .[1, "ret1"], ret1_def[1])
})
