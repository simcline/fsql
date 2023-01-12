require(fsql)

test_that("basic across works", {
  expect_equal(across(mean, ret1, ret2), alist(ret1 = (mean)(ret1), ret2 = (mean)(ret2)))
})

test_that("basic across + prefixing works", {
  expect_equal(across(mean, ret1, ret2, pref = "av_"), alist(av_ret1 = (mean)(ret1), av_ret2 = (mean)(ret2)))
})

test_that("by list across works", {
  mycols <- c("ret1", "ret2")
  expect_equal(across(mean, bylist = mycols, pref="av_"), alist(av_ret1 = (mean)(ret1), av_ret2 = (mean)(ret2)))
})

test_that("mixed across works", {
  mycols <- c("ret1", "ret2")
  expect_equal(across(mean, ret3, bylist = mycols, pref="av_"), alist(av_ret3 = (mean)(ret3), av_ret1 = (mean)(ret1), av_ret2 = (mean)(ret2)))
})



ret1_def <- rnorm(10)
d <- data.frame(
  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
  ret1 = ret1_def,
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("across with lambda function works", {
  expect_equal(d %>% select(across(function(x) mean(x), ret3, pref="av_")), d %>% select(av_ret3= mean(ret3)))
})
