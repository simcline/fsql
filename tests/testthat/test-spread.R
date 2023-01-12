require(fsql)

d <- data.frame(
  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
  ret1=rnorm(10),
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("spread o gather = Identity (up to columns reordering)", {
  expect_equal(d %>% gather(ret1,ret2) %>% spread(key, value) %>% .[,c(1,2,4,5,3)], d)
})

test_that("spread o gather = Identity (up to columns reordering, three columns)", {
  expect_equal(d %>% gather(ret1,ret2,ret3) %>% spread(key, value), d)
})
