require(fsql)

d <- data.frame(ret = 1:10, time = as.POSIXlt("2021-06-08 13:44:20 UTC") + 60*c(1.1,2.2,3.3,4.4,4.5,6.6,7.7,8.8,9.9,10.1))

test_that("frequencies work with freq = 1", {

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1m" ) %>% .$ret, c(1.0,  2.0,  3.0,  4.5,  6.0,  7.0,  8.0,  9.5))
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1m" ) %>% .$time, as.character(as.POSIXlt("2021-06-08 13:45:00 UTC") + 60*(c(0:3,5, 7:9))))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1Y" ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1Y" ) %>% .$time, "2021-01-01")

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1M" ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "1M" ) %>% .$time, "2021-06-01")
})

test_that("frequencies work with freq != 1", {

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5m" ) %>% .$ret, c(3,8))
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5m" ) %>% .$time, as.character(as.POSIXlt("2021-06-08 13:45:00 UTC") + 5*60*(c(0,1))))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5Y" ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5Y" ) %>% .$time, "2020-01-01")

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5M" ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "5M" ) %>% .$time, "2021-06-01")
})

test_that("frequencies work with different start time (smaller)", {
  start <- d[1,"time"] - 3*60

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2m", start = start  ) %>% .$ret, c(1.0, 2.5,  4.5, 6.5, 8.5, 10.0))
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2m" , start = start) %>% .$time, as.character(start + 2*60*(1:6)))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2Y", start = start ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2Y", start = start ) %>% .$time, as.character(start))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2M", start = start ) %>% .$ret, 5.5)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2M", start = start ) %>% .$time, as.character(start))
})

test_that("frequencies work with different start time (bigger)", {
  start <- d[1,"time"] + 3*60

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2m", start = start  ) %>% .$ret, c(4.5, 6.5, 8.5, 10))
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2m" , start = start) %>% .$time, as.character(start + 2*60*(0:3)))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2Y", start = start ) %>% .$ret, 7)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2Y", start = start ) %>% .$time, as.character(start))

  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2M", start = start ) %>% .$ret, 7)
  expect_equal(d %>% sampleby(ret = mean(ret), by = time, freq = "2M", start = start ) %>% .$time, as.character(start))
})

test_that("sampleby = identity if freq=NULL", {
  expect_equal(d %>% sampleby(ret= mean(ret), by = time), d)
})
