test_that("basic aj works", {
  d1 <- data.frame(ts = c(Sys.time(),Sys.time()+1,Sys.time()+3), bid = c(100,99,100))
  d2 <- data.frame(ts = c(Sys.time()+0.1,Sys.time()+2,Sys.time()+2), ask = c(101,100,102))

  expect_equal(aj(ts,x=d1,y=d2) %>% .$bid, c(100,99,100) )
  expect_equal(aj(ts,x=d1,y=d2) %>% .$ask, c(NA,101,102) )
})
