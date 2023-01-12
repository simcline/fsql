d <- data.frame(
  date=c(seq(as.Date("2019-01-01"), as.Date("2019-01-10"), by=1),
         seq(as.Date("2019-01-01"), as.Date("2019-01-10"), by=1)),
  symbol=c(rep("1 HK", 10), rep("2 HK", 10)),
                price=c(cumsum(rnorm(10)*100), cumsum(rnorm(10))*10))

d %>% 
xgroup()