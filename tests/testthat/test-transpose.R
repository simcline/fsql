require(fsql)

d <- data.frame(
  date= c(as.Date("2001-01-01"), as.Date("2001-01-02")),
  ticker=c("1 HK","2 HK"),
  ret1=rnorm(2),
  ret2=rnorm(2),
  ret3=rnorm(2)
)

test_that("one-row transpose works", {
  r <- data.frame(Var = names(d), value =  1:ncol(d) %.>% \(x) as.character(d[1,x]) )
  expect_equal(d[1,] %>% transpose, r)
})

test_that("one-var transpose works", {
  r <- data.frame(Var = names(d) %>% setdiff("ticker"))
  r[, d$ticker[1]] <- names(d) %>% setdiff("ticker") %.>% \(x) as.character(d[1,x])
  r[, d$ticker[2]] <- names(d) %>% setdiff("ticker") %.>% \(x) as.character(d[2,x])
  expect_equal(d %>% transpose(ticker), r)
})

test_that("two-var transpose works", {
  r <- data.frame(Var = names(d) %>% setdiff(c("ticker","date")))
  r[, paste0("(",d$ticker[1],",",d$date[1] %>% as.character,")")] <- names(d) %>% setdiff(c("ticker","date")) %.>% \(x) d[1,x]
  r[, paste0("(",d$ticker[2],",",d$date[2] %>% as.character,")")] <- names(d) %>% setdiff(c("ticker","date")) %.>% \(x) d[2,x]
  expect_equal(d %>% transpose(ticker, date), r)
})
