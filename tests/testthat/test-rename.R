require(fsql)

ret1_def <- rnorm(10)
d <- data.frame(
  ret1 = ret1_def,
  ret2=rnorm(10),
  ret3=rnorm(10)
)

test_that("basic renaming (single) works", {
  expect_equal(d %>% rename(newret1 = ret1) %>% colnames, c( "ret2", "ret3", "newret1") )
})

test_that("basic renaming (multiple) works", {
  expect_equal(d %>% rename(newret1=ret1,newret2=ret2) %>% colnames, c( "ret3","newret1", "newret2"))
})

test_that("character list renaming works", {
  expect_equal(d %>% rename(list(newret2 = "ret2", newret3 = "ret3")) %>% colnames, c("ret1","newret2", "newret3"))
})


test_that("alist renaming works", {
  expect_equal(d %>% rename(alist(newret2 = ret2, newret3 = ret3)) %>% colnames, c("ret1","newret2", "newret3"))
})


test_that("mixed renaming works", {
  expect_equal(d %>% rename(newret1=ret1, alist(newret2=ret2), list(newret3="ret3") ) %>% colnames , c("newret1","newret2", "newret3"))
})

test_that("numerical variables outside d are masked", {
  ret1 <- 1:10
  expect_equal(d %>% rename(newret1=ret1) %>% .[1:10,"newret1"] , ret1_def)
})

test_that("string variables outside d are masked", {
  ret1 <- list(newret2="ret2", newret3="ret3")
  expect_equal(d %>% rename(newret1=ret1) %>% .[1:10,"newret1"] , ret1_def)
})

test_that("alist variables outside d are masked", {
  ret1 <- alist(newret2=ret2, newret3=ret3)
  expect_equal(d %>% select(newret1=ret1) %>% .[1:10,"newret1"] , ret1_def)
})
