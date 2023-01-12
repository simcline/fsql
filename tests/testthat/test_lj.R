require(fsql)

test_that("join on 2 keys work", {
  x <- data.frame(a=c(1,2,3),
                  b=c("x", "y", "z"),
                  c=c(10, 20, 20)
  )
  y <- data.frame(a=c(1,3),
                  b=c("x", "z"),
                  c=c(1,2),
                  d=c(10,20))

  z <- x %>% lj(y %>% xkey(c("a", "b")))
  expect_equal(z$a, x$a)
  expect_equal(z$b, x$b)
  expect_equal(z$c, c(1, 20, 2))
  expect_equal(z$d, c(10, NA, 20))

})

