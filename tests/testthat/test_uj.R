require(fsql)

test_that("unkeyed uj works", {

x <- data.frame(a=c(1,2),
                b=c(2,3),
                c=c(5,7), stringsAsFactors = FALSE
)
y <- data.frame(a=1:3,
                b=c(2,3,7),
                c=c(10,20,30),
                d=c("A" , "B", "C"), stringsAsFactors = FALSE
)

z <- x %>% uj(y)
expect_equal(z$d, c(NA, "A", NA, "B", "C"))
}
)

test_that("keyed uj works", {
  x <- data.frame(a=c(1,2),
                  b=c(2,3),
                  c=c(5,7), stringsAsFactors = FALSE
  )
  y <- data.frame(a=1:3,
                  b=c(2,3,7),
                  c=c(10,20,30),
                  d=c("A" , "B", "C"), stringsAsFactors = FALSE
  )
z <- (x %>% xkey(c("a", "b"))) %>% uj(y %>% xkey(c("a", "b")))

expect_equal(z$a, 1:3)
expect_equal(z$b, c(2,3,7))
expect_equal(z$c, c(10,20,30))
expect_equal(z$d, c("A","B","C"))
})
