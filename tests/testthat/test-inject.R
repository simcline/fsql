require(fsql)


test_that("pure injection works", {
  expect_equal(1:3 %>>% myfun(myvar), alist(myvar_myfun_1 = myfun(myvar, 1), myvar_myfun_2 = myfun(myvar, 2), myvar_myfun_3 = myfun(myvar, 3) ))
})
