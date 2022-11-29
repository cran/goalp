test_that("goalp object is generated", {
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200 | 1/1200  5/1200
          Profit   : 11*A + 16*B +  8*C = 1000 |10/1000  1/1000
          Batteries:  4*A +  3*B +  6*C =  200 | 1/200  10/200 "
  m <- goalp(eqs, silent=TRUE)

  expect_s3_class(m, "goalp")
})

test_that("results are as expected in a weighted GP problem", {
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200 |  1/1200  5/1200
          Profit   : 11*A + 16*B +  8*C = 1000 | 10/1000  1/1000
          Batteries:  4*A +  3*B +  6*C =  200 |  1/200  10/200"
  m <- goalp(eqs, silent=TRUE)

  expect_equal(m$x, c(1, 61, 2), ignore_attr=TRUE)
})

test_that("results are as expected in a lexicographic GP problem", {
  eqs <- "Revenue : 8*xs + 15*s + 20*m + 35*l + 47*xl  = 5000 | 1#  5#
          Labour  : 4*xs +  9*s + 10*m + 15*l + 18*xl  = 1440 | 5#  2#
          Glass   : 3*xs +  7*s + 12*m + 18*l + 25*xl  = 2500 | 5#  5#
          xs lBound 25
          s  lBound 30
          m  lBound 50"
  m <- goalp(eqs, silent=TRUE)

  expect_equal(m$x, c(xs=27, s=30, m=50, l=0, xl=71))
})
