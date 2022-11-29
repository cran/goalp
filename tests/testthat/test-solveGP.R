test_that("extended matrices A_ and w_ are correct", {
  # Set up
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C >= 14
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C uBound 40"
  L <- parseGoal(eqs)
  vT <- rep("c", 3)

  # Solve
  lp <- solveGP(L$A, L$b, L$w, L$vT, silent=TRUE)

  # Check A_
  A_ <- t(lp$constraints)
  A_ <- A_[, grep("const.dir.num|const.rhs", colnames(A_), invert=TRUE)]
  A  <- matrix(c(1, 2, 3, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0,
                 4, 5, 6, 0, 0, 1,-1, 0, 0, 0, 0, 0, 0,
                 7, 8, 9, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0,
                10,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,
                 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0,
                 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
               nrow=8, ncol=13, byrow=TRUE,
               dimnames=list(c("north", "east", "south", "west",
                               "A_lBound", "A_uBound", "B_lBound", "C_uBound" ),
                             c("A", "B", "C",
                               "north-", "north+",
                               "east-", "east+",
                               "south-", "south+",
                               "A_lBound+", "A_uBound-",
                               "B_lBound+", "C_uBound-")))
  expect_equal(A_,A)

  # Check w_
  w_ <- lp$objective
  w  <- setNames(c(0,0,0,1,1,1,0,0,1,0,0,0,0), colnames(A_))
  expect_equal(w_, w)
})

test_that("results are the expected", {
  # Set up
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200 |  1/1200  5/1200
          Profit   : 11*A + 16*B +  8*C = 1000 | 10/1000  1/1000
          Batteries:  4*A +  3*B +  6*C =  200 |  1/200  10/200"
  A <- matrix(c(20, 12, 40,
                11, 16, 08,
                04, 03, 06), nrow=3, ncol=3, byrow=TRUE)
  b <- c(1200, 1000, 200)
  w <- matrix(c(1/1200, 5/1200,
                10/1000, 1/1000,
                1/200, 10/200), nrow=3, ncol=2, byrow=TRUE)
  vT<- rep("i", ncol(A))

  # Solve
  lp <- solveGP(A, b, w, vT, silent=TRUE)

  # Check solution
  expect_equal(lp$objval, 0.314666666666666667, ignore_attr=TRUE)
  expect_equal(lp$solution, c(1, 61, 2, 368, 0, 0, 3, 1, 0), ignore_attr=TRUE)
})
