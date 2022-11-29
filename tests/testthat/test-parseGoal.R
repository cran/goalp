test_that("matrix 'A' is parsed correctly", {
  # Without names
  A <- matrix(c(20, 12, 40,
                11, 16, 08,
                04, 03, 06), nrow=3, ncol=3, byrow=TRUE,
              dimnames=list(paste0("goal", 1:3),
                            c("A", "B", "C")))
  eqs <- "20*A + 12*B + 40*C = 1200
          11*A + 16*B +  8*C = 1000
           4*A +  3*B +  6*C =  200"
  expect_equal(parseGoal(eqs)$A, A)

  # With names
  rownames(A) <- c("Labour", "Profit", "Materials")
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200
          Profit   : 11*A + 16*B +  8*C = 1000
          Materials:  4*A +  3*B +  6*C =  200"
  expect_equal(parseGoal(eqs)$A, A)

  # With names, weights
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200 | 1/1200 1/1200
          Profit   : 11*A + 16*B +  8*C = 1000 | 1/1000 1/1000
          Materials:  4*A +  3*B +  6*C =  200 | 1/200  1/200 "
  expect_equal(parseGoal(eqs)$A, A)

  # With names, weights, and priorities
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200 | 1/1200 1/1200 | 1# 1#
          Profit   : 11*A + 16*B +  8*C = 1000 | 1/1000 1/1000 | 1# 1#
          Materials:  4*A +  3*B +  6*C =  200 | 1/200  1/200  | 1# 1#"
  expect_equal(parseGoal(eqs)$A, A)

  # With ranges
  A <- rbind(A,
             A_lBound=c(1, 0, 0),
             A_uBound=c(1, 0, 0),
             B_lBound=c(0, 1, 0),
             C_uBound=c(0, 0, 1))
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200
          Profit   : 11*A + 16*B +  8*C = 1000
          Materials:  4*A +  3*B +  6*C =  200
          A range [10,20]
          B range (30,)
          C range (,40)"
  expect_equal(parseGoal(eqs)$A, A)

  # With lBound uBound
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200
          Profit   : 11*A + 16*B +  8*C = 1000
          Materials:  4*A +  3*B +  6*C =  200
          A lBound 10
          A uBound 20
          B lBound 30
          C uBound 40"
  expect_equal(parseGoal(eqs)$A, A)

  # Repeated bounds
  eqs <- "Labour   : 20*A + 12*B + 40*C = 1200
          Profit   : 11*A + 16*B +  8*C = 1000
          Materials:  4*A +  3*B +  6*C =  200
          A lBound 10
          A uBound 20
          A range (30, 40)"
  expect_error(parseGoal(eqs))

})

test_that("vector 'm' is parsed correctly", {
  # Defaults
  m <- setNames(c("=", ">=", "<=", "==",
                  "lBound", "uBound", "lBound", "uBound"),
                c(paste0("goal", 1:4),
                  "A_lBound", "A_uBound", "B_lBound", "C_uBound"))
  eqs <- " 1*A +  2*B +  3*C  = 13
           4*A +  5*B +  6*C >= 14
           7*A +  8*B +  9*C <= 15
          10*A + 11*B + 12*C == 16
          A range (10, 20)
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$m, m)

  # Defaults with names
  names(m)[1:4] <- c("north", "east", "south", "west")
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C >= 14
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16
          A range (10, 20)
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$m, m)
})


test_that("vector 'b' is parsed correctly", {
  # Defaults
  b <- setNames(c(13:16, 10, 20, 30, 40),
                c(paste0("goal", 1:4),
                  "A_lBound", "A_uBound", "B_lBound", "C_uBound"))
  eqs <- " 1*A +  2*B +  3*C  = 13
           4*A +  5*B +  6*C >= 14
           7*A +  8*B +  9*C <= 15
          10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$b, b)

  # Defaults with names
  names(b)[1:4] <- c("north", "east", "south", "west")
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C >= 14
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$b, b)

  # No b value
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16"
  expect_error(parseGoal(eqs))
})


test_that("matrix 'w' is parsed correctly", {
  # Defaults
  w <- list(c(paste0("goal", 1:4),
              "A_lBound", "A_uBound", "B_lBound", "C_uBound"),
            c("w-", "w+"))
  w <- matrix(c( 1, 1, #  1*A +  2*B +  3*C  = 13
                 1, 0, #  4*A +  5*B +  6*C >= 14
                 0, 1, #  7*A +  8*B +  9*C <= 15
                NA,NA, # 10*A + 11*B + 12*C == 16
                NA, 0, # A lBound 10
                 0,NA, # A uBound 20
                NA, 0, # B lBound 30
                 0,NA),# C uBound 40
                nrow=8, ncol=2, byrow=TRUE, dimnames=w)
  eqs <- " 1*A +  2*B +  3*C  = 13
           4*A +  5*B +  6*C >= 14
           7*A +  8*B +  9*C <= 15
          10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$w, w)

  # Defaults with names
  rownames(w)[1:4] <- c("north", "east", "south", "west")
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C >= 14
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$w, w)

  # With names, weights
  w[1, ] <- c(5,4)
  w[2,1] <- 3
  w[3,2] <- 2
  eqs <- "north:  1*A +  2*B +  3*C  = 13 | 5 4
          east :  4*A +  5*B +  6*C >= 14 | 3
          south:  7*A +  8*B +  9*C <= 15 | 2
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$w, w)

  # Inconsistent weights
  expect_error(parseGoal(" 1*A +  2*B +  3*C  = 13 | 5 4 3"))
  expect_error(parseGoal(" 4*A +  5*B +  6*C >= 14 | 4 5"))
  expect_error(parseGoal(" 7*A +  8*B +  9*C <= 15 | 6 7"))
  expect_error(parseGoal("10*A + 11*B + 12*C == 16 | 8"  ))
})


test_that("matrix 'p' is parsed correctly", {
  # Defaults
  p <- list(c(paste0("goal", 1:4),
              "A_lBound", "A_uBound", "B_lBound", "C_uBound"),
            c("p-", "p+"))
  p <- matrix(c(  1,  1, #  1*A +  2*B +  3*C  = 13
                  1,Inf, #  4*A +  5*B +  6*C >= 14
                Inf,  1, #  7*A +  8*B +  9*C <= 15
                 NA, NA, # 10*A + 11*B + 12*C == 16
                 NA,Inf, # A lBound 10
                Inf, NA, # A uBound 20
                 NA,Inf, # B lBound 30
                Inf, NA),# C uBound 40
              nrow=8, ncol=2, byrow=TRUE, dimnames=p)
  eqs <- " 1*A +  2*B +  3*C  = 13
           4*A +  5*B +  6*C >= 14
           7*A +  8*B +  9*C <= 15
          10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$p, p)

  # Defaults with names
  rownames(p)[1:4] <- c("north", "east", "south", "west")
  eqs <- "north:  1*A +  2*B +  3*C  = 13
          east :  4*A +  5*B +  6*C >= 14
          south:  7*A +  8*B +  9*C <= 15
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$p, p)

  # With names, weights
  p[1, ] <- c(5,4)
  p[2,1] <- 3
  p[3,2] <- 2
  eqs <- "north:  1*A +  2*B +  3*C  = 13 | 5# 4#
          east :  4*A +  5*B +  6*C >= 14 | 3#
          south:  7*A +  8*B +  9*C <= 15 | 2#
          west : 10*A + 11*B + 12*C == 16
          A range [10, 20]
          B lBound 30
          C range (,40)"
  expect_equal(parseGoal(eqs)$p, p)

  # Inconsistent weights
  expect_error(parseGoal(" 1*A +  2*B +  3*C  = 13 | 5# 4# 3#"))
  expect_error(parseGoal(" 4*A +  5*B +  6*C >= 14 | 4# 5#"))
  expect_error(parseGoal(" 7*A +  8*B +  9*C <= 15 | 6# 7#"))
  expect_error(parseGoal("10*A + 11*B + 12*C == 16 | 8#"   ))
})
