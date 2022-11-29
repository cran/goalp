test_that("defaults are set correctly", {
  # Set up
  A <- matrix(c(11, 12, 13,
                21, 22, 23,
                31, 32, 33,
                41, 42, 43,
                01, 00, 00,
                01, 00, 00), nrow=6, ncol=3, byrow=TRUE)
  m <- c("=", "<=", ">=", "==", "lBound", "uBound")
  b <- rep(0, nrow(A))
  L <- validateMatrices(A, b, m, setDefaults=TRUE)


  # Check A
  dimnames(A) <- list(c(paste0("goal", 1:4), "x1_lBound", "x1_uBound"),
                      paste0("x", 1:3))
  expect_equal(L$A, A)
  # Check m
  names(m) <- rownames(A)
  expect_equal(L$m, m)
  # Check b
  names(b) <- rownames(A)
  expect_equal(L$b, b)
  # Check w
  w <- matrix(c( 1, 1,
                 0, 1,
                 1, 0,
                NA,NA,
                NA, 0,
                 0,NA), nrow=6, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("w-", "w+")))
  expect_equal(L$w, w)
  # Check p
  p <- matrix(c(  1,  1,
                Inf,  1,
                  1,Inf,
                 NA, NA,
                 NA,Inf,
                Inf, NA), nrow=6, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("p-", "p+")))
  expect_equal(L$p, p)
})

test_that("dimensions are checked correctly", {
  # Set up
  nC <- 6
  nV <- 3
  A <- list(c(paste0("goal", 1:(nC-2)), "x1_lBound", "x1_uBound"),
            paste0("x", 1:nV))
  A <- matrix(c(11, 12, 13,
                21, 22, 23,
                31, 32, 33,
                41, 42, 43,
                01, 00, 00,
                01, 00, 00), nrow=nC, ncol=nV, byrow=TRUE, dimnames=A)
  m <- setNames(c("=", "<=", ">=", "==", "lBound", "uBound"), rownames(A))
  b <- setNames(rep(0, nC), rownames(A))
  w <- matrix(c( 1, 1,
                 0, 1,
                 1, 0,
                NA,NA,
                NA, 0,
                 0, NA), nrow=nC, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("w-", "w+")))
  p <- matrix(c(  1,  1,
                Inf,  1,
                  1,Inf,
                 NA, NA,
                 NA,Inf,
                Inf, NA), nrow=nC, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("p-", "p+")))
  # data.frame(A, m=m, b=b, w, p) # DEBUG

  # Check that definition is correct
  expect_equal(validateMatrices(A, b, m, w, p), list(A=A, b=b, m=m, w=w, p=p))
  # Check A fails with wrong dimensions
  AA <- rbind(A, const5=runif(nV))
  expect_error(validateMatrices(AA, b, m, w, p))
  # Check b fails with wrong dimensions
  bb <- c(b, const5=0)
  expect_error(validateMatrices(A, bb, m, w, p))
  # Check m fails with wrong dimensions
  mm <- c(m, const5="=")
  expect_error(validateMatrices(A, b, mm, w, p))
  # Check w fails with wrong dimensions
  ww <- rbind(w, const5=c(1,1))
  expect_error(validateMatrices(A, b, m, ww, p))
  # Check p fails with wrong dimensions
  pp <- rbind(p, const5=c(1,1))
  expect_error(validateMatrices(A, b, m, w, pp))
})

test_that("A is checked correctly", {
  # Set up
  A <- matrix(c(11, 12, 13,
                21, 22, 23,
                31, 32, 33,
                41, 42, 43,
                01, 00, 00,
                01, 00, 00), nrow=6, ncol=3, byrow=TRUE)
  m <- c("=", "<=", ">=", "==", "lBound", "uBound")
  b <- rep(0, nrow(A))
  L <- validateMatrices(A, b, m, setDefaults=TRUE)
  rm(A, m, b)

  # Check dimensions
  AA <- rbind(L$A, const7=51:53)
  expect_error(validateMatrices(AA, L$b, L$m, L$w, L$p))
  # Check bounds
  mm <- L$m; mm[length(L$m)] <- "lBound"
  expect_error(validateMatrices(L$A, L$b, mm, L$w, L$p))
  mm <- L$m; mm[length(L$m)-1] <- "uBound"
  expect_error(validateMatrices(L$A, L$b, mm, L$w, L$p))
  AA <- L$A; AA[5,2] <- 1
  expect_error(validateMatrices(AA, L$b, L$m, L$w, L$p))
  AA <- L$A; AA[5,1] <- 2
  expect_error(validateMatrices(AA, L$b, L$m, L$w, L$p))
})

test_that("NAs are checked correctly in 'w' and 'p'", {
  # Set up
  nC <- 6
  nV <- 3
  A <- list(c(paste0("goal", 1:(nC-2)), "x1_lBound", "x1_uBound"),
            paste0("x", 1:nV))
  A <- matrix(c(11, 12, 13,
                21, 22, 23,
                31, 32, 33,
                41, 42, 43,
                01, 00, 00,
                01, 00, 00), nrow=nC, ncol=nV, byrow=TRUE, dimnames=A)
  m <- setNames(c("=", "<=", ">=", "==", "lBound", "uBound"), rownames(A))
  b <- setNames(rep(0, nC), rownames(A))
  w <- matrix(c( 1, 1,
                 0, 1,
                 1, 0,
                 NA,NA,
                 NA, 0,
                 0, NA), nrow=nC, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("w-", "w+")))
  p <- matrix(c(  1,  1,
                Inf,  1,
                  1,Inf,
                 NA, NA,
                 NA,Inf,
                Inf, NA), nrow=nC, ncol=2, byrow=TRUE,
              dimnames=list(rownames(A), c("p-", "p+")))
  # data.frame(A, m=m, b=b, wN=w[,1], wP=w[,2], pN=p[,1], pP=p[,2]) # DEBUG

  # Check that definition is correct
  expect_equal(validateMatrices(A, b, m, w, p), list(A=A, b=b, m=m, w=w, p=p))

  # Check w
  ww <- w; ww[1,1] <- Inf; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[1,2] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[2,1] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[2,1] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  #ww <- w; ww[2,2] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  #ww <- w; ww[3,1] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[3,2] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[3,2] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[4,1] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[4,2] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[5,1] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[5,2] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[5,2] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[6,1] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[6,1] <-  NA; expect_error(validateMatrices(A, b, m, ww, p))
  ww <- w; ww[6,2] <-   1; expect_error(validateMatrices(A, b, m, ww, p))
  # Check p
  pp <- p; pp[1,2] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[2,1] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[2,1] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  #pp <- p; pp[2,2] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  #pp <- p; pp[3,1] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[3,2] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[3,2] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[4,1] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[4,2] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[5,1] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[5,2] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[5,2] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[6,1] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[6,1] <- NA; expect_error(validateMatrices(A, b, m, w, pp))
  pp <- p; pp[6,2] <-  1; expect_error(validateMatrices(A, b, m, w, pp))
})


