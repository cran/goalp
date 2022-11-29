test_that("printing works", {
  # Set up
  eqs <- paste("goal1 : 20*x1 + 12*x2 + 40*x3 = 1200 | 1 1 | 1# 1#",
               "goal2 : 11*x1 + 16*x2 + 8*x3 = 1000 | 1 1 | 1# 1#",
               "goal3 : 4*x1 + 3*x2 + 6*x3 = 200 | 1 1 | 1# 1#",
               sep="\n")
  A <- matrix(c(20, 12, 40,
                11, 16, 08,
                04, 03, 06), nrow=3, ncol=3, byrow=TRUE)
  m <- rep("=", nrow(A))
  b <- c(1200, 1000, 200)

  m <- goalp(A=A, m=m, b=b, silent=TRUE)
  out <- paste(capture.output(print(m)), collapse="\n")
  expect_equal(out, eqs)
})
