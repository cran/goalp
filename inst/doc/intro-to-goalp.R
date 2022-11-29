## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic equal only---------------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E = 3000
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E = 2000"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----basic inequal------------------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----basic bounds-------------------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000
          A lBound 10
          B lBound 10
          C range [10,20]
          D uBound 5"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----weights per goal---------------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000 | 1/3000
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50    | 1/50
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000 | 1/2000
          A lBound 10
          B lBound 10
          C range [10,20]
          D uBound 5"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----weights per deviation----------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000 | 1/3000
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50    | 1/50   10/50
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000 | 1/2000
          A lBound 10
          B lBound 10
          C range [10,20]
          D uBound 5"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----eval=FALSE---------------------------------------------------------------
#  # Load library
#  library(goalp)
#  
#  # Write goals to a text variable
#  goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000
#            Water:      .8*A + .2*B + .3*C + .1*D + .3*E  = 50   | 1 10
#            Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000
#            A lBound 10
#            B lBound 10
#            C range [10,20]
#            D uBound 5"
#  
#  # Solve problem
#  gp <- goalp(goals, normW=TRUE)
#  
#  # Print results to screen
#  summary(gp)

## ----lexicographic------------------------------------------------------------
# Load library
library(goalp)

# Write goals to a text variable
goals <- "Land :      60*A + 30*B + 20*C + 10*D + 20*E >= 3000 | 1#
          Water:      .8*A + .2*B + .3*C + .1*D + .3*E = 50    | 3# 2#
          Fertiliser: 30*A + 20*B + 10*C +  5*D + 20*E <= 2000 | 3#
          A lBound 10
          B lBound 10
          C range [10,20]
          D uBound 5"
          
# Solve problem
gp <- goalp(goals)

# Print results to screen
summary(gp)

## ----eval=FALSE---------------------------------------------------------------
#  goals <- "g1: 3*x + 2*y + z  = 20
#            g2:   x +   y     >= 5  | NA"
#  gp <- goalp(goals)
#  summary(gp)

