library(tidyverse)
library(expm)
library(MASS)

# markov matrix
A <- c(0.8, 0.2, 0.05, 0.95) %>% matrix(nrow=2)
u0 <- c(.02,.98) 

A %*% u0
A %^%2 %*% u0
A %^%1000 %*% u0
eigen_A <- A %>% eigen()

# leslie matrix
A <- c(0.04,1.1,0.01, 0.98,0,0, 0,0.92,0) %>%
  matrix(nrow=3, byrow=TRUE)
eigen_A <- A %>% eigen()
v0 <- c(0,0,1)
A %^%2 %*% v0
A %^%1000 %*% v0

# consumptrion matrix
A <- c(.2,.3,.4, .4,.4,.1, .5,.1,.3) %>%
  matrix(nrow=3, byrow=TRUE)
eigen_A <- A %>% eigen()

input <- c(1,1,1)
demand <- (diag(3) - A) %*%p

demand <- c(2,3,4)
input = ginv(diag(3) - A) %*% demand

# this consumption matrix consumes more than it produces
A <- c(0,4,1,0) %>% matrix(nrow=2, byrow=TRUE)
eigen(A)
ginv(diag(2) - A)