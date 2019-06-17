library(tidyverse)
library(MASS)
library(pracma)

#12
A <- c(1,1,1,1, 0,1,3,4) %>% matrix(ncol=2, byrow = FALSE)
b <- c(0,8,8,20) %>% matrix(ncol=1)

x_hat <- ginv(t(A) %*% A) %*% t(A) %*% b
p <- A %*% x_hat
e <- b-p

E <- e %>% norm()

#17
a <- c(1,1,1,1) %>% matrix(ncol=1)
p1 <- a %*% (t(a)%*%a)^(-1) %*% t(a) %*% b
e1 <- b-p1
