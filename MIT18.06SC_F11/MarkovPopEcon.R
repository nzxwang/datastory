library(tidyverse)
library(MASS)
library(expm)

#1
A <- c(0.9, 0.1, 0.15, 0.85) %>% matrix(nrow=2)
steadystate <- eigen(A) %>% {.$vectors[,1]}
A %^%1000 %*% steadystate

#2
S <- eigen(A) %>% .$vectors
Lambda <- eigen(A) %>% .$values %>% diag()
S %*% Lambda %*% ginv(S)
#just the projection on the stable eigenvector
S %*% matrix(c(1,0,0,0), nrow=2) %*% ginv(S)

#3
c(1,0, .2,.8) %>% matrix(nrow=2) %>% eigen()
c(.2,.8, 1,0) %>% matrix(nrow=2) %>% eigen()
c(.5,.25,.25, .25,.5,.25, .25,.25,.5) %>% matrix(nrow=3) %>% eigen()

#4 
# for every 4*4 Markov matrix A, t(A) has eigenvector c(1,1,1,1) corresponding to eigenvalue lambda=1 since all of the columns of t(A) must add up to one.

#5
# everyone will end up dead. lambda=1 obviously has eigenvector c(0,0,1).
c(.98,.02,0, 0,.97,.03, 0,0,1) %>% matrix(nrow=3) %>% eigen()

#6
# Ax = lambda1*x + lambda2*x + ... + lambda_n*x, but lambda1=1, so for the sum of the terms in Ax to equal to x, all of the sums of the components of the other than lambda1*x must add to zero.

#7
A <- c(.8,.2, .3,.7) %>% matrix(nrow=2)
A %>% eigen()
A %^% 100
# A %^%100 approaches the projection matrix for the eigenvector with eigenvalue 1
# Any markov matrix with eigenvector c(0.6,0.4) will produce that steady state matrix

#8
P <- c(0,0,0,1, 1,0,0,0, 0,1,0,0, 0,0,1,0) %>% matrix(nrow=4)
P %>% eigen()

u0 <- c(0,0,0,1)
P %^%1000 %*% u0

v <- c(0.4, 0.3, 0.2, 0.1)
P %^% 1000 %*% v

#9
# [1...1]M=[1...1], so [1...1]M^2=[1...1]M*M=[1...1]M=[1...1] still Markov

#10
# the trace is equal to product of eigenvalues so lambda1=1, lambda2=a+d-1.
# the eigenvector for lambda1 is c(b,(1-a))

#13
A <- c(.2,.4,.5, .3,.4,.1, .4,.1,.3) %>% matrix(nrow=3)
#since each row adds to 0.9, if you subtract 0.9I from A, it is singular and has c(1,1,1) in its null space.
A %>% eigen()

#14
A <- c(0,0.5,1,0) %>% matrix(nrow=2, byrow=TRUE)
ginv(diag(2)-A)
diag(2) + A + A%^%2 + A%^%3 + A%^%4 + A%^%5 + A%^%6 + A%^%6 + A%^%7 + A%^%8 + A%^%9 + A%^%10
1:20 %>% reduce(function(x, y) x + A%^%y, .init=diag(2))

#15
matrix(c(0,0,1,0), nrow=2) %>% eigen()
ginv(diag(2) - matrix(c(0,0,1,0), nrow=2)) %*% c(2,6)
matrix(c(0,.2,0,.4), nrow=2) %>% eigen()
ginv(diag(2) - matrix(c(0,.2,0,.4), nrow=2)) %*% c(2,6)
matrix(c(.5,.5,1,0), nrow=2) %>% eigen()
ginv(diag(2) - matrix(c(.5,.5,1,0), nrow=2))
