library(tidyverse)
library(MASS)

#1
W <- diag(c(1,1,1/sqrt(2)))
A <- c(1,1,1, 0,1,2) %>% matrix(ncol=2)
b <- c(1,2,4) %>% matrix(nrow=3)
x_hat <- ginv(t(A) %*% t(W) %*% W %*% A) %*% t(A) %*% t(W) %*% W %*% b

cbind(w=diag(W),A,b=b) %>%
  as_tibble() %>%
  ggplot() + 
  geom_point(aes(x=V3, y=b, size = w)) +
  geom_abline(intercept = x_hat[1], slope=x_hat[2])

#2
# if the third measurement is totally unreliable, the variance is inf and w=0
# in this case, b is in the column space of A and is solveable.
W[3,3] <- 0
x_hat <- ginv(t(A) %*% t(W) %*% W %*% A) %*% t(A) %*% t(W) %*% W %*% b
cbind(w=diag(W),A,b=b) %>%
  as_tibble() %>%
  ggplot() + 
  geom_point(aes(x=V3, y=b, size = w)) +
  geom_abline(intercept = x_hat[1], slope=x_hat[2])

#3
# If the third measurement is totally reliable, the variance is 0 and w=inf.
# in this case, the third equation becomes a constraint, then minimize 
# the sum of squares of the first two equations.
W[3,3] <- 999
x_hat <- ginv(t(A) %*% t(W) %*% W %*% A) %*% t(A) %*% t(W) %*% W %*% b
cbind(w=diag(W),A,b=b) %>%
  as_tibble() %>%
  ggplot() + 
  geom_point(aes(x=V3, y=b, size = w)) +
  geom_abline(intercept = x_hat[1], slope=x_hat[2]) +
  ylim(c(0,4))

#4
m=1
(0-m)^2*0.25 + (1-m)^2*0.5 + (2-m)^2*0.25

#5
m = c(0.5,0.5)
# since the flips are independent
cov_matrix = diag(1/4,1/4)
# by variance sum law for independent events the total variance is the sum

#6
# mean is always p
# var is (1-m)^2*p + (0-m)^2*(1-p)

# Example 6
A <- c(-6,2,0,4, 0,4,-6,2, 4,0,2,-6, 2,-6,4,0) %>%
  matrix(nrow=4, byrow=TRUE)
A %>% svd()
