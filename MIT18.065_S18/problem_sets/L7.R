library(tidyverse)

#2 
#closest rank1 approximation to first matrix is just c(3,0,0).
#second matrix can be multiplied by a permutation matrix to get it diagonal. we know that multiplying a matrix by a orthonormal martix does not change its singular values so the best rank1 approximation is c(1,0).
A <- c(2,1,1,2) %>% matrix(nrow=2)
u1 <- A %>% svd() %>% {.$u[,1,drop=FALSE]}
v1 <- A %>% svd() %>% {.$v[,1,drop=FALSE]}
sigma1 <- A %>% svd() %>% {.$d[1]}
sigma1 * u1 %*% t(v1)

#10
# spectral norm of A^-1 is 1/sigma2. 
# Frobenius norm of A^-1 is sqrt(sigma1^-2 + sigma2^-2)