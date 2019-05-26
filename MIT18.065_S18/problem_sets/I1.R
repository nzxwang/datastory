library(tidyverse)

#1
A <- c(1,2,2,2, 1,1,1,1, 2,3,3,3) %>% matrix(nrow=4)
x <- c(1,1,-1)
A %*% x
# A is 4x3. rank is 2. null space has dimension 1. left null space has dimension 2.

#4
A <- rep(1,9) %>% matrix(nrow=3)
x <- c(1,-1,0)
y <- c(1,0,-1)
A%*%x
A%*%y
#there is no Az=0 because the nullspace of A has dimension 2. z would be a combination of x and y.

#9
# if the column space of a m*n matrix is ALL of R^3, then r=3, m=3, and n>=3.

#18
# The new column space becomes [C C]^T, and the new row space becomes [0 R]. the rank factorization is not unique.