library(tidyverse)

is.spd <- function(matrix) {
  eigenvalues <- matrix %>% eigen() %>% .$values
  all(eigenvalues > 0)
}

#3
b <- -2.9
c(1,b, b,9) %>% matrix(nrow=2) %>% is.spd()
b <- 3.1
c(1,b, b,9) %>% matrix(nrow=2) %>% is.spd()

c <-7.9
c(2,4, 4,c) %>% matrix(nrow=2) %>% is.spd()
c <-8.1
c(2,4, 4,c) %>% matrix(nrow=2) %>% is.spd()
# the last one just needs c^2>b^2 (determinant needs to be greater than 0)

#14
A <- c(1,-1,2) %>% matrix(nrow=1)
S <- 4 * t(A) %*% A

#15
S1 <- 2
S2 <- 6
S3 <- c(2,2,0, 2,5,3, 0,3,8) %>% matrix(nrow=3) %>% det()
