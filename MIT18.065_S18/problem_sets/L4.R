library(tidyverse)

#2
#A has the same eigenvectors as A^-1, but the eigenvalues are inverted

#11
# A and A^T have the same eigenvalues because |A-lambdaI| = |A^T-lambdaI|
# their eigenvectors are different. This is easily true as the column space and row space can be different. Even if A is full rank, the column vectors and row vectors are different.
A <- c(1,2,3, 3,6,5, 7,1,2) %>% matrix(nrow=3)
A %>% eigen()
A %>% t() %>% eigen()

#15
#this is not diagonalizeable, it only has one eigenvalue with multiplicity 1
c(1,0, 2,1) %>% matrix(nrow=2) %>% eigen()
c(1,3, 1,3) %>% matrix(nrow=2) %>% eigen()