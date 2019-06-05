library(tidyverse)

#1
# x^T*x will just be the sum of the squares of the c's because all of the "cross products" of the v's sum to 0 by orthogonality.
# x^T*S*x  case is the same logic, except each eigenvector gets scaled by the eigenvalue first.

#6
c(3,0,4,5) %>% matrix(nrow=2) %>% svd()
