#2
#A and A^+ have the same rank because they both have the same # of non-zero singular values. 
#A and A^+ have the same eigenvectors as (A^+)Ax=x if x is in the row space and (A^+)Ax=0 if x is in the nullspace. The eigenvalues of A^+ are just the inverses of the eiganvaluse of A, unless 0.

#8
a <- c(1,1) %>% matrix(nrow=2)
q1 <- a/sqrt(sum(a^2))

b <- c(4,0) %>% matrix(nrow=2)
proj_b_on_q1 <- (t(q1) %*% b)[[1]] * q1

A2 <- b - proj_b_on_q1

#9
q2 = A2/ sqrt(sum(A2^2))