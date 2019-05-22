library(tidyverse)
N <- 500

x <- rnorm(n=N)
y <- rnorm(n=N)
X <- cbind(x,y)
X %>% cov()
X %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=x,y=y))

#center the matrix
X_centered <- X - colMeans(X)

#these are the same
X_centered %>% cov()
Q_X <- (t(X_centered) %*% X_centered)/(N-1)

X_centered %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=x,y=y))+
  xlim(c(-10,10)) +
  ylim(c(-10,10))

#scaling matrix scales x by 0.7, y by 3.4
S <- c(0.7,3.4) %>% diag
Y <- X %*% S

#these are the same
(t(Y) %*% Y)/(N-1)
Q_Y <- t(S) %*% Q_X %*% S

eigenstuff_Y <- Q_Y %>% eigen()

Y %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=V1,y=V2)) +
  xlim(c(-10,10)) +
  ylim(c(-10,10))

#rotating matrix rotates by theta
theta <- 0.77*pi
R <- c(cos(theta), sin(theta), -1*sin(theta), cos(theta)) %>% matrix(nrow=2)

Z <- X %*% S %*% R 
Q_Z <- (t(R) %*% t(S) %*% t(X) %*% X %*% S %*% R)/(N-1)

#the eigenstuff of a symmetric matrix are the same as the svd
# eigenstuff <- Q_Z %>% svd()

# the eigenvalues are just the variances scaled
# the eigenvectors are the rotated unit vectors.
eigenstuff_Z <- Q_Z %>% eigen()
Z %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=V1,y=V2), alpha=0.3) +
  xlim(c(-10,10)) +
  ylim(c(-10,10)) +
  coord_fixed() +
  geom_spoke(aes(
    x=0,
    y=0,
    angle = atan(eigenstuff$vectors[2,1]/eigenstuff$vectors[1,1]),
    radius = 3.4*3),
    color = "red"
  ) +
  geom_spoke(aes(
    x=0,
    y=0,
    angle = atan(eigenstuff$vectors[2,2]/eigenstuff$vectors[1,2]),
    radius = 0.7*3),
    color = "red"
  )

#whitening...
S_ <- eigenstuff_Z$values %>% sqrt() %>% diag()
R_ <-  eigenstuff_Z$vectors
