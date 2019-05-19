library(tidyverse)

x <- rnorm(n=500)
y <- rnorm(n=500)
X <- cbind(x,y)
X %>% cov()
X %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=x,y=y))

#center the matrix
X_centered <- X - colMeans(X)
X_centered %>% cov()
X_centered %>%
  as_tibble() %>%
  ggplot() +
  geom_point(aes(x=x,y=y))+
  xlim(c(-10,10)) +
  ylim(c(-10,10))

#scaling matrix scales x by 0.7, y by 3.4
S <- c(0.7,0,0,3.4) %>% matrix(nrow=2)
Y <- X %*% S
Y %>% cov()
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
C <- Z %>% cov()
# eigenstuff <- C %>% svd()
eigenstuff <- C %>% eigen()
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
