library(tidyverse)

# Alice is X hours late, where X~Uniform(0,theta).
# Theta is either 1/4 or 3/4 with equal probability of each.

df <- tibble(
  theta = c(1/4,3/4),
  prior = c(1/2,1/2),
  likelihood_10min = 1/theta,
  likelihood_30min = c(0,4/3),
  posterior_10min = likelihood_10min*prior / sum(likelihood_10min*prior),
  posterior_30min = likelihood_30min*prior / sum(likelihood_30min*prior)
)
