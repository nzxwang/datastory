library(tidyverse)

# theta is the chance the coin lands heads
# the data is we get 3 heads, then one tails.

df <- tibble(
  theta = c(1/2, 3/4),
  prior = length(theta),
  likelihood = theta^3*(1-theta),
  num = likelihood * prior,
  posterior = num / sum(num)
)

# P{fair coin | 3 heads then tails} = 0.372.

ppp <- sum(df$theta * df$posterior)
# P{heads | 3 heads then tails} 