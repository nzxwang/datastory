library(tidyverse)
# We want to estimate the mean and give an 80% bootstrap CI.

data <- c(30,37,36,43,42,43,43,46,41,42)
sample_mean <- data %>% mean()

df <- tibble(
  i = seq(1,20),
  #TODO why cant I use map_dbl here?
  bootstrap_sample = map(i, function(i) {sample(data, replace=TRUE)}),
  bootstrap_mean = map_dbl(bootstrap_sample, function(sample){
    sample %>% mean()
  }),
  delta = bootstrap_mean - sample_mean
)

# at 90% CI
lower_delta <- df %>% arrange(delta) %>% .$delta %>% .[[2]]
upper_delta <- df %>% arrange(delta) %>% .$delta %>% .[[18]]
`0.9 CI` <- sample_mean + c(lower_delta,upper_delta)
