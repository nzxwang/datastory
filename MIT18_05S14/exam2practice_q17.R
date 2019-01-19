library(tidyverse)

# Waiting times are modeled as an exponential RV with parameter lambda.

get_prior <- function(lambda) {
  lambda^4 * exp(-lambda) / factorial(4)
}

get_posterior <- function(lambda) {
  3^10 * lambda^9 * exp(-3*lambda) / factorial(9)
}

dlambda <- 0.01
data <- c(0.23,0.8,0.12,0.35,0.5)
df <- tibble(
  lambda = seq(from=0,to=18, by=dlambda),
  prior = get_prior(lambda),
  likelihood = map_dbl(lambda, function(element) {
    dexp(data, rate=element) %>% prod()
    }),
  num = prior*likelihood,
  post = num/sum(num*dlambda)
)

#visualize the prior
df %>% ggplot(aes(x=lambda)) +
  stat_function(fun=get_prior) +
  stat_function(fun=get_posterior, color="blue", linetype=5) +
  geom_line(aes(y=post), colour="red", linetype=4)
