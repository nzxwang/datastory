library(tidyverse)

# particles decay at a distance X~exp(lambda).
# scientists can only observe decay events for x in [1,20].

# a) integrating by hand, the probability that an emitted particle decays in the window of detection is 
# P_detected = exp(-lambda) - exp(-20*lambda)

# b) the likelihood of observing a particle at x, is lambda*exp(-lambda*x)/P_detected

# c)
obs <- c(5,11,13,14)
mean_obs <- obs %>% mean()

df <- tibble(
  lambda = seq(from=1/30, to=1/5, length.out=1000),
  prior = 1/(25*lambda^2),
  likelihood_analytic = lambda^4*exp(-43*lambda)/ (exp(-lambda) - exp(-20*lambda))^4,
  likelihood = map_dbl (lambda, function (lambda)
    {(dexp(obs,lambda) /(exp(-lambda) - exp(-20*lambda))) %>% prod()}),
  likelihood_wo_censoring = map_dbl (lambda, function (lambda)
  {(dexp(obs,lambda)) %>% prod()}),
  num = prior*likelihood,
  post = num/(sum(num)*(lambda[[2]]-lambda[[1]])),
  post_wo_censoring = likelihood_wo_censoring*prior / sum((likelihood_wo_censoring*prior)*(lambda[[2]]-lambda[[1]]))
)

# as expected, the peak is at the mean of the observations
df %>% ggplot(aes(x=lambda)) + 
  geom_line(aes(y=post)) +
  geom_line(aes(y=post_wo_censoring))

posterior_mean_greater_than_10 <- df %>% filter(lambda<1/10) %>% .$post %>% sum()
posterior_mean_not_greater_than_10 <- df %>% filter(lambda>=1/10) %>% .$post %>% sum()
posterior_odds_mean_greater_than_10 <- posterior_mean_greater_than_10 %>% `/`(posterior_mean_not_greater_than_10)

# sanity check that the distributions sum to 1
sum_of_prior <- sum(df$prior) *(df$lambda[[2]]-df$lambda[[1]])
sum_of_post <- sum(df$post) *(df$lambda[[2]]-df$lambda[[1]])

df %>% ggplot(aes(x=lambda)) + 
  geom_line(aes(y=likelihood_wo_censoring)) +
  geom_line(aes(y=likelihood))

df %>% ggplot(aes(x=lambda)) + geom_line(aes(y=prior))