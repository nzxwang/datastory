library(tidyverse)

# MIT basketball team's scores are ~N(theta, sigma^2).

data <- c(59,62,59,74,70,61,62,66,62,75)

n <- data %>% length()
sample_mean <- data %>% mean()
sample_sd <- data %>% sd()

# a) This CI means in 95% of experiments, the random interval will contain true theta.
t_0.95CI <- sample_mean + sample_sd/sqrt(n) * c(qt(0.05/2, df=n-1, lower.tail=TRUE),
                                              qt(0.05/2, df=n-1, lower.tail=FALSE))

# b) Suppose the scores are ~N(theta, 25)
z_0.95CI <- sample_mean + 5/sqrt(n) * c(qnorm(0.05/2, lower.tail=TRUE), qnorm(0.05/2, lower.tail=FALSE))

# c) Suppose the scores X~N(theta,25) and theta~N(60,16)

dtheta <- 0.01
df <- tibble(
  theta = seq(from=45, to=75, by=dtheta),
  prior = dnorm(theta, mean=60, sd=4),
  likelihood = map_dbl(theta, function(theta) {
    dnorm(data, mean=theta, sd=5) %>% prod()
  }),
  num = prior*likelihood,
  post = num/sum(num),
  cum_post = cumsum(post)
)

`0.95PI` <- c(df %>% filter(cum_post<0.025) %>% tail(1) %>% .$theta,
              df %>% filter(cum_post<0.975) %>% tail(1) %>% .$theta)
