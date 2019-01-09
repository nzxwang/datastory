library(tidyverse)

# A coin is spun 250 times and lands heads 140 times.
# Recall from set7q1, P(coin is fair|data) = 0.066.
size <- 250
df <- crossing(
  theta = c(0.5,0.3,0.1),
  x = seq(from=0, to=250, by=1)
) %>%
  mutate(likelihood = dbinom(x=x, size=250, prob=theta),
         mean = theta*250,
         var = 250 * theta * (1-theta),
         normal = dnorm(x=x, mean=mean, sd=sqrt(var)),
         normal_rot = dnorm(x=x,mean=mean,sd=sqrt(250/4))
         )
df_tidy <- df %>% gather(key=plot, value=probability, -x, -mean, -var, -theta)

df_tidy %>% 
  ggplot(aes(x=x)) + 
  geom_line(aes(y=probability, colour=plot), position='jitter') +
  xlim(0, 175) + 
  facet_grid(~theta)

# a) the normal distribution with the same mean and variance visually overlaps the likelihood. the normal distribution with the rule of thumb variance is bad for lower theta since it assumes maximum theta=0.5.

# b) In the rule-of-thumb approximation, the mean ~ N(theta, 250/4).
CI <- crossing(
  theta = c(0.5,0.3,0.1),
  confidence = c(0.80,0.95)
) %>%
  mutate(
    alpha = 1-confidence,
    lb = theta + qnorm(alpha/2, lower.tail=TRUE)/(2*sqrt(250)),
    ub = theta + qnorm(alpha/2, lower.tail=FALSE)/(2*sqrt(250))
  )
# oops... the CI doesn't depend on the hypothesis of theta.
CI_0.8 <- 140/250 + c(qnorm(0.2/2, lower.tail=TRUE)/(2*sqrt(250)), qnorm(0.2/2, lower.tail=FALSE)/(2*sqrt(250)))
CI_0.95 <- 140/250 + c(qnorm(0.05/2, lower.tail=TRUE)/(2*sqrt(250)), qnorm(0.05/2, lower.tail=FALSE)/(2*sqrt(250)))

# c) Find an 80% posterior probability interval centered around 0.1 and 0.9 for theta using a flat prior.

x <- 140
dtheta <- 0.01
bayes <- tibble(
  theta = seq(from=0, to=1,by=dtheta),
  prior = dbeta(theta,1,1), #this is flat
  likelihood = dbinom(x=x, size=size, prob=theta),
  num = prior*likelihood,
  posterior = num /sum(num)
)
PPI_0.8 <- c(qbeta(0.1,141,111,lower.tail=TRUE), qbeta(0.1,141,111,lower.tail=FALSE))
# This is super close to the 0.8 CI, though it has a different technical meaning.
bayes %>%
  filter(posterior>0.0005) %>%
  ggplot(aes(x=theta)) + 
  geom_col(aes(y=posterior)) +
  geom_vline(aes(xintercept=PPI_0.8[1])) + 
  geom_vline(aes(xintercept=PPI_0.8[2]))
