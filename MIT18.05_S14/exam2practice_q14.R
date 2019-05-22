library(tidyverse)

# theta is the percentage of students in 18.05 who prefer Bayesian statistics over frequentist statistics.
# x1 is the number of people in a sample of 10 who prefer Bayesian statistics. Then, x1 ~ Binom(10,theta).

dtheta=0.01
df <- tibble(
  theta = seq(0,1,by=dtheta),
  prior = dbeta(theta,shape1=2,shape2=2),
  likelihood_x1_is_6 = dbinom(x=6, size=10, prob=theta),
  num = prior * likelihood_x1_is_6,
  posterior = num / sum(num*dtheta),
  analytic_posterior = dbeta(theta,shape1=2+6,shape2=2+4)
)
df %>% ggplot() + geom_col(aes(x=theta,y=analytic_posterior))

# c) Calculate some probability intervals
fifty_percent_p_interval_lower <- qbeta(0.25,shape1=2+6,shape2=2+4,lower.tail=TRUE)
fifty_percent_p_interval_upper <- qbeta(0.25,shape1=2+6,shape2=2+4,lower.tail=FALSE)
ninety_percent_p_interval_lower <- qbeta(0.05,shape1=2+6,shape2=2+4,lower.tail=TRUE)
ninety_percent_p_interval_upper <- qbeta(0.05,shape1=2+6,shape2=2+4,lower.tail=FALSE)

# d) calculate MAP
MAP_theta <- df$posterior %>% which.max() %>% df$theta[[.]]

# Although the MAP estimate is 0.58, the 50% probability interval includes theta<0.5, and the 90% probability interval includes much fewer values, so it is not a strnog case for the argument that the majority of students are bayesians.
df %>% ggplot() + 
  geom_col(aes(x=theta,y=analytic_posterior)) +
  geom_vline(xintercept=c(fifty_percent_p_interval_lower,
                          fifty_percent_p_interval_upper)) +
  geom_vline(xintercept=c(ninety_percent_p_interval_lower,
                          ninety_percent_p_interval_upper), colour="red") +
  geom_vline(xintercept=MAP_theta, colour="green")

# e) what is the chance that the majority of another sample of 10 students prefer Bayesian statistics?
num_bayesians <- seq(6,10) 
probability_majority_bayesians <- num_bayesians %>% 
  map_dbl(function(num) {
    sum(dbinom(x=num, size=10, prob=df$theta) * df$posterior * dtheta)
    }) %>%
  sum()
