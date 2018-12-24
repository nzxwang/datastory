library(tidyverse)

#bag full of 4, 6, 8, 12, 20-sided dice, in proportions 1:2:10:2:1

df <- tibble(
  hypothesis = factor(c("4","6","8","12","20"), levels=c("4","6","8","12","20")),
  number_of_each = c(1,2,10,2,1),
  prior = number_of_each / sum(number_of_each),
  likelihood_two5s = c(0,1/6^2, 1/8^2, 1/12^2,1/20^2),
  numerator = prior*likelihood_two5s,
  posterior = numerator / sum(numerator)
)

df %>% ggplot(aes(x=hypothesis)) +
  geom_point(aes(y=prior)) + 
  geom_col(aes(y=posterior), alpha=0.3, fill="blue")
# 4-sided dice loses all its probability. Both the 6,8 die gain probability.

#What is the probability of getting another 5? a 15?
df <- df %>% mutate(
  likelihood_5 = c(0,1/6, 1/8, 1/12,1/20),
  likelihood_15 = c(0,0,0,0,1/20),
)
ppd_another5 <- sum(df$posterior * df$likelihood_5)
ppd_15 <- sum(df$posterior * df$likelihood_15)
