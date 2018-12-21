library(tidyverse)

# Chi-square test for goodness of fit
# By theorem, under the null hypothesis X^2 is close to G, and both are approximately chi-square.
# H0 data is from binom(8,0.5). HA data is not from binom(8,0.5).

df <- tibble(
  outcomes = ordered(c(0,1,2,3,4,">=5"), levels=c(0,1,2,3,4,">=5")),
  observed_counts = c(3,10,15,13,7,3),
  null_probabilities = c(dbinom(c(0,1,2,3,4), size=8, prob=0.5),
                         pbinom(4, size=8, prob=0.5, lower.tail=FALSE)),
  expected_counts = null_probabilities * sum(observed_counts)
)

df %>% ggplot(aes(x=outcomes)) + 
  geom_col(aes(y=observed_counts)) +
  geom_point(aes(y=expected_counts))
