library(tidyverse)

# Suppose X~Binom(12,theta).
# The rejection column is TRUE for whenever the lower_cdf or upper_cdf is less than 0.05, ie falls outside the alpha=0.1 region.
dtheta <- 0.1
df <- expand.grid(theta = seq(from=0, to=1, by=dtheta),
            x = seq(from=0,to=12,by=1)) %>%
  as_tibble() %>%
  mutate(likelihood = dbinom(x=x, size=12, prob=theta),
         lower_cdf = pbinom(q=x, size=12, prob=theta, lower.tail=TRUE),
         # x-1 to include x in this calculation.
         upper_cdf = pbinom(q=x-1, size=12, prob=theta, lower.tail=FALSE),
         reject = case_when(
           (lower_cdf < 0.05 | upper_cdf < 0.05) ~ TRUE,
           (lower_cdf > 0.05 & upper_cdf > 0.05) ~ FALSE)
)

# This recreates Table4 using a normal approximation of the binomial confidence interval.
df %>% ggplot() + geom_tile(aes(x=x,y=theta, fill=reject), colour="black")

# According to the table (WHICH USES THE NORMAL APPROXIMATION), the 0.90 CI when x=8 is theta in [0.4,0.8]

# Why is the expected type one CI error at most 0.092, given that the true value of theta is in the table?
df %>%
  filter(reject==TRUE) %>%
  group_by(theta) %>%
  summarise(significance = sum(likelihood))

# Actually, according to this table where the normal approximation is used, the expected type one CI error is at most 0.0524, at theta=0.3 and theta=0.7. The table only carries finite values of theta.

# Just for visualization.
df %>% ggplot() + geom_tile(aes(x=x,y=theta, fill=likelihood), colour="black")
