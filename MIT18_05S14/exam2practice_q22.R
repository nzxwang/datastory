library(tidyverse)

# 60 centre's each test 12 subjects
# each subject's success is ~Bern(theta),

# a) each centre P(X=x)~Binom(12,theta).

# b) by hand, the analytic MLE estimate for theta is sample_mean/12, which also makes intuitive sense

df <- tibble(
  x = seq(0,12),
  counts = c(4,15,17,10,8,6, rep(0,6))
)

# c)
sample_mean <- sum(df$x * df$counts)/60
# d)
MLE_theta <- sample_mean/12

# e) chisq goodness of fit to test the assumption that the probability of success is the same at each center
df2 <- df %>% mutate(
  expected_counts = dbinom(x,size=12,prob=MLE_theta)*60
)
# Likelihood ratio statistic
G <- 2 * sum(df2$counts * log(df2$counts/df2$expected_counts) )
X2 <- sum((df2$counts-df2$expected_counts)^2/df2$expected_counts)
p_G <- pchisq(G,df=4,lower.tail=FALSE)
p_X2 <- pchisq(X2,df=4,lower.tail=FALSE)

# Either way, do not reject the nulll hypothesis the each centr was drawn from Binom(12,MLE_theta)