library(tidyverse)

# Chi-square test for goodness of fit
# By theorem, under the null hypothesis X^2 is close to G, and both are approximately chi-square.
# H0 is that the data was drawn from Mendel's distribution.

df <- tibble(
  color= c("yellow", "yellow", "green","green"),
  skin = c("smooth", "wrinkled", "smooth", "wrinkled"),
  observed_count = c(315,102,108,31),
  expected_probability = c(9/16,3/16,3/16,1/16),
  expected_count = expected_probability * sum(observed_count)
)

G <- 2 * sum( df$observed_count * log(df$observed_count/df$expected_count) )
X2 <- sum( (df$observed_count-df$expected_count)^2 / df$expected_count )

p <- pchisq(G,df=3, lower.tail=FALSE)
