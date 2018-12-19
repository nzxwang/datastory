library(tidyverse)

# A paired two-sample t-test is just a one-sample t-test for the differences between the two samples.
# H0 mean_after - mean_before = 0
# HA mean_after - mean_before =/= 0

# does treatment do anything?
data <- tibble(
  before = c(25,25,27,44,30,67,53,53,52,60,28),
  after = c(27,29,37,56,46,82,57,80,61,59,43),
  difference = after - before
)

n <- data$difference %>% length()
mean <- data$difference %>% mean()
sd <- data$difference %>%  sd()
t <- (mean-0) / (sd/sqrt(n))
p <- 2*(1-pt(q=abs(t), df=n-1, lower.tail=TRUE))
# p=0.001633<0.05.

result = t.test(data$after, data$before, alternative ="two.sided", mu=0, paired=TRUE)
