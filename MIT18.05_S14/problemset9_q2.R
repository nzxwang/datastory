library(tidyverse)

# The height in inches of twenty college men are measured.
# Suppose it is a normal distribution with unknown mean and variance.

n<-20
sample_mean <- 69.55
sample_variance <- 14.26
sample_sd <- sample_variance %>% sqrt()

confidence <- 0.9
alpha <- 1 - confidence

# a)
tCI_0.9 <- sample_mean + c(qt(p=alpha/2, df=n-1, lower.tail=TRUE), 
                          qt(p=alpha/2, df=n-1, lower.tail=FALSE)) * sample_sd/sqrt(n)

# b) suppose we find out that sd = 3.77.
sd <- 3.77
zCI_0.9 <- sample_mean + c(qnorm(p=alpha/2, lower.tail=TRUE), 
                           qnorm(p=alpha/2, lower.tail=FALSE)) * sd/sqrt(n)
# c) in b), how many people to bring the width of 90% CI to 1inch?
width_0.9 <- 1
n_z <- (2*qnorm(p=alpha/2, lower.tail=FALSE)*sd/width_0.9)^2
#154 people.

# d) in a), how many people to bring the width of 90% CI to 1inch? Use sample_variance=14.26.
n_t <- (2*qt(p=alpha/2, df=n-1, lower.tail=FALSE)*sample_sd/width_0.9)^2
# At least 157 people, but this may be more as the sample_sd can increase.