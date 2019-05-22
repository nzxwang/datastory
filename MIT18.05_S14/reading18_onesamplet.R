library(tidyverse)

#By theorem, if the data is normal, then the studentized mean follows a t-distribution.
#H0 mean=0; HA mean>0. alpha=0.05
data <- c(1,2,3,6,-1)
n <- length(data)
sample_mean <- mean(data)
sample_var <- var(data)
sample_sd <- sd(data)
studentized_mean <- (mean(data) - 0) / (sample_sd/sqrt(n))

p <- pt(studentized_mean, df=n-1, lower.tail=FALSE)
# p=0.0066<0.05 so reject H0.