library(tidyverse)

n <- 49
sample_mean <- 6.25
sample_var <- 12
sample_sd <- sample_var %>% sqrt()

# H0 is data drawn from N(4,10^2)
# HA is data drawn from N(mew, 10^2), where mew =/=4

null_mean <- 4
null_var <- 10

standard_error <- sample_sd / sqrt(n)
z_score <- (sample_mean - null_mean)/(null_var/sqrt(n))
p <- pnorm(-z_score, lower.tail=TRUE) + pnorm(z_score, lower.tail=FALSE)

tibble(z=seq(from=-4,to=4,by=0.1)) %>%
  ggplot(aes(x=z)) +
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm, 
                xlim=c(-4, -z_score),
                geom = "area") +
  stat_function(fun=dnorm, 
                xlim=c(z_score, 4),
                geom = "area")