library(tidyverse)

n <- 49
null_mean <- 4
sample_mean <- 6.25
sample_var <- 36
sample_sd <- sample_var %>% sqrt()

# H0 is data drawn from N(4,sigma^2)
# HA is data drawn from N(mew, sigma^2), where mew =/=4
density_t <- function(x) {
  dt(x=x, df=n-1)
}

t_score <- (sample_mean - null_mean) / (sample_sd/sqrt(n))
p <- pt(-t_score, n-1, lower.tail=TRUE) + pt(t_score, n-1, lower.tail=FALSE)
t_crit <- qt(0.05/2, n-1, lower.tail=FALSE)

tibble(t=c(-4,4)) %>%
  ggplot(aes(x=t)) +
  stat_function(fun=density_t) +
  stat_function(fun=density_t, 
                xlim=c(-4, -t_score),
                geom = "area", fill="green", alpha=0.5) +
  stat_function(fun=density_t, 
                xlim=c(t_score, 4),
                geom = "area", fill="green", alpha=0.5) +  
  stat_function(fun=density_t, 
                xlim=c(-4, -t_crit),
                geom = "area", fill="red", alpha=0.5) +
  stat_function(fun=density_t, 
                xlim=c(t_crit, 4),
                geom = "area", fill="red", alpha=0.5)

