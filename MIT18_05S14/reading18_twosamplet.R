library(tidyverse)

#assume we have two sets of data from normal distributions with unknown means, and the same unknown variance.

#H0 they have the same mean
#HA they have different means
mean_m <- 39.08
var_m <- 7.77
n_m <- 775
  
mean_e <- 39.6
var_e <- 4.95
n_e <-633
  
pooled_variance <- ((n_m-1)*var_m + (n_e-1)*var_e) / (n_m+n_e+2) 
estimated_var_of_diff_of_means <- pooled_variance* (n_m^-1+n_e^-1)

t <- (mean_m - mean_e) / estimated_var_of_diff_of_means^0.5
p <- pt(q=t, df=n_m+n_e-2, lower.tail=TRUE) + pt(q=-t, df=n_m+n_e-2, lower.tail=FALSE)
# p=0.00014<0.05 so we reject H0.

#at df=1406, the t-distribution is essentially normal.
p_normal <- pnorm(t, lower.tail=TRUE) + pnorm(-t, lower.tail=FALSE)
# (p_normal - p) / p = -0.0414283