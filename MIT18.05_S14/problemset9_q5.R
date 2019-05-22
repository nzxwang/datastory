library(tidyverse)

n <- 10
sample_var <- 4.2
sample_sd <- sample_var %>% sqrt()

get_chiCI <- function(sample_var, n, alpha) {
  (n-1) * sample_var / c(qchisq(alpha/2, df=n-1,lower.tail=FALSE),
                        qchisq(alpha/2, df=n-1,lower.tail=TRUE))
}
# a)
sd_chiCI <- get_chiCI(sample_var,n,0.05) %>% sqrt()

# b) assume each pack of candy is independent and is normally distributed