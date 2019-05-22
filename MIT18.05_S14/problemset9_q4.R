library(tidyverse)

data <- c(6.0,6.4,7.0,5.8,6.0,5.8,5.9,6.7,6.1,6.5,6.3,5.8)

# in order to make a CI estimate, we have to assume each trial is independent and normally distributed, even though it is pretty horrible...
sample_mean <- data %>% mean()
sample_sd <- data %>% sd()
data %>%
  as_tibble() %>%
  ggplot(aes(x=value)) +
  geom_histogram(aes(x=value,y=..count../sum(..count..*0.1)), binwidth=0.1) +
  stat_function(fun = function(x) {
    dnorm(x=x, mean = sample_mean, sd = sample_sd)
    })

get_chiCI <- function(data, alpha) {
  n <- length(data)
  (n-1) * var(data) / c(qchisq(alpha/2, df=n-1,lower.tail=FALSE),
                        qchisq(alpha/2, df=n-1,lower.tail=TRUE))
}
chiCI_0.95 <- get_chiCI(data,0.05)
