library(tidyverse)

# Unknown mean, the data follows a normal distribution.
data <- c(352,351,361,353,352,358,360,358,359)
n <- length(data)
sd <- 3
sample_sd <- data %>% sd()
var <- sd^2
sample_mean <- data %>% mean()


# a/b)
get_zCI <- function(mean, sd, n, alpha) {
  mean + c(qnorm(alpha/2, lower.tail=TRUE), 
           qnorm(alpha/2, lower.tail=FALSE)) *(sd/sqrt(n))
}
zCI_0.95 <- get_zCI(sample_mean, sd, n, 0.05)
zCI_0.98 <- get_zCI(sample_mean, sd, n, 0.02)

# c)
get_tCI <- function(mean, sd, n, alpha) {
  mean + c(qt(alpha/2, df=n-1, lower.tail=TRUE),
           qt(alpha/2, df=n-1, lower.tail=FALSE))*(sd/sqrt(n))
}
tCI_0.95 <- get_tCI(sample_mean, sample_sd, n, 0.05)
tCI_0.98 <- get_tCI(sample_mean, sample_sd, n, 0.02)