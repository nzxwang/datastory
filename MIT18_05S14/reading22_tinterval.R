library(tidyverse)

# Suppose data is drawn from ~N(mew,sigma^2), mew and sigma are unknown
data <- c(2.5,5.5,8.5,11.5)
mean <- data %>% mean()
sd <- data %>% sd()

# Then the average is ~T(mew, sigma^2/4)
distance_0.95 <- qt(0.05/2, df=3, lower.tail=FALSE)
distance_0.80 <- qt(0.20/2, df=3, lower.tail=FALSE)
distance_0.50 <- qt(0.5/2, df=3, lower.tail=FALSE)

interval_0.95 <- distance_0.95 * c(-sd,sd)/sqrt(4) + mean
interval_0.80 <- distance_0.80 * c(-sd,sd)/sqrt(4) + mean
interval_0.50 <- distance_0.50 * c(-sd,sd)/sqrt(4) + mean