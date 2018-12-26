library(tidyverse)

# Suppose data is drawn from ~N(mew,10^2), mew is unknown
data <- c(2.5,5.5,8.5,11.5)
mean <- data %>% mean()

# Then the average is ~N(mew, 10^2/4)
left_0.95 <- qnorm(p=0.05/2, mean=7, sd=sqrt(10^2/4))
right_0.95 <- qnorm(p=0.05/2, mean=7, sd=sqrt(10^2/4), lower.tail=FALSE)
left_0.80 <- qnorm(p=0.2/2, mean=7, sd=sqrt(10^2/4))
right_0.80 <- qnorm(p=0.2/2, mean=7, sd=sqrt(10^2/4), lower.tail=FALSE)
left_0.5 <- qnorm(p=0.5/2, mean=7, sd=sqrt(10^2/4))
right_0.5 <- qnorm(p=0.5/2, mean=7, sd=sqrt(10^2/4), lower.tail=FALSE)

# b) For H0: mew0=1, it would not be rejected at alpha=0.95 and alpha-0.80, but it would be rejected at alpha=0.5, as it falls within [-2,8,16.8], [0.59,13.4], but is outside of the confidence interval [3.6,10.4].

# Alternatively, we can construct rejection regions around mew0=1.
# Under the null hypothesis, the test statistic is drawn from ~N(mew0=1,10^2)
dx <- 0.01
df <- tibble(
  x <- seq(-9,11,by=dx),
  
)
tibble(x=seq(1-18,1+18)) %>% 
  ggplot(aes(x=x)) + 
  stat_function(fun = function(x) {dnorm(x, mean=1, sd=sqrt(10^2/4))}) +
  geom_vline(xintercept=mean) +
  geom_vline(xintercept=c(
    qnorm(p=0.05/2, mean=1, sd=sqrt(10^2/4)),
    qnorm(p=0.05/2, mean=1, sd=sqrt(10^2/4), lower.tail=FALSE)
  ), colour="red") +  
  geom_vline(xintercept=c(
    qnorm(p=0.2/2, mean=1, sd=sqrt(10^2/4)),
    qnorm(p=0.2/2, mean=1, sd=sqrt(10^2/4), lower.tail=FALSE)
  ), colour="orange") + 
  geom_vline(xintercept=c(
    qnorm(p=0.5/2, mean=1, sd=sqrt(10^2/4)),
    qnorm(p=0.5/2, mean=1, sd=sqrt(10^2/4), lower.tail=FALSE)
  ), colour="yellow")

    