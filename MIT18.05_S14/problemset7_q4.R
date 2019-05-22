library(tidyverse)

#Each radar gun's reading is ~N(mew, 5^2)
#Average radar gun reading is ~N(mew,5^2/3)
#H0 is mew=40

#4% of tickets are wrongly given out. (significance level)
threshold <- qnorm(0.04, mean=40, sd=sqrt(5^2/3), lower.tail=FALSE)
xlim<-c(25,55)
tibble(x = xlim) %>%
  ggplot(aes(x=x)) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/3)))) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/3))),
                xlim = c(max(xlim),threshold),
                geom = "area")

#HA is mew=45
tibble(x = xlim) %>%
  ggplot(aes(x=x)) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/3)))) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/3))),
                xlim = c(max(xlim),threshold),
                geom = "area")+ 
  stat_function(fun= function(x) (dnorm(x,mean=45,sd=sqrt(5^2/3))), colour="red") + 
  stat_function(fun= function(x) (dnorm(x,mean=45,sd=sqrt(5^2/3))),
                xlim = c(max(xlim),threshold),
                geom = "area",
                fill="red", alpha=0.5)
#the power of the test is the probability that you will catch a person speeding if they are going at 45mph.
power <- pnorm(threshold, mean=45, sd=sqrt(5^2/3), lower.tail=FALSE)

#How many cameras are needed to achieve a power of 0.9 with alpha=0.04?
#As cameras increases, threshold decreases because the null distribution becomes narrower. At the same time, the alternative hypothesis distribution becomes narrower.
df <- tibble(
  cameras = seq(from=3, to=100),
  threshold = qnorm(p=0.04, mean=40, sd=5^2/cameras, lower.tail=FALSE),
  power = pnorm(threshold, mean=45, sd=sqrt(5^2/cameras), lower.tail=FALSE)
) 
df %>% ggplot(aes(cameras,power)) +
  geom_line() +
  geom_hline(yintercept=0.9)
cameras_needed <- (df$power>0.9) %>% which() %>% min()
new_threshold <- df$threshold[[cameras_needed]]

tibble(x = xlim) %>%
  ggplot(aes(x=x)) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/cameras_needed)))) + 
  stat_function(fun= function(x) (dnorm(x,mean=40,sd=sqrt(5^2/cameras_needed))),
                xlim = c(max(xlim),new_threshold),
                geom = "area")+ 
  stat_function(fun= function(x) (dnorm(x,mean=45,sd=sqrt(5^2/cameras_needed))), colour="red") + 
  stat_function(fun= function(x) (dnorm(x,mean=45,sd=sqrt(5^2/cameras_needed))),
                xlim = c(max(xlim),new_threshold),
                geom = "area",
                fill="red", alpha=0.5)