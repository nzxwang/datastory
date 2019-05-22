library(tidyverse)

# Jerry gets THTTHTTTTTTH, 3 heads and 9 tails.
# H0 theta=0.5, HA theta<0.5.
# Perform a test at alpha=0.05.

#a) Experiment1: count the number of heads in 12 flips.
expt1 <- tibble(
  tails = seq(0,12),
  null_distrib = dbinom(x=tails, size=12,prob=0.5)
)
threshold1 <- qbinom(0.05,size=12,prob=0.5,lower.tail=FALSE)
expt1 %>% ggplot() + geom_point(aes(tails,null_distrib)) +
  geom_vline(xintercept=threshold1)
p1 <- pbinom(8, size=12,prob=0.5, lower.tail=FALSE)
# At 9 tails, we are in the non-rejection region (p=0.07). we need MORE than 9 tails to reject the null hypothesis at alpha=0.05

#b) Experiment2: stop after the third heads and report the number of tails.
expt2 <- tibble(
  tails_before_heads = seq(0,15),
  null_distrib = dnbinom(x=tails_before_heads,size=3,prob=0.5)
)
threshold2 <- qnbinom(0.05, size=3, prob=0.5, lower.tail=FALSE)
p2 <- pnbinom(8, size=3, prob=0.5, lower.tail=FALSE)
expt2 %>% ggplot() + geom_point(aes(tails_before_heads,null_distrib))+
  geom_vline(xintercept=threshold2)
# At 9 tails, we are in the rejection region (p=0.03). we need MORe than 8 tails to reject the null hypothesis at alpha=0.05

# c) Experiment3: count the number of heads in 100 flips.
# We cannot compute a p-value. What does it mean "data at least as extreme"? We need the full experiment to be specified.

# d) Bayesian Perspective. 
# Experiment1: likelihood is 12C3*theta^9*(1-theta)^3
# Experiment2: likelihood is 11C2*theta^9*(1-theta)^3
# likelihoods only differ by a constant factor so the posterior ends up being the same!
dtheta=0.05
bayes <- tibble(
  theta = seq(from=0+dtheta,to=1-dtheta,by=dtheta),
  prior = 1,
  likelihood1 = dbinom(x=9, size=12, prob=theta),
  likelihood2 = dnbinom(x=9, size=3, prob=(1-theta)),
  num1= prior * likelihood1,
  post1 = num1 / sum(num1),
  num2= prior * likelihood2,
  post2 = num2 / sum(num2)
)
bayes %>% ggplot(aes(x=theta)) + 
  geom_point(aes(y=post1), alpha=0.5, colour="red") +
  geom_point(aes(y=post2), alpha=0.5, colour="blue")
# Frequentist framework uses unseen data.