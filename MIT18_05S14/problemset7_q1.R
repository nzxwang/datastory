library(tidyverse)

# A coin is spun 250 times.
# The data is 140 heads.

#null hypothesis
theta <- 0.5


likelihood_H0 <- pbinom(q=139, size=250, prob=theta, lower.tail=FALSE)
# a) likelihood_H0 * 2 is 6.6%; the probability of at least as extreme is 6%. 
# b) if alpha=0.1, can reject H_0. alpha=0.05 cannot reject H_0.

# c) How many heads to reject at alpha=0.01? >145 heads! 
num_heads_0.01 <- qbinom(0.01/2, size=250, prob=theta, lower.tail=FALSE)

# d) alpha = 0.05. What is the probability of getting a result extreme enough to reject the null hypothesis at a significance of 0.05, given theta=0.55? 0.60?
left <- qbinom(0.05/2, size=250, prob=theta, lower.tail=TRUE)
right <- qbinom(0.05/2, size=250, prob=theta, lower.tail=FALSE)

power_0.55 <- pbinom(q=left, size=250, prob=0.55, lower.tail=TRUE) + pbinom(q=right, size=250, prob=0.55, lower.tail=FALSE)
power_0.60 <- pbinom(q=left, size=250, prob=0.60, lower.tail=TRUE) + pbinom(q=right, size=250, prob=0.60, lower.tail=FALSE)

df <- tibble(
  x = seq(from=80, to=170),
  likelihood_null = dbinom(x, size=250, prob=0.5),
  likelihood_0.55 = dbinom(x, size=250, prob=0.55),
  likelihood_0.60 = dbinom(x, size=250, prob=0.60)
)

df %>% ggplot() + 
  geom_vline(xintercept=c(left,right)) +
  geom_line(aes(x=x, y=likelihood_null), colour = "yellow") +
  geom_line(aes(x=x, y=likelihood_0.55), colour = "orange") +
  geom_line(aes(x=x, y=likelihood_0.60), colour = "red") 

# e) alpha = 0.05. How many spins to get a power of 0.9 given H_A=0.9?
for (spins in 250:1200){
  left <- qbinom(0.05/2, size=spins, prob=theta, lower.tail=TRUE)
  right <- qbinom(0.05/2, size=spins, prob=theta, lower.tail=FALSE)
  power_0.55 <- pbinom(q=left, size=spins, prob=0.55, lower.tail=TRUE) + pbinom(q=right, size=spins, prob=0.55, lower.tail=FALSE)
  if (power_0.55>0.9) {
    spins <- spins
    break}
}
df <- tibble(
  x = seq(from=round(left*0.8), to=round(right*1.2)),
  likelihood_null = dbinom(x, size=spins, prob=0.5),
  likelihood_0.55 = dbinom(x, size=spins, prob=0.55)
)
df %>% ggplot() + 
  geom_vline(xintercept=c(left,right)) +
  geom_line(aes(x=x, y=likelihood_null), colour = "yellow") +
  geom_line(aes(x=x, y=likelihood_0.55), colour = "orange")

# f) H_A=0.55. Suppose these are the only two hypotheses with flat priors. What is the posterior probability of H_A?
P_HA <- 0.5
P_H0 <- 0.5
P_data_HA <- dbinom(140,250,0.55)
P_data_H0 <- dbinom(140,250,0.50)
P_HA_given_data <- P_data_HA*P_HA / (P_data_HA*P_HA + P_data_H0*P_H0)
# This is so much more intuitive... What possibility would I personally put on the coin being biased towards heads?
dtheta <- 0.01
df <- tibble(
  theta = seq(from=0, to=1, by=dtheta),
  prior = 1,
  likelihood = dbinom(x=140, size=250, prob=theta),
  num = prior*likelihood,
  post = num / sum(num)
)
df %>% ggplot(aes(theta, post)) + geom_line()
df %>% filter(theta>0.5) %>% .$post %>% sum()
