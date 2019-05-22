library(tidyverse)

# Problem 1
bayesfactor <- dbinom(x=140, size=250, prob=0.5)/dbinom(x=140,size=250,prob=14/25)

# which is the most reasonable prior?
dtheta=0.01
df <- tibble(
  theta = seq(from=0, to=1, by=dtheta),
  flat = dbeta(theta, 1,1),
  prior1 = dbeta(theta,10,10),
  prior2 = dbeta(theta,50,50),
  prior3 = dbeta(theta,500,500),
  prior4 = dbeta(theta,30,70)
)
df %>% ggplot(aes(x=theta)) +
  geom_line(aes(y=flat), color='red') +
  geom_line(aes(y=prior1), color='pink') +
  geom_line(aes(y=prior2), color='blue') +
  geom_line(aes(y=prior3), color='green') +
  geom_line(aes(y=prior4), color='yellow')
# prior2 is probably most reasonable. it isnt too strong and its middle

df <- df %>% 
  mutate(likelihood = dbinom(x=140, size=250, prob=theta),
         num = flat * likelihood,
         num1 = prior1 * likelihood,
         num2 = prior2 * likelihood,
         num3 = prior3 * likelihood,
         num4 = prior4 * likelihood,
         post = flat* likelihood / sum(flat*likelihood*dtheta),
         post2 = prior2 *likelihood / sum(prior2*likelihood*dtheta),
         post3 = prior3 *likelihood / sum(prior3*likelihood*dtheta),
         post4 = prior4 *likelihood / sum(prior4*likelihood*dtheta),
         post1 = prior1 *likelihood / sum(prior1*likelihood*dtheta)
)
df %>% ggplot(aes(x=theta)) +
  geom_line(aes(y=flat), color='red', alpha=0.7) +
  geom_line(aes(y=prior1), color='pink', alpha=0.7) +
  geom_line(aes(y=prior2), color='blue', alpha=0.7) +
  geom_line(aes(y=prior3), color='green', alpha=0.7) +
  geom_line(aes(y=prior4), color='yellow', alpha=0.7) +
  geom_line(aes(y=post), color='red') +
  geom_line(aes(y=post1), color='pink') +
  geom_line(aes(y=post2), color='blue') +
  geom_line(aes(y=post3), color='green') +
  geom_line(aes(y=post4), color='yellow') +
  geom_vline(xintercept=0.5)

bias <- c(
  pbeta(0.5, 141, 111, lower.tail = FALSE),
  pbeta(0.5, 150, 120, lower.tail = FALSE),
  pbeta(0.5, 190, 160, lower.tail = FALSE),
  pbeta(0.5, 640, 610, lower.tail = FALSE),
  pbeta(0.5, 170, 180, lower.tail = FALSE)
)

#H1 is theta [0.55,0.57]
#H0 is theta [0.49,0.51]
P_D_given_H1 <- pbeta(0.57, 141, 111) - pbeta(0.55, 141, 111)
P_D_given_H0 <- pbeta(0.51, 141, 111) - pbeta(0.49, 141, 111)
P_H1_prior_i <- pbeta(0.57, 1, 1) - pbeta(0.55, 1, 1)
P_H0_prior_i <- pbeta(0.51, 1, 1) - pbeta(0.49, 1, 1)
odds_i <- P_D_given_H1 * P_H1_prior_i / (P_D_given_H0 * P_H0_prior_i)

P_H1_prior_iv <- pbeta(0.57, 641, 611) - pbeta(0.55, 641, 611)
P_H0_prior_iv <- pbeta(0.51, 641, 611) - pbeta(0.49, 641, 611)
odds_iv<- P_D_given_H1 * P_H1_prior_iv / (P_D_given_H0 * P_H0_prior_iv)

#odds_iv is so much smaller than odds_i because the prior_iv is so  much stronger than the data, whereas prior_i is pretty much nothing.