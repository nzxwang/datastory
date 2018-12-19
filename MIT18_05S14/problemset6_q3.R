library(tidyverse)

# jane is late to a random class by X hours, uniformly distributed over [0,theta].
# Jon's prior belief ~U(0,1), since the class is one hour long.

# Jane is late by 0.5, then theta must be greater than 0.5
dtheta=0.001
x=0.5
likelihood <- function(theta, x) {
  ifelse(x<=theta, 1/theta, 0)
}
df <- tibble(
  theta = seq(from=0.01, to=1, by=dtheta),
  prior = 1,
  likelihood = likelihood(theta,x),
  num = prior * likelihood,
  post = num / sum(num*dtheta)
  #post = -1/theta / log(x)
)
df %>% ggplot() + 
  geom_line(aes(theta,post))

# Jane is late by c(0.1,0.2,0.3,0.4,0.5). Assume independence.
# Still, theta must be greater than 0.5
x2=c(0.1,0.2,0.3,0.4,0.5)
#x2=c(0.1,0.5)
likelihood2 <- function(theta, x) {
  ifelse(max(x)<=theta, 1/theta^length(x), 0)
}
df2 <- tibble(
  theta = seq(from=0.01, to=1, by=dtheta),
  prior = 1,
  likelihood = likelihood2(theta,x2),
  num = prior * likelihood,
  post = num / sum(num*dtheta)
  #post = -1/theta / log(x)
)
df2 %>% ggplot() + 
  geom_line(aes(theta,post))

# find the posterior predictive probability that Jane is less than 30min late to the next class

df_ppp <- df2 %>% 
  mutate(likelihood3 = 1/theta) #remember theta>x

# multiply by 0.5 because integrate from 0.5 to 1.
ppp <- sum(df_ppp$likelihood3 * df_ppp$post * dtheta) *0.5 

#the best MAP estimate for theta is obviously max(x).
#find the conditional expectation - expected value of posterior distribution

cond_exp <- sum(df2$post * df2$theta * dtheta)
df2 %>% ggplot() + 
  geom_line(aes(theta,post)) + 
  geom_vline(xintercept = df2$theta[which.max(df2$post)], color="blue") +
  geom_vline(xintercept = cond_exp, color="red")

#the conditional expectation is always bigger than MAP and allows space for being a bit more late depending on how fast the post falls