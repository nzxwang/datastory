library(tidyverse)

# prior knowledge on length of the cable is Theta~N(9,1).
# measurement of the cable has error with variance=10^(-4)
# ie. likelihood of getting measurement=x is X~N(theta,1e-4)

dtheta <- 0.01
mean_theta <- 9
actual_theta <- tibble(
  seq = seq(mean_theta-4,mean_theta+4, by=dtheta),
  prior = dnorm(x=seq, mean=mean_theta, sd=1)
)

measurement <- tibble(
  x = seq(mean_theta-4,mean_theta+4, by=dtheta),
  likelihood = map_dbl(x, function(x) {
    (dnorm(x=x, mean=actual_theta$seq)*actual_theta$prior*dtheta) %>% sum()
  })
)

# we calculated the likelihood of each X=x, which isn't what the question asked.
measurement %>% ggplot() + geom_line(aes(x=x,y=likelihood), colour="red") +
  geom_line(data=actual_theta, aes(x=seq,y=prior))

# a) posterior pdf given data: x=10?
prior_var <- 1
measurement_var <- 1e-4
post_var <- (1/prior_var + 1/measurement_var)^(-1)
mean_post <- (mean_theta/prior_var + 10/measurement_var)*post_var

df <- tibble(
  theta = seq(mean_theta-4,mean_theta+4, by=dtheta),
  prior = dnorm(x=theta, mean=mean_theta, sd=1),
  likelihood_measure_10 = dnorm(x=10, mean=theta, sd=1e-4),
  num = prior*likelihood_measure_10,
  post = num/sum(num*dtheta)
)

# Our resolution is too low to see this...
df %>% 
  ggplot(aes(x=theta)) + 
  geom_line(aes(y=post)) +
  xlim(mean_post+1e-2*c(-1,1))

# The measurement variance is so small that likelihood overtakes the prior in the event that x=10.
tibble(x=mean_post+500*c(-post_var,post_var)) %>% 
  ggplot(aes(x=x)) +
  stat_function(fun=dnorm, args=list(mean=mean_post, sd=sqrt(post_var)))

# b) how many measurements to get posterior variance less than 1e-6?
# since likelihood of getting measurement=x is X~N(theta,1e-4), likelihood of getting average measurement=x_bar is X_bar~N(theta,1e-4/n)
target_post_var <- 1e-6
n_required <- (1/target_post_var - 1/prior_var) * measurement_var
