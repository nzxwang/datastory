library(tidyverse)

# Bob collects 15 tix/hour, Alice collects 10tix/hour
# Over 5h, 12, 10, 11, 4, 11 tix are collected
data = c(12,10,11,4,11)

likelihood_Bob <- dpois(data, lambda=15) %>% prod()
likelihood_Alice <- dpois(data, lambda=10) %>% prod()
odds_Alice_to_Bob <- 1/10

post_odds_Alice_to_Bob <- likelihood_Alice / likelihood_Bob * odds_Alice_to_Bob
bayes_factor <- likelihood_Alice / likelihood_Bob

#visualize why 
df <- tibble(
  tix = seq(0,30),
  likelihood_Alice = dpois(tix, lambda=10),
  likelihood_Bob = dpois(tix, lambda=15)
)
df %>% ggplot(aes(x=tix)) +
  geom_line(aes(y=likelihood_Alice), colour="red") +
  geom_line(aes(y=likelihood_Bob), colour="blue") +
  geom_vline(xintercept=data, alpha=0.5,)
