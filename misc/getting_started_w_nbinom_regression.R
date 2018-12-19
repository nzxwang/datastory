library(tidyverse)

# https://data.library.virginia.edu/getting-started-with-negative-binomial-regression-modeling/

# Table 13.6 | Agresti, p. 561
black <- c(119,16,12,7,3,2,0)
white <- c(1070,60,14,4,0,0,1)
resp <- c(rep(0:6,times=black), rep(0:6,times=white))
race <- factor(c(rep("black", sum(black)), rep("white", sum(white))), levels = c("white","black"))
victim <- tibble(resp, race)

race %>% table()
#most respondents are white
# white black 
# 1149   159 

victim %>% group_by(race) %>% summarise(mean(resp))
# blacks have a higher mean count than whites
# race  `mean(resp)`
# <fct>        <dbl>
# 1 white       0.0923
# 2 black       0.522 


victim %>% group_by(race) %>% summarise(var(resp))
# Variance is double the mean. We have overdispersion!
# race  `var(resp)`
# <fct>       <dbl>
# 1 white       0.155
# 2 black       1.15 

victim %>% table()
#    race
# resp white black
# 0  1070   119
# 1    60    16
# 2    14    12
# 3     4     7
# 4     0     3
# 5     0     2
# 6     1     0

# Poisson model
pGLM <- glm(resp ~ race, data=victim, family = poisson)
summary(pGLM)
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -2.38321    0.09713  -24.54   <2e-16 ***
#   raceblack    1.73314    0.14657   11.82   <2e-16 ***

coef(pGLM)[2] %>% exp()
# raceblack 
# 5.658419