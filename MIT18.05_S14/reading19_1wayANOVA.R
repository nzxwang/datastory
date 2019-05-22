library(tidyverse)

# A one-way ANOVA(F-test for equal means) tests if population means from n groups of m samples each, are all the same.
# Assume each group is an independent normal sample drawn from distributions with possibly different means and the same variance.
# H0 all means are identical. HA not all means are identical
# Under the null distribution, W ~ F(n-1, n(m-1))

data <- c(2,4,1,5,3,
          3,4,6,1,4,
          2,1,3,3,5) %>% matrix(ncol=3)

m <- data %>% dim() %>% .[1]
n <- data %>% dim() %>% .[2]

group_means <- data %>% colMeans()
grand_mean <- group_means %>% mean()
group_var <- data %>% apply(MARGIN=2, FUN=var)

btwn_group_var <- m * group_means %>% var()
mean_within_group_var <- group_var %>% mean()

w <- btwn_group_var / mean_within_group_var
p <- pf(w, df1=n-1, df2=n*(m-1), lower.tail=FALSE)

tibble(x = c(0,10)) %>%
  ggplot(aes(x=x)) + 
  stat_function(fun= function(x) (df(x,df1=n-1, df2=n*(m-1)))) + 
  stat_function(fun= function(x) (df(x,df1=n-1, df2=n*(m-1))),
                xlim = c(10,p),
                geom = "area", alpha=0.5)

#alternatively using aov()
df <- tibble(
  procedure = c(rep("T1",5),rep("T2",5),rep("T3",5)),
  pain = data[1:15]
)
aov(pain~procedure, data=df) %>% summary()