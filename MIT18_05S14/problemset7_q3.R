library(tidyverse)

#H0 is mew=10. take alpha=0.05.
#16 data points give sample mean =11 and sample variance=4
mean_H0<-10
mean_sample<-11
variance_sample<-4
n<-16
t <- (mean_sample-mean_H0)/(sqrt(variance_sample)/sqrt(n))

tibble(x = seq(from=-3,to=3,by=0.01)) %>%
  ggplot(aes(x=x)) + 
  stat_function(fun= function(x) (dt(x,df=15))) + 
  stat_function(fun= function(x) (dt(x,df=15)),
                xlim = c(min(df$x),-t),
                geom = "area") +
  stat_function(fun= function(x) (dt(x,df=15)),
                xlim = c(max(df$x),t),
                geom = "area")


p_2side <- pt(q=-t, df=15,lower.tail=TRUE) + pt(q=t, df=15,lower.tail=FALSE)
p_greater <- pt(q=t, df=15,lower.tail=FALSE)
