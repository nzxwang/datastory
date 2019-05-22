library(tidyverse)
# Example1 from reading 19
# H0 students IQ's ~N(100,15^2)
# HA students IQ's have mean greater than 100, but same variance!
# We test 9 students and mean-112. Use a ztest at alpha=0.05

z <- (112-100)/ ( 15/sqrt(9))
p <- pnorm(z, lower.tail=FALSE)
# p=0.008 passes the test.