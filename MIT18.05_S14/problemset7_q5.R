library(tidyverse)

# X~U(0,theta). H0 says theta=2; HA says theta=/=2.
# Reject H0 when x<=0.1 or x>=1.9.

# a)
# P(x<=0.1 or x>=1.9 | theta=2)=0.1, therefore significance is 0.1

# b)
# P(0.1<x<1.9 | theta=2.5)=18/25, therefore type2 error is 18/25=0.72, if the true theta is 2.5.