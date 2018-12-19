library(tidyverse)

# Accounting data should follow Benford's distribution. Run a test at 0.001 significance level to see if the accounting data is fraud.

df <- tibble(
  digit = seq(1,9),
  uniform = 1/length(digit),
  count = c(7,13,12,9,9,13,11,10,16),
  count_normalized = count/sum(count),
  benford = c(0.301,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)
)
df %>% ggplot(aes(x=digit)) + 
  geom_line(aes(y=uniform), alpha=0.5) +
  geom_col(aes(y=count_normalized), alpha=0.5) +
  geom_line(aes(y=benford))
