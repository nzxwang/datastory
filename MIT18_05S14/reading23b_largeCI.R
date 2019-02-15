library(tidyverse)
library(parallel)

df <- expand.grid(n = c(20,50,100,400),
                  nominal_confidence = c(0.95,0.90,0.80),
                  distribution = c("exp","N"),
                  stringsAsFactors=FALSE) %>%
  arrange(n) %>%
  as_tibble()

# This function draws n samples from exp(1) or N(1,1), computes the sample mean and sample standard deviation, then checks if the true mean (mew=1) lies inside the large sample CI - type1 error.
simulate_confidence <- function(n, nominal_confidence, distribution) {
  if (distribution == "exp"){
    run_trial <- function() {
      samples <- rexp(n, rate=1)
      mean <- samples %>% mean()
      sd <- samples %>% sd()
      alpha <- 1-nominal_confidence
      large_sample_CI <- qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE) * sd/sqrt(n)
      ifelse(1 > mean - large_sample_CI & 1 < mean + large_sample_CI,
             return(1), return(0))
    }
    mclapply(1:100000, function(garbage) run_trial(), mc.cores = 6) %>% unlist() %>% sum() %>% `/`(100000) %>% return()
  }
  else if (distribution == "N"){
    run_trial <- function() {
      samples <- rnorm(n, mean=1)
      mean <- samples %>% mean()
      sd <- samples %>% sd()
      alpha <- 1-nominal_confidence
      large_sample_CI <- qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE) * sd/sqrt(n)
      ifelse(1 > mean - large_sample_CI & 1 < mean + large_sample_CI,
             return(1), return(0))
    }
    mclapply(1:100000, function(garbage) run_trial(), mc.cores = 6) %>% unlist() %>% sum() %>% `/`(100000) %>% return()
  }
}

df <- df %>% mutate(simulated_confidence = pmap_dbl(.,simulate_confidence))
