library(tidyverse)
library(openintro)
library(infer)
library(gssr)


num_vars <- c("coninc", "hrs1")
cat_vars <- c("sex", "satfin", "class", "wrkstat")
my_vars <- c(num_vars, cat_vars)

gss18 <- gss_get_yr(2018)
data <- gss18
data <- data %>% 
  select(all_of(my_vars)) %>% 
  mutate(
    across(everything(), haven::zap_missing),
    across(all_of(cat_vars), forcats::as_factor)
  ) %>% 
  mutate(
    coninc = as.numeric(coninc),
    hrs1 = as.numeric(hrs1)
  )

colSums(is.na(data))



x_bar <- data %>% 
  specify(response = hrs1) %>%
  calculate(stat = "mean")
x_bar

boot_dist <- data %>%
  specify(response = hrs1) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

percentile_ci <- get_ci(boot_dist)
percentile_ci

visualize(boot_dist) +
  shade_confidence_interval(endpoints = percentile_ci)


null_dist <- data %>%
  specify(response = hrs1) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean")

visualize(null_dist) +
  shade_p_value(obs_stat = x_bar, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = x_bar, direction = "two-sided")

t_test(data, response = hrs1, mu = 40, alternative = "two-sided")



s_med <- data %>% 
  specify(response = hrs1) %>%
  calculate(stat = "median")
s_med
