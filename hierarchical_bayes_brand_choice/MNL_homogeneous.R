library(tidyverse)
library(bayesm)
library(cmdstanr)
library(rstan)
library(bayesplot)
library(loo)

color_scheme_set("brightblue")

rm(list = ls())
set.seed(42)


# data preparation ------------------------------------------------------------

data("margarine")

purchase_df <- margarine$choicePrice
purchase_df[,3:12] <- scale(purchase_df[,3:12])
# apply(purchase_df[, 3:12], 2, mean)
# apply(purchase_df[, 3:12], 2, sd)

P1 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,3))
P2 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,4))
P3 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,5))
P4 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,6))
P5 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,7))
P6 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,8))
P7 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,9))
P8 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,10))
P9 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,11))
P10 <-  data.frame(intercept = rep(1, nrow(purchase_df))) %>% bind_cols(select(purchase_df,12))


## parameters for Stan
N <- nrow(purchase_df)
K <- 10
D <- ncol(P1)
y <- as.integer(purchase_df$choice)

data <- list(
  N = N,
  K = K,
  D = D,
  P1 = P1,
  P2 = P2,
  P3 = P3,
  P4 = P4,
  P5 = P5,
  P6 = P6,
  P7 = P7,
  P8 = P8,
  P9 = P9,
  P10 = P10,
  Y = y
)


# Fitting -----------------------------------------------------------------
burn <- 1000
draw <- 1000
thin <- 1
model <- cmdstan_model('./stan/MNL_homogeneous.stan')
model$print()

fit <- model$sample(
  data,
  iter_warmup = burn,
  iter_sampling = draw,
  chains = 3,
  parallel_chains = 3,
  save_warmup = TRUE
)


# Evaluation --------------------------------------------------------------

# fit %>% bayesplot::rhat() %>% hist(main='Hist. of Rhat')
fit$summary('beta')
fit$draws("beta") %>% mcmc_trace
fit$draws('lp__', inc_warmup = FALSE) %>% mcmc_trace(n_warmup = burn)

# stanfit <- rstan::read_stan_csv(fit$output_files())
# log_lik <- extract_log_lik(stanfit)
# waic(log_lik)

purchase_df %>% 
  mutate(buy = ifelse(choice == 5, 1, 0)) %>% 
  select(c(buy, PGen_Stk)) %>% 
  ggplot(aes(x = factor(buy), y = PGen_Stk)) +
  geom_violin() +
  labs(x = "buy", y = "PGen_Stk")

purchase_df %>% 
  mutate(buy = ifelse(choice == 2, 1, 0)) %>% 
  select(c(buy, PBB_Stk)) %>% 
  ggplot(aes(x = factor(buy), y = PBB_Stk)) +
  geom_violin() +
  labs(x = "buy", y = "PBB_Stk")

hhid_purchase <- purchase_df %>% 
  filter(choice == 5) %>% 
  select(hhid)

margarine$demos %>% 
  mutate(buy = ifelse(hhid %in% hhid_purchase$hhid, 1, 0)) %>% 
  select(c(buy, Income)) %>% 
  ggplot(aes(x=factor(buy), y=Income)) +
  geom_boxplot() +
  labs(x = "buy", y="Income")
