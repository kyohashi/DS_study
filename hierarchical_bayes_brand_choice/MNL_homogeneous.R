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

# Use brand 1,2,4
purchase_df <- margarine$choicePrice %>%
  filter(choice %in% c(1,2,4))

for(i in 1:10)
  purchase_df[purchase_df['choice']==i, 'price'] <- log(purchase_df[purchase_df['choice']==i, i+2])

purchase_df <- purchase_df　%>% select(c(hhid, choice, price))
purchase_df$choice[purchase_df$choice == 4] <- 3
table(purchase_df$choice)

## parameters for Stan
N <- nrow(purchase_df)
K <- 3
D <- 2
X <- purchase_df %>% select(-c(hhid, choice))
X <- data.frame(intercept = rep(1, nrow(X))) %>% 
  bind_cols(X)
Y <- as.integer(purchase_df$choice)

data <- list(
  N = N,
  K = K,
  D = D,
  X = X,
  Y = Y
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

fit %>% bayesplot::rhat() %>% hist
fit$summary('beta')
fit$draws("beta") %>% mcmc_trace
fit$draws('lp__', inc_warmup = FALSE) %>% mcmc_trace(n_warmup = burn)

stanfit <- rstan::read_stan_csv(fit$output_files())
log_lik <- extract_log_lik(stanfit)
waic(log_lik)
