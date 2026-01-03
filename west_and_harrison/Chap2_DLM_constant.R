library(tidyverse)


# data creation -----------------------------------------------------------

## true model (random walk)
set.seed(42)
T = 100
V_true = 1.0
W_true = 0.1
mu_true = cumsum(c(20, rnorm(n=T-1, mean=0, sd=sqrt(W_true))))
Y_obs = rnorm(n=T, mean=mu_true, sd=sqrt(V_true))

## plot
tibble(
  t = 1:T,
  mu = mu_true,
  y = Y_obs
) |> 
  pivot_longer(cols = c(mu, y), names_to = "type", values_to = "value") |> 
  ggplot(aes(x = t, y = value, color = type)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  labs(title = "True model and observations")


# DLM ---------------------------------------------------------------------

dlm_step = function(state, y_t, V_t, W_t){
  m_prev = state$m
  C_prev = state$C
  
  R_t = C_prev + W_t
  f_t = m_prev
  Q_t = R_t + V_t
  A_t = R_t / Q_t
  e_t = y_t - f_t
  C_t = A_t * V_t
  m_t = m_prev + A_t * e_t
  
  return(
    list(m = m_t, C = C_t)
  )
}

# hyper params
m0 = 20; C0 = 100
W_t = 0.1; V_t = 1.0

# Learning
current_state = list(m = m0, C = C0)
restults = vector("list", T)
for (t in 1:T){
  next_state = dlm_step(current_state, Y_obs[t], V_t, W_t)
  results[[t]] = next_state
  current_state = next_state
}

res_df = tibble(
  t = 1:T,
  mu_true = mu_true,
  y = Y_obs
) |> 
  bind_cols(bind_rows(results)) |> 
  rename(m_pred = m) |> 
  mutate(
    lower = m_pred - 1.96 * sqrt(C),
    upper = m_pred + 1.96 * sqrt(C)
  )

# Viz
ggplot(res_df, aes(x = t)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.2) +
  geom_point(aes(y = y, color = "Observation (y)"), alpha = 0.8, size = 1.5) +
  geom_line(aes(y = y, color = "Observation (y)"), alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = mu_true, color = "True Level (mu)"), size = 0.8) +
  geom_line(aes(y = m_pred, color = "Filtered Estimate (m)"), size = 1.0) +
  scale_color_manual(values = c(
    "Observation (y)" = "grey70",
    "True Level (mu)" = "black",
    "Filtered Estimate (m)" = "steelblue"
  )) +
  scale_fill_manual(values = c("95% Credible Interval" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "DLM Filtering Result",
    x = "Time", y = "Value",
    color = "Metrics",
    fill = ""
  )
