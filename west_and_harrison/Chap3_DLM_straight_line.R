library(tidyverse)


# data creation -----------------------------------------------------------

## true model (random walk)
set.seed(123)
T = 100
V_true = 1.0
W_true = 0.1
theta_true = cumsum(c(0, rnorm(n=T-1, mean=0, sd=sqrt(W_true))))
# F_t = rep_len(0.1, length.out = T)
F_t = cumsum(rnorm(n=T, mean=0, sd=1/2)) 
# F_t = sin(seq(0, 2*pi, length.out = T)) + 1
plot(F_t, type="l")
Y_obs = rnorm(n=T, mean=F_t * theta_true, sd=sqrt(V_true))

## plot
plot_data = tibble(
  t = 1:T,
  theta = theta_true,
  mu = F_t * theta_true,
  y = Y_obs
) |> 
  pivot_longer(cols = c(mu, y, theta), names_to = "type", values_to = "value") |> 
  mutate(group = if_else(type == "theta", "State(theta)", "Observation (mu, y)"))

ggplot(plot_data, aes(x = t, y = value, color = type)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  facet_wrap(~ group, nrow = 2, scales = "free_y") +
  theme_minimal()

# DLM ---------------------------------------------------------------------

dlm_step = function(state, y_t, F_t, V_t, W_t){
  m_prev = state$m
  C_prev = state$C
  
  R_t = C_prev + W_t
  f_t = F_t * m_prev
  Q_t = (F_t**2) * R_t + V_t
  A_t = R_t*F_t / Q_t
  e_t = y_t - f_t
  C_t = R_t * V_t / Q_t
  m_t = m_prev + A_t * e_t
  
  return(
    list(m = m_t, C = C_t)
  )
}

# hyper params
m0 = 0; C0 = 100
W_t = 0.1; V_t = 1.0

# Learning
current_state = list(m = m0, C = C0)
results = vector("list", T)
for (t in 1:T){
  next_state = dlm_step(current_state, Y_obs[t], F_t[t], V_t, W_t)
  results[[t]] = next_state
  current_state = next_state
}

res_df = tibble(
  t = 1:T,
  theta_true = theta_true,
  mu_true = F_t * theta_true,
  y = Y_obs
) |> 
  bind_cols(bind_rows(results)) |> 
  rename(theta_pred = m) |> 
  mutate(
    mu_pred = F_t * theta_pred,
    theta_lower = theta_pred - 1.96 * sqrt(C),
    theta_upper = theta_pred + 1.96 * sqrt(C),
    mu_lower = mu_pred - 1.96 * sqrt(C) * F_t,
    mu_upper = mu_pred + 1.96 * sqrt(C) * F_t,
  )

# Viz
ggplot(res_df, aes(x = t)) +
  geom_ribbon(aes(ymin = mu_lower, ymax = mu_upper, fill = "95% Credible Interval"), alpha = 0.2) +
  geom_point(aes(y = y, color = "Observation (y)"), alpha = 0.8, size = 1.5) +
  geom_line(aes(y = y, color = "Observation (y)"), alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = mu_true, color = "True Level (mu)"), linewidth = 0.8) +
  geom_line(aes(y = mu_pred, color = "Filtered Estimate (mu)"), linewidth = 1.0) +
  scale_color_manual(values = c(
    "Observation (y)" = "grey70",
    "True Level (mu)" = "black",
    "Filtered Estimate (mu)" = "steelblue"
  )) +
  scale_fill_manual(values = c("95% Credible Interval" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "DLM Filtering Result",
    x = "Time", y = "Value",
    color = "Metrics",
    fill = ""
  )

ggplot(res_df, aes(x = t)) +
  geom_ribbon(aes(ymin = theta_lower, ymax = theta_upper, fill = "95% Credible Interval"), alpha = 0.2) +
  geom_point(aes(y = theta_true, color = "Theta(true)"), alpha = 0.8, size = 1.5) +
  geom_line(aes(y = theta_true, color = "Theta(true"), alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = theta_pred, color = "Theta(est)"), linewidth = 0.8) +
  scale_color_manual(values = c(
    "Theta(true)" = "black",
    "Theta(est)" = "steelblue"
  )) +
  scale_fill_manual(values = c("95% Credible Interval" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "Theta: true v.s. estimated",
    x = "Time", y = "Value",
    color = "Value",
    fill = ""
  )
  

