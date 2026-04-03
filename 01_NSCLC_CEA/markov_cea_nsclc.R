
# =============================================================================
# MARKOV COST-EFFECTIVENESS MODEL — NSCLC
# New Immunotherapy vs Standard of Care
# Canadian Payer Perspective | 5-Year Horizon
# Author: Rahul
# =============================================================================

library(heemod)
library(dampack)
library(ggplot2)
library(dplyr)

# --- Parameters --------------------------------------------------------------
param <- define_parameters(
  disc_rate_monthly = 0.015 / 12,
  p_PF_to_P_soc = 0.10,
  p_PF_to_D_soc = 0.01,
  p_P_to_D_soc  = 0.15,
  hr_progression = 0.70,
  p_PF_to_P_new = 1 - (1 - p_PF_to_P_soc) ^ hr_progression,
  p_PF_to_D_new = p_PF_to_D_soc,
  p_P_to_D_new  = p_P_to_D_soc,
  u_PF   = 0.75,
  u_P    = 0.55,
  u_dead = 0.00,
  c_drug_soc  = 2500,
  c_drug_new  = 9000,
  c_manage_PF = 800,
  c_manage_P  = 1500,
  c_dead      = 0
)

# --- Transition matrices -----------------------------------------------------
mat_soc <- define_transition(
  state_names = c("PF", "Progressed", "Dead"),
  1 - p_PF_to_P_soc - p_PF_to_D_soc, p_PF_to_P_soc, p_PF_to_D_soc,
  0, 1 - p_P_to_D_soc, p_P_to_D_soc,
  0, 0, 1
)

mat_new <- define_transition(
  state_names = c("PF", "Progressed", "Dead"),
  1 - p_PF_to_P_new - p_PF_to_D_new, p_PF_to_P_new, p_PF_to_D_new,
  0, 1 - p_P_to_D_new, p_P_to_D_new,
  0, 0, 1
)

# --- State values ------------------------------------------------------------
state_PF_soc <- define_state(cost = c_drug_soc + c_manage_PF, qaly = u_PF / 12)
state_P_soc  <- define_state(cost = c_manage_P, qaly = u_P / 12)
state_dead   <- define_state(cost = c_dead, qaly = u_dead)
state_PF_new <- define_state(cost = c_drug_new + c_manage_PF, qaly = u_PF / 12)
state_P_new  <- define_state(cost = c_manage_P, qaly = u_P / 12)

# --- Strategies --------------------------------------------------------------
strategy_soc <- define_strategy(
  transition = mat_soc,
  PF = state_PF_soc, Progressed = state_P_soc, Dead = state_dead
)
strategy_new <- define_strategy(
  transition = mat_new,
  PF = state_PF_new, Progressed = state_P_new, Dead = state_dead
)

# --- Run deterministic model -------------------------------------------------
model <- run_model(
  parameters = param,
  soc = strategy_soc,
  new = strategy_new,
  cycles = 60,
  cost = cost,
  effect = qaly,
  method = "beginning",
  init = c(1000, 0, 0)
)

summary(model)

# --- ICER --------------------------------------------------------------------
total_costs <- get_values(model) %>%
  group_by(.strategy_names, value_names) %>%
  summarise(total = sum(value), .groups = "drop")

delta_cost <- total_costs$total[total_costs$.strategy_names == "new" & total_costs$value_names == "cost"] -
              total_costs$total[total_costs$.strategy_names == "soc" & total_costs$value_names == "cost"]

delta_qaly <- total_costs$total[total_costs$.strategy_names == "new" & total_costs$value_names == "qaly"] -
              total_costs$total[total_costs$.strategy_names == "soc" & total_costs$value_names == "qaly"]

icer <- delta_cost / delta_qaly

cat(sprintf("Incremental Cost  : CAD $%.0f
", delta_cost))
cat(sprintf("Incremental QALYs : %.3f
", delta_qaly))
cat(sprintf("ICER              : CAD $%.0f per QALY gained
", icer))
cat(sprintf("Decision: %s
",
    ifelse(icer < 50000,
           "Cost-effective at CADTH $50,000/QALY threshold",
           "NOT cost-effective at CADTH $50,000/QALY threshold")))

# --- PSA ---------------------------------------------------------------------
set.seed(42)
n_sim <- 1000

p_PF_to_P_soc_psa <- rbeta(n_sim, 50, 450)
p_PF_to_D_soc_psa <- rbeta(n_sim, 5, 495)
p_P_to_D_soc_psa  <- rbeta(n_sim, 75, 425)
hr_psa             <- rlnorm(n_sim, meanlog = log(0.70), sdlog = 0.15)
u_PF_psa           <- rbeta(n_sim, 60, 20)
u_P_psa            <- rbeta(n_sim, 44, 36)
c_drug_soc_psa     <- rgamma(n_sim, shape = (2500/250)^2, rate = 2500/250^2)
c_drug_new_psa     <- rgamma(n_sim, shape = (9000/900)^2, rate = 9000/900^2)
c_manage_PF_psa    <- rgamma(n_sim, shape = (800/80)^2,   rate = 800/80^2)
c_manage_P_psa     <- rgamma(n_sim, shape = (1500/150)^2, rate = 1500/150^2)

delta_cost_vec <- numeric(n_sim)
delta_qaly_vec <- numeric(n_sim)

for (i in 1:n_sim) {
  p_prog_soc <- p_PF_to_P_soc_psa[i]
  p_die_PF   <- p_PF_to_D_soc_psa[i]
  p_die_P    <- p_P_to_D_soc_psa[i]
  p_prog_new <- 1 - (1 - p_prog_soc) ^ hr_psa[i]

  soc <- c(1000, 0, 0)
  new <- c(1000, 0, 0)
  cost_soc <- 0; cost_new <- 0
  qaly_soc <- 0; qaly_new <- 0

  for (t in 1:60) {
    cost_soc <- cost_soc + soc[1] * (c_drug_soc_psa[i] + c_manage_PF_psa[i]) + soc[2] * c_manage_P_psa[i]
    cost_new <- cost_new + new[1] * (c_drug_new_psa[i] + c_manage_PF_psa[i]) + new[2] * c_manage_P_psa[i]
    qaly_soc <- qaly_soc + soc[1] * u_PF_psa[i] / 12 + soc[2] * u_P_psa[i] / 12
    qaly_new <- qaly_new + new[1] * u_PF_psa[i] / 12 + new[2] * u_P_psa[i] / 12

    soc <- c(
      soc[1] * (1 - p_prog_soc - p_die_PF),
      soc[1] * p_prog_soc + soc[2] * (1 - p_die_P),
      soc[1] * p_die_PF + soc[2] * p_die_P + soc[3]
    )
    new <- c(
      new[1] * (1 - p_prog_new - p_die_PF),
      new[1] * p_prog_new + new[2] * (1 - p_die_P),
      new[1] * p_die_PF + new[2] * p_die_P + new[3]
    )
  }

  delta_cost_vec[i] <- cost_new - cost_soc
  delta_qaly_vec[i] <- qaly_new - qaly_soc
}

icer_vec <- delta_cost_vec / delta_qaly_vec

cat(sprintf("Mean ICER: CAD $%.0f
", mean(icer_vec)))
cat(sprintf("Prob cost-effective at $50,000/QALY: %.1f%%
",
    mean(icer_vec < 50000) * 100))

# --- Plots -------------------------------------------------------------------
psa_df <- data.frame(delta_cost = delta_cost_vec, delta_qaly = delta_qaly_vec)

ggplot(psa_df, aes(x = delta_qaly, y = delta_cost)) +
  geom_point(alpha = 0.3, size = 1.2, color = "#1D9E75") +
  geom_abline(slope = 50000, intercept = 0, linetype = "dashed", color = "#D85A30") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Cost-Effectiveness Plane",
       subtitle = "New Immunotherapy vs SoC | NSCLC | Canadian Payer",
       x = "Incremental QALYs", y = "Incremental Cost (CAD $)") +
  theme_minimal(base_size = 12)

ggsave("/Users/rahulrathi/heor-portfolio/01_NSCLC_CEA/ce_plane.png", width = 8, height = 6, dpi = 300)

wtp_range <- seq(0, 1000000, by = 10000)
prob_ce   <- sapply(wtp_range, function(wtp) mean(icer_vec < wtp))
ceac_df   <- data.frame(wtp = wtp_range, prob = prob_ce)

ggplot(ceac_df, aes(x = wtp, y = prob)) +
  geom_line(color = "#1D9E75", linewidth = 1) +
  geom_vline(xintercept = 50000, linetype = "dashed", color = "#D85A30") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Cost-Effectiveness Acceptability Curve (CEAC)",
       subtitle = "New Immunotherapy vs SoC",
       x = "Willingness-to-Pay Threshold (CAD $/QALY)",
       y = "Probability Cost-Effective") +
  theme_minimal(base_size = 12)

ggsave("/Users/rahulrathi/heor-portfolio/01_NSCLC_CEA/ceac.png", width = 8, height = 6, dpi = 300)

cat("All files saved to heor-portfolio/01_NSCLC_CEA
")

