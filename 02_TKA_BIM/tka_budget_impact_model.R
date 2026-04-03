
# =============================================================================
# BUDGET IMPACT MODEL — AMBULATORY TKA PAIN MANAGEMENT
# Optimized vs Standard Post-Discharge Opioid Prescribing
# Ontario Public Payer Perspective | 3-Year Time Horizon
# Author: Rahul
# Data sources: CIHI, Ontario Drug Benefit Formulary, published literature
# Note: Clinical assumptions informed by real-world ambulatory TKA data
# from a Canadian academic hospital (not named per confidentiality)
# =============================================================================

library(ggplot2)
library(dplyr)
library(scales)

# --- Population --------------------------------------------------------------
tka_canada_annual <- 70000
ontario_share     <- 0.38
tka_ontario_y1    <- round(tka_canada_annual * ontario_share)

amb_rate_y1 <- 0.30
amb_rate_y2 <- 0.38
amb_rate_y3 <- 0.46

pop_y1 <- round(tka_ontario_y1 * amb_rate_y1)
pop_y2 <- round(tka_ontario_y1 * amb_rate_y2)
pop_y3 <- round(tka_ontario_y1 * amb_rate_y3)

# --- Cost parameters ---------------------------------------------------------
cost_hydromorphone_per_tab <- 0.50
tabs_standard              <- 20
tabs_optimized             <- 10
cost_opioid_standard       <- tabs_standard  * cost_hydromorphone_per_tab
cost_opioid_optimized      <- tabs_optimized * cost_hydromorphone_per_tab
cost_blister_pack          <- 85.00
cost_er_visit              <- 550.00
cost_readmission           <- 4200.00
cost_clinic_return         <- 180.00

rate_er_standard           <- 0.08
rate_readmission_standard  <- 0.02
rate_clinic_standard       <- 0.10
rate_er_optimized          <- 0.06
rate_readmission_optimized <- 0.02
rate_clinic_optimized      <- 0.07

# --- Model function ----------------------------------------------------------
calculate_annual_cost <- function(pop, cost_opioid, rate_er,
                                   rate_readmission, rate_clinic) {
  list(
    drug    = pop * (cost_opioid + cost_blister_pack),
    er      = pop * rate_er * cost_er_visit,
    readmit = pop * rate_readmission * cost_readmission,
    clinic  = pop * rate_clinic * cost_clinic_return,
    total   = pop * (cost_opioid + cost_blister_pack) +
              pop * rate_er * cost_er_visit +
              pop * rate_readmission * cost_readmission +
              pop * rate_clinic * cost_clinic_return
  )
}

# --- Run model ---------------------------------------------------------------
std_y1 <- calculate_annual_cost(pop_y1, cost_opioid_standard, rate_er_standard, rate_readmission_standard, rate_clinic_standard)
std_y2 <- calculate_annual_cost(pop_y2, cost_opioid_standard, rate_er_standard, rate_readmission_standard, rate_clinic_standard)
std_y3 <- calculate_annual_cost(pop_y3, cost_opioid_standard, rate_er_standard, rate_readmission_standard, rate_clinic_standard)
opt_y1 <- calculate_annual_cost(pop_y1, cost_opioid_optimized, rate_er_optimized, rate_readmission_optimized, rate_clinic_optimized)
opt_y2 <- calculate_annual_cost(pop_y2, cost_opioid_optimized, rate_er_optimized, rate_readmission_optimized, rate_clinic_optimized)
opt_y3 <- calculate_annual_cost(pop_y3, cost_opioid_optimized, rate_er_optimized, rate_readmission_optimized, rate_clinic_optimized)

total_std     <- std_y1$total + std_y2$total + std_y3$total
total_opt     <- opt_y1$total + opt_y2$total + opt_y3$total
total_savings <- total_std - total_opt

# --- Plots -------------------------------------------------------------------
dir.create("/Users/rahulrathi/heor-portfolio/02_TKA_BIM", recursive = TRUE)

bim_df <- data.frame(
  year     = rep(c("Year 1", "Year 2", "Year 3"), 2),
  scenario = rep(c("Standard", "Optimized"), each = 3),
  cost     = c(std_y1$total, std_y2$total, std_y3$total,
               opt_y1$total, opt_y2$total, opt_y3$total)
)

ggplot(bim_df, aes(x = year, y = cost, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0("$", format(round(cost/1000), big.mark=","), "K")),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("Standard" = "#D85A30", "Optimized" = "#1D9E75")) +
  scale_y_continuous(labels = dollar_format(prefix = "CAD $"),
                     limits = c(0, max(bim_df$cost) * 1.15)) +
  labs(title    = "Budget Impact: Optimized vs Standard Opioid Prescribing in Ambulatory TKA",
       subtitle = "Ontario Public Payer Perspective | 3-Year Time Horizon",
       x = NULL, y = "Total Annual Cost (CAD $)", fill = "Scenario",
       caption  = "Assumes ambulatory TKA uptake grows from 30% to 46% of Ontario TKA volume over 3 years") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("/Users/rahulrathi/heor-portfolio/02_TKA_BIM/bim_bar_chart.png", width = 9, height = 6, dpi = 300)

savings_df <- data.frame(
  year       = c("Year 1", "Year 2", "Year 3"),
  cumulative = cumsum(c(std_y1$total - opt_y1$total,
                        std_y2$total - opt_y2$total,
                        std_y3$total - opt_y3$total))
)

ggplot(savings_df, aes(x = year, y = cumulative, group = 1)) +
  geom_line(color = "#1D9E75", linewidth = 1.2) +
  geom_point(color = "#1D9E75", size = 4) +
  geom_text(aes(label = paste0("$", format(round(cumulative/1000), big.mark=","), "K")),
            vjust = -1, size = 3.5) +
  scale_y_continuous(labels = dollar_format(prefix = "CAD $"),
                     limits = c(0, max(savings_df$cumulative) * 1.2)) +
  labs(title    = "Cumulative Budget Savings — Optimized Opioid Prescribing in Ambulatory TKA",
       subtitle = "Ontario Public Payer Perspective",
       x = NULL, y = "Cumulative Savings (CAD $)",
       caption  = paste0("3-year total savings: CAD $", format(round(total_savings), big.mark=","))) +
  theme_minimal(base_size = 12)

ggsave("/Users/rahulrathi/heor-portfolio/02_TKA_BIM/bim_savings_curve.png", width = 9, height = 6, dpi = 300)

cat("Done. All files saved.
")

