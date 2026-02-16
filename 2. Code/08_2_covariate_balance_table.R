# Create LaTeX Covariate Balance Table
# Created by Claude, Sonnet 4.5 through my prompt
rm(list = ls())

pacman::p_load(readxl, tidyverse, tidysynth, Synth, haven, xtable, kableExtra)

source('2. Code/06_2_Helper_Funcitons.R')

ian_lee <- readRDS("1. Data/main_synth_model.rds")

balance_table <- ian_lee %>% grab_balance_table()

# Create a nicely formatted variable name lookup
variable_names <- c(
  "mean_unr" = "Unemployment Rate (\\%)",
  "mean_unr_lag1m" = "Unemployment Rate, 1-Month Lag (\\%)",
  "mean_unr_lag1y" = "Unemployment Rate, 1-Year Lag (\\%)",
  "asian" = "Asian Population (\\%)",
  "bb_coverage_pct" = "Broadband Coverage (\\%)",
  "black" = "Black Population (\\%)",
  "hispanic" = "Hispanic Population (\\%)",
  "ln_med_inc" = "Log Median Income",
  "mean_ln_rgdp_per_cap" = "Mean Log Real GDP per Capita",
  "mean_pop_growth_rate" = "Population Growth Rate (\\%)",
  "native" = "Native American Population (\\%)",
  "other" = "Other Race Population (\\%)",
  "owner_occ_pct" = "Owner Occupancy Rate (\\%)",
  "pacific_islander" = "Pacific Islander Population (\\%)",
  "pcg_hs_18p" = "High School Graduate, 18+ (\\%)",
  "pct_18p" = "Population 18+ (\\%)",
  "pct_bachelorh_18p" = "Bachelor's Degree or Higher, 18+ (\\%)",
  "pct_less_hs_18p" = "Less than High School, 18+ (\\%)",
  "pct_some_college_18p" = "Some College, 18+ (\\%)",
  "renter_occ_pct" = "Renter Occupancy Rate (\\%)",
  "white" = "White Population (\\%)"
)

# Format the balance table
balance_formatted <- balance_table %>%
  mutate(
    # Replace variable names with formatted versions
    Variable = ifelse(variable %in% names(variable_names), 
                      variable_names[variable], 
                      variable),
    # Round values to 2 decimal places
    `Lee County` = round(Lee, 2),
    `Synthetic Lee` = round(synthetic_Lee, 2),
    `Donor Pool` = round(donor_sample, 2)
  ) %>%
  select(Variable, `Lee County`, `Synthetic Lee`, `Donor Pool`)

# Create LaTeX table using xtable
latex_table <- xtable(balance_formatted,
                      caption = "Covariate Balance Table: Lee County vs. Synthetic Control",
                      label = "tab:balance",
                      align = c("l", "l", "r", "r", "r"))

# Print LaTeX code to console
print(latex_table,
      include.rownames = FALSE,
      caption.placement = "top",
      booktabs = TRUE,
      sanitize.text.function = identity,  # Preserve LaTeX formatting
      file = "4. LaTeX/Tables/covariate_balance_table.tex")

cat("LaTeX table saved to: 4. LaTeX/Tables/covariate_balance_table.tex\n")