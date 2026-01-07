# Lee county Analysis

# clear environment before starting
rm(list = ls())

# import packages
pacman::p_load(readxl, tidyverse, tidysynth, Synth, haven, cluster, factoextra, mclust, urca, xtable)

df <- read_dta('1. Data/data.dta')

# create a new column that is a combination of month and year
df <- df %>%
  mutate(month_year = as.Date(paste(year, month, "01", sep = "-")))

# Creates helper function that exports results of synthetic control
source('2. Code/06_2_Helper_Funcitons.R')

unit <- unit_of_analysis(df, 'Lee')

# create the data set to analyze hurricane ian
df_ian <- transform_for_synth(df, month_year_=as.Date("2021-01-01"))

# Runs cluster analysis and selects donor pool
source('2. Code/06_3_Donor_Pool_Cluster_Analysis.R')

# Running the synthetic control ------------------

# run_synth is specified in the HelperFunctions.R file

# Create a list of models
synth_models = list()
for (name in names(df_k)){
  #print(name)
  synth_models[[name]] <- run_synth(df_k[[name]])
}

# other metrics


for (name in names(df_k)){
  #print(name)
  print(synth_models[[name]]  %>% plot_differences())
}


for (name in names(df_k)){
  print(name)
  print(synth_models[[name]]  %>% grab_balance_table())
}

for (name in names(df_k)){
  #print(name)
  print(synth_models[[name]]  %>%  plot_mspe_ratio())
}

for (name in names(df_k)){
  #print(name)
  print(synth_models[[name]]  %>%  plot_weights())
}


for (name in names(df_k)){
  print(name)
  print(synth_models[[name]]  %>% grab_significance())
}

for (name in names(df_k)){
  # create each entry of the list, this is like a python dictionary or hash 
  if (name=='df_k2'){
    temp <- synth_models[[name]]  %>% grab_loss() %>% 
      filter(,.placebo == 0) %>% select(control_unit_mspe)
    temp$df_k <- name
    rmspe_table <- temp
  }
  else{
    temp <- synth_models[[name]]  %>% grab_loss() %>% 
      filter(,.placebo == 0) %>% select(control_unit_mspe)
    temp$df_k <- name
    rmspe_table <- rbind(rmspe_table, temp)
  }
}

# k=3 yields the smallest MSPE
k_use <- rmspe_table %>% 
  filter(control_unit_mspe == min(control_unit_mspe)) %>% 
  select(df_k)

ian_lee <- synth_models[[k_use$df_k]]

ian_lee %>% export_results("Lee_UNR")

ian_lee %>% sig_table('Lee_UNR')

ian_lee %>% export_weights('Lee_UNR')

# unit weights table -----------
unit_weights<- ian_lee[[7]][[1]]
predictor_weights<- ian_lee[[8]][[1]]
loss <-ian_lee[[11]][[1]]


# Add RMSE at the bottom
rmse <- round(loss$control_unit_mspe, 3)

unit_weights_sorted <- unit_weights %>%
  arrange(desc(weight))


# Round weights for display
unit_weights_sorted <- unit_weights_sorted %>%
  mutate(weight = round(weight, 2))

# Split into two columns
n <- nrow(unit_weights_sorted)
half_n <- ceiling(n / 2)

left <- unit_weights_sorted[1:half_n, ]
right <- unit_weights_sorted[(half_n + 1):n, ]

# Pad right side if needed
if (nrow(right) < nrow(left)) {
  right[(nrow(right) + 1):nrow(left), ] <- NA
}

# Combine side by side
table_df <- data.frame(
  County1 = left$unit,
  Weight1 = left$weight,
  County2 = right$unit,
  Weight2 = right$weight
)

# Step 4: Insert RMSE into first empty County2/Weight2 slot
rmse <- round(loss$control_unit_mspe, 3)

rmse_row <- which(is.na(table_df$County2))[1]  # First empty County2 slot

if (!is.na(rmse_row)) {
  table_df$County2[rmse_row] <- "MSPE"
  table_df$Weight2[rmse_row] <- rmse
} else {
  # If no empty row, append it as a new row
  table_df <- rbind(table_df, c(NA, NA, "MSPE", as.character(rmse)))
}

# Step 5: Export to .tex
xtab <- xtable(table_df, caption = "Composition of Synthetic Control Group")
print(xtab,
      include.rownames = FALSE,
      caption.placement = "top",
      file = "4. LaTeX/Tables/synthetic_table.tex")