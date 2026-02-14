#############################
# Author: Glen Martin
# Date Updated: 12/29/25
# Purpose: Preform robustness checks
#         
#
#
###############################
rm(list = ls())

source('2. Code/06_2_Helper_Funcitons.R')

k_use <- readRDS("1. Data/k_use.rds")
df_k <- readRDS('1. Data/dfs_cluster_analysis.rds')
ian_lee <- readRDS("1. Data/main_synth_model.rds")
unit <- readRDS('1. Data/unit.rds')

df <- readRDS('1. Data/df_transformed_for_synth.rds')

df_robust_chk <- df_k[[k_use$df_k]]

# Donor Pool Sensitivity Analysis ------------------
robust_models_weight = list()

unit_weights <- ian_lee %>% 
  grab_unit_weights() %>% 
  filter(weight > 0.01)

counter <- length(unit_weights$unit)

# Model needs to run with at least two control units
while (counter > 3){
  if(counter == 7){
    county_list <- unique(c(unit_weights$unit, unit$County))
    }
  else{
    val <- max(unit_weights$weight)
    print(val)
    unit_weights <- unit_weights %>%
      filter(weight < val)
    county_list <- unique(c(unit_weights$unit, unit$County))
  }
  
  df_run <- df_robust_chk %>% filter(County %in% county_list)

  robust_models_weight[[paste0('County N = ', counter)]] <- run_synth(df_run, placebos = F)
  print(paste0('County N = ', counter))
  counter = counter - 1
}

# Donors less than 0.01
unit_weights <- ian_lee %>% 
  grab_unit_weights() %>% 
  filter(weight < 0.01)

county_list <- unique(c(unit_weights$unit, unit$County))

df_run <- df_robust_chk %>% filter(County %in% county_list)

robust_models_weight[['Donors Weight lt 0.01']] <- run_synth(df_run, placebos = F)


# Run model with none of the original donors
unit_weights <- ian_lee %>% 
  grab_unit_weights() 

df <- df %>% filter(!(County %in% unit_weights$unit))

unique(df$County)

robust_models_weight[['Diff Donor Pool']] <- run_synth(df, placebos = F)

# Export robustness checks with donor pool sensitivity analysis
for (i in names(robust_models_weight)){
  print(i)
  export_results(robust_models_weight[[i]], name=paste0('Robust_Check_', i))
}


for (name in names(robust_models_weight)){
  #print(name)
  print(robust_models_weight[[name]]  %>% plot_differences())
}


for (name in names(robust_models_weight)){
  print(name)
  print(robust_models_weight[[name]]  %>% grab_balance_table())
}


for (name in names(robust_models_weight)){
  #print(name)
  print(robust_models_weight[[name]]  %>%  plot_weights())
}

# leave one out donor pool analysis ----------

# Co-variate sensitivity analysis -----------

#pred_weights <- ian_lee %>% grab_predictor_weights()

#ian_lee %>% plot_weights()