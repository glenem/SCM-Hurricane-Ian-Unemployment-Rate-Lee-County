#############################
# Author: Glen Martin
# Date Updated: 12/29/25
# Purpose: Preform robustness checks
#         
#
#
###############################
df_robust_chk <- df_k[[k_use$df_k]]

rm(df, df_ian, left, loss, predictor_weights, right, rmspe_table, 
   table_df, tmep, unit_weights, unit_weights_sorted, 
   xtab, half_n, n, name, rmse, rmse_row, temp, df_k, 
   synth_models, k_use)

# Donor Pool Sensitivity Analysis ------------------
robust_models_weight = list()

unit_weights <- ian_lee %>% 
  grab_unit_weights() %>% 
  filter(weight > 0.01)

counter <- length(unit_weights$unit)

# Model needs to run with at least two control units
while (counter > 1){
  if(counter == 7){
    county_list <- unique(c(unit_weights$unit, unit$County))
    }
  else{
    min_val <- min(unit_weights$weight)
    unit_weights <- unit_weights %>%
      filter(weight > min_val)
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

pred_weights <- ian_lee %>% grab_predictor_weights()

ian_lee %>% plot_weights()