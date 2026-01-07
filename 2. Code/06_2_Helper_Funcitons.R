# Functions to get data ready for Synth ---------------

# create a vector that has lee county name and fips code from the data set df
unit_of_analysis <- function(df, County_Name){
  unit_code <- df %>% filter(County == County_Name) %>% select(County,GeoFips)
  unit_code <- unit_code[1,]
  return(unit_code)
}


remove_hurricane_counties <- function(df, unit_=(unit$GeoFips)[1]){
  # make a vector of GeoFips for which hurricane == 1 that is not the unit
  hurricane_fips <- df %>% filter(hurricane == 1) %>% select(GeoFips)
  # remove unit from hurricane_fips
  hurricane_fips <- hurricane_fips %>% filter(GeoFips != unit_)
  
  # use hurricane_fip to drop rows in df_ian that are in hurricane_fips
  df <- df %>% filter(!GeoFips %in% hurricane_fips$GeoFips)
  
  return(df)
}

transform_for_synth <- function(df, unit_=(unit$GeoFips)[1], month_year_, oper='ge'){
  
  # in df if fipsneighbor === lee_county then drop those observations 
  df <- df %>% filter(fipsneighbor != unit_)
  
  # reduce dimensionality of the data and only keep unique GeoFips, month, year combinations. 
  df <- df %>%
    distinct(GeoFips, month, year, .keep_all = TRUE)
  df<- df %>% select(-c('countyname', 'neighborname', 'fipsneighbor'))
  
  if (oper == 'gt'){
    df <- df %>% filter(month_year > month_year_)  
  } else if (oper == 'lt'){
    df <- df %>% filter(month_year < month_year_)  
  }else if (oper == 'ge'){
    df <- df %>% filter(month_year >= month_year_)  
  }else if (oper == 'le'){
    df <- df %>% filter(month_year <= month_year_)  
  }else if (oper == 'eq'){
    df <- df %>% filter(month_year = month_year_)  
  }
  
  df <- remove_hurricane_counties(df)
  
  return(df)
}


# Function to Run Synthetic Control -------------------------------------
run_synth <- function(df, unit_ = (unit$GeoFips)[1], in_time_placebo=FALSE, date=0, placebos=T){

  if (in_time_placebo == FALSE){
    # create a vector for the month_year of the unit
    month_year <- c(df %>% filter(GeoFips == unit_ & hurricane ==1) %>% select(month_year))
    month_year <- month_year$month_year[1]
  }else{
    # used in in-time placebo analysis
    month_year = as.Date(date)
  }
  #smallest month year combination in the data set
  min_date <- min(df$month_year)
  # end_date is one month before the month_year date vector
  end_date <- as.Date(paste(year(month_year), month(month_year)-1, "01", sep = "-"))
  end_date_y <- as.Date(paste(year(month_year)-1, "12", "01", sep = "-"))
  
  
  synth_df <- df %>% 
    synthetic_control(outcome = unr,
                      unit = County,
                      time = month_year,
                      i_unit = unit$County,
                      i_time = month_year,
                      generate_placebos = placebos) %>% 
    # monthly predictors
    generate_predictor(
      time_window = c(min_date, end_date),
      mean_unr = mean(unr, na.rm = TRUE),
      mean_unr_lag1y = mean(unr_lag1y, na.rm = TRUE),
      mean_unr_lag1m = mean(unr_lag1m, na.rm = TRUE),
      # don't have to worry about this lagged value having 2022 data
    ) %>%
    # annual predictors (ensure we don't have values for 2022 that would contaminate the model)
    generate_predictor(
      time_window = c(min_date, end_date_y),
      mean_ln_rgdp_per_cap = mean(ln_rgdp_per_cap, na.rm = TRUE),
      mean_pop_growth_rate = mean(pop_growth_rate, na.rm = TRUE),
      black = mean(black, na.rm = TRUE),
      native = mean(native_american, na.rm = TRUE),
      hispanic = mean(hispanic, na.rm = TRUE),
      white = mean(white, na.rm = TRUE),
      asian = mean(asian, na.rm = TRUE),
      other = mean(other, na.rm = TRUE),
      pacific_islander = mean(pacific_islander, na.rm = TRUE),
      pct_18p = mean(pct_18p, na.rm = TRUE),
      pct_less_hs_18p = mean(pct_less_hs_18p, na.rm = TRUE),
      pcg_hs_18p = mean(pct_hs_18p, na.rm = TRUE),
      pct_some_college_18p = mean(pct_some_college_18p, na.rm = TRUE),
      pct_bachelorh_18p = mean(pct_bachelorh_18p, na.rm = TRUE),
      bb_coverage_pct = mean(bb_coverage_pct, na.rm = TRUE),
      ln_med_inc = mean(ln_med_inc, na.rm = TRUE),
      renter_occ_pct = mean(renter_occ_pct, na.rm = TRUE),
      owner_occ_pct = mean(owner_occ_pct, na.rm = TRUE),
    )%>%
    generate_weights(
      optimization_window = c(min_date, end_date),
      margin_ipop = .05, 
      sigf_ipop = 5,
      bound_ipop = 10
    ) %>%
    generate_control()
  
  return(synth_df)
}
# Graphing Synths ----------------------------------------------------------
scm_plotting <- function(df, unit_name, title_name, x_name, y_name, file_name){
  
  synth_name = paste("Synthetic ", unit_name)
  
  # better plot of the observed and synthetic 
  scm_plot <- ggplot(df, aes(x = time_unit)) +
    # trend lines
    geom_line(aes(y = real_y, color = unit_name, linetype = unit_name), size = 1) +
    geom_line(aes(y = synth_y, color = synth_name, linetype = synth_name), size = 1) +
    # add dots for each point
    geom_point(aes(y = real_y, color = unit_name), size = 2) +
    geom_point(aes(y = synth_y, color = synth_name), size = 2) +
    # addes a line for when the intervention occured
    geom_vline(xintercept = Meta_data$treatment_time, linetype = "dashed", color = "black") +
    
    # Correct dynamic mapping of colors and linetypes
    scale_color_manual(values = setNames(c("navy", "maroon"), c(unit_name, synth_name))) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), c(unit_name, synth_name))) +
    
    scale_x_date(date_labels = "%b %Y", date_breaks = "5 months") +
    labs(
      title = title_name,
      x = x_name,
      y = y_name,
      color = "Series",
      linetype = "Series"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",        # Move legend to bottom
      legend.title = element_blank(),    # Remove legend titles
      legend.text = element_text(size = 12),  # Optional: adjust legend text size
      plot.background = element_rect(fill = "white", color = NA),    # <- force white
      panel.background = element_rect(fill = "white", color = NA)    # <- force white
    )
  
  print(scm_plot)
  ggsave(paste0("3. Graphs/", file_name, ".png"), scm_plot, width = 8, height = 6, dpi = 300)
}
# Function to export the results of synthetic control ---------------------
export_results <- function(df, name, in_time_placebo=F){
  if(in_time_placebo==F){
    export <- df[[6]][[1]]
    # save export as a CSV file
    write_csv(export, paste0("1. Data/Synth_Results/Synth_",name,"_Results.csv"))
    }
  else{export <- df[[6]][[1]]
  # save export as a CSV file
  write_csv(export, paste0("1. Data/Synth_Results/Synth_In_Time_Placebo_",name,"_Results.csv"))
  }
}

# Finding a structural break for in-time placebo analysis-------

structural_break <- function(df, start = c(2021, 1), variable, graph=TRUE) {
  # Ensure variable exists in df
  if (!variable %in% names(df)) {
    stop(paste("Variable", variable, "not found in dataframe"))
  }
  
  # Filter for Franklin County and desired time period
  county <- subset(df, County == "Lee")
  
  # Extract the variable dynamically
  y_ts <- ts(county[[variable]], start = start, frequency = 12)
  
  # Run Zivot-Andrews test
  za_test <- ur.za(y_ts, model = "both")
  
  
  
  # Extract break index and map to year
  breakpoint <- za_test@bpoint
  break_time <- time(y_ts)[breakpoint]
  cat("Estimated structural break at:", break_time, "\n")
}


# export data to create in-place placebos graphs in Python ------------------------------
collect_placebos <- function(synth_df, name, treated_unit,county_seq=seq(from = 1, to = length(donor_pool), by = 2)) {
  df <- NULL
  for (c in county_seq){
    helper <- synth_df[[6]][[c]]
    helper$unit_name <- as.character(synth_df$.id[c])
    df <- rbind(df, helper)
  }
  # Add difference and label control/treatment
  df$difference <- df$real_y - df$synth_y
  df$control <- ifelse(df$unit_name == treated_unit, treated_unit, "control units")
  
  sig_df <- synth_df %>% grab_significance()
  
  # in sig_df only keep the veriables unit_name and pre_mspe
  sig_df <- sig_df %>% select(unit_name, pre_mspe, post_mspe, mspe_ratio)
  
  # left merge of df and sig_df on unit_name
  df <- left_join(df, sig_df, by = "unit_name")
  df$pre_rmspe <- sqrt(df$pre_mspe)
  df$post_rmspe <- sqrt(df$post_mspe)
  df$rmspe_ratio <- df$post_rmspe/df$pre_mspe
  
  # Export
  write_csv(df, paste0("1. Data/Synth_Results/Synth_In-Space_Placebos_",name,"_Results.csv"))
}

sig_table <- function(synth_df, name) {
  
  sig_df <- synth_df %>% grab_significance()
  
  # Export
  write_csv(sig_df, paste0("1. Data/Synth_Results/Synth_",name,"_Sig_table.csv"))
}

export_weights <- function(df, name){
  export <- df %>% grab_unit_weights()
  # save export as a CSV file
  write_csv(export, paste0("1. Data/Synth_Results/Synth_",name,"_Donor_Weights.csv"))
}
