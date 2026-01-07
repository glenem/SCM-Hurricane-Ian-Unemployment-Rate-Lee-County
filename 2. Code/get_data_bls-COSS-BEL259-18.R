# clear environment before starting
rm(list = ls())

# Packages used to collect data for analysis ----
library(pacman)
pacman::p_load(readxl, tidyverse,httr, jsonlite, blscrapeR, haven)

# Collect BLS data from blsAPI ----

laus_series_ids <- read_excel("Data/Raw_Data/bls_series_ids.xlsx", 
                              sheet = "BLS Data Series")

# read in data transpose and take first row as column headers
laus_data <- t(read_excel("Data/Raw_Data/laus_data_Jan1990_Mar2025.xlsx", 
                        sheet = 'Intermediate'))


clean_laus <- function(input_df, value_name){
  
  # create empty dataframe of county_code, year, and value (either gdp or rgdp)
  output_df <- data.frame(county_code = character(),
                          mont_year = character(),
                          value = numeric())
  
  # create a new data set by filtering for rows for LineDescription == 1
  # transpose the data frame so that the years are in columns
  
  # drop rows in RGDP dataset by index name
  raw_df <- as.data.frame(laus_data[-c(1:2),])
  # Create year vector from 2013 to 2023
  row_names <- rownames(raw_df)[2:nrow(raw_df)]
  first_row <- (row_names[1])
  last_row <- (row_names[length(row_names)])
  
  n_rows = as.numeric(nrow(raw_df))
  
  n_col = as.numeric(ncol(raw_df))
  # Create a vector of strings from V1 to V67
  colnames <- paste0("V", 1:n_col)
  
  for (i in colnames){
    
    temp <- c(raw_df[i])
    #convert temp to a vector
    temp <- temp[[i]]
    # save row 1 as a variable
    values <- temp[1]
    # duplicate values as a vector of length 12
    values <- rep(values, n_rows)
    #remove row 1 in temp
    temp <- temp[-1]
    values <- values[-1]
    # convert temp to numeric
    temp <- as.numeric(temp)
    
    #append temp to county_rgdp with year and county_code
    output_df <- rbind(output_df, data.frame(county_code = values, mont_year = row_names, value = temp))
  }
  # rename value column to value_name
  names(output_df)[3] <- value_name
  return(output_df)
}


laus_clean = clean_laus(laus_data, "unr")
