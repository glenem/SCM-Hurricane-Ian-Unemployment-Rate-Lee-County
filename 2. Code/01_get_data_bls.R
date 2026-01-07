# clear environment before starting
rm(list = ls())

# Packages used to collect data for analysis ----
library(pacman)
pacman::p_load(readxl, tidyverse, httr, jsonlite, blscrapeR, haven)

# Collect BLS data from blsAPI ----

# read in data transpose and take first row as column headers
laus_data <- read_excel("1. Data/Raw_Data/laus_data_Jan1990_Mar2025.xlsx")

clean_laus <- function(input_df, ID_Code, value_name){
  
  # create empty dataframe of county_code, year, and value (either gdp or rgdp)
  output_df <- data.frame(county_code = character(),
                          month_year = character(),
                          value = numeric())
  
  raw_df <- input_df %>% filter(ID == as.character(ID_Code))
  
  raw_df <- raw_df %>% select(-`Series ID`, -ID)
  
  raw_df <- as.data.frame(t(raw_df))
  
  row_names <- rownames(raw_df)[2:nrow(raw_df)]
  n_rows <- as.numeric(length(rownames(raw_df)[2:nrow(raw_df)]))
  
  n_col = as.numeric(length(raw_df))
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
    # convert temp to numeric
    temp <- as.numeric(temp)
    
    #append temp to county_rgdp with year and county_code
    output_df <- rbind(output_df, data.frame(county_code = values, month_year = row_names, value = temp))
  }
  # rename value column to value_name
  names(output_df)[3] <- value_name
  return(output_df)
}

unr <- clean_laus(laus_data, 3, 'unr')
unr <- unr %>% separate(month_year, c('month', 'year'))
unr$year <- as.numeric(unr$year)
# renames character month to numeric month 

char_month_to_num <- function(df){
  df$month <- ifelse(df$month == 'Jan', '1', 
                      ifelse(df$month == 'Feb', '2', 
                      ifelse(df$month == 'Mar', '3', 
                      ifelse(df$month == 'Apr', '4', 
                      ifelse(df$month == 'May', '5', 
                      ifelse(df$month == 'Jun', '6',
                      ifelse(df$month == 'Jul', '7',
                      ifelse(df$month == 'Aug', '8',
                      ifelse(df$month == 'Sep', '9',
                      ifelse(df$month == 'Oct', '10',
                      ifelse(df$month == 'Nov', '11',
                      ifelse(df$month == 'Dec', '12', ""
                             ))))))))))))
  df$month <- as.numeric(df$month)
  return(df)
}

unr <- char_month_to_num(unr)

# save fl_county_gdp as an dta file
write_dta(unr, "1. Data/Inter_Data/unr.dta")
