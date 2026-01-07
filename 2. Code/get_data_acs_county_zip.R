# clear environment before starting
rm(list = ls())

# Packages used to collect data for analysis ----
library(pacman)
pacman::p_load(readr, readxl, tidyverse, tigris, blscrapeR, tidycensus, haven)

census_key = '1c8f92bff63bd634ed424ff48d26618d06731c41'

# need to look further into the zip code and how to download that data with tidy census

# Collect ACS 5-year estimates from Tidycensus ----
# To find the code go to here https://www.census.gov/data/developers/data-sets/acs-5year.html
# Afterward choose the HTML of the subject table



vector_vars = c("S2301_C02_030E", #DLFPR
                "S2301_C04_030E",# DUNR
                "S2301_C02_001E", # LFPR
                "S2301_C04_001E" # UNR
                )

zcta_vars = c("S2301_C02_030E", #DLFPR
             "S2301_C04_030E",# DUNR
             "S2301_C02_001E", # LFPR
             "S2301_C04_001E" # UNR
)

get_ACS5_project <- function(vector_var=vector_vars, year_start, year_end=0, geo="county", state=12){
  
  if (year_end == 0){
    df <- get_acs(geography = geo, state = state, variables = vector_vars, 
                       survey = "acs5", year = year_start, key= census_key)
    
    # add a column for year
    df$year <- year_start
    
  }else{
  
    # create an empty df
    df <- data.frame(GEOID = as.character(), NAME = as.character(), 
                     variable = as.character(), estimate = as.numeric(), 
                     moe = as.numeric(), year = as.numeric())
    
    years <- seq(year_start, year_end, 1)
    for (t in years){
      
      acs5_fl <- get_acs(geography = geo, state = state, variables = vector_vars, 
                         survey = "acs5", year = t, key= census_key)
      
      # add a column for year
      acs5_fl$year <- t
      
      # append rows to df
      df <- rbind(df, acs5_fl)
      
    }
  }
  # rename variables based on comments in above vector_vars, i.e., rename values in column called variable
  df <- df %>%
    mutate(variable = case_when(
      variable == "S2301_C02_030" ~ "DLFPR",
      variable == "S2301_C04_030" ~ "DUNR",
      variable == "S2301_C02_001" ~ "LFPR",
      variable == "S2301_C04_001" ~ "UNR",
      TRUE ~ variable  # Retain original value if no match
    ))
  return(df)
}

# Retrieves data by county level 
acs5_FLcounty <- get_ACS5_project(year_start=2023)

# Retrieves data by zip code or the Zip Code Tabulation Area ZCTA
acs5_FLzcta <- get_ACS5_project(year_start=2018, geo="zcta") 

df <- df %>%
  mutate(variable = case_when(
    variable == "S2301_C02_030" ~ "DLFPR",
    variable == "S2301_C04_030" ~ "DUNR",
    variable == "S2301_C02_001" ~ "LFPR",
    variable == "S2301_C04_001" ~ "UNR",
    TRUE ~ variable  # Retain original value if no match
  ))
ZCTAs <- acs5_FLzcta$GEOID
df <- df %>% filter(GEOID %in% ZCTAs)

clean_acs <- function(df) {
  df <- df %>%
    select(-moe) %>%  # Remove moe column
    pivot_wider(
      names_from = variable,  
      values_from = estimate,  # Only keep estimate values
      values_fill = list(estimate = NA)  # Fill missing values with NA
    )
  
  # get colmns that have a list data type into the usual type for a df or stata file
  df <- df %>%
    mutate(across(where(is.list), ~ sapply(., function(x) ifelse(length(x) > 0, x[[1]], NA_character_))))
  return(df)
}

acs5_FLcounty <- clean_acs(acs5_FLcounty)

acs5_FLzcta <- clean_acs(acs5_FLzcta)

acs5_FLcounty <- acs5_FLcounty %>% sort_by(acs5_FLcounty$GEOID)

acs5_FLcounty %>% write_csv("acs5_fl_county_2023.csv")
