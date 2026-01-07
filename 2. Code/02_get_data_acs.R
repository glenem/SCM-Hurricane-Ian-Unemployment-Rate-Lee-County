# clear environment before starting
rm(list = ls())

# Packages used to collect data for analysis ----
library(pacman)
pacman::p_load(readr, readxl, tidyverse, tigris, blscrapeR, tidycensus, haven)

census_api_key(Sys.getenv("USCENSUS_KEY"))


# Collect ACS 5-year estimates from Tidycensus ----

vector_vars = c("S1901_C01_012", # Median Income
                "S0601_C01_001", # Total Population
                "S0601_C01_022", # White, not Hispanic or Latino
                "S0601_C01_017", # Asian
                "S0601_C01_015", # Black or African American
                "S0601_C01_018", # Native Hawaiian and Other Pacific Islander
                "S0601_C01_021", # Hispanic or Latino
                "S0601_C01_016", # American Indian and Alaska Native
                "S0601_C01_019", # Some other
                "S1501_C01_001", # Total Pop 18-24
                "S1501_C01_002", #	Less than HS
                "S1501_C01_003", # HS Grad
                "S1501_C01_004", # Some College
                "S1501_C01_005", # Bachelor's or higher
                "S1501_C01_006", # Total Pop 25+
                "S1501_C01_007", # Less than 9th grade
                "S1501_C01_008", # 9th to 12th grade, no diploma
                "S1501_C01_009", # High school graduate (includes equivalency)
                "S1501_C01_010", # Some college, no degree
                "S1501_C01_015", # Bachelor's degree or higher
                "S2502_C01_001", # Total Occupied Housing Units
                "S2502_C01_003", # Owner Occupied
                "S2502_C01_005", # Renter Occupied
                "S0101_C01_026", # 18 and over
                "S0101_C01_028", # 60 and over
                "S2801_C01_001", # Total Households
                "S2801_C01_017" # broadband coverage
) 



get_ACS5_project <- function(vector_var=vector_vars, year_start, year_end, state=12){
  
  # create an empty df
  df <- data.frame(GEOID = as.character(), NAME = as.character(), 
                   variable = as.character(), estimate = as.numeric(), 
                   moe = as.numeric(), year = as.numeric())
  
  
  years <- seq(year_start, year_end, 1)
  
  for (t in years){
    
    acs5_fl <- get_acs(geography = "county", state = state, variables = vector_vars, 
                       survey = "acs5", year = t)
    
    # add a column for year
    acs5_fl$year <- t
    
    # append rows to df
    df <- rbind(df, acs5_fl)
    
  }
  # rename variables based on comments in above vector_vars, i.e., rename values in column called variable
  df <- df %>%
    mutate(variable = case_when(
      variable == "S1901_C01_012" ~ "median_income",
      variable == "S0601_C01_001" ~ "total_pop",
      variable == "S0601_C01_022" ~ "white",
      variable == "S0601_C01_017" ~ "asian",
      variable == "S0601_C01_015" ~ "black",
      variable == "S0601_C01_018" ~ "pacific_islander",
      variable == "S0601_C01_021" ~ "hispanic",
      variable == "S0601_C01_016" ~ "native_american",
      variable == "S0601_C01_019" ~ "other",
      variable == "S1501_C01_001" ~ "pop_18_24",
      variable == "S1501_C01_002" ~ "less_than_hs_18",
      variable == "S1501_C01_003" ~ "hs_grad_18",
      variable == "S1501_C01_004" ~ "some_college_18",
      variable == "S1501_C01_005" ~ "bachelors_or_higher_18",
      variable == "S1501_C01_006" ~ "pop_25_plus",
      variable == "S1501_C01_007" ~ "less_than_9th_25",
      variable == "S1501_C01_008" ~ "grade_9th_to_12th_25",
      variable == "S1501_C01_009" ~ "hs_grad_25",
      variable == "S1501_C01_010" ~ "some_college_25",
      variable == "S1501_C01_015" ~ "bachelors_or_higher_25p",
      variable == "S2502_C01_001" ~ "total_occupied_housing_units",
      variable == "S2502_C01_003" ~ "owner_occupied",
      variable == "S2502_C01_005" ~ "renter_occupied",
      variable == "S0101_C01_026" ~ "age_18_plus",
      variable == "S0101_C01_028" ~ "age_60_plus",
       variable == "S2801_C01_017" ~ "bb_coverage_households",
      variable == "S2801_C01_001" ~ "ttl_households",
      TRUE ~ variable  # Retain original value if no match
    ))
  return(df)
}

acs5_fl_2019_2023 <- get_ACS5_project(vector_var=vector_vars, year_start=2019, year_end=2023, state=12) 

clean_acs <- function(df) {
  df <- df %>%
    select(-moe) %>%  # Remove moe column
    pivot_wider(
      names_from = variable,  
      values_from = estimate,  # Only keep estimate values
      values_fill = list(estimate = NA)  # Fill missing values with NA
    )
  
  df <- df %>%
    mutate(total_pop = gsub("c\\(|\\)", "", total_pop),  # Remove "c(" and ")"
           total_pop = sub(",.*", "", total_pop))         # Keep only first value
  
  # get colmns that have a list data type into the usual type for a df or stata file
  df <- df %>%
    mutate(across(where(is.list), ~ sapply(., function(x) ifelse(length(x) > 0, x[[1]], NA_character_))))
  return(df)
}

acs5_fl_2019_2023 <- clean_acs(acs5_fl_2019_2023)

acs5_fl_2019_2023 <- acs5_fl_2019_2023 %>% sort_by(acs5_fl_2019_2023$GEOID, acs5_fl_2019_2023$year)

acs5_fl_2019_2023$pct_18p <- (acs5_fl_2019_2023$age_18_plus/as.numeric(acs5_fl_2019_2023$total_pop))*100

acs5_fl_2019_2023$pct_less_hs_18p <- ((acs5_fl_2019_2023$less_than_hs_18+acs5_fl_2019_2023$less_than_9th_25+acs5_fl_2019_2023$grade_9th_to_12th_25)/(acs5_fl_2019_2023$pop_18_24 + acs5_fl_2019_2023$pop_25_plus))*100
acs5_fl_2019_2023$pct_hs_18p <- ((acs5_fl_2019_2023$hs_grad_18+acs5_fl_2019_2023$hs_grad_25)/(acs5_fl_2019_2023$pop_18_24 + acs5_fl_2019_2023$pop_25_plus))*100
acs5_fl_2019_2023$pct_some_college_18p <- ((acs5_fl_2019_2023$some_college_18+acs5_fl_2019_2023$some_college_25)/(acs5_fl_2019_2023$pop_18_24 + acs5_fl_2019_2023$pop_25_plus))*100
acs5_fl_2019_2023$pct_bachelorh_18p <- ((acs5_fl_2019_2023$bachelors_or_higher_18+acs5_fl_2019_2023$bachelors_or_higher_25p)/(acs5_fl_2019_2023$pop_18_24 + acs5_fl_2019_2023$pop_25_plus))*100

acs5_fl_2019_2023$bb_coverage_pct <- (acs5_fl_2019_2023$bb_coverage_households/acs5_fl_2019_2023$ttl_households)*100

acs5_fl_2019_2023$ln_med_inc <- log(acs5_fl_2019_2023$median_income)
acs5_fl_2019_2023$renter_occ_pct <- (acs5_fl_2019_2023$renter_occupied/acs5_fl_2019_2023$total_occupied_housing_units)*100
acs5_fl_2019_2023$owner_occ_pct <- (acs5_fl_2019_2023$owner_occupied/acs5_fl_2019_2023$total_occupied_housing_units)*100

acs5_fl_2019_2023 <- acs5_fl_2019_2023 %>% select(c(GEOID, NAME, year, black, native_american, 
                               asian, pacific_islander, other, hispanic, white, 
                               pct_18p, pct_less_hs_18p, pct_hs_18p, pct_some_college_18p, 
                               pct_bachelorh_18p, bb_coverage_pct, median_income, ln_med_inc,
                               renter_occ_pct, owner_occ_pct))

acs5_fl_2019_2023 %>% write_dta("1. Data/Inter_Data/acs5_fl_2019-2023.dta")