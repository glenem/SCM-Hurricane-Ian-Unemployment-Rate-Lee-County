# clear environment before starting
rm(list = ls())

# Packages used to collect data for analysis ----
library(pacman)
pacman::p_load(readr, readxl, tidyverse, tigris, blscrapeR, tidycensus, haven)

# Collect population estimates from the Census API ----
options(tigris_use_cache = TRUE)

fl_county_pop <- get_estimates(geography = "county", state = 12, estimate = "population",
                               variable='POPESTIMATE', vintage = 2023, time_series = TRUE, key= Sys.getenv("USCENSUS_KEY"))

fl_county_pop2 <- get_estimates(geography = "county", state = 12, estimate = "population",
                               variable='POP', vintage = 2019, time_series = TRUE, key= Sys.getenv("USCENSUS_KEY"))

# from fl_county_pop2 drop rows where DATE is equal to 1 or 2
fl_county_pop2 <- fl_county_pop2 %>% filter(DATE != 1 & DATE != 2)

# in fl_county_pop2, rename DATE to year, and rename value to popestimate. Adjust the values 3 through 12 to be equal to 2010 to 2019 respectivly
fl_county_pop2 <- fl_county_pop2 %>% 
  mutate(year = DATE + 2007,
         popestimate = value) %>% 
  select(-DATE, -value)

# in fl_county_pop, rename value to popestimate
fl_county_pop <- fl_county_pop %>% 
  mutate(popestimate = value) %>% 
  select(-value)

# combine fl_county_pop and fl_county_pop2 by appending the rows
fl_county_pop_df <- rbind(fl_county_pop, fl_county_pop2)

# save fl_county_pop_df as a .dta file
write_dta(fl_county_pop_df, "1. Data/Inter_Data/fl_county_pop_df_2010-2023.dta")


