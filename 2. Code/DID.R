# clear environment before starting
rm(list = ls())

# import packages
pacman::p_load(readxl, 
               tidyverse, # tidyverse
               haven, # used to read dta files
               estimatr # for robust estimation
               )

df <- read_dta('1. Data/data.dta')

# Reduce df dimensionality because we are not excluding neighbors
df <- df %>%
  distinct(GeoFips, month, year, .keep_all = TRUE)

# add a month_year field
df <- df %>%
  mutate(month_year = as.Date(paste(year, month, "01", sep = "-")))

# Subset for time period greater than or equal to January 2021, to not include COVID-19 effects
df <- df %>% filter(month_year >= as.Date("2021-01-01"))

# Reorder fields
df <- df %>% relocate(c('GeoFips', 'month', 'year', 'month_year', 'hurricane', 'unr'))

# Remove counties impacted by hurricane Idalia in 2023
hurricane_fips <- df %>% filter(hurricane == 1 & year==2023) %>% select(GeoFips)
df <- df %>% filter(!GeoFips %in% hurricane_fips$GeoFips)
rm(hurricane_fips)

# need to think how I want to specify my did model since my data is at the monthly frequency
did <- df %>% 
  lm_robust(as.formula("unr ~ hurricane*month_year + month + year + ln_rgdp_per_cap + pop_growth_rate + ln_med_inc + renter_occ_pct + owner_occ_pct + GeoFips"), 
            data = ., 
            clusters = GeoFips
            )
summary(did)
