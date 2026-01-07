# clear environment before starting
rm(list = ls())

library(pacman)
# import pacakges
pacman::p_load(readxl, tidyverse, httr, jsonlite, blscrapeR, haven)


unr <- read_dta('1. Data/Inter_Data/unr.dta')
pop <- read_dta('1. Data/Inter_Data/fl_county_pop_df_2010-2024.dta')
gdp <- read_dta('1. Data/Inter_Data/fl_county_gdp.dta')
county_adj <- read_csv('1. Data/Raw_Data/county_adjacency2010.csv')
storm_data <- read_xlsx('1. Data/Raw_Data/storm_data_search_results.xlsx', 
                        sheet="storm_data_search_results")
acs <- read_dta("1. Data/Inter_Data/acs5_fl_2019-2023.dta")

#lag all ACS data

# List of variables to lag (excluding GEOID, NAME, and year)
vars_to_lag <- c("black", "native_american", "asian", "pacific_islander", "other",
                 "hispanic", "white", "pct_18p", "pct_less_hs_18p", "pct_hs_18p",
                 "pct_some_college_18p", "pct_bachelorh_18p", "bb_coverage_pct",
                 "median_income", "ln_med_inc", "renter_occ_pct", "owner_occ_pct")

# Apply 1-year lag within each GEOID group
for (var in vars_to_lag) {
  lagged_var <- paste0(var, "_lag1y")
  acs <- acs %>%
    group_by(GEOID) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(!!lagged_var := lag(.data[[var]], n = 1)) %>%
    ungroup()
}

# clean storm data ------

# keep rows in storm_data that have a unique combination of GeoFips and BEGIN_DATE, if there are duplicates only keep one
storm_data <- storm_data %>%
  mutate(BEGIN_DATE = as.Date(BEGIN_DATE, format = "%m/%d/%Y"),
         ) %>%
  mutate(GeoFips = as.character(GeoFips)) %>%
  mutate(EVENT_TYPE = as.character(EVENT_TYPE))

storm_data <- storm_data %>% select(c(GeoFips, BEGIN_DATE, END_DATE, 
                                      EVENT_TYPE))
# in storm_data remove duplicates
storm_data <- storm_data %>%
  group_by(GeoFips, BEGIN_DATE) %>%
  slice(1) %>%
  ungroup()

#extract month and year from BEGIN_DATE
storm_data <- storm_data %>%
  mutate(month = month(BEGIN_DATE),
         year = year(BEGIN_DATE))

# lag unr data ------

# lag unemployment data by 1 year or 12 months
unr <- unr %>%
arrange(county_code, month) %>%
  group_by(county_code) %>%
  mutate(unr_lag1y = lag(unr, n = 1))

# lag unemployment data by 1 month
unr <- unr %>%
  arrange(county_code, year, month) %>%
  group_by(county_code) %>%
  mutate(unr_lag1m = lag(unr, n = 1))


# merge data ------

# subsetting data to 2010 since that is our last population data
gdp <- gdp %>% filter(year >= 2010)
unr <- unr %>% filter(year >= 2010)
storm_data <- storm_data %>% filter(year >= 2010)

# merge population and GDP data
merge <- gdp %>% left_join(pop, by = c("GeoFips" = "GEOID", 'year'='year'))
merge$gdp_per_cap <- merge$gdp/merge$popestimate
merge$rgdp_per_cap <- merge$rgdp/merge$popestimate

# create natural log versions of RGDP and GDP per capital
merge$ln_rgdp_per_cap <- log(merge$rgdp_per_cap)
merge$ln_gdp_per_cap <- log(merge$gdp_per_cap)

# create a 1 year lag of the log RGDP and GDP per capital variables
merge <- merge %>%
  arrange(GeoFips, year) %>%
  group_by(GeoFips) %>%
  mutate(ln_rgdp_per_cap_lag1y = lag(ln_rgdp_per_cap, n = 1),
         ln_gdp_per_cap_lag1y = lag(ln_gdp_per_cap, n = 1)) %>%
  ungroup()

# create a log of the population estimate
merge$ln_popestimate <- log(merge$popestimate)

# create an annual population growth rate using the popestimate variable for each county
merge <- merge %>%
  group_by(GeoFips) %>%
  mutate(pop_growth_rate = ((popestimate - lag(popestimate, n = 1)) / lag(popestimate, n = 1))*100) %>%
  ungroup()

merge <- merge %>%
  arrange(GeoFips, year) %>%
  group_by(GeoFips) %>%
  mutate(pop_growth_rate_lag1y = lag(pop_growth_rate, n = 1),
         ) %>%
  ungroup()

# merge ACS data
merge <- merge %>% left_join(acs, by = c("GeoFips" = "GEOID", 'year'='year'))

merge <- merge %>% select(-c("NAME.y"))

# merge laus data
merge <- merge %>% left_join(unr, by = c("GeoFips" = "county_code", 'year'='year'))
# merge hurricane data
merge <- merge %>% left_join(storm_data, by = c("GeoFips" = "GeoFips", 'year'='year', 'month'='month'))

# if EVENT_TYPE == na then 0, else 1
merge <- merge %>% mutate(hurricane = ifelse(is.na(EVENT_TYPE), 0, 1))

# add in county_adjacency data
merge <- merge %>% left_join(county_adj, by = c("GeoFips" = "fipscounty"),relationship = "many-to-many")

# if GEOFips == fipsneighbor then drop the row
merge <- merge %>% filter(GeoFips != fipsneighbor)

merge <- merge %>% select(-c("NAME.x"))

# Organize file in an order that makes sense

merge <- merge %>% relocate(c('GeoFips', 'month', 'year', 'hurricane', 'unr'))

write_final_data <- function(df){
  write_dta(df, '1. Data/data.dta')
}
# write the final data to a dta file
write_final_data(merge)

