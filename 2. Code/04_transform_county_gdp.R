# Transform County GDP Data

# clear environment before starting
rm(list = ls())

library(pacman)
pacman::p_load(readr, readxl, tidyverse, haven)

# Clean BEA data
# Read in the data from the excel file
bea_GDP <- read_excel("1. Data/Raw_Data/bea-gdp-FL-counties_2001-2023.xlsx", 
                      range = "A6:AA142")
# drop rows using Florida GeomName
bea_GDP <- bea_GDP[!grepl("Florida", bea_GDP$GeoName),]

# drop all columns except geomfips and county
geomFips_df <- bea_GDP %>%  filter(LineCode == "1") %>% select(GeoFips, GeoName)

# function to clean the GDP data
clean_gdp <- function(input_df, line_code, 
                      year_start, year_end, value_name){
  
  # create empty dataframe of county_code, year, and value (either gdp or rgdp)
  output_df <- data.frame(county_code = character(),
                            year = numeric(),
                            value = numeric())
  
  # create a new data set by filtering for rows for LineDescription == 1
  raw_df <- input_df %>% filter(LineCode == as.character(line_code))
  # transpose the data frame so that the years are in columns
  raw_df <- as.data.frame(t(raw_df))
  
  # drop rows in RGDP dataset by index name
  raw_df <- raw_df[-c(2:4),]
  # Create year vector from 2013 to 2023
  row_names <- rownames(raw_df)[2:nrow(raw_df)]
  first_row <- as.numeric(row_names[1])
  last_row <- as.numeric(row_names[length(row_names)])
  
  years <- seq(first_row, last_row, 1)
  n_rows = last_row-first_row+1
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
    output_df <- rbind(output_df, data.frame(county_code = values, year = years, value = temp))
  }
  # rename value column to value_name
  names(output_df)[3] <- value_name
  return(output_df)
}

county_rgdp <- clean_gdp(bea_GDP, 1, 2001, 2023, 'rgdp')

county_gdp <- clean_gdp(bea_GDP, 3, 2001, 2023, 'gdp')

# join county_gdp, county_rgdp, and geomFips_df so that we have a dataframe with GeoFips, year, gdp, rgdp, and GeoName using GeoFips as the key
fl_county_gdp <- county_gdp %>% left_join(county_rgdp, by = c("county_code" = "county_code", "year" = "year"))
# rename county_code to GeoFips in fl_county_gdp df
names(fl_county_gdp)[1] <- "GeoFips"
fl_county_gdp <- fl_county_gdp %>% left_join(geomFips_df, by = c("GeoFips" = "GeoFips"))

# Split the county name from ', FL' in the GeomName column
fl_county_gdp$County <- gsub(", FL", "", fl_county_gdp$GeoName)

# separate state code as the first two digits of GeoFips
fl_county_gdp$State_Code <- substr(fl_county_gdp$GeoFips, 1, 2)
fl_county_gdp$State_Name <- 'Florida'

# transform GDP and RGDP so that it is not in thousands. Will help us to calculate per capita metrics in a different file.
fl_county_gdp$rgdp <- fl_county_gdp$rgdp*1000 
fl_county_gdp$gdp <- fl_county_gdp$gdp*1000

# save fl_county_gdp as an dta file
write_dta(fl_county_gdp, "1. Data/Inter_Data/fl_county_gdp.dta")
