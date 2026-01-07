#############################
# Author: Glen Martin
# Date Updated: 12/23/25
# Purpose: Main file to run my research on the impact of hurricane Ian on 
#          lee county's unemployment rate
#
#
###############################

# Run the following if pacman is not installed. 
# Pacman will load and install all packages used in this project
#install.packages("pacman")

# Clean the data --------------
# Clean the BLS data that was downloaded from the website. This is LAUS data
source('2. Code/01_get_data_bls.R')

# Download 5-year ACS data from 2019 to 2023, used as predictors in our model
source('2. Code/02_get_data_acs.R')

# Download Population Data From the County Totals Table
source('2. Code/03_get_data_us_census.R')

# Transform the county level GDP data into a usable format, so from short-to-long
source('2. Code/04_transform_county_gdp.R')

# Merge files from 1 to 4 and adds the county adj data and storm events data
source('2. Code/05_merge_data.R')

# Results ------------
# Run the Lee County Synthetic Control files including 062 and 063
source('2. Code/06_1_Lee_County_Synth.R')

# Robustness Checks -----------------
# In Space Placebo Analysis
source('2. Code/06_4_In_Space_Placebo_Analysis.R')

# In-Time Placebo Analysis
source('2. Code/06_5_In_Time_Placebo_Analysis.R')

# Robustness Checks

source('2. Code/06_6_Robustness_Checks.R')

# Run Python Script to create nice graphs-----------
rm(list = ls())
pacman::p_load(reticulate)
org_wd <- getwd()
source_python('2. Code/07_SCM_Graphs_Python.py')
setwd(org_wd)
rm(list = ls())