####################################################################################
## Script Name : data_cleaning.R                                                  ##                                         
## Description : Takes raw data from OWID and Hofstede, cleans and combines them ##
## Changes :                                                                      ##
####################################################################################

# packages
require(tidyverse)
require(countrycode)
library(wbstats)
require(stargazer)
library(readxl)
library(plm)
library(AER)
library(plyr)

# set directories
dir = dirname(rstudioapi::getActiveDocumentContext()$path)
dirData = paste0(dir,'/data')

###########################
###### Data Cleaning ######
###########################

owid = read.csv(paste0(dirData, '/owid-covid-data.csv'))
hof = read.csv(paste0(dirData, '/raw_6_dimensions.csv'), sep=';')
ob = as.data.frame(read.table(paste0(dirData, '/obesity-data.tsv'), sep = '\t', header = TRUE))
cc = as.data.frame(countrycode::codelist)[c('country.name.en', 'iso2c', 'iso3c')]
pop_dens  = wb_data(
  indicator = c("EN.POP.DNST"),
  country = "countries_only",
  start_date = 2020,
  end_date    = 2021)[c('iso3c', 'EN.POP.DNST')]
colnames(pop_dens)[colnames(pop_dens) == 'EN.POP.DNST'] = 'population_density'
freedom = read_excel(paste0(dirData, '/freedom-house-data.xlsx'), 2)[c('Country/Territory', 'Edition', 'Status', 'Total')]
freedom = freedom[freedom$Edition == '2021', ]

# filter to eu countries
eu_iso = c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL',
                  'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE')
eu_countries = c('Austria','Belgium','Bulgaria','Croatia','Cyprus','Czech Rep','Denmark','Estonia','Finland','France',
                 'Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania','Luxembourg','Malta',
                 'Netherlands','Poland','Portugal','Romania','Slovak Rep','Slovenia','Spain','Sweden')

# obesity with country codes
ob$bmi30 = str_split_fixed(ob$unit.bmi.geo.time, ",", 3)[,2]
ob$iso2c = str_split_fixed(ob$unit.bmi.geo.time, ",", 3)[,3]
ob = ob[c('iso2c', 'bmi30', 'X2019')] %>% inner_join(cc)
colnames(ob)[colnames(ob) == 'X2019'] = 'obesity_rate_2019'
ob = filter(ob, bmi30 == 'BMI_GE30')

# freedom house with country codes
freedom = freedom %>% inner_join(cc, by = c('Country/Territory' = 'country.name.en'))
colnames(freedom)[colnames(freedom) == 'Status'] = 'dFreedom_status'
colnames(freedom)[colnames(freedom) == 'Total'] = 'cFreedom_score'

owid_eu = owid %>%
  filter(iso_code %in% eu_iso)
hof_eu = hof %>%
  filter(country %in% eu_countries)

# sanity check
if (length(eu_iso) != n_distinct(owid_eu$iso_code)) {
  cat("Error: nr. of eu countries (",length(eu_iso), ") does not equal nr. of countries found in owid df (", n_distinct(owid_eu$iso_code), ").")
  cat("Missing value(s):", eu_iso[which(!eu_iso %in% owid_eu$iso_code)])
} else if (length(eu_countries) != n_distinct(hof_eu$country)) {
  cat("Error: nr. of eu countries (",length(eu_countries), ") does not equal nr. of countries found in owid df (", n_distinct(hof_eu$country), ").")
  cat("Missing value(s):", eu_iso[which(!eu_countries %in% hof_eu$country)])
} 

# subset and keep only variables of interest
vars = c('iso_code','date','positive_rate', 'hosp_patients_per_million', 'stringency_index')
owid_eu = owid_eu[vars]

# typecasting
owid_eu$date = as.Date(owid_eu$date)

#################################################################
# !!! CHANGE excess_mortality MEASURE TO TOTAL INSTEAD OF % !!! #
#################################################################

# change to correct iso codes
for (i in 1:nrow(hof_eu)) {
  country = eu_countries[i]
  iso = eu_iso[i]
  hof_eu$ctr[hof_eu$country == country] <- iso
}
hof_eu = subset(hof_eu, select = -country)


# calculate aggregations
start_date = "2021-02-01"
end_date = "2021-06-01"
df = owid_eu %>%
  filter(date >= start_date & date <= end_date) %>%
  inner_join(pop_dens, by= c('iso_code' = 'iso3c')) %>%
  left_join(ob[c('iso3c', 'obesity_rate_2019')], by = c('iso_code' = 'iso3c')) %>%
  inner_join(hof_eu, by = c('iso_code' = 'ctr')) %>%
  inner_join(freedom[c('iso3c', 'dFreedom_status', 'cFreedom_score')], by = c('iso_code' = 'iso3c'))
write.csv(df, paste0(dirData, '/final_df.csv'))

