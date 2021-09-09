####################################################################################
## Script Name : data_cleaning.R                                                  ##                                         
## Description : Takes raw data from OWID and Hofstede, cleans and combines them ##
## Changes :                                                                      ##
####################################################################################

# packages
require(tidyverse)


###########################
###### Data Cleaning ######
###########################

owid = read.csv('./data/owid-covid-data.csv')
hof = read.csv('./data/raw_6_dimensions.csv', sep=';')

# filter to eu countries
eu_iso = c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL',
                  'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE')
eu_countries = c('Austria','Belgium','Bulgaria','Croatia','Cyprus','Czech Rep','Denmark','Estonia','Finland','France',
                 'Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania','Luxembourg','Malta',
                 'Netherlands','Poland','Portugal','Romania','Slovak Rep','Slovenia','Spain','Sweden')

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
#vars = c('new_cases', 'hosp_patients', 'stringency_index', 'excess_mortality')
#df = owid_eu[vars]

# typecasting
owid_eu$date = as.Date(owid_eu$date)

#################################################################
# !!! CHANGE excess_mortality MEASURE TO TOTAL INSTEAD OF % !!! #
#################################################################

# calculate aggregations
start_date = "2021-02-01"
end_date = "2021-05-21"
df = owid_eu %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(iso_code) %>%
    summarise(new_cases = sum(new_cases, na.rm=TRUE),
            hosp_patients = sum(hosp_patients, na.rm=TRUE),
            stringency_index = mean(stringency_index, na.rm=TRUE),
            excess_mortality = mean(excess_mortality, na.rm=TRUE))

# change to correct iso codes
for (i in 1:nrow(hof_eu)) {
  country = eu_countries[i]
  iso = eu_iso[i]
  hof_eu$ctr[hof_eu$country == country] <- iso
}
hof_eu = subset(hof_eu, select = -country)

# join on iso codes
final_df = df %>% inner_join(hof_eu, by = c("iso_code" = "ctr"))

# write csv
write.csv(final_df, "./data/final_df.csv", row.names=FALSE)
