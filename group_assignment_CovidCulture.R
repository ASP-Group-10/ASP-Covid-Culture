####################################################################################
## Script Name : group_assignment_covidCulture.R                                  ##                                         
## Description : Group 10's take on the ASAP group assignment                     ##                                                                     
####################################################################################

# packages
require(tidyverse)
require(countrycode)
library(wbstats)
require(stargazer)
library(readxl)
library(plm)
library(AER)
library(dplyr)
library(lubridate)
library(zoo)

# set directories
dir = dirname(rstudioapi::getActiveDocumentContext()$path)
dirData = paste0(dir,'/data')
dirRes = paste0(dir,'/figures')

# =====================================================================
# Acquire data from different sources
# =====================================================================

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
gov = read.csv(paste0(dirData, '/OxCGRT_latest.csv'))

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

# keep EU countries only
owid_eu = owid %>%
  filter(iso_code %in% eu_iso)
hof_eu = hof %>%
  filter(country %in% eu_countries)
gov_eu = gov %>%
  filter(CountryCode %in% eu_iso)
freedom = freedom %>%
  filter(iso3c %in% eu_iso)

# sanity check
check_eu = function(df, df_col, eu_lst) {
  if (length(eu_lst) != n_distinct(df[df_col])) {
    cat("Error: nr. of eu countries (",length(eu_lst), ") does not equal nr. of countries found in df (", n_distinct(df[df_col]), ").")
  } else {
    print("All EU countries are included!")
  }
}
check_eu(owid_eu, 'iso_code', eu_iso)
check_eu(hof_eu, 'country', eu_countries)
check_eu(gov_eu, 'CountryCode', eu_iso)
check_eu(freedom, 'iso3c', eu_iso) # missing CZE

# subset and keep only variables of interest
vars = c('iso_code','date','positive_rate', 'hosp_patients_per_million', 'people_vaccinated_per_hundred')
owid_eu = owid_eu[vars]

gov_vars = c('CountryCode', 'Date', 'C1_School.closing', 'C2_Workplace.closing', 'C3_Cancel.public.events', 
             'C4_Restrictions.on.gatherings', 'C6_Stay.at.home.requirements')
gov_eu = gov_eu[gov_vars]

# change column names
old_names = c('C1_School.closing', 'C2_Workplace.closing', 'C3_Cancel.public.events',
              'C4_Restrictions.on.gatherings','C6_Stay.at.home.requirements')
new_names = c('school_closing', 'workplace_closing', 'cancel_public_events',
              'restrictions_on_gatherings', 'stay_at_home')
for (i in 1:length(old_names)) {
  print(paste('Changing column name for:', old_names[i], 'to', new_names[i]))
  colnames(gov_eu)[colnames(gov_eu) == old_names[i]] = new_names[i]
  print('Done!')
}

# typecasting
owid_eu$date = as.Date(owid_eu$date)
gov_eu$Date = as.Date(as.character(gov_eu$Date), "%Y%m%d")

# =====================================================================
# Join dataframes & correct mistaken Hofstede values
# =====================================================================

# change to correct iso codes
for (i in 1:nrow(hof_eu)) {
  country = eu_countries[i]
  iso = eu_iso[i]
  hof_eu$ctr[hof_eu$country == country] <- iso
}
hof_eu = subset(hof_eu, select = -country)
hof_eu = hof_eu[c('ctr','pdi','idv','ivr')]

# calculate aggregations
start_date = "2021-02-01"
end_date = "2021-06-01"
df = owid_eu %>%
  inner_join(gov_eu, by = c('iso_code' = 'CountryCode', 'date' = 'Date') ) %>%
  filter(date >= start_date & date <= end_date) %>%
  inner_join(pop_dens, by= c('iso_code' = 'iso3c')) %>%
  inner_join(hof_eu, by = c('iso_code' = 'ctr')) %>%
  left_join(ob[c('iso3c', 'obesity_rate_2019')], by = c('iso_code' = 'iso3c')) %>%
  left_join(freedom[c('iso3c', 'dFreedom_status', 'cFreedom_score')], by = c('iso_code' = 'iso3c'))

# sanity check for having all EU countries
check_eu(df, 'iso_code', eu_iso)

# typecast
numcols = c('obesity_rate_2019','pdi','idv','ivr')

for (col in numcols) {
  print(paste('Typecasting column:', col))
  print(paste('Old type:', class(df[[col]])))
  df[[col]] = as.numeric(as.character(df[[col]]))
  print(paste('New type:', class(df[[col]])))
}

# subset data and summarize
df.sub = df[complete.cases(df[c('hosp_patients_per_million', 'pdi')]),]
cat("Missing value(s):",eu_iso[which(!eu_iso %in% df.sub$iso_code)])
df.sub$positive_rate = df.sub$positive_rate * 100
stargazer(df.sub, type='text')

# order by country and date
df.sub = arrange(df.sub, iso_code, date)

# change outlier numbers to 0-100 scale 
# following https://geerthofstede.com/research-and-vsm/dimension-data-matrix/
df.sub$pdi[df.sub$iso_code == 'SVK'] = 100

# =====================================================================
# Handle missing vaccination data + add lagged effect for vaccination
# =====================================================================

# remove missing values for 'people_vaccinated_per_hundred'
# by taking the average of trailing and subsequent non-missing values
# & create lagged effects, courtesy of https://stackoverflow.com/questions/3558988/basic-lag-in-r-vector-dataframe 
lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

df.final = data.frame()
for (ctr in unique(df.sub$iso_code)) {
  df_ctr = df.sub %>%
    filter(iso_code == ctr) %>%
    mutate(people_vaccinated_per_hundred = round(na.approx(people_vaccinated_per_hundred, na.rm=FALSE),2))
  df.final = rbind(df.final, df_ctr)
}

# Summary table (INCLUDE THIS IN OUR PAPER)
df.final = as.data.frame(df.final)
stargazer(df.final)

# remove not needed objects
rm(list=setdiff(ls(), c("df", "df.sub", "df.final", "dir", 'dirData', 'dirRes')))

# write final df
write.csv(df.final, paste0(dirData, '/final_df.csv'))

# =====================================================================
# Factorize restrictions, calculate averages and balance the dataset
# =====================================================================

# calculate averages
library(plyr)
df.avg = 
  ddply(df.final, .(iso_code), summarise,
        avg.positive_rate = mean(positive_rate, na.rm=TRUE),
        avg.hosp_patients_per_million = mean(hosp_patients_per_million, na.rm=TRUE),
        avg.people_vaccinated_per_hundred = mean(people_vaccinated_per_hundred, na.rm=TRUE),
        avg.population_density = mean(population_density, na.rm=TRUE),
        avg.obesity_rate_2019 = mean(obesity_rate_2019, na.rm=TRUE),
        avg.pdi = mean(pdi, na.rm=TRUE),
        avg.idv = mean(idv, na.rm=TRUE),
        avg.ivr = mean(ivr, na.rm=TRUE),
        avg.cFreedom_score = mean(cFreedom_score, na.rm=TRUE),
        numValid = length(iso_code)
  )

# join the avg df with original
df.final = merge(df.final, df.avg, on="iso_code")

# select countries present in all periods
max_valid = max(df.avg$numValid)
df.final = df.final[df.final$numValid == max_valid,]
df.avg = df.avg[df.avg$numValid == max_valid,]

# =====================================================================
# Check regression assumptions + create plots
# =====================================================================

# scatter plot of vaccination and hospitalized patients (INCLUDE THIS IN OUR PAPER)
# because of the non-linear nature of vaccination, use quadratic term
ggplot(df.final, aes(x=people_vaccinated_per_hundred,y=hosp_patients_per_million)) +
  geom_smooth(aes(color=iso_code),show.legend = T, alpha=0.65, se=F) + 
  labs(title="Relationship between VAC and HOSPI",
       subtitle="Using a scatter plot",
       x="VAC",
       y="HOSPI") +
  guides(size = F,
         color = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.2)),
      axis.text = element_text(size = rel(1.2))) 
ggsave(paste0(dirRes, "/Vacc_hospPatients.png"),  width=8, height=4)

# relationship between Power Distance and hospitalized patients (INCLUDE THIS IN OUR PAPER)
ggplot(df.avg, aes(x=avg.pdi,y=avg.hosp_patients_per_million)) +
  geom_point(aes(fill=iso_code), colour="black", pch=21, size=9, show.legend = F, alpha=0.65) + 
  geom_smooth(method='lm', se=T, colour='black', alpha=0.2) +
  geom_text(aes(label=iso_code),hjust=-0.45, vjust=0.8) +
  labs(title="Relationship between PDI and HOSPI",
       x="Power Distance",
       y="Avg. Hospitalized patients per million") +
  xlim(10,103) +
  guides(size = F,
         color = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) 
ggsave(paste0(dirRes, "/PD_hospPatients.png"),  width=8, height=4)


# relationship between Individualism and hospitalized patients (INCLUDE THIS IN OUR PAPER)
ggplot(df.avg, aes(x=avg.idv,y=avg.hosp_patients_per_million)) +
  geom_point(aes(fill=iso_code), colour="black", pch=21, size=9, show.legend = F, alpha=0.65, se=F) +
  geom_smooth(method='lm', se=T, colour='black', alpha=0.2) +
  geom_text(aes(label=iso_code),hjust=-0.45, vjust=0.8) +
  labs(title="Relationship between IND and HOSPI",
       x="IND",
       y="Avg. HOSPI") +
  xlim(27,82) +
  guides(size = F,
         color = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) 
ggsave(paste0(dirRes, "/Indiv_hospPatients.png"),  width=8, height=4)

# relationship between Indulgence and hospitalized patients (INCLUDE THIS IN OUR PAPER)
ggplot(df.avg, aes(x=avg.ivr,y=avg.hosp_patients_per_million)) +
  geom_point(aes(fill=iso_code), colour="black", pch=21, size=9, show.legend = F, alpha=0.65, se=F) + 
  geom_smooth(method='lm', se=T, colour='black', alpha=0.2) +
  geom_text(aes(label=iso_code),hjust=-0.45, vjust=0.8) +
  labs(title="Relationship between IDR and HOSPI",
       x="IDR",
       y="Avg. HOSPI") +
  xlim(14,82) +
  guides(size = F,
         color = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) 
ggsave(paste0(dirRes, "/indulgence_hospPatients.png"),  width=8, height=4)

# =====================================================================
# formulate the model
# =====================================================================

mdlA = hosp_patients_per_million ~ positive_rate + people_vaccinated_per_hundred + I(people_vaccinated_per_hundred^2) + 
                        factor(school_closing) + factor(workplace_closing) +
                        factor(cancel_public_events) + factor(restrictions_on_gatherings) + factor(stay_at_home) +
                        population_density + obesity_rate_2019 +
                        pdi + idv + ivr

# estimate models
rslt.Pooling = plm(mdlA, data=df.final, model="pooling")
rsltFE.Country = plm(mdlA, data=df.final,
                     index=c("iso_code","date"), model="within")
rsltRE.Country = plm(mdlA, data=df.final,
                     index=c("iso_code","date"), model="random", random.method = "walhus")
pooltest(rslt.Pooling, rsltFE.Country)

# Hausman test: compare random and fixed effects models
# H0: no correlation between disturbance and explanatory
# variables, both RE and FE are consistent (though FE not efficient)
# H1: correlation between disturbance, only FE is consistent
phtest(rsltFE.Country, rsltRE.Country)

# Hausman-Taylor estimator --> an instrumental variable
# estimator without external instruments; it considers
# time-variant variables as instruments for time invariant variables
mdlB = hosp_patients_per_million ~ positive_rate + people_vaccinated_per_hundred + I(people_vaccinated_per_hundred^2) + 
  factor(school_closing) + factor(workplace_closing) +
  factor(cancel_public_events) + factor(restrictions_on_gatherings) + factor(stay_at_home) +
  population_density + obesity_rate_2019 +
  pdi + idv + ivr | positive_rate + people_vaccinated_per_hundred + I(people_vaccinated_per_hundred^2) + 
  factor(school_closing) + factor(workplace_closing) +
  factor(cancel_public_events) + factor(restrictions_on_gatherings) + factor(stay_at_home) | 
  population_density + obesity_rate_2019 + pdi + idv + ivr
  
rsltREHT.Country = plm(mdlB, data=df.final,
            index=c("iso_code","date"), model="random", random.method = "ht", inst.method = "baltagi")

# Hausman test
phtest(rsltFE.Country, rsltREHT.Country) # HT model is consistent

# check multicollinearity with VIF
vif(rsltREHT.Country) # only multicollinear variables are the vaccination terms with quadratics which is fine

# heteroskedasticity for simple RE model
lmtest::bptest(rsltREHT.Country)

# robust se
seBasic = sqrt( diag ( vcov (rsltREHT.Country)))
seWhite = sqrt( diag ( vcovHC (rsltREHT.Country, type="HC0"))) # not working
stargazer(rslt.Pooling, rsltFE.Country, rsltRE.Country, rsltREHT.Country, type='text') # ADD THIS TABLE TO FINAL REPORT
