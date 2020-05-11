rm(list = ls())

library(vroom)
library(stringr)
library(purrr)
library(dplyr)
library(readxl)
library(tidyr)

# TODO: Should you use ARCOS population estimates? i.e. ACS 3 year
nj_pop_data <- 
  vroom(file.path("data-raw", "nj-pop-data.csv"))

nj_data <- 
  vroom(file.path("data-raw", "nj-data.csv")) %>% 
  select(buyer_county, year, dosage_unit)

nj_prescription_rates <-
  vroom(file.path("data-raw", "prescription-rates-nj.csv")) %>% 
  select(County, year, prescription_rate)

nj_disability_data <- 
  vroom(file.path("data", "nj-ssi-disability-data.csv")) %>% 
  mutate(county = str_to_upper(county))
nj_unemployment_data <-
  vroom(file.path("data", "nj-unemployment-data.csv")) %>% 
  mutate(Geography = str_to_upper(str_replace(Geography, " County, New Jersey", "")))

race_census_data <- 
  vroom(file.path("data", "census_race_data.csv"))

income_census_data <-
  vroom(file.path("data", "census_income_data.csv"))

age_sex_census_data <-
  vroom(file.path("data", "census_age-sex_data.csv"))

sex_census_data <-
  age_sex_census_data %>% 
  filter(category %in% c("Male", "Female")) %>% 
  pivot_wider(names_from = category,
              values_from = estimate) %>% 
  mutate(male_perc = (Male/(Male+Female)) * 100) %>% 
  select(Geography, year, male_perc)

age_cumulative_census_data <-
  age_sex_census_data %>% 
  filter(!category %in% c("Male", "Female", "Total")) %>% 
  group_by(Geography, year) %>% 
  arrange(category) %>% 
  mutate(estimate = (estimate/sum(estimate)) * 100) %>% 
  ungroup() %>% 
  pivot_wider(names_from = category,
              values_from = estimate) %>% 
  select_if(~sum(is.na(.)) == 0) %>% 
  mutate(up_to_18 = 
           `Under 5 years` + `5 to 9 years` + `10 to 14 years` + `15 to 17 years`,
         from_18_to_34 = 
           `18 and 19 years` + `20 years` + `21 years` + `22 to 24 years` + 
           `25 to 29 years` + `30 to 34 years`, 
         from_35_to_59_years = 
           `35 to 39 years` + `40 to 44 years` + `45 to 49 years` +
           `50 to 54 years` + `55 to 59 years`) %>% 
  select(Geography, year, up_to_18, from_18_to_34, from_35_to_59_years)

hospital_visit_data <-
  vroom("data-raw/opioid-hospital-visits.csv") %>% 
  mutate(county = str_to_upper(county))
  
nj_data_per_county_year <-
  nj_pop_data %>% 
  left_join(nj_disability_data,
             by = c("Geography" = "county", "year")) %>% 
  left_join(nj_prescription_rates, 
             by = c("Geography" = "County", "year")) %>% 
  left_join(nj_unemployment_data) %>% 
  left_join(race_census_data) %>% 
  left_join(sex_census_data) %>% 
  left_join(age_cumulative_census_data) %>% 
  left_join(income_census_data) %>% 
  left_join(nj_data,
            by = c("Geography" = "buyer_county", "year")) %>% 
  left_join(hospital_visit_data,
            by = c("Geography" = "county", "year")) %>% 
  mutate(opioids_supplied_per_capita = dosage_unit/population,
         total_prescriptions = (prescription_rate/100)*population,
         potential_pills_per_prescription = dosage_unit/total_prescriptions) %>% 
  # left_join(arcos_population, by = c("Geography" = "buyer_county", "year")) %>% 
  rename(buyer_county = Geography)

# https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-age-group/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D for justification why you use perc_age_up_to_24. It seems like the biggest jump in deaths happens at this age category in 2012
# perform proportion test for white, black, hispanic on # of opiod deaths and pop distribution in 2012 from acs
# perform proportion test for age groups on # of opiod deaths and age distribution in 2012 from acs

nj_data_per_county_year %>% 
  mutate(buyer_county = str_to_title(buyer_county),
         blind_and_disability_perc = (blind_and_disability/population)*100) %>% 
  select(buyer_county, year, prescription_rate, 
         total_prescriptions, opioids_supplied_per_capita, 
         median_household_income,
         dosage_unit, potential_pills_per_prescription, population, 
         blind_and_disability_perc, unemployment_rate_bls, 
         white_alone:from_35_to_59_years,
         opioid_visit_count) %>% 
  vroom_write(file.path("data", "nj-regression-data.csv"), ",")
