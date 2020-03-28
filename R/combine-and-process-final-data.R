rm(list = ls())

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(data.table)
library(vroom)


nj_data <- fread(file.path("data-raw", "nj-data.csv"))
nj_data[, year := as.numeric(str_extract(transaction_date, "^\\d{4}"))]

nj_pop_data <- fread(file.path("data-raw", "nj-pop-data.csv"))
nj_prescription_rates <-
  fread(file.path("data-raw", "prescription-rates-nj.csv")) %>% 
  left_join(nj_pop_data, by = c("year", "County" = "BUYER_COUNTY")) %>% 
  mutate(prescription_rate_per_capita = prescription_rate/100)
nj_disability_data <- 
  fread(file.path("data", "nj-ssi-disability-data.csv")) %>% 
  mutate(county = str_to_upper(county))

nj_num_pharmacies_per_county <-
  nj_data[, .(num_pharmacies = uniqueN(buyer_dea_no)), by = .(year, buyer_county)]

census_data <- 
  vroom("data/census-data.csv") %>% 
  mutate(Geography = str_to_upper(str_replace(Geography, " County, New Jersey", "")))

nj_data_per_county_year <-
  nj_data[, .(sum_dosage = sum(dosage_unit)), by = .(year, buyer_county)] %>% 
  merge(nj_prescription_rates,
        by.x = c("buyer_county", "year"),
        by.y = c("County", "year")) %>% 
  .[, dosage_per_capita := sum_dosage/population] %>% 
  .[, sum_prescriptions := (prescription_rate/100) * population] %>% 
  .[, dosage_rate_per_100 := dosage_per_capita * 100] %>% 
  .[, potential_pills_per_prescription := sum_dosage/sum_prescriptions] %>% 
  merge(census_data,
        by.x = c("buyer_county", "year"),
        by.y = c("Geography", "year")) %>% 
  merge(nj_disability_data,
        by.x = c("buyer_county", "year"),
        by.y = c("county", "year"))

nj_data_per_county_year_age_clean <-
  nj_data_per_county_year %>% 
  pivot_longer(matches("Total; Estimate; AGE"), names_to = "age_category", values_to = "age_count") %>% 
  mutate(age_category = as.numeric(str_replace_all(str_extract(age_category, "\\d+ years"), " years", ""))) %>% 
  group_by(buyer_county, year) %>% 
  arrange(age_category) %>% 
  mutate(cumsum_age = cumsum(age_count)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(buyer_county, year),
              names_from = age_category, 
              values_from = cumsum_age, 
              names_prefix = "perc_age_up_to_") %>% 
  inner_join(nj_data_per_county_year) %>% 
  select(-matches("AGE - "))

# https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-age-group/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D for justification why you use perc_age_up_to_24. It seems like the biggest jump in deaths happens at this age category in 2012
# perform proportion test for white, black, hispanic on # of opiod deaths and pop distribution in 2012 from acs
# perform proportion test for age groups on # of opiod deaths and age distribution in 2012 from acs

nj_data_per_county_year_age_clean %>% 
  mutate(buyer_county = str_to_title(buyer_county)) %>% 
  select(buyer_county, year, population,
         perc_age_up_to_24, 
         blind_and_disability, 
         `Estimate; White alone`, 
         estimate_race_total = `Estimate; Total:`,
         unemployment_rate_bls, 
         prescription_rate, 
         dosage_rate_per_100, 
         potential_pills_per_prescription) %>% 
  vroom_write(file.path("data", "nj-regression-data.csv"), ",")
