rm(list = ls())

library(vroom)
library(stringr)
library(purrr)
library(dplyr)
library(readxl)
library(tidyr)
library(tidycensus)

# Tidycensus api functions -----
generate_census_vars <- function(census_table, num_vars) {
  paste0(census_table, "_", 
         str_pad(1:num_vars, 3, side = "left", "0"))
}
pull_census_data <-
  function(census_table, num_vars) {
    fname <- file.path("data-raw", 
                       paste0(census_table, "-acs1-", "2006-2015.csv"))
    if (file.exists(fname)) {
      vroom(fname)
    } else {
      census_df <- 
        map_df(2006:2015, function(year) {
          message(year)
          get_acs(geography = "county", 
                  variables = generate_census_vars(census_table, num_vars), 
                  year = year, 
                  survey = "acs1",
                  state = "NJ") %>% 
            mutate(year = year) 
        }) %>% 
        inner_join(load_variables(2015, "acs1"),
                   by = c("variable" = "name")) %>% 
        select(Geography = NAME, year, variable_name = label, variable, estimate, moe)
      vroom_write(census_df, fname, delim = ",")
      census_df
    }
  }


# Missing Cape May in 2011
race_df <-
  pull_census_data("B02001", 10) %>% 
  filter(!(Geography == "Cape May County, New Jersey" & 
             year == 2011)) %>% 
  bind_rows(get_acs(geography = "county", 
                    variables = generate_census_vars("B02001", 10), 
                    year = 2011, 
                    survey = "acs5",
                    county = "Cape May",
                    state = "NJ") %>% 
              mutate(year = 2011) %>% 
              inner_join(load_variables(2015, "acs1"),
                         by = c("variable" = "name")) %>% 
              select(Geography = NAME, year, variable_name = label, variable, estimate, moe)) %>%
  select(Geography, year, category = variable_name, estimate) %>% 
  mutate(category = str_replace(category, "Estimate!!Total!!", ""),
         category = str_replace(category, "Estimate!!", ""))
  

age_sex_df <- 
  pull_census_data("B01001", 29) %>% 
  separate(variable_name, 
           into = c("cat1", "cat2", "cat3", "age_cat"),
           sep = "!!") %>% 
  mutate(age_cat = ifelse(!is.na(age_cat), age_cat,
                          ifelse(!is.na(cat3), cat3, cat2))) %>% 
  select(-cat1, -cat2, -cat3) %>% 
  group_by(Geography, year, age_cat) %>% 
  summarise(estimate = sum(estimate)) %>% 
  ungroup() %>% 
  rename(category = age_cat)

load_cpi_us_rs <- function() {
  fname <-
    file.path("data-raw", "cpi-us-rs.xlsx")
  if (!file.exists(fname)) {
    download.file("https://www.bls.gov/cpi/research-series/allitems.xlsx", 
                  fname)
  }
  read_excel(fname, skip = 5)
}

income_df <-
  pull_census_data("B19013", 1) %>%  
  select(Geography, year, category = variable_name, estimate) %>% 
  mutate(category = "median_household_income") %>% 
  inner_join(load_cpi_us_rs() %>% 
              select(year = YEAR, avg = AVG)) %>% 
  group_by(Geography) %>%
  mutate(avg_2015 = ifelse(year == 2015, avg, NA)) %>% 
  arrange(desc(year)) %>% 
  fill(avg_2015) %>%
  ungroup() %>% 
  mutate(estimate = (avg_2015/avg) * estimate) %>% 
  select(-avg, -avg_2015)

# Total populations
nj_pop_data <- 
  map_df(2005:2016, function(yr) {
    get_acs(geography = "county",
            variables = "B01003_001", 
            year = yr,
            state = "NJ",
            survey = "acs1") %>% 
      mutate(year = yr)
  }) %>% 
  mutate(variable_name = "population") %>% 
  select(Geography = NAME, year, variable_name, estimate)
  
# Write out combined census data ----
clean_county_names <- function(df) {
  df %>% 
    mutate(Geography = 
             str_to_upper(str_replace(Geography, " County, New Jersey", "")))
}

create_category_percentages <- function(df) {
  df %>% 
    mutate(category = 
             str_replace_all(
               str_replace(
                 str_replace(
                   str_replace(
                     str_replace_all(str_to_lower(category), " +", "_"),
                     "'", ""),
                   "\\(", ""),
                 "\\)", ""), 
               "'", "")) %>% 
    pivot_wider(id_cols = c(Geography, year),
                names_from = category,
                values_from = estimate) %>% 
    mutate_at(vars(-Geography, -year, -total),  ~((./total) * 100)) %>% 
    select(-total)
}

nj_pop_data %>% 
  clean_county_names() %>% 
  pivot_wider(names_from = variable_name,
              values_from = estimate) %>% 
  vroom::vroom_write(file.path("data-raw", "nj-pop-data.csv"), 
                     delim = ",")

race_df %>% 
  clean_county_names() %>% 
  create_category_percentages() %>% 
  vroom_write(file.path("data", "census_race_data.csv"), 
              delim = ",")

income_df %>% 
  clean_county_names() %>% 
  mutate(category = str_replace_all(str_to_lower(category), " +", "_")) %>% 
  pivot_wider(names_from = category,
              values_from = estimate) %>% 
  vroom_write(file.path("data", "census_income_data.csv"), 
              delim = ",")

age_sex_df %>% 
  clean_county_names() %>% 
  vroom_write(file.path("data", "census_age-sex_data.csv"),
              delim = ",")
