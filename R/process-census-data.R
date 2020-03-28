rm(list = ls())

library(vroom)
library(stringr)
library(purrr)
library(dplyr)
library(readxl)

census_variables <-
  c("B02001", "S0101", "S2301")
years <- c("06", "07", "08", "09", "10", "11", "12")

import_census_df <- function(census_var) {
  map_df(years,
         function(year) {
           mid_file_part <- "1YR"
           if (year == "06") 
             mid_file_part <- "EST"
           main_directory <-
             paste("ACS", year, mid_file_part, census_var, sep = "_")
           file_name <- paste0(main_directory, "_with_ann.csv")
           
           message(file_name)
           vroom(file.path("data-raw", main_directory, file_name), 
                 skip = 1) %>% 
             mutate(year = as.numeric(paste0("20", year))) %>% 
             mutate_at(vars(-Geography), as.numeric)
         })
}

race_df_bad_names <- 
  import_census_df("B02001")

# Cape may county data is missing for 1 year acs estimates in 2011 so using 3 year
race_df_capemay3year <-
  vroom("data-raw/ACS_11_3YR_B02001/ACS_11_3YR_B02001_with_ann.csv", skip = 1) %>% 
  mutate(year = 2011) %>% 
  mutate_at(vars(-Geography), as.numeric)

race_df_gte2010 <-
  race_df_bad_names %>% 
  bind_rows(race_df_capemay3year) %>% 
  filter(year >= 2010) %>% 
  select(Geography, year, contains("Total: - "), `Estimate; Total:`, `Margin of Error; Total:`)
colnames(race_df_gte2010) <- str_replace(colnames(race_df_gte2010), "Total: - ", "")

race_df <-
  race_df_gte2010 %>% 
  bind_rows(
    race_df_bad_names %>% 
      filter(year < 2010) %>% 
      `[`(,
          (!str_detect(colnames(.), "Total: - ") & !str_detect(colnames(.), "Id"))| colnames(.) == "year" | 
            colnames(.) == "Geography" | colnames(.) == "Estimate; Total:" | 
      colnames(.) == "Margin of Error; Total:")
  )



age_sex_df_bad_names <- 
  import_census_df("S0101")
age_sex_df_lt2010 <-
  age_sex_df_bad_names %>% 
  filter(year < 2010) %>% 
  select(Geography, year, 
         contains("Total; Estimate; Total population - AGE"),
         contains("Total; Margin of Error; Total population - AGE"),
         `Male; Estimate; Total population`, 
         `Male; Margin of Error; Total population`,
         `Female; Estimate; Total population`, 
         `Female; Margin of Error; Total population`)
colnames(age_sex_df_lt2010) <- 
  str_replace(colnames(age_sex_df_lt2010), "Total population - ", "")

age_sex_df <-
  age_sex_df_lt2010 %>% 
  bind_rows(
    age_sex_df_bad_names %>% 
      filter(year >= 2010) %>% 
      `[`(, 
          (!str_detect(colnames(.), "Total population - ") & 
             str_detect(colnames(.), "^Total.*; AGE")) |
            colnames(.) == "year" | colnames(.) == "Geography" | 
            (str_detect(colnames(.), "; Total population$") & 
               !str_detect(colnames(.), "^Total")))
  )

unemployment_acs_df <- 
  import_census_df("S2301") %>% 
  select(Geography, 
         year, 
         `Unemployment rate; Estimate; Population 16 years and over`, 
         `Unemployment rate; Margin of Error; Population 16 years and over`)

unemployment_bls_df <- 
  map_df(years, 
         function(year) {
           read_xlsx(file.path("data-raw", paste0("laucnty", year, ".xlsx")),
                     skip = 4) %>% 
             filter(str_detect(`County Name/State Abbreviation`, ", NJ$"),
                    !is.na(Year)) %>% 
             select(Geography = `County Name/State Abbreviation`,
                    year = Year,
                    unemployment_rate_bls = `(%)`)
         }) %>% 
  mutate(Geography = str_replace(Geography, "NJ", "New Jersey"),
         year = as.numeric(year))

race_df %>% 
  inner_join(age_sex_df, by = c("Geography", "year")) %>% 
  inner_join(unemployment_acs_df, by = c("Geography", "year")) %>% 
  inner_join(unemployment_bls_df, by = c("Geography", "year")) %>% 
  vroom_write(file.path("data", "census-data.csv"), delim = ",")
