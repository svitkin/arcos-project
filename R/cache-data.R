rm(list=ls())

library(arcos)
library(dplyr)
library(tidyr)
library(glue)
library(purrr)
library(vroom)
library(readxl)
library(rvest)
library(stringr)

nj_counties <-
  c("Atlantic", "Bergen", "Burlington", "Camden", "Cape May", "Cumberland", "Essex", "Gloucester", "Hudson", "Hunterdon", "Mercer", "Middlesex", "Monmouth", "Morris", "Ocean", "Passaic", "Salem", "Somerset", "Sussex", "Union", "Warren")
years <- 2006:2016

# ARCOS data -----
if (!file.exists(file.path("data-raw", "nj-data.csv"))) {
  nj_data <- 
    map_df(nj_counties, function(county) {
      arcos::summarized_county_annual(county, "NJ", key = "4KwzCnE")
    })
  
  colnames(nj_data) <- 
    unlist(lapply(colnames(nj_data), function(col) tolower(col)))
  vroom_write(nj_data, file.path("data-raw", "nj-data.csv"), ",")
}

# Prescription rates ----
if (!file.exists(file.path("data-raw", "prescription-rates-nj.csv"))) {
  prescription_rates <-
    map_df(years, function(year) {
      read_html(glue("https://www.cdc.gov/drugoverdose/maps/rxcounty{year}.html")) %>% 
        html_table() %>% 
        `[[`(1)
    })
  
  prescription_rates %>% 
    filter(State == "NJ") %>% 
    select(-`FIPS County Code`) %>% 
    pivot_longer(`2006 Prescribing Rate`:`2015 Prescribing Rate`, 
                 names_to = "year", 
                 values_to = "prescription_rate",
                 values_drop_na = TRUE) %>% 
    mutate(year = as.numeric(str_replace(year, ' Prescribing Rate', '')),
           County = toupper(str_replace(County, ', NJ', ''))) %>% 
    vroom::vroom_write(file.path("data-raw", "prescription-rates-nj.csv"), delim = ",")
}


# Disability data ----
if (!file.exists(file.path("data", "nj-ssi-disability-data.csv"))) {
  ssi_disability_data <- 
    map_df(years, function(year) {
      message(year)
      fname <- glue("nj_{year}_ssi.xlsx")
      download.file(glue("https://www.ssa.gov/policy/docs/statcomps/ssi_sc/{year}/nj.xlsx"),
                    file.path("data-raw", fname))
      
      sheet <- ifelse(year %in% 2012:2016, 1, 3)
      skip <- ifelse(year == 2010, 2, 3)
      ssi_data <-
        read_excel(file.path("data-raw", fname),
                   sheet = sheet,
                   skip = skip) %>% 
        mutate(year = year) %>% 
        rename(county = `...1`)
      if ("Blind and\r\ndisabled" %in% colnames(ssi_data)) {
        ssi_data %>% 
          select(county, year, blind_and_disability = `Blind and\r\ndisabled`)
      } else {
        ssi_data %>% 
          select(county, year, blind_and_disability = `Blind and disabled`)
      }
    }) %>% 
    filter(!is.na(county) & !is.na(blind_and_disability))
  
  vroom_write(ssi_disability_data, 
              file.path("data", "nj-ssi-disability-data.csv"), 
              delim=",")
}

# Unemployment data -----
if (!file.exists(file.path("data", "nj-unemployment-data.csv"))) {
  unemployment_bls_df <- 
    map_df(years, 
           function(year) {
             year_2dig <- str_extract(year, "\\d{2}$")
             fname <- glue("laucnty{year_2dig}.xlsx")
             
             download.file(glue("https://www.bls.gov/lau/laucnty{year_2dig}.xlsx"),
                           file.path("data-raw", fname))
             
             read_xlsx(file.path("data-raw", fname),
                       skip = 4) %>% 
               filter(str_detect(`County Name/State Abbreviation`, ", NJ$"),
                      !is.na(Year)) %>% 
               select(Geography = `County Name/State Abbreviation`,
                      year = Year,
                      unemployment_rate_bls = `(%)`)
           }) %>% 
    mutate(Geography = str_replace(Geography, "NJ", "New Jersey"),
           year = as.numeric(year))
  
  vroom_write(unemployment_bls_df, 
              file.path("data", "nj-unemployment-data.csv"),
              delim = ",")
  
}
