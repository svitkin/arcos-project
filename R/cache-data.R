rm(list=ls())

library(arcos)
library(dplyr)
library(tidyr)
library(purrr)
library(vroom)
library(readxl)
library(rvest)

nj_counties <-
  c("Atlantic", "Bergen", "Burlington", "Camden", "Cape May", "Cumberland", "Essex", "Gloucester", "Hudson", "Hunterdon", "Mercer", "Middlesex", "Monmouth", "Morris", "Ocean", "Passaic", "Salem", "Somerset", "Sussex", "Union", "Warren")
years <- 2006:2012

if (!file.exists(file.path("data-raw", "nj-data.csv"))) {
  nj_data_list <- list()
  for (county in nj_counties) {
    message(county)
    nj_data_list[[length(nj_data_list) + 1]] <- 
      arcos::county_raw(county, "NJ", key = "4KwzCnE")
  }
  
  save(nj_data_list, file="data-raw/nj-raw-data-list.rda")
  nj_data <- 
    map_df(nj_data_list, function(df) df %>% mutate(dos_str = as.numeric(dos_str)))
  
  colnames(nj_data) <- unlist(lapply(colnames(nj_data), function(col) tolower(col)))
  nj_data <- as.data.table(nj_data)
  nj_data[, transaction_date := as_date(paste0(str_extract(transaction_date, "\\d{4}$"), "-",
                                               str_extract(transaction_date, "^\\d{2}"), "-",
                                               str_replace(str_extract(transaction_date, "^\\d{4}"),
                                                           "^\\d{2}", "")))]
  vroom_write(nj_data, file.path("data-raw", "nj-data.csv"), ",")
}



# Prescription rates ----
base_url <- "https://www.cdc.gov/drugoverdose/maps/rxcounty"
prescription_rates <-
  map_df(years, function(year) {
  read_html(paste0(base_url, year, ".html")) %>% 
    html_table() %>% 
    `[[`(1)
})

prescription_rates %>% 
  filter(State == "NJ") %>% 
  select(-`FIPS County Code`) %>% 
  pivot_longer(`2006 Prescribing Rate`:`2012 Prescribing Rate`, 
               names_to = "year", values_to = "prescription_rate",
               values_drop_na = TRUE) %>% 
  mutate(year = as.numeric(str_replace(year, ' Prescribing Rate', '')),
         County = toupper(str_replace(County, ', NJ', ''))) %>% 
  vroom::vroom_write(file.path("data-raw", "prescription-rates-nj.csv"), delim = ",")

map_df(nj_counties, 
       function(county) arcos::county_population(county, "NJ", key = "4KwzCnE")) %>% 
  vroom::vroom_write(file.path("data-raw", "nj-pop-data.csv"), delim = ",")

# Disability data ----
ssi_disability_data <- 
  map_df(years, function(year) {
    sheet <- ifelse(year == 2012, 1, 3)
    skip <- ifelse(year == 2010, 2, 3)
    ssi_data <-
      read_excel(file.path("data-raw", 
                           paste0("nj_", year, "_ssi.xlsx")),
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
