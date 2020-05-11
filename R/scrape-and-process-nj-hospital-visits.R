rm(list = ls())
library(jsonlite)
library(vroom)
library(stringr)
library(dplyr)
library(purrr)

run_shell_script <- function(year) {
  # Create script specific to the year
  fname <-
    paste0("shell/scrape-hospital-visits-", year, ".sh")
  readLines("shell/scrape-hospital-visits-basis.sh") %>% 
    str_replace("REPLACE_YEAR", as.character(year)) %>% 
    writeLines(con = fname)
  # Run year-specific shell script and then delete it
  system2("chmod",  
          args = c("u+x", fname))
  system2(paste0("./", fname),
          stdout = paste0("shell/json/hospital-visits-", year, ".json"))
  system2("rm",
          args = fname)
}

read_hospital_json <- function(year) {
  message(year)
  fname <- 
    paste0("shell/json/hospital-visits-", year, ".json")
  fromJSON(fname)$result$result$data$dsr$DS[[1]]$PH[[1]]$DM1[[2]] %>% 
    mutate(opioid_visit_count = 
             map_dbl(X, function(val) {
               tryCatch({
                 na.omit(ifelse(class(val) == "list",
                                val[[1]]$M0,
                                val$M0))},
                 error = function(c) {
                   NA
                 })
               
             }))   %>% 
    select(county = G0, opioid_visit_count) %>% 
    mutate(year = year) %>% 
    filter(county != "-Out of State-")
}

create_hospital_visit_df <- function() {
  map_df(2008:2015, function(year) {
    run_shell_script(year)
    read_hospital_json(year)
  }) %>% 
    # manual fix for Sussex in 2015; for some reason data not coming through with usual network query
    mutate(opioid_visit_count = ifelse(county == "Sussex" & year == 2015,
                                       43,
                                       opioid_visit_count))
}

create_hospital_visit_df() %>% 
  vroom_write(file.path("data-raw", "opioid-hospital-visits.csv"),
              delim = ",")
