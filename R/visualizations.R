rm(list = ls())

library(ggplot2)
library(vroom)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski)
library(plotly)


source("R/visualization-functions.R")

nj_data <- 
  vroom(file.path("data", "nj-regression-data.csv")) %>% 
  group_by(buyer_county) %>% 
  arrange(year) %>% 
  mutate(lag_opioids = lag(opioids_supplied_per_capita),
         lag_prescriptions = lag(prescription_rate),
         opioid_visit_perc = (opioid_visit_count / population) * 100) %>% 
  ungroup() %>% 
  mutate(opioid_visit_perc_10 = opioid_visit_count/(population/10000))

opioid_binned_unenmp_plot <-
  nj_data %>% 
  group_by(buyer_county) %>% 
  summarise(unemp_mean = mean(unemployment_rate_bls, na.rm = TRUE),
            hosp_mean = mean(opioid_visit_perc_10, na.rm = TRUE),
            lag_opioids_grp = factor(round(mean(lag_opioids, na.rm = TRUE), -1))) %>% 
  ungroup() %>% 
  ggplot(aes(unemp_mean, hosp_mean, color = lag_opioids_grp)) +
  geom_point() +
  labs(x = "Average Unemployment Rate",
       y = "Average # of Prescription Opioid Related Hospitalizations",
       color = str_wrap("Binned Opioid Pills Supplied per capita", 20)) +
  theme_bw() +
  ggtitle(str_wrap("Unemployment Rate - Hospitalization Relationship within Opioid Pills Supplied Groups", 60))

saveRDS(opioid_binned_unenmp_plot, 
        file.path("output", "unemp-hosp-example.rds"))

plot_var_against_hospitalization(nj_data,
                                 lag_prescriptions, 
                                 "Prescription Rate in Previous Year", "Relationship Between Prescription Rate and Prescription Opioid Related Hospitalizations")

plot_var_against_hospitalization(nj_data,
                                 lag_opioids, 
                                 "Opioid Pills Supplied per capita in Previous Year", 
                                 "Relationship Between Opioid Pills Supplied and Prescription Opioid Related Hospitalizations")

plot_var_against_hospitalization(nj_data,
                                 blind_and_disability_perc,
                                 "Percent of Residents Receiving SSI Blind and/or Disability Benefits",
                                 "Relationship Between SSI Benefits and Prescription Opioid Related Hospitalizations")


plot_county_var(nj_data %>% filter(year %in% 2006:2015),
                prescription_rate,
                "Prescription Rate",
                "NJ County Level Changes in Opioid Prescription Rate",
                "https://www.cdc.gov/drugoverdose/maps/rxrate-maps.html")
plot_county_var(nj_data %>% filter(year %in% 2006:2014),
                opioids_supplied_per_capita,
                "Opioid Pills Supplied per capita", 
                "NJ County Level Changes in Opioid Pill Supplied",
                "https://CRAN.R-project.org/package=arcos")
plot_county_var(nj_data %>% 
                  filter(year %in% 2008:2015),
                opioid_visit_perc_10,
                "# of Hospital Visits per 10,000 Residents", 
                "NJ County Level Changes in Opioid Pill Related Hospital Visits",
                "https://www.nj.gov/health/populationhealth/opioid/opioid_hospital.shtml")


