rm(list = ls())

library(ggplot2)
library(trelliscopejs)
library(vroom)
library(dplyr)
library(tidyr)
library(plotly)
library(panelr)

nj_data <- 
  vroom(file.path("data", "nj-regression-data.csv")) %>% 
  mutate(white_perc = (`Estimate; White alone`/estimate_race_total)*100,
         blind_and_disability_perc = (blind_and_disability/population)*100)

# Visualizations ---------
source("R/visualization-functions.R")

white_perc_plots <-
  plot_all_opiod_outputs(nj_data, 
                         white_perc, 
                         "Percentage of White People")

unemployment_plots <- 
  plot_all_opiod_outputs(nj_data, 
                         unemployment_rate_bls, 
                         "Unemployment Rate")

perc_age_24_plots <-
  plot_all_opiod_outputs(nj_data, 
                         perc_age_up_to_24, 
                         "Percentage of People Age 24 or Younger")

blind_disability_plots <-
  plot_all_opiod_outputs(nj_data,
                         blind_and_disability_perc, 
                         "Percentage of People Receiving Blind or Disability SSI Benefits")

plot_county_var(nj_data, 
                white_perc, 
                "Percentage of White People", 
                TRUE)
plot_county_var(nj_data, 
                unemployment_rate_bls, 
                "Unemployment Rate",
                TRUE)
plot_county_var(nj_data, 
                perc_age_up_to_24, 
                "Percentage of People Age 24 or Younger",
                TRUE)
plot_county_var(nj_data,
                blind_and_disability_perc, 
                "Percentage of People Receiving Blind or Disability SSI Benefits",
                TRUE)

plot_county_var(nj_data,
                potential_pills_per_prescription,
                "Potential Pills per Prescription")
plot_county_var(nj_data,
                prescription_rate,
                "Prescription Rate (per 100 people)")
plot_county_var(nj_data,
                dosage_rate_per_100,
                "Dosage Rate (per 100 people)")

# Regressions -------
nj_panel_regression_data <- panel_data(nj_data, id = buyer_county, wave = year)

panel_model_dosage <-
  wbm(dosage_rate_per_100 ~ white_perc + unemployment_rate_bls + perc_age_up_to_24 + blind_and_disability_perc,
      data = nj_panel_regression_data,
      model = "w-b",
      t.df = "Kenward-Roger")

panel_model_prescription <-
  wbm(prescription_rate ~ white_perc + unemployment_rate_bls + perc_age_up_to_24 + blind_and_disability_perc,
      data = nj_panel_regression_data,
      model = "w-b",
      t.df = "Kenward-Roger")

panel_model_pills_per_prescription <-
  wbm(potential_pills_per_prescription ~ white_perc + unemployment_rate_bls + perc_age_up_to_24 + blind_and_disability_perc,
      data = nj_panel_regression_data,
      model = "w-b",
      t.df = "Kenward-Roger")

# Regression Diagnostics -----
level1_residual_analysis <- function(model) {
  #plot(fitted(model), resid(model, type = "pearson"))
}
level2_residual_analysis <- function(model) {
  #plot(fitted(model), resid(model, type = "pearson"))
}
