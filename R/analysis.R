rm(list = ls())

library(ggplot2)
library(vroom)
library(dplyr)
library(tidyr)
library(panelr)
library(cowplot)
library(car)
library(GLMMadaptive)
library(glmmTMB)
library(performance)
library(DHARMa)
library(broom)
library(stringr)
library(gt)
library(geepack)

# Prepare data for modelling -------
nj_data <- 
  vroom(file.path("data", "nj-regression-data.csv")) %>% 
  group_by(buyer_county) %>% 
  arrange(year) %>% 
  mutate(lag_opioids = lag(opioids_supplied_per_capita),
         lag_prescriptions = lag(prescription_rate)) %>% 
  ungroup() %>% 
  mutate(year_07 = year == 2007,
         year_08 = year == 2008,
         year_09 = year == 2009,
         year_10 = year == 2010,
         year_11 = year == 2011,
         year_12 = year == 2012,
         year_13 = year == 2013,
         year_14 = year == 2014,
         year_15 = year == 2015) %>%
  filter(!is.na(opioid_visit_count))

create_mean_and_demean <- function(df, var) {
  mean_name <- paste0("mean_", quo_name(enquo(var)))
  df %>% 
    mutate({{ var }} := {{ var }} / 100,
           !!mean_name := mean({{var}}),
           {{ var }} := {{ var }} - mean({{ var }}))
}

nj_data_demeaned <-
  nj_data %>% 
  group_by(buyer_county) %>% 
  create_mean_and_demean(lag_opioids) %>% 
  create_mean_and_demean(lag_prescriptions) %>% 
  create_mean_and_demean(unemployment_rate_bls) %>% 
  create_mean_and_demean(white_alone) %>% 
  create_mean_and_demean(black_or_african_american_alone) %>% 
  create_mean_and_demean(up_to_18) %>% 
  create_mean_and_demean(from_18_to_34) %>% 
  create_mean_and_demean(from_35_to_59_years) %>% 
  create_mean_and_demean(blind_and_disability_perc) %>% 
  mutate(med_income_ten = median_household_income/10000) %>% 
  create_mean_and_demean(med_income_ten) %>%
  ungroup() %>% 
  mutate(pop10 = population/10000)

# Regressions  ---------------------------------------------------------
poisson_model <- 
  glmer(opioid_visit_count ~ 
          mean_lag_opioids +
          mean_lag_prescriptions +
          mean_unemployment_rate_bls +
          mean_med_income_ten +
          # mean_white_alone +
          # mean_black_or_african_american_alone +
          mean_blind_and_disability_perc +
          # mean_up_to_18 +
          # mean_from_18_to_34 +
          # mean_from_35_to_59_years +
          lag_opioids +
          lag_prescriptions +
          unemployment_rate_bls +
          med_income_ten +
          white_alone +
          black_or_african_american_alone +
          blind_and_disability_perc +
          up_to_18 +
          from_18_to_34 +
          from_35_to_59_years +
          year_09 + year_10 + year_11 +
          year_12 + year_13 + year_14 +
          year_15 +
          (1 | buyer_county),
        data = nj_data_demeaned,
        family = poisson,
        offset = log(pop10),
        # These parameters had best optimization results
        nAGQ = 7,
        control=glmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e5)))

model_gee <-
  geeglm(opioid_visit_count ~ 
           mean_lag_opioids +
           mean_lag_prescriptions +
           mean_unemployment_rate_bls +
           mean_med_income_ten +
           # mean_white_alone +
           # mean_black_or_african_american_alone +
           mean_blind_and_disability_perc +
           # mean_up_to_18 +
           # mean_from_18_to_34 +
           # mean_from_35_to_59_years +
           lag_opioids +
           lag_prescriptions +
           unemployment_rate_bls +
           med_income_ten +
           white_alone +
           black_or_african_american_alone +
           blind_and_disability_perc +
           up_to_18 +
           from_18_to_34 +
           from_35_to_59_years +
           year_09 + year_10 + year_11 +
           year_12 + year_13 + year_14 +
           year_15,
         data = nj_data_demeaned %>% 
           mutate(county_id = factor(buyer_county)) %>% 
           arrange(county_id, year),
         family = poisson,
         offset = log(pop10),
         id = county_id,
         corstr = "exchangeable",
         waves = year)

summary(model_gee)
summary(poisson_model)
saveRDS(poisson_model, file.path("output", "poisson-model-object.rds"))

# Create presentation ready output -------------------
formatted_names_df <-
  tribble(
    ~term, ~nice_name,
    # -------| ----------|
    "(Intercept)", "Intercept",
    "lag_opioids", "Opioids per Capita in Previous Year (in 5 pill increments)",
    "lag_prescriptions", "Prescription Rate in Previous Year",
    "med_income_ten", "Median Household Income (In 2015 USD $10,000s)",
    "unemployment_rate_bls", "Unemployment Rate",
    "blind_and_disability_perc", "Percent Receiving SSI Blind & Disability Benefits (in tenths of a percentage point)",
    "white_alone", "Percent of White Alone",
    "black_or_african_american_alone", "Percent of Black or African-American Alone",
    "up_to_18", "Percent of 18 or younger",
    "from_18_to_34", "Percent between 18 and 34 years old",
    "from_35_to_59_years", "Percent between 35 to 59 years old",
    "year_09TRUE", "Year 2009",
    "year_10TRUE", "Year 2010",
    "year_11TRUE", "Year 2011",
    "year_12TRUE", "Year 2012",
    "year_13TRUE", "Year 2013",
    "year_14TRUE", "Year 2014",
    "year_15TRUE", "Year 2015",
    
    "mean_lag_opioids", "Average Opioids per Capita from 2007-2014 (in 5 pill increments)",
    "mean_lag_prescriptions", "Average Prescription Rate from 2007-2014",
    "mean_med_income_ten", "Average Median Household Income (In 2015 USD $10,000s)",
    "mean_unemployment_rate_bls", "Average Unemployment Rate",
    "mean_blind_and_disability_perc", "Average Percent Receiving SSI Blind & Disability Benefits (in tenths of a percentage point)",
    "mean_up_to_18", "Average Percent of 18 or younger",
    "mean_from_18_to_34", "Average Percent between 18 and 34 years old",
    "mean_from_35_to_59_years", "Average Percent between 35 to 59 years old",
    "mean_white_alone", "Average Percent of White Alone",
    "mean_black_or_african_american_alone", "Average Percent of Black or African-American Alone",
    "sd_(Intercept).buyer_county", "Std. Dev. of Intercept"
  )


model_df <-
  tidy(poisson_model) %>% 
  mutate(divisor = 
           case_when(
             str_detect(term, "year_\\d{2}") ~ 1,
             str_detect(term, "blind_and_disability_perc") ~ 1000,
             str_detect(term, "lag_opioids") ~ 20,
             TRUE ~ 100),
         pctg = (1 - exp(estimate/divisor)) * 100 * -1,
         pctg_low = (1 - exp((estimate - 2*std.error)/divisor)) * 100 * -1,
         pctg_high = (1 - exp((estimate + 2*std.error)/divisor)) * 100 * -1) %>% 
  left_join(formatted_names_df) 

saveRDS(model_df, file.path("output", "poisson-model-data.rds"))

model_df %>% 
  mutate(`Variable Type` = 
           ifelse(str_detect(term, "mean") | str_detect(term, "year_\\d{2}"), 
                  "Between", "Within")) %>% 
  filter(group == "fixed", nice_name != "Intercept") %>%
  mutate_if(is.numeric, function(col) round(col, 2)) %>% 
  mutate(nice_name = 
           ifelse(p.value < 0.05, 
                  paste0("<strong>", nice_name, "</strong>"), 
                  nice_name),
         pctg = paste0(pctg, " (", pctg_low, ", ", pctg_high, ")")) %>% 
  group_by(`Variable Type`) %>% 
  select(`Variable Type`, 
         `Independent Variable` = nice_name, 
         `Percent Change in Prescription Opioid Related Hopsitalizations (per 10,000)` = pctg, 
         `p-value` = p.value) %>% 
  gt() %>% 
  tab_header(
    title = "Poisson Random Intercept Model Results"
  ) %>% 
  fmt_markdown(columns = vars(`Independent Variable`)) %>% 
  tab_footnote(
    footnote = paste0("The random intercept has raw estimate of ",
                      model_df %>% filter(term == "(Intercept)") %>% pull(estimate) %>% round(2),
                      " and a standard deviation of ",
                      model_df %>% filter(term == "sd_(Intercept).buyer_county") %>% pull(estimate) %>% round(2), 
                      ", with a p-value of ",
                      model_df %>% filter(term == "(Intercept)") %>% pull(p.value) %>% round(2)),
    locations = cells_column_labels(
      columns = vars(`Independent Variable`))
  ) %>% 
  saveRDS(file.path("output", "poisson-pctchange-table.rds"))

# Diagnostics of poisson model ------------------
check_overdispersion(poisson_model)
resids <- simulateResiduals(poisson_model, n = 10000)
saveRDS(resids, file.path("output", "simulated-residuals.rds"))
plot(resids)
hist(resids)
plot(density(resids$scaledResiduals))
residuals(poisson_model) %>% density() %>% plot()
sqrt(mean((poisson_model@frame$opioid_visit_count - 
             exp(predict(poisson_model)))^2))

get_nice_name <- function(variable, prepend = "") {
  formatted_names_df %>% 
    filter(term == variable) %>% 
    pull(nice_name) %>% 
    paste0(prepend, .)
}
plot_residuals <- function(simulated_residuals, model, variable) {
  prepend <-
    ifelse(!str_detect(variable, "med_income_10"), "Scaled by 100 & County-Mean Centered: ", "Mean Centered: ")
  plotResiduals(simulated_residuals, 
                form = model@frame[[variable]], 
                xlab = str_replace(get_nice_name(variable, prepend), 
                                   " \\(in 5 pill increments\\)", ""))
}
plot_residuals(resids, poisson_model, "lag_opioids")
plot_residuals(resids, poisson_model, "lag_prescriptions")
plot_residuals(resids, poisson_model, "unemployment_rate_bls")
plot_residuals(resids, poisson_model, "med_income_ten")
plot_residuals(resids, poisson_model, "black_or_african_american_alone")
plot_residuals(resids, poisson_model, "blind_and_disability_perc")
plot_residuals(resids, poisson_model, "up_to_18")
plot_residuals(resids, poisson_model, "from_18_to_34")
plot_residuals(resids, poisson_model, "from_35_to_59_years")

# Output effects plot --------
plot_names_df <-
  tribble(
    ~term, ~plot_name,
    # -------| ----------|
    "mean_unemployment_rate_bls", "1% increase in Average Unemployment Rate",
    "mean_med_income_ten", "$10,000 2015 USD increase in the Average Median Household Income",
    "blind_and_disability_perc", ".1% increase in the % of Residents Receiving Blind and/or Disability SSI Benefits",
    "mean_lag_opioids", "Increase in Average Opioid Pill Supply by 5 Pills per capita",
    "year_15TRUE", "Year 2015"
  ) %>% 
  mutate(plot_name = str_wrap(plot_name, 20))

model_plot <-
  model_df %>% 
  filter(p.value < 0.05,
         nice_name != "Intercept") %>%
  mutate(neg = pctg < 0) %>% 
  left_join(plot_names_df) %>% 
  ggplot(aes(pctg, reorder(str_wrap(plot_name, 40), -pctg),
             text = paste0(round(pctg, 2), "%"),
             color = neg)) +
  geom_point() +
  geom_errorbarh(aes(xmin = pctg_low, 
                     xmax = pctg_high,
                     height = 0.2)) +
  labs(x = "Percent Change in Hospitalizations per 10,000",
       y = "Independent Variable") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle(str_wrap("Expected Change in Prescription Opioid Related Hospitalizations in New Jersey Counties", 60))

saveRDS(model_plot, file.path("output", "model-plot.rds"))