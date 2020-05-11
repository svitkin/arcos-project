library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(knitr)

make_filename_appropriate <- function(var_name) {
  var_name %>% 
    make.names() %>% 
    str_replace_all("\\.+", "_") %>% 
    str_replace("_+$", "") %>% 
    str_to_lower()
}
wrap_title <- function(title_name) {
  str_wrap(title_name, 55)
}
calculate_title_margin <- function(title_name) {
  ifelse(length(str_match_all(wrap_title(title_name), "\n")[[1]]) >= 2,
         125,
         75)
}

plot_var_against_hospitalization <- function(nj_df, indep_var, indep_nice_name, title_name) {
  plt <-
    ggplot(nj_df,
         aes({{ indep_var }}, opioid_visit_perc_10, 
             color = buyer_county,
             text = paste0("County: ", buyer_county, "<br>",
                           "Year: ", year))) +
    geom_point() +
    labs(x = indep_nice_name, 
         y = "# of Hospital Visits per 10,000 Residents",
         color = "County") +
    ggtitle(wrap_title(title_name)) +
    theme_bw()
  pltly <-
    ggplotly(plt, tooltip = c("text")) %>%
    layout(margin=list(t = 85, b = 100))
  
  saveRDS(pltly, file = file.path("output", paste0(make_filename_appropriate(indep_nice_name), "-w-hosp_visits.rds")))
  
  pltly
}

plot_county_var <- function(nj_df, indep_var, indep_nice_name, title_name, data_source, add_perc = FALSE) {
  perc_text <- ifelse(add_perc, "%", "")
  plt <-
    ggplot(nj_df, 
           aes(factor(year), {{ indep_var }},
               color = buyer_county,
               group = buyer_county,
               text = paste0("County: ", buyer_county, "<br>",
                             indep_nice_name, ": ", round({{ indep_var }}, 2),
                             perc_text))) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x = "Year", y = indep_nice_name, color = "County", 
         caption = paste0("Data Source: ", data_source)) + 
    ggtitle(wrap_title(title_name)) +
    scale_y_continuous(labels = 
                         ifelse(add_perc, 
                                scales::percent_format(scale = 1),
                                function(x) x))
  
  pltly <-
    ggplotly(plt, tooltip = c("text")) %>%
    layout(margin=list(t = 85, b = 100),
           title = list(text = paste0(wrap_title(title_name),
                                      '<br>',
                                      '<sup>',
                                      "Data Source: ",
                                      data_source,
                                      '</sup>')))

  saveRDS(pltly,
          file = file.path("output",
                           paste0(make_filename_appropriate(indep_nice_name),
                                  "-", "county", ".rds")))
  pltly
}
