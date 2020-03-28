plot_opioid_scatter <- 
  function(nj_df, indep_var, dep_var, indep_nice_name, dep_nice_name) {
    plt <-
      ggplot(nj_df, aes({{ indep_var }}, {{ dep_var }}, 
                        color = buyer_county,
                        text = paste0("County: ", buyer_county, "<br>",
                                      "Year: ", year))) +
      geom_point() +
      theme_bw() +
      labs(x = indep_nice_name, y = dep_nice_name, color = "County") 
    ggplotly(plt, tooltip = c("text"))
  }

plot_all_opiod_outputs <- function(nj_df, indep_var, indep_nice_name) {
  list(
    "dosage_rate" = 
      plot_opioid_scatter(nj_df, 
                          {{ indep_var }}, 
                          dosage_rate_per_100,
                          indep_nice_name,
                          "Dosage Rate (per 100 people)"),
    "prescription_rate" = 
      plot_opioid_scatter(nj_df, 
                          {{ indep_var }}, 
                          prescription_rate, 
                          indep_nice_name,
                          "Prescription Rate (per 100 people)"),
    "potential_pills_per_prescription" = 
      plot_opioid_scatter(nj_df, 
                          {{ indep_var }},
                          potential_pills_per_prescription,
                          indep_nice_name,
                          "Potential Pills per Prescription")
  )
}

plot_county_var <- function(nj_df, indep_var, indep_nice_name, add_perc = FALSE) {
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
    labs(x = "Year", y = indep_nice_name, color = "County")
  ggplotly(plt, tooltip = c("text"))
}