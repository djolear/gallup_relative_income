coef_plot <- function(outcome, data){
  plot <-
    data %>% 
    dplyr::select(
      mi_var, 
      outcome = outcome, 
      outcome_key
    ) %>% 
    mutate(
      #outcome = abs(outcome),
      mi_var =
        case_when(
          mi_var == "median_income_demo_esa_scale" ~ "age, sex, educ",
          mi_var == "median_income_demo_sar_scale" ~ "age, sex, race",
          mi_var == "median_income_demo_esar_scale" ~ "age, sex, educ, race",
          mi_var == "median_income_demo_sarcr_scale" ~ "age, sex, race, region",
          mi_var == "median_income_demo_esacr_scale" ~ "age, sex, educ, race, region",
          mi_var == "median_income_demo_sa_scale" ~ "age, sex",
          mi_var == "median_income_demo_sacr_scale" ~ "age, sex, region",
          mi_var == "median_income_demo_esaiq_scale" ~ "age, sex, educ, iq",
          mi_var == "median_income_demo_sariq_scale" ~ "age, sex, race, iq",
          mi_var == "median_income_demo_saiq_scale" ~ "age, sex, iq"
        ),
      outcome_key =
        case_when(
          outcome_key == "bmi_scale" ~ "BMI",
          outcome_key == "fruits_veggies_scale" ~ "fruits/veggies",
          outcome_key == "ladder_now_scale" ~ "Cantril ladder",
          outcome_key == "sr_health_scale" ~ "sr health",
          outcome_key == "PURPOSE_scale" ~ "purpose",
          outcome_key == "FINANCIAL_scale" ~ "financial",
          outcome_key == "SOCIAL_scale" ~ "social",
          outcome_key == "COMMUNITY_scale" ~ "community",
          outcome_key == "eat_healthy" ~ "healthy eating",
          outcome_key == "smoke" ~ "smoking",
          outcome_key == "diabetes" ~ "diabetes",
          outcome_key == "hbp" ~ "hbp",
        )
    ) %>% 
    ggplot(aes(fct_reorder(mi_var, outcome), outcome)) + 
    geom_col() + 
    labs(
      x = "median income reference",
      y = "absolute value of coef."
    ) +
    coord_flip() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    facet_grid(. ~ outcome_key, scales = "free")
  
  return(plot)
}


coef_diff_plot <- function(outcome, data){
  plot <-
    data %>% 
    dplyr::select(
      mi_var, 
      outcome = outcome, 
      outcome_key
    ) %>% 
    mutate(
      #outcome = abs(outcome),
      mi_var =
        case_when(
          mi_var == "median_income_demo_esa_scale" ~ "age, sex, educ",
          mi_var == "median_income_demo_sar_scale" ~ "age, sex, race",
          mi_var == "median_income_demo_esar_scale" ~ "age, sex, educ, race",
          mi_var == "median_income_demo_sarcr_scale" ~ "age, sex, race, region",
          mi_var == "median_income_demo_esacr_scale" ~ "age, sex, educ, race, region",
          mi_var == "median_income_demo_sa_scale" ~ "age, sex",
          mi_var == "median_income_demo_sacr_scale" ~ "age, sex, region",
          mi_var == "median_income_demo_esaiq_scale" ~ "age, sex, educ, iq",
          mi_var == "median_income_demo_sariq_scale" ~ "age, sex, race, iq",
          mi_var == "median_income_demo_saiq_scale" ~ "age, sex, iq"
        ),
      outcome_key =
        case_when(
          outcome_key == "bmi_scale" ~ "BMI",
          outcome_key == "fruits_veggies_scale" ~ "fruits/veggies",
          outcome_key == "ladder_now_scale" ~ "Cantril ladder",
          outcome_key == "sr_health_scale" ~ "sr health",
          outcome_key == "PURPOSE_scale" ~ "purpose",
          outcome_key == "FINANCIAL_scale" ~ "financial",
          outcome_key == "SOCIAL_scale" ~ "social",
          outcome_key == "COMMUNITY_scale" ~ "community",
          outcome_key == "eat_healthy" ~ "healthy eating",
          outcome_key == "smoke" ~ "smoking",
          outcome_key == "diabetes" ~ "diabetes",
          outcome_key == "hbp" ~ "hbp",
        )
    ) %>% 
    ggplot(aes(fct_reorder(mi_var, outcome), outcome)) + 
    geom_col() + 
    labs(
      x = "median income reference",
      y = "difference in value of coef."
    ) +
    coord_flip() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    facet_grid(. ~ outcome_key, scales = "free")
  
  return(plot)
}
