many_models_demographic_reference_w_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_lm(
            data = .,
            outcome = "scale(value)",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)", 
                "scale(education)",
                "scale(age)",
                "employment_all",
                "race",
                "sex",
                "married",
                "scale(total_pop_county)",
                "scale(median_monthly_housing_cost_county)",
                "scale(land_area_2010)",
                "physicians_scale",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}


many_models_demographic_reference_wo_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_lm(
            data = .,
            outcome = "scale(value)",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}


many_models_demographic_reference_mediators_w_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        PURPOSE_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_lm(
            data = .,
            outcome = "scale(value)",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)", 
                "scale(education)",
                "scale(age)",
                "employment_all",
                "race",
                "sex",
                "married",
                "scale(total_pop_county)",
                "scale(median_monthly_housing_cost_county)",
                "scale(land_area_2010)",
                "physicians_scale",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}


many_models_demographic_reference_mediators_wo_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        PURPOSE_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_lm(
            data = .,
            outcome = "scale(value)",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}


many_models_demographic_reference_glm_w_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_glm(
            data = .,
            outcome = "value",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)", 
                "scale(education)",
                "scale(age)",
                "employment_all",
                "race",
                "sex",
                "married",
                "scale(total_pop_county)",
                "scale(median_monthly_housing_cost_county)",
                "scale(land_area_2010)",
                "physicians_scale",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}


many_models_demographic_reference_glm_wo_covariates <- function(outcomes, df){
  df <-
    df %>%
    filter_at(
      vars(
        ladder_now_scale,
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        education_scale,
        raw_income_scale,
        median_home_value_county_scale,
        median_monthly_housing_cost_county_scale,
        physicians_scale,
        total_pop_county_scale,
        land_area_2010_scale,
        race,
        sex,
        age_scale,
        married,
        employment_all,
        year
      ),
      all_vars(!is.na(.))
    )
  
  df <- 
    df %>%
    sample_n(500000) %>% 
    gather(outcome_key, value, outcomes)
  
  df <-
    df %>% 
    gather(
      mi_var, 
      mi_value, 
      c(
        median_income_demo_sa_scale, 
        median_income_demo_esa_scale, 
        median_income_demo_esar_scale, 
        median_income_demo_sar_scale, 
        median_income_demo_saiq_scale, 
        median_income_demo_sariq_scale, 
        median_income_demo_esaiq_scale,
        median_income_demo_sacr_scale, 
        median_income_demo_sarcr_scale, 
        median_income_demo_esacr_scale,
        # median_income_demo_cr_scale,
        # median_income_demo_st_scale,
        # median_income_demo_iq_scale
      )
    )
  
  input_fun_lm <- function(data, outcome, predictor, covariates){
    lm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data)
  }
  
  input_fun_glm <- function(data, outcome, predictor, covariates){
    glm(as.formula(paste(outcome, "~", predictor, "+", paste(covariates, collapse = "+"))), data = data, family = binomial())
  }
  
  models <- 
    df %>% 
    group_by(outcome_key, mi_var) %>% 
    nest %>%
    mutate(
      model1 = 
        map(
          data, 
          ~input_fun_glm(
            data = .,
            outcome = "value",
            predictor = "scale(mi_value)",
            covariates = 
              c(
                "scale(income)",
                "year"
              )
          ) %>%
            tidy %>%
            dplyr::select(term, estimate) %>%
            spread(term, estimate)
        )
    ) %>%  
    dplyr::select(-data) %>% 
    unnest(cols = c(model1))
  
  rm(df)
  
  return(models)
}

linear_models_outcomes_wc <- many_models_demographic_reference_w_covariates(c("fruits_veggies_scale", "bmi_scale", "ladder_now_scale", "sr_health_scale"), dfg_rs)
write_csv(linear_models_outcomes_wc, "D:/data/gallup/results/many_models_reference_income/linear_models_outcomes_wc.csv")

linear_models_outcomes_woc <- many_models_demographic_reference_w_covariates(c("fruits_veggies_scale", "bmi_scale", "ladder_now_scale", "sr_health_scale"), dfg_rs)
write_csv(linear_models_outcomes_woc, "D:/data/gallup/results/many_models_reference_income/linear_models_outcomes_woc.csv")

linear_models_mediators_wc <- many_models_demographic_reference_mediators_w_covariates(c("PURPOSE_scale", "FINANCIAL_scale", "COMMUNITY_scale", "SOCIAL_scale"), dfg_rs)
write_csv(linear_models_mediators_wc, "D:/data/gallup/results/many_models_reference_income/linear_models_mediators_wc.csv")

linear_models_mediators_woc <- many_models_demographic_reference_mediators_wo_covariates(c("PURPOSE_scale", "FINANCIAL_scale", "COMMUNITY_scale", "SOCIAL_scale"), dfg_rs)
write_csv(linear_models_mediators_woc, "D:/data/gallup/results/many_models_reference_income/linear_models_mediators_woc.csv")

logistic_models_outcomes_wc <- many_models_demographic_reference_glm_w_covariates(c("obese", "depression"), dfg_rs)
write_csv(logistic_models_outcomes_wc, "D:/data/gallup/results/many_models_reference_income/logistic_models_outcomes_wc.csv")

logistic_models_outcomes_woc <- many_models_demographic_reference_glm_wo_covariates(c("obese", "depression"), dfg_rs)
write_csv(logistic_models_outcomes_woc, "D:/data/gallup/results/many_models_reference_income/logistic_models_outcomes_woc.csv")



