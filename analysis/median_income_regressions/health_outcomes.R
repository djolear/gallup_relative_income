###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("broom.mixed", lib.loc = "/home/djolear/R")
library("lme4", lib.loc = "/home/djolear/R")


####################
## Setup Parallel ##
####################

plan(multicore, workers = 2)

###############
## Functions ##
###############

health_regression_function <- function(median_income_var_name, dfg) {
  
  # select data and set median income variable
 
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      diabetes,
      raw_income_scale,
      education_scale,
      total_pop_county_scale,
      median_monthly_housing_cost_county_scale,
      land_area_2010_scale,
      physicians_scale,
      employment_all,
      sex,
      age_scale,
      race,
      married, 
      year,
      fips_code
    ) %>% 
    filter_at(
      vars(
        median_income_var_scale,
        diabetes,
        raw_income_scale,
        education_scale,
        total_pop_county_scale,
        median_monthly_housing_cost_county_scale,
        land_area_2010_scale,
        physicians_scale,
        employment_all,
        sex,
        age_scale,
        race,
        married, 
        year,
        fips_code
      ),
      all_vars(!is.na(.))
    ) %>% 
    mutate_at(
      vars(
        employment_all,
        sex,
        race,
        married,
        year,
        fips_code
      ),
      as.factor
    )
  
  lm1 <-
    glm(
      diabetes ~
        raw_income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current %>% mutate(diabetes = ifelse(diabetes == 1, 1, ifelse(diabetes == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "diabetes",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      hbp,
      raw_income_scale,
      education_scale,
      total_pop_county_scale,
      median_monthly_housing_cost_county_scale,
      land_area_2010_scale,
      physicians_scale,
      employment_all,
      sex,
      age_scale,
      race,
      married, 
      year,
      fips_code
    ) %>% 
    filter_at(
      vars(
        median_income_var_scale,
        hbp,
        raw_income_scale,
        education_scale,
        total_pop_county_scale,
        median_monthly_housing_cost_county_scale,
        land_area_2010_scale,
        physicians_scale,
        employment_all,
        sex,
        age_scale,
        race,
        married, 
        year,
        fips_code
      ),
      all_vars(!is.na(.))
    ) %>% 
    mutate_at(
      vars(
        employment_all,
        sex,
        race,
        married,
        year,
        fips_code
      ),
      as.factor
    )
  
  
  lm1 <-
    glm(
      hbp ~
        raw_income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current %>% mutate(hbp = ifelse(hbp == 1, 1, ifelse(hbp == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "hbp",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      obese,
      raw_income_scale,
      education_scale,
      total_pop_county_scale,
      median_monthly_housing_cost_county_scale,
      land_area_2010_scale,
      physicians_scale,
      employment_all,
      sex,
      age_scale,
      race,
      married, 
      year,
      fips_code
    ) %>% 
    filter_at(
      vars(
        median_income_var_scale,
        obese,
        raw_income_scale,
        education_scale,
        total_pop_county_scale,
        median_monthly_housing_cost_county_scale,
        land_area_2010_scale,
        physicians_scale,
        employment_all,
        sex,
        age_scale,
        race,
        married, 
        year,
        fips_code
      ),
      all_vars(!is.na(.))
    ) %>% 
    mutate_at(
      vars(
        employment_all,
        sex,
        race,
        married,
        year,
        fips_code
      ),
      as.factor
    )
  
  lm1 <-
    glm(
      obese ~
        raw_income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current %>% mutate(obese = ifelse(obese == 1, 1, ifelse(obese == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "obesity",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      depression,
      raw_income_scale,
      education_scale,
      total_pop_county_scale,
      median_monthly_housing_cost_county_scale,
      land_area_2010_scale,
      physicians_scale,
      employment_all,
      sex,
      age_scale,
      race,
      married, 
      year,
      fips_code
    ) %>% 
    filter_at(
      vars(
        median_income_var_scale,
        depression,
        raw_income_scale,
        education_scale,
        total_pop_county_scale,
        median_monthly_housing_cost_county_scale,
        land_area_2010_scale,
        physicians_scale,
        employment_all,
        sex,
        age_scale,
        race,
        married, 
        year,
        fips_code
      ),
      all_vars(!is.na(.))
    ) %>% 
    mutate_at(
      vars(
        employment_all,
        sex,
        race,
        married,
        year,
        fips_code
      ),
      as.factor
    )
  
  lm1 <-
    glm(
      depression ~
        raw_income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current %>% mutate(depression = ifelse(depression == 1, 1, ifelse(depression == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "depression",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  return(master_df)
}

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.csv"

master_function <- function(path) {
  dfg <- 
    read_csv(path)
  
  med_inc_vars <-
    c("median_income_county_scale", "median_income_demo_scale")
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = health_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/home/djolear/gallup/relative_status/regressions/median_income_models/results/health_outcomes_mi.csv"))
  
}

master_function(data_path)