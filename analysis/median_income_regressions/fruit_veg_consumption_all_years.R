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

fv_regression_function <- function(median_income_var_name, dfg) {
  
  # select data and set median income variable
  
  dfg <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      fruits_veggies_scale,
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
        fruits_veggies_scale,
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
  
  # set up coding of factor variables
  contrasts(dfg$sex) <- contr.sum(2)
  contrasts(dfg$employment_all) <- contr.sum(2)
  contrasts(dfg$race) <- contr.sum(5)
  contrasts(dfg$married) <- contr.sum(6)
  
  # create dataframe for results
  master_df <- data.frame()
  
  # fit main effect model
  lm1b <-
    lmer(
      fruits_veggies_scale ~
        raw_income_scale +
        median_income_var_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married +
        year +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg
    )
  
  df <-
    tidy(lm1b)
  
  fit_stats <-
    glance(lm1b) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "fruit_veggies_scale",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  # fit interactive model
  lm1c <-
    lmer(
      fruits_veggies_scale ~
        median_income_var_scale * raw_income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        median_income_var_scale * year +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married +
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = dfg
    )
  
  df <-
    tidy(lm1c)
  
  fit_stats <-
    glance(lm1c) %>% 
    mutate(
      id_controls = "yes_int"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "fruit_veggies_scale",
      id_controls = "yes_int"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  master_df <-
    bind_rows(
      master_df,
      df
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
    future_map_dfr(.x = med_inc_vars, .f = fv_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/home/djolear/gallup/relative_status/regressions/median_income_models/results/fv_mi_all_years.csv"))
  
}

master_function(data_path)