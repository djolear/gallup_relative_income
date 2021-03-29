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

plan(multicore, workers = 8)

###############
## Functions ##
###############

fv_regression_function <- function(median_income_var_name, dfg) {
  
  # select data and set median income variable
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      ladder_now_scale,
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
        ladder_now_scale,
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

  
  # create dataframe for results
  master_df <- data.frame()
  
  # fit main effect model
  lm1_ladder_now <-
    lmer(
      ladder_now_scale ~
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
    tidy(lm1_ladder_now)
  
  fit_stats <-
    glance(lm1_ladder_now) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "ladder_now",
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
      ladder_now_scale,
      ladder_5yrs_scale,
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
        ladder_now_scale,
        ladder_5yrs_scale,
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
  
  lm1_ladder_5yrs <-
    lmer(
      ladder_5yrs_scale ~
        ladder_now_scale +
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
    tidy(lm1_ladder_5yrs)
  
  fit_stats <-
    glance(lm1_ladder_5yrs) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "ladder_5yrs",
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
      bmi_scale,
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
        bmi_scale,
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
  
  lm1_bmi <-
    lmer(
      bmi_scale ~
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
    tidy(lm1_bmi)
  
  fit_stats <-
    glance(lm1_bmi) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "bmi",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  lm1_sr_health <-
    lmer(
      sr_health_scale ~
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
    tidy(lm1_sr_health)
  
  fit_stats <-
    glance(lm1_sr_health) %>% 
    mutate(
      id_controls = "yes"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "sr_health",
      id_controls = "yes"
    ) %>% 
    left_join(
      fit_stats,
      by = "id_controls"
    )
  
  
  lm1_diab <-
    glm(
      diabetes ~
        raw_income_scale +
        median_income_county_scale +
        physicians_scale +
        unweighted_pop_county_scale +
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
      data = dfg %>% mutate(diabetes = ifelse(diabetes == 1, 1, ifelse(diabetes == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1_diab)
  
  fit_stats <-
    glance(lm1_diab) %>% 
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
  
  
  lm1_hbp <-
    glm(
      hbp ~
        raw_income_scale +
        median_income_county_scale +
        physicians_scale +
        unweighted_pop_county_scale +
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
      data = dfg %>% mutate(hbp = ifelse(hbp == 1, 1, ifelse(hbp == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1_hbp)
  
  fit_stats <-
    glance(lm1_hbp) %>% 
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
  
  lm1_obese <-
    glm(
      obese ~
        raw_income_scale +
        median_income_county_scale +
        physicians_scale +
        unweighted_pop_county_scale +
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
      data = dfg %>% mutate(obese = ifelse(obese == 1, 1, ifelse(obese == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1_obese)
  
  fit_stats <-
    glance(lm1_obese) %>% 
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
  
  lm1_depression <-
    glm(
      depression ~
        raw_income_scale +
        median_income_county_scale +
        physicians_scale +
        unweighted_pop_county_scale +
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
      data = dfg %>% mutate(depression = ifelse(depression == 1, 1, ifelse(depression == 2, 0, NA)))
    )
  
  df <-
    tidy(lm1_depression)
  
  fit_stats <-
    glance(lm1_depression) %>% 
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

data_path <- "/project/ourminsk/gallup/exports/for_regression_analyses/"

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv")
  )

master_function <- function(path) {
  dfg <- 
    read_csv(paste0("/project/ourminsk/gallup/exports/for_regression_analyses/", path))
  
  med_inc_vars <-
    c("median_income_county_scale", "median_income_demo_scale")
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = fv_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/home/djolear/gallup/relative_status/regressions/median_income_models/results/health_outcomes_mi_", dfg$year[1], ".csv"))
  
}

future_map(.x = file_list$file_list, .f = master_function)
