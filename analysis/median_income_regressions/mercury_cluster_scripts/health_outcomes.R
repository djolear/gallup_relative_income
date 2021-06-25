###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/Rpackages")
library("lavaan", lib.loc = "/home/djolear/Rpackages")
library("furrr", lib.loc = "/home/djolear/Rpackages")
library("broom", lib.loc = "/home/djolear/Rpackages")
library("broom.mixed", lib.loc = "/home/djolear/Rpackages")
library("lme4", lib.loc = "/home/djolear/Rpackages")


####################
## Setup Parallel ##
####################

plan(multicore, workers = 2)

###############
## Functions ##
###############

health_regression_function <- function(median_income_var_name, dfg) {

  library("broom.mixed", lib.loc = "/home/djolear/Rpackages")
  
  
  # select data and set median income variable
  dfg <-
    dfg %>% 
    mutate(income_scale = scale(income)) %>%  
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
  
  if (median_income_var_name == "income_demo_ranger_sar_vars_scale") {
    dfg$income_scale <- as.numeric(dfg$income_scale)
  } else if (median_income_var_name == "median_income_county_scale") {
    dfg$income_scale <- as.numeric(dfg$raw_income_scale)
  } else {
    dfg$income_scale <- NA
  }
  
  contrasts(dfg$sex) <- contr.sum(2)
  contrasts(dfg$employment_all) <- contr.sum(2)
  contrasts(dfg$race) <- contr.sum(5)
  contrasts(dfg$married) <- contr.sum(6)
  
  ## Diabetes ##
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      diabetes,
      income_scale,
      education_scale,
      total_pop_county_scale,
      median_home_value_county_scale,
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
        income_scale,
        education_scale,
        total_pop_county_scale,
        median_home_value_county_scale,
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
    ) 
  

  
  master_df <- data.frame()
  
  lm0 <-
    glmer(
      diabetes ~
        income_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
    )
  
  
  
  df <-
    tidy(lm0)
  
  fit_stats <-
    glance(lm0) %>% 
    mutate(
      id_controls = "yes_base"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "diabetes",
      id_controls = "yes_base"
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
  
  lm1 <-
    glmer(
      diabetes ~
        income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current 
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
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  lm2 <-
    glmer(
      diabetes ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current 
    )
  
  
  
  df <-
    tidy(lm2)
  
  fit_stats <-
    glance(lm2) %>% 
    mutate(
      id_controls = "yes_int"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "diabetes",
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
  
  
  ## HBP ##
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      hbp,
      income_scale,
      education_scale,
      total_pop_county_scale,
      median_home_value_county_scale,
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
        income_scale,
        education_scale,
        total_pop_county_scale,
        median_home_value_county_scale,
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
  
  lm0 <-
    glmer(
      hbp ~
        income_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
    )
  
  df <-
    tidy(lm0)
  
  fit_stats <-
    glance(lm0) %>% 
    mutate(
      id_controls = "yes_base"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "hbp",
      id_controls = "yes_base"
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
  
  
  lm1 <-
    glmer(
      hbp ~
        income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
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
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  
  lm2 <-
    glmer(
      hbp ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
    )
  
  df <-
    tidy(lm2)
  
  fit_stats <-
    glance(lm2) %>% 
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
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  
  ## Obesity ##
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      obese,
      income_scale,
      education_scale,
      total_pop_county_scale,
      median_home_value_county_scale,
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
        income_scale,
        education_scale,
        total_pop_county_scale,
        median_home_value_county_scale,
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
  
  lm0 <-
    glmer(
      obese ~
        income_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
    )
  
  df <-
    tidy(lm0)
  
  fit_stats <-
    glance(lm0) %>% 
    mutate(
      id_controls = "yes_base"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "obesity",
      id_controls = "yes_base"
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
  
  lm1 <-
    glmer(
      obese ~
        income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
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
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  lm2 <-
    glmer(
      obese ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current
    )
  
  df <-
    tidy(lm2)
  
  fit_stats <-
    glance(lm2) %>% 
    mutate(
      id_controls = "yes_int"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "obesity",
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
  
  
  ## Depression ##
  
  dfg_current <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      depression,
      income_scale,
      education_scale,
      total_pop_county_scale,
      median_home_value_county_scale,
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
        income_scale,
        education_scale,
        total_pop_county_scale,
        median_home_value_county_scale,
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
  
  lm0 <-
    glmer(
      depression ~
        income_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current 
    )
  
  df <-
    tidy(lm0)
  
  fit_stats <-
    glance(lm0) %>% 
    mutate(
      id_controls = "yes_base"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "depression",
      id_controls = "yes_base"
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
  
  lm1 <-
    glmer(
      depression ~
        income_scale +
        median_income_var_scale +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current 
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
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  
  lm2 <-
    glmer(
      depression ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        physicians_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg_current 
    )
  
  df <-
    tidy(lm2)
  
  fit_stats <-
    glance(lm2) %>% 
    mutate(
      id_controls = "yes_int"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "depression",
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

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"


master_function <- function(path) {
  dfg <- 
    read_rds(path)
  
  med_inc_vars <-
    c("median_income_county_scale", "income_demo_ranger_sar_vars_scale")
  
  # res <- 
  #   future_map_dfr(.x = med_inc_vars, .f = health_regression_function, dfg = dfg)
  
  res <-
    foreach(i = 1:length(med_inc_vars), .combine = cbind, .packages = c("tidyverse", "doParallel", "lme4", "broom"))%dopar%{
      health_regression_function(med_inc_vars[i], dfg)
    }
  
  write_csv(res, paste0("/project/ourminsk/gallup/results/regression/health_outcomes_mi_all_years.csv"))
  
}

master_function(data_path)