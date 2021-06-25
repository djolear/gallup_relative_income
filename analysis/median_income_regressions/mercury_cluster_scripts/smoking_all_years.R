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

smoke_regression_function <- function(median_income_var_name, dfg) {
  
  dfg <-
    dfg %>% 
    mutate(income_scale = scale(income)) %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      smoke,
      income_scale,
      raw_income_scale,
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
        smoke,
        income_scale,
        raw_income_scale,
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
  
  if (median_income_var_name == "income_demo_ranger_sar_vars_scale") {
    dfg$income_scale <- dfg$income_scale
  } else if (median_income_var_name == "median_income_county_scale") {
    dfg$income_scale <- dfg$raw_income_scale
  } else {
    dfg$income_scale <- NA
  }
  
  contrasts(dfg$sex) <- contr.sum(2)
  contrasts(dfg$employment_all) <- contr.sum(2)
  contrasts(dfg$race) <- contr.sum(5)
  contrasts(dfg$married) <- contr.sum(6)
  
  master_df <- data.frame()
  
  lm1 <-
    glmer(
      smoke ~
        income_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married +
        (1 + income_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      mod = "no_mi"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "smoke",
      mod = "no_mi"
    ) %>% 
    left_join(
      fit_stats,
      by = "mod"
    )
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  
  lm1 <-
    glmer(
      smoke ~
        income_scale +
        median_income_var_scale +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married +
        (1 + median_income_var_scale|fips_code) +
        (1 + income_scale|fips_code),
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      mod = "mi"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "smoke",
      mod = "mi"
    ) %>% 
    left_join(
      fit_stats,
      by = "mod"
    )
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  lm1 <-
    glmer(
      smoke ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        total_pop_county_scale +
        median_home_value_county_scale +
        land_area_2010_scale +
        physicians_scale +        
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married +
        (1 + median_income_var_scale|fips_code) +
        (1 + income_scale|fips_code),      
      family = "binomial",
      control = glmerControl(optimizer = "bobyqa"),
      data = dfg
    )
  
  df <-
    tidy(lm1)
  
  fit_stats <-
    glance(lm1) %>% 
    mutate(
      mod = "mi_int"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      outcome = "smoke",
      mod = "mi_int"
    ) %>% 
    left_join(
      fit_stats,
      by = "mod"
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
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = smoke_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/project/ourminsk/gallup/results/regression/smoking_mi_all_years.csv"))

}

master_function(data_path)