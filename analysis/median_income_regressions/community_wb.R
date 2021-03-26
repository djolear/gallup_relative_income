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

plan(multicore, workers = 4)

###############
## Functions ##
###############

com_regression_function <- function(median_income_var_name, dfg) {
  
  dfg <-
    dfg %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      COMMUNITY_scale,
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
        COMMUNITY_scale,
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
  
  contrasts(dfg$sex) <- contr.sum(2)
  contrasts(dfg$employment_all) <- contr.sum(2)
  contrasts(dfg$race) <- contr.sum(5)
  contrasts(dfg$married) <- contr.sum(6)
  
  master_df <- data.frame()
  
  master_df <-
    bind_rows(
      master_df,
      df
    )
  
  lm1b <-
    lmer(
      COMMUNITY_scale ~
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
        (0 + raw_income_scale|fips_code) +
        (0 + median_income_var_scale|fips_code),
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
      year = dfg$year[1],
      outcome = "COMMUNITY_scale",
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
  
  lm1c <-
    lmer(
      COMMUNITY_scale ~
        median_income_var_scale * raw_income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
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
        (0 + raw_income_scale|fips_code) +
        (0 + median_income_var_scale|fips_code),
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
      year = dfg$year[1],
      outcome = "COMMUNITY_scale",
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

data_path <- "/home/djolear/gallup/relative_status/data/regression/"

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv")
  ) %>% 
  mutate(
    year = as.numeric(str_extract(file_list, "[[:digit:]]+"))
  ) %>% 
  filter(year %in% c(2014:2017))

master_function <- function(path) {
  dfg <- 
    read_csv(paste0("/home/djolear/gallup/relative_status/data/regression/", path))
  
  med_inc_vars <-
    c("median_income_county_scale", "median_income_demo_scale")
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = com_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/home/djolear/gallup/relative_status/regressions/community_mi_", dfg$year[1], ".csv"))
  
}

future_map(.x = file_list$file_list, .f = master_function)
