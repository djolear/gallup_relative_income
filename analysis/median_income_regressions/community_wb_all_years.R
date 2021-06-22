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

com_regression_function <- function(median_income_var_name, dfg) {
  
  dfg <-
    dfg %>% 
    mutate(
      income_scale = 
        ifelse(
          median_income_var_name == "income_demo_ranger_sar_vars_scale",
          scale(income),
          ifelse(
            median_income_var_name == "median_income_county_scale",
            raw_income_scale,
            NA
          )
        )
    ) %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      COMMUNITY_scale,
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
        COMMUNITY_scale,
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
  
  contrasts(dfg$sex) <- contr.sum(2)
  contrasts(dfg$employment_all) <- contr.sum(2)
  contrasts(dfg$race) <- contr.sum(5)
  contrasts(dfg$married) <- contr.sum(6)
  
  master_df <- data.frame()
  
  lm1 <-
    lmer(
      COMMUNITY_scale ~
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
        year +
        (1 + income_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
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
      outcome = "COMMUNITY_scale",
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
  
  gc()
  
  lm1 <-
    lmer(
      COMMUNITY_scale ~
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
        year +
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
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
      outcome = "COMMUNITY_scale",
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
  
  gc()
  
  lm1 <-
    lmer(
      COMMUNITY_scale ~
        median_income_var_scale * income_scale +
        median_income_var_scale * education_scale +
        median_income_var_scale * employment_all +
        median_income_var_scale * sex +
        median_income_var_scale * age_scale +
        median_income_var_scale * race +
        median_income_var_scale * married +
        median_income_var_scale * year +
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
        (1 + income_scale|fips_code) +
        (1 + median_income_var_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
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
      outcome = "COMMUNITY_scale",
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
  
  gc()
  
  return(master_df)
}


data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

master_function <- function(path) {
  dfg <- 
    read_rds(path)
  
  dfg <- 
    dfg %>% 
    filter(year %in% c(2014:2017))
  
  med_inc_vars <-
    c("median_income_county_scale", "income_demo_ranger_sar_vars_scale")
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = com_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/home/djolear/gallup/relative_status/regressions/median_income_models/results/community_mi_all_years.csv"))
  
}

master_function(data_path)