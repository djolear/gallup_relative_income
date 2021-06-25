###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/Rpackages")
library("lavaan", lib.loc = "/home/djolear/Rpackages")
library("furrr", lib.loc = "/home/djolear/Rpackages")
library("broom", lib.loc = "/home/djolear/Rpackages")
library("broom.mixed", lib.loc = "/home/djolear/Rpackages")
library("lme4", lib.loc = "/home/djolear/Rpackages")
library("foreach", lib.loc = "/home/djolear/Rpackages")
library("doParallel", lib.loc = "/home/djolear/Rpackages")


####################
## Setup Parallel ##
####################

# plan(multicore, workers = 8)

###############
## Functions ##
###############

eh_regression_function <- function(median_income_var_name, dfg) {
  
  library("broom.mixed", lib.loc = "/home/djolear/Rpackages")
  
  
  dfg <-
    dfg %>% 
    mutate(income_scale = scale(income)) %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      eat_healthy,
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
        eat_healthy,
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
  
  master_df <- data.frame()
  

  lm1b <-
    glmer(
      eat_healthy ~
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
      outcome = "eat_healthy",
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
    glmer(
      eat_healthy ~
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
    tidy(lm1c)
  
  fit_stats <-
    glance(lm1c) %>% 
    mutate(
      id_controls = "yes2"
    )
  
  df <-
    df %>%
    mutate(
      median_income_var = median_income_var_name,
      year = dfg$year[1],
      outcome = "eat_healthy",
      id_controls = "yes2"
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

dfg <- 
  read_rds(data_path)

years <-
  dfg %>% 
  count(year) %>% 
  dplyr::select(
    year
  )
  

master_function <- function(current_year, dfg) {
  dfg <- 
    dfg %>% 
    filter(year == current_year)
  
  med_inc_vars <-
    c("median_income_county_scale", "income_demo_ranger_sar_vars_scale")
  
  res <- 
    future_map_dfr(.x = med_inc_vars, .f = eh_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/project/ourminsk/gallup/results/regression/eh_mi_", dfg$year[1], ".csv"))
  
}

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

plan(multicore, workers = 8)

###############
## Functions ##
###############

eh_regression_function <- function(median_income_var_name, dfg) {
  
  # select data and set median income variable
  
  dfg <-
    dfg %>% 
    mutate(income_scale = scale(income)) %>% 
    select(
      median_income_var_scale = !!enquo(median_income_var_name),
      fruits_veggies_scale,
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
        fruits_veggies_scale,
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
        median_home_value_county_scale +
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
      year = dfg$year[1],
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

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

dfg <- 
  read_rds(data_path)

years <-
  dfg %>% 
  count(year) %>% 
  dplyr::select(
    year
  )


master_function <- function(current_year, dfg) {
  dfg <- 
    dfg %>% 
    filter(year == current_year)
  
  med_inc_vars <-
    c("median_income_county_scale", "income_demo_ranger_sar_vars_scale")
  
  res <-
    foreach(i = 1:length(med_inc_vars), .combine = cbind, .packages = c("tidyverse", "doParallel", "lme4", "broom"))%dopar%{
      eh_regression_function(med_inc_vars[i], dfg)
    }
  
  # res <- 
  #   future_map_dfr(.x = med_inc_vars, .f = eh_regression_function, dfg = dfg)
  
  write_csv(res, paste0("/project/ourminsk/gallup/results/regression/fv_mi_", dfg$year[1], ".csv"))
  
}

myCluster <- makeCluster(8, type = "PSOCK")

registerDoParallel(myCluster)

#future_map(.x = years$year, .f = master_function, dfg)

foreach(i = 1:nrow(years), .packages = c("tidyverse", "doParallel"))%dopar%{
  master_function(years$year[i], dfg)
}

stopCluster(myCluster)

