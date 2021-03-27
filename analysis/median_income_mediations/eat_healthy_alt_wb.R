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

eh_mediation1_function <- function(current_year, dfg){
  print(current_year)
  model <- 
    '
    # direct effect
    eat_healthy ~ b1 * PURPOSE_scale + b2 * enough_money_scale + b3 * comp_satis_std_liv_scale + b4 * COMMUNITY_scale + b5 * SOCIAL_scale + c * median_income_demo_scale  + control2b * raw_income_scale + control3b * education_scale + control4b * median_monthly_housing_cost_county_scale + 
    control5b * total_pop_county_scale + control6b * land_area_2010_scale + control7b * sex_1 + control8b * race_1 + control9b * race_2 + control10b * race_3 + control11b * race_4 + 
    control12b * employment_all_1 + control13b * age_scale + control14b * physicians_scale + control15b * married_1 + control16b * married_2 + 
    control17b * married_3 + control18b * married_4 + control19b * married_5
    
    # mediators
    PURPOSE_scale ~ a1 * median_income_demo_scale + control2a * raw_income_scale + control3a * education_scale + 
    control4a * median_monthly_housing_cost_county_scale + control5a * total_pop_county_scale + control6a * land_area_2010_scale + control7a * sex_1 + control8a * race_1 + 
    control9a * race_2 + control10a * race_3 + control11a * race_4 + control12a * employment_all_1 + control13a * age_scale + control14a * physicians_scale + 
    control15a * married_1 + control16a * married_2 + control17a * married_3 + control18a * married_4 + control19a * married_5
    
    enough_money_scale ~ a2 * median_income_demo_scale + control2c * raw_income_scale + control3c * education_scale + 
    control4c * median_monthly_housing_cost_county_scale + control5c * total_pop_county_scale + control6c * land_area_2010_scale + control7c * sex_1 + control8c * race_1 + 
    control9c * race_2 + control10c * race_3 + control11c * race_4 + control12c * employment_all_1 + control13c * age_scale + control14c * physicians_scale + 
    control15c * married_1 + control16c * married_2 + control17c * married_3 + control18c * married_4 + control19c * married_5 

    comp_satis_std_liv_scale ~ a3 * median_income_demo_scale + control2d * raw_income_scale + control3d * education_scale + 
    control4d * median_monthly_housing_cost_county_scale + control5d * total_pop_county_scale + control6d * land_area_2010_scale + control7d * sex_1 + control8d * race_1 + 
    control9d * race_2 + control10d * race_3 + control11d * race_4 + control12d * employment_all_1 + control13d * age_scale + control14d * physicians_scale + 
    control15d * married_1 + control16d * married_2 + control17d * married_3 + control18d * married_4 + control19d * married_5 
    
    COMMUNITY_scale ~ a4 * median_income_demo_scale + control2e * raw_income_scale + control3e * education_scale + 
    control4e * median_monthly_housing_cost_county_scale + control5e * total_pop_county_scale + control6e * land_area_2010_scale + control7e * sex_1 + control8e * race_1 + 
    control9e * race_2 + control10e * race_3 + control11e * race_4 + control12e * employment_all_1 + control13e * age_scale + control14e * physicians_scale + 
    control15e * married_1 + control16e * married_2 + control17e * married_3 + control18e * married_4 + control19e * married_5 
    
    SOCIAL_scale ~ a5 * median_income_demo_scale + control2f * raw_income_scale + control3f * education_scale + 
    control4f * median_monthly_housing_cost_county_scale + control5f * total_pop_county_scale + control6f * land_area_2010_scale + control7f * sex_1 + control8f * race_1 + 
    control9f * race_2 + control10f * race_3 + control11f * race_4 + control12f * employment_all_1 + control13f * age_scale + control14f * physicians_scale + 
    control15f * married_1 + control16f * married_2 + control17f * married_3 + control18f * married_4 + control19f * married_5 
    

    # direct effect
    direct := c
    
    # indirect effect (a*b)
    indirect1 := a1 * b1
    indirect2 := a2 * b2
    indirect3 := a3 * b3
    indirect4 := a4 * b4
    indirect5 := a5 * b5

    
    # total effect
    total := c + (a1 * b1)
    prop_mediated_1 := (abs(indirect1) )/abs(total)
    
    # covariances
    
'
  fit <- 
    lavaan::sem(
      model = model,
      data = dfg,
      ordered = c("eat_healthy", "sex_1", "race_1", "race_2", "race_3", "race_4", "employment_all_1", "married_1", "married_2", "married_3", "married_4", "married_5"),
      se = "bootstrap",
      bootstrap = 5000,
      estimator = "DWLS"
    )

  
  df <-
    lavaan::parameterestimates(
      fit,
      ci = TRUE, 
      standardize = TRUE,
      level = 0.95
    )
  
  df$year = current_year
  
  return(df)
}


data_path <- "/project/ourminsk/gallup/exports/for_mediation_analyses/"

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
    read_csv(paste0("/project/ourminsk/gallup/exports/for_mediation_analyses/", path))
  
  res <- eh_mediation1_function(dfg$year[1], dfg)
  
  write_csv(res, paste0("/project/ourminsk/gallup/results/mediation/eh_mediation_alt_wb_", dfg$year[1], ".csv"))
  
}

future_map(.x = file_list$file_list, .f = master_function)

