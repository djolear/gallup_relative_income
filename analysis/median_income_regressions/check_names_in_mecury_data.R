###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("broom.mixed", lib.loc = "/home/djolear/R")
library("lme4", lib.loc = "/home/djolear/R")


data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

dfg <- read_rds(data_path)

dfg <-
  dfg %>% 
  select(
    median_income_county_scale,
    ladder_now,
    ladder_5yrs,
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
  ) 

print(head(dfg))