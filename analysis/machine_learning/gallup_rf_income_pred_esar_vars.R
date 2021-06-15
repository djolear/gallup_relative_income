###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("glmnet", lib.loc = "/home/djolear/R")
library("randomForest", lib.loc = "/home/djolear/R")
library("ranger", lib.loc = "/home/djolear/R")
# library("foreach", lib.loc = "/home/djolear/R")
# library("doParallel", lib.loc = "/home/djolear/R")
library("caret", lib.loc = "/home/djolear/R")



## Functions ##

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

## Load Data ##

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

data <- 
  read_rds(path)

data <-
  data %>% 
  mutate(income_scale = scale(income)) %>% 
  dplyr::select(
    subid,
    median_home_value_county_scale,
    land_area_2010_scale,
    physicians_scale,
    total_pop_county_scale,
    education_scale,
    sex,
    age_scale,
    race,
    married,
    employment_all,
    year,
    children_scale,
    census_region,
    income_scale
  ) %>% 
  filter_at(
    vars(
      median_home_value_county_scale,
      land_area_2010_scale,
      physicians_scale,
      total_pop_county_scale,
      education_scale,
      sex,
      age_scale,
      race,
      married,
      employment_all,
      year,
      children_scale,
      census_region,
      income_scale
    ),
    all_vars(!is.na(.))
  )

data_train <-
  data %>% 
  dplyr::select(
    age_scale,
    sex,
    race,
    education_scale,
    income_scale
  )


rf <-
  ranger(
    income_scale ~ .,
    data = data_train,
    num.threads = 8
  )

## Export Results ##

preds <- predict(model_caret, data_train)

data <-
  bind_cols(
    data,
    income_demo_ranger_all_vars_scale = scale(preds)
  ) %>% 
  dplyr::select(
    subid,
    income_demo_ranger_all_vars_scale
  )

## Export Results ##

preds <- predict(model_caret, data_train)

data <-
  bind_cols(
    data,
    income_demo_ranger_all_vars_scale = scale(preds)
  ) %>% 
  dplyr::select(
    subid,
    income_demo_ranger_esar_vars_scale
  )

write_csv(data, "/project/ourminsk/gallup/results/ml/preds_rf_income_esar_vars.csv")

