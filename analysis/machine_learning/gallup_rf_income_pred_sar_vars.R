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
  read_rds(data_path)

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

data <- 
  read_rds(data_path)

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
    income_scale,
    COMB_WEIGHT
  ) %>% 
  filter_at(
    vars(
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
      income_scale,
      COMB_WEIGHT
    ),
    all_vars(!is.na(.))
  )

data_train <-
  data %>% 
  dplyr::select(
    age_scale,
    sex,
    race,
    income_scale
  )




rf <-
  ranger(
    formula         = income_scale ~ ., 
    data            = data_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = 0.632,
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123,
    case.weights    = data$COMB_WEIGHT,
    num.threads     = 8
  )


## Export Results ##

preds <- predict(rf, data_train)


data <-
  bind_cols(
    data,
    income_demo_ranger_sar_vars_scale = scale(preds$predictions)
  ) %>% 
  dplyr::select(
    subid,
    income_demo_ranger_sar_vars_scale
  )

write_csv(data, "/project/ourminsk/gallup/results/ml/preds_rf_income_sar_vars_non_caret_tune.csv")

