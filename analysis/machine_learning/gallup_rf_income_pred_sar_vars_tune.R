###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/Rpackages")
library("lavaan", lib.loc = "/home/djolear/Rpackages")
library("furrr", lib.loc = "/home/djolear/Rpackages")
library("broom", lib.loc = "/home/djolear/Rpackages")
library("glmnet", lib.loc = "/home/djolear/Rpackages")
library("randomForest", lib.loc = "/home/djolear/Rpackages")
library("ranger", lib.loc = "/home/djolear/Rpackages")
# library("foreach", lib.loc = "/home/djolear/Rpackages")
# library("doParallel", lib.loc = "/home/djolear/Rpackages")
library("caret", lib.loc = "/home/djolear/Rpackages")


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
    year,
    income_scale
  )


## Set up Tune Grid ##

tgrid <-
  expand.grid(
    mtry = c(1, 2, 3),
    splitrule = "variance",
    min.node.size = c(5, 10, 15, 20, 30, 40, 50, 60)
  )


## Parallelize ##

# cl <- makePSOCKcluster(8)
# registerDoParallel(cl)

## Tune Model ##

model_caret <- 
  train(
    x = data_train[, 1:4],
    y = data_train[, 5],
    method = "ranger",
    trControl = 
      trainControl(
        method="cv", 
        number = 5, 
        verboseIter = T,
        allowParallel = TRUE
      ),
    tuneGrid = tgrid,
    num.trees = 1000,
    num.threads = 8,
    weights = data$COMB_WEIGHT
  )

# stopCluster(cl)

## Export Results ##

results <- 
  get_best_result(model_caret)

write_csv(results, "/project/ourminsk/gallup/results/ml/results_rf_income_sar_vars.csv")



preds <- predict(model_caret, data_train)

data <-
  bind_cols(
    data,
    income_demo_ranger_all_vars_scale = scale(preds)
  ) 

data <-
  data %>% 
  dplyr::select(
    subid,
    income_demo_ranger_sar_vars_scale
  )

write_csv(data, "/project/ourminsk/gallup/results/ml/preds_rf_income_sar_vars.csv")

