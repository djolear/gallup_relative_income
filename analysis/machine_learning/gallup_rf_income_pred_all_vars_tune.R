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


## Set up Tune Grid ##

tgrid <-
  expand.grid(
    mtry = 3:6,
    splitrule = "variance",
    min.node.size = c(10, 20)
  )


## Parallelize ##

# cl <- makePSOCKcluster(8)
# registerDoParallel(cl)

## Tune Model ##

model_caret <- 
  train(
    income_scale  ~ ., 
    data = data,
    method = "ranger",
    trControl = 
      trainControl(
        method="cv", 
        number = 5, 
        verboseIter = T,
        allowParallel = TRUE
      ),
    tuneGrid = tgrid,
    num.trees = 100,
    importance = "permutation",
    num.threads = 8
  )

# stopCluster(cl)

## Export Results ##

results <- 
  get_best_result(model_caret)

write_csv(results, "/project/ourminsk/gallup/results/ml/results_rf_income_all_vars.csv")



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

write_csv(data, "/project/ourminsk/gallup/results/ml/preds_rf_income_all_vars.csv")

