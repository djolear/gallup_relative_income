library(randomForest)
library(gbm)
library(xgboost)
library(ranger)
library(caret)

set.seed(1)

data <-
  dfg_rs %>% 
  mutate(income_scale = scale(income)) %>% 
  dplyr::select(
    subid,
    sex,
    age_scale,
    race,
    year,
    income_scale
    COMB_WEIGHT
  ) %>% 
  filter_at(
    vars(
      subid
      sex,
      age_scale,
      race,
      year,
      income_scale
      COMB_WEIGHT
    ),
    all_vars(!is.na(.))
  )

data_train <-
  data %>%
  dplyr::select(
    sex,
    age_scale,
    race,
    year,
    income_scale
  )


rf <-
  ranger(
    income_scale ~ .,
    data = data_train,
    case.weights = data$COMB_WEIGHT,
    mtry = 3,
    min.node.size = 10
  )

data <-
  bind_cols(
    data,
    income_demo_ranger_sar_age_m2_scale = scale(rf$predictions)
  )

dfg_rs <-
  dfg_rs %>% 
  left_join(
    data %>% 
      dplyr::select(
        subid,
        income_demo_ranger_sar_age_m2_scale
      )
  )

gallup_rf_income <-
  data %>% 
  dplyr::select(
    subid,
    income_demo_ranger_sar_age_m2_scale
  )

write_csv(gallup_rf_income, "D:\data\gallup\results\ml_predictions\gallup_rf_income_preds.csv")