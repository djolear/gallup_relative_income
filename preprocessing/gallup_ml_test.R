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
    ladder_now_scale,
    fruits_veggies_scale,
    eat_healthy,
    smoke,
    diabetes,
    hbp,
    obese,
    depression,
    COMB_WEIGHT
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
      income_scale,
      ladder_now_scale,
      fruits_veggies_scale,
      eat_healthy,
      smoke,
      diabetes,
      hbp,
      obese,
      depression,
      COMB_WEIGHT
    ),
    all_vars(!is.na(.))
  )

#train_ind <- sample(1:nrow(data), 250000)

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


lm1 <-
  lm(
    scale(ladder_now) ~
      scale(income) +
      total_pop_county_scale +
      scale(median_income_demo_sar) +
      median_home_value_county_scale +
      land_area_2010_scale +
      physicians_scale +
      scale(education) +
      employment_all +
      sex +
      scale(age) +
      race +
      married + 
      year,
    data = data
  )


summary(lm1)
lm.beta(lm1)
VIF(lm1)



gbm <- 
  gbm(
    income_scale ~ .,
    data = data_train
  )


data <-
  bind_cols(
    data,
    income_demo_gbm_scale = scale(gbm$fit)
  )

lm1 <- 
  lm(
    smoke ~ 
      income_demo_ranger_sar_scale + 
      income_scale + 
      married +
      race + 
      sex + 
      age_scale + 
      education_scale +
      year +
      employment_all, 
    data
  )


summary(lm1)
lm.beta(lm1)
regclass::VIF(lm1)



X_train <-
  data_train %>% 
  dplyr::select(-income_scale) %>% 
  as.matrix()


dtrain <-
  model.matrix(~ sex + race, data=data_train, 
             contrasts.arg=list(sex=contrasts(data_train$sex, contrasts=F), 
                                race=contrasts(data_train$race, contrasts=F)))

dtrain <-
  cbind(
    as.numeric(data$age_scale),
    dtrain
  )

dtrain <- xgb.DMatrix(data = dtrain, label = data_train$income_scale)



xgb <- 
  xgboost(
    dtrain, max.depth = 4, eta = 1, nthread = 2, nrounds = 1000
  )

pred <- predict(xgb, dtrain)

data <-
  bind_cols(
    data,
    income_demo_xgb = scale(pred)
  )


lm1 <- 
  lm(
    PURPOSE_scale ~ 
      income_demo_xgb + 
      income + 
      married +
      race + 
      sex + 
      age + 
      education, 
    data
  )


summary(lm1)
lm.beta(lm1)
VIF(lm1)


###

data_train <-
  data %>% 
  dplyr::select(
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
    income_demo_gbm_scale ,
    income_scale,
    smoke
  ) %>% 
  sample_n(100000)

outcome_train <-
  data_train %>% 
  dplyr::select(
    smoke
  ) %>%
  data.matrix()

data_train <-
  data_train %>% 
  dplyr::select(-smoke)

x_train <- model.matrix( ~ .-1, data_train)
x_train


# library(glmnet)
# 
# set.seed(123) 
# cv <- cv.glmnet(x = data.matrix(x_train), y = outcome_train)
# # Display the best lambda value
# cv$lambda.min
# 
# 
# model <- glmnet(x = x_train, y = outcome_train, alpha = 0, lambda = cv$lambda.min)
# 
# coef(model)
# 
# library(caret)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(
  x = x_train,
  y = as.factor(outcome_train),
  method = "glmnet",
  trControl = cv_5,
  verbose = TRUE
)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(hit_elnet)

varImp(hit_elnet)

coef(hit_elnet$finalModel, hit_elnet$bestTune$lambda)
