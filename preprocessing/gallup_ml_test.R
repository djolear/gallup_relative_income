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
  ) %>% 
  sample_n(50000)


# lm1 <- 
#   lm(
#     fruits_veggies ~ 
#       median_income_demo_sar + 
#       income + 
#       married +
#       race + 
#       sex + 
#       age + 
#       education +
#       employment_all +
#       year, 
#     data
#   )
# 
# summary(lm1)
# lm.beta(lm1)
# VIF(lm1)

data_train <-
  data %>%
  dplyr::select(
    -subid
  )
# 
# rf <- 
#   ranger(
#     raw_income_scale ~ .,
#     data = data_train
#   )

# tgrid <- 
#   expand.grid(
#     mtry = 3:6,
#     splitrule = "variance",
#     min.node.size = c(10, 20)
#   )

tgrid <-
  expand.grid(
    mtry = 3,
    splitrule = "variance",
    min.node.size = c(10)
  )

model_caret <- 
  train(
    income_scale  ~ ., 
    data = data_train,
    method = "ranger",
    trControl = 
      trainControl(
        method="cv", 
        number = 5, 
        verboseIter = T
      ),
    tuneGrid = tgrid,
    num.trees = 100,
    importance = "permutation"
  )

get_best_result(model_caret)

preds <- predict(model_caret, data_train)


data <-
  bind_cols(
    data,
    income_demo_ranger_scale = scale(preds)
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
    income ~ .,
    data = data_train
  )


data <-
  bind_cols(
    data,
    income_demo_gbm = gbm$fit
  )

lm1 <- 
  lm(
    fruits_veggies ~ 
      income_demo_gbm + 
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



X_train <-
  data_train %>% 
  dplyr::select(-income) %>% 
  as.matrix()


dtrain <-
  model.matrix(~ sex + race, data=data_train, 
             contrasts.arg=list(sex=contrasts(data_train$sex, contrasts=F), 
                                race=contrasts(data_train$race, contrasts=F)))

dtrain <-
  cbind(
    as.numeric(data$age),
    dtrain
  )

dtrain <- xgb.DMatrix(data = dtrain, label = data_train$income)



xgb <- 
  xgboost(
    dtrain, max.depth = 4, eta = 1, nthread = 2, nrounds = 1000
  )

pred <- predict(xgb, dtrain)

data <-
  bind_cols(
    data,
    income_demo_xgb = pred
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
    median_income_demo_sar,
    raw_income_scale
  ) %>% 
  mutate(
    median_income_demo_sar_scale = scale(median_income_demo_sar)
  ) %>% 
  dplyr::select(-median_income_demo_sar)

outcome_train <-
  data %>% 
  dplyr::select(
    ladder_now_scale
  ) %>%
  data.matrix()

x_train <- model.matrix( ~ .-1, data_train)
x_train


library(glmnet)

set.seed(123) 
cv <- cv.glmnet(x = data.matrix(x_train), y = outcome_train)
# Display the best lambda value
cv$lambda.min


model <- glmnet(x = x_train, y = outcome_train, alpha = 0, lambda = cv$lambda.min)

coef(model)

library(caret)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(
  eat_healthy ~ ., data = data,
  method = "glmnet",
  trControl = cv_5
)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(hit_elnet)

varImp(hit_elnet)
