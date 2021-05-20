calculate_median_income <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      education,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      education
    ) %>% 
    summarise(
      mean_income_demo = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("education", "sex", "age_dec")
    )
  
  return(df)
}

