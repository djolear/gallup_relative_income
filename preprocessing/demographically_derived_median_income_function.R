calculate_median_income <- function(df){
  med_inc_gallup <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      education
    ) %>% 
    summarise(
      median_income_demo = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    filter_all(all_vars(!is.na(.)))
  
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

