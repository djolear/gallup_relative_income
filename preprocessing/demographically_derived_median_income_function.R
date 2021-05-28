calculate_median_income_sa <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex
    ) %>% 
    summarise(
      mean_income_demo_sa = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sa = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sa = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec")
    )
  
  return(df)
}

calculate_median_income_esa <- function(df){
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
      mean_income_demo_esa = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_esa = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_esa = reldist::gini(income, w = COMB_WEIGHT)
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

calculate_median_income_esar <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      education,
      race,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      education,
      race
    ) %>% 
    summarise(
      mean_income_demo_esar = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_esar = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_esar = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("education", "sex", "age_dec", "race")
    )
  
  return(df)
}

calculate_median_income_sar <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      race,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      race
    ) %>% 
    summarise(
      mean_income_demo_sar = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sar = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sar = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "race")
    )
  
  return(df)
}

calculate_median_income_saiq <- function(df){
  df <-
    df %>% 
    mutate(
      income_quintile = 
        .bincode(
          median_income_county, 
          breaks = quantile(probs = seq(0, 1, 0.2), median_income_county, na.rm = T), 
          include.lowest = TRUE
        )
    )
  
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      income_quintile,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      income_quintile
    ) %>% 
    summarise(
      mean_income_demo_sariq = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sariq = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sariq = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "income_quintile")
    )
  
  return(df)
}


calculate_median_income_sariq <- function(df){
  df <-
    df %>% 
    mutate(
      income_quintile = 
        .bincode(
          median_income_county, 
          breaks = quantile(probs = seq(0, 1, 0.2), median_income_county, na.rm = T), 
          include.lowest = TRUE
        )
    )
  
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      race,
      income_quintile,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      race,
      income_quintile
    ) %>% 
    summarise(
      mean_income_demo_sariq = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sariq = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sariq = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "race", "income_quintile")
    )
  
  return(df)
}


calculate_median_income_esaiq <- function(df){
  df <-
    df %>% 
    mutate(
      income_quintile = 
        .bincode(
          median_income_county, 
          breaks = quantile(probs = seq(0, 1, 0.2), median_income_county, na.rm = T), 
          include.lowest = TRUE
        )
    )
  
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      education,
      income_quintile,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      education,
      income_quintile
    ) %>% 
    summarise(
      mean_income_demo_esaiq = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_esaiq = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_esaiq = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "education", "income_quintile")
    )
  
  return(df)
}

calculate_median_income <- function(df) {
  df <- calculate_median_income_sa(df)
  df <- calculate_median_income_esa(df)
  df <- calculate_median_income_esar(df)
  df <- calculate_median_income_sar(df)
  df <- calculate_median_income_saiq(df)
  df <- calculate_median_income_sariq(df)
  df <- calculate_median_income_esaiq(df)
}

