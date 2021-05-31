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
      mean_income_demo_saiq = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_saiq = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_saiq = reldist::gini(income, w = COMB_WEIGHT)
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


calculate_median_income_sacr <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      census_region,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      census_region
    ) %>% 
    summarise(
      mean_income_demo_sacr = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sacr = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sacr = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "census_region")
    )
  
  return(df)
}


calculate_median_income_sarcr <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      race,
      census_region,
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
      census_region
    ) %>% 
    summarise(
      mean_income_demo_sarcr = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sarcr = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sarcr = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "race", "census_region")
    )
  
  return(df)
}


calculate_median_income_esacr <- function(df){

  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      age,
      sex,
      education,
      census_region,
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
      census_region
    ) %>% 
    summarise(
      mean_income_demo_esacr = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_esacr = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_esacr = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "education", "census_region")
    )
  
  return(df)
}


calculate_median_income_cr <- function(df){
  
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      census_region,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    group_by(
      census_region
    ) %>% 
    summarise(
      mean_income_demo_cr = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_cr = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_cr = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    left_join(
      med_inc_gallup,
      by = c("census_region")
    )
  
  return(df)
}


calculate_median_income_st <- function(df){
  
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      income,
      STATE_NAME,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    group_by(
      STATE_NAME
    ) %>% 
    summarise(
      mean_income_demo_st = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_st = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_st = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    left_join(
      med_inc_gallup,
      by = c("STATE_NAME")
    )
  
  return(df)
}


calculate_median_income_iq <- function(df){
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
      income_quintile,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    group_by(
      income_quintile
    ) %>% 
    summarise(
      mean_income_demo_iq = matrixStats::weightedMean(income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_iq = matrixStats::weightedMedian(income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_iq = reldist::gini(income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("income_quintile")
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
  df <- calculate_median_income_sacr(df)
  df <- calculate_median_income_sarcr(df)
  df <- calculate_median_income_esacr(df)
  df <- calculate_median_income_cr(df)
  df <- calculate_median_income_st(df)
  df <- calculate_median_income_iq(df)
}

