
standardize_variables <- function(df) {
  scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
  df <-
    df %>% 
    mutate_at(
      vars(
        raw_income,
        education,
        age,
        children,
        adults,
        
        unweighted_pop_county,
        total_pop_county,
        land_area_2010,
        median_income_county,
        median_monthly_housing_cost_county,
        median_home_value_county,
        gini_county,
        
        starts_with("median"),
        
        fruits_veggies,
        num_alc,
        sr_health,
        bmi,
        starts_with("ladder"),
        PURPOSE:little_pleasure
      ),
      as.numeric
    ) %>% 
    mutate_at(
      vars(
        raw_income,
        education,
        age,
        children,
        adults,
        
        unweighted_pop_county,
        total_pop_county,
        land_area_2010,
        median_income_county,
        median_monthly_housing_cost_county,
        median_home_value_county,
        gini_county,
        
        starts_with("median"),
        
        fruits_veggies,
        num_alc,
        sr_health,
        bmi,
        starts_with("ladder"),
        PURPOSE:little_pleasure,
        neg_aff
      ),
      list(scale = scale2)
    ) %>% 
    mutate_at(
      vars(
        ends_with("scale")
      ),
      as.numeric
    )
  
  return(df)
}

