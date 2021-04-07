if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  fastDummies
)

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

dfg_rs <-
  read_rds(paste0(machine_path, "research/projects/gallup/gallup_data/relative_status/dfg_rs.rds"))

data <-
  dfg_rs %>%
  filter_at(
    vars(
      eat_healthy,
      fruits_veggies_scale,
      smoke,
      median_income_demo_scale,
      education_scale,
      raw_income_scale,
      median_home_value_county_scale,
      median_monthly_housing_cost_county_scale,
      physicians_scale,
      PURPOSE_scale,
      COMMUNITY_scale,
      FINANCIAL_scale,
      SOCIAL_scale,
      PHYSICAL_scale,
      # neg_aff_scale,
      enough_money_scale,
      comp_satis_std_liv_scale,
      # bmi_scale,
      total_pop_county_scale,
      land_area_2010_scale,
      race,
      sex,
      age_scale,
      married,
      employment_all,
      year
    ),
    all_vars(!is.na(.))
  ) %>%
  dplyr::select(
    eat_healthy,
    fruits_veggies_scale,
    smoke,
    median_income_demo_scale,
    education_scale,
    raw_income_scale,
    median_home_value_county_scale,
    median_monthly_housing_cost_county_scale,
    physicians_scale,
    PURPOSE_scale,
    COMMUNITY_scale,
    FINANCIAL_scale,
    SOCIAL_scale,
    PHYSICAL_scale,
    # neg_aff_scale,
    enough_money_scale,
    comp_satis_std_liv_scale,
    # bmi_scale,
    total_pop_county_scale,
    land_area_2010_scale,
    race,
    sex,
    age_scale,
    married,
    employment_all,
    year
  ) %>%
  dummy_cols(., select_columns = c("sex", "race", "married", "employment_all")) %>%
  mutate(across(eat_healthy:land_area_2010_scale, as.numeric)) %>%
  dplyr::select(-c(sex, race, married))

write_csv(data, paste0("D:/data/gallup/exports/for_mediation_analyses/dfg_rs_med_data_all_years.csv"))

years <-
  data %>% 
  count(year)

for(i in 1:length(years$year)){
  df <-
    data %>% 
    filter(year == years$year[i])
  
  write_csv(df, paste0("D:/data/gallup/exports/for_mediation_analyses/dfg_rs_med_", years$year[i], ".csv"))
}

