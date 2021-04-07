###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("lavaan", lib.loc = "/home/djolear/R")
library("furrr", lib.loc = "/home/djolear/R")
library("broom", lib.loc = "/home/djolear/R")
library("broom.mixed", lib.loc = "/home/djolear/R")
library("lme4", lib.loc = "/home/djolear/R")
library("mediation", lib.loc = "/home/djolear/R")
library("boot", lib.loc = "/home/djolear/R")


###############
## Functions ##
###############

indirect_effect <- function(dataset, indices){
  
  data <- dataset[indices, ]
  
  lm_out <-
    lmer(
      fruits_veggies_scale ~
        raw_income_scale +
        median_income_demo_scale +
        PURPOSE_scale +
        FINANCIAL_scale +
        COMMUNITY_scale +
        SOCIAL_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
      (1 + raw_income_scale|fips_code) +
      (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = data 
    )
  
  lm_out <- summary(lm_out)
  
  lm_med <-
    lmer(
      PURPOSE_scale ~
        raw_income_scale +
        median_income_demo_scale +
        total_pop_county_scale +
        median_monthly_housing_cost_county_scale +
        land_area_2010_scale +
        physicians_scale +
        education_scale +
        employment_all +
        sex +
        age_scale +
        race +
        married + 
        year +
        #(1|fips_code),
      (1 + raw_income_scale|fips_code) +
      (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = data 
    )
  
  lm_med <- summary(lm_med)
  
  ie = lm_out$coefficients[4] * lm_med$coefficients[3]
  
  return(ie)
  
}

###############
## Load Data ##
###############

data_path <- "/project/ourminsk/gallup/exports/dfg_rs.rds"

dfg <- 
  read_rds(
    data_path
    #col_types = c("dddddddddddddddddddddddddddddddddddd")
  ) %>% 
  filter(year %in% c(2014:2017)) %>% 
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
      enough_money_scale,
      comp_satis_std_liv_scale,
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
  )

#######################
## Testing Mediation ##
#######################

boot_results <- boot::boot(data = dfg, statistic = indirect_effect, R = 1000, parallel = "multicore", ncpus = 6)


##################
## Save Results ##
##################

ci_results <- boot.ci(boot_results, type = "bca")

ci_results <- data.frame(ci_results$bca)

names(ci_results) <- c("x", "x", "x", "low", "high")

ci_results <- ci_results %>% dplyr::select(low, high)

res <-
  data.frame(
  ie = boot_results$t0,
  se = sd(boot_results$t),
  ci_results
)

write_csv(res, paste0("/project/ourminsk/gallup/results/mediation/fv_pwb_mediation_bootpkg.csv"))

