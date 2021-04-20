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


####################
## Setup Parallel ##
####################

###############
## Functions ##
###############

mediation_results_munge <- function(results, mediator_name, model_type) {
  ie_ci <- data.frame(t(results$d1.ci))
  names(ie_ci) <- c("ie_ci_low", "ie_ci_high")
  
  de_ci <- data.frame(t(results$z1.ci))
  names(de_ci) <- c("de_ci_low", "de_ci_high")
  
  te_ci <- data.frame(t(results$tau.ci))
  names(te_ci) <- c("te_ci_low", "te_ci_high")
  
  pm_ci <- data.frame(t(results$n1.ci))
  names(pm_ci) <- c("pm_ci_low", "pm_ci_high")
  
  med_results <-
    data.frame(
      mediatior = mediator_name,
      model = model_type,
      indirect_effect = results$d1,
      ie_ci,
      direct_effect = results$z1,
      de_ci,
      total_effect = results$tau.coef,
      te_ci,
      prop_med = results$n1,
      pm_ci
    )
  
  return(med_results)
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


################
## Fit Models ##
################

# Outcome Model

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
      #(1|fips_code),
      (1 + raw_income_scale|fips_code) +
      (1 + median_income_demo_scale|fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = dfg 
  )


# Mediator Model

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
      # (1|fips_code),
     (1 + raw_income_scale|fips_code) +
     (1 + median_income_demo_scale|fips_code),
    REML = FALSE,
    control = lmerControl(optimizer = "bobyqa"),
    data = dfg 
  )


#######################
## Testing Mediation ##
#######################

results_purpose <- 
  mediation::mediate(
    lm_med, 
    lm_out, 
    treat = 'median_income_demo_scale', 
    mediator = 'PURPOSE_scale',
    covariates = 
      c(
        "raw_income_scale",
        "total_pop_county_scale",
        "median_monthly_housing_cost_county_scale",
        "land_area_2010_scale",
        "physicians_scale",
        "education_scale",
        "employment_all",
        "sex",
        "age_scale",
        "race",
        "married",
        "year",
        "PURPOSE_scale",
        "COMMUNITY_scale",
        "SOCIAL_scale"
      ),
    parallel = "multicore",
    ncpus = 6,
    sims = 1000
  )


##################
## Save Results ##
##################

res <- mediation_results_munge(results_purpose, "purpose_wb", "full_parallel")

write_csv(res, paste0("/project/ourminsk/gallup/results/mediation/fv_pwb_mediation_medpkg.csv"))



