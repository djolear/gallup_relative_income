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
library("foreach", lib.loc = "/home/djolear/R")
library("doParallel", lib.loc = "/home/djolear/R")

####################
## Setup Parallel ##
####################

# plan(multicore, workers = 8)

cores = detectCores()
cl <- makeCluster(cores[1])
registerDoParallel(cl)


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
        (1 + raw_income_scale|fips_code) +
        (1 + median_income_demo_scale|fips_code),
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa"),
      data = data 
    )
  
  lm_med <- summary(lm_med)
  
  
  ie = lm_out$coefficients[4] * lm_med$coefficients[3]
  
  rm(lm_out)
  rm(lm_med)
  
  gc()
  
  return(ie)
  
}

###############
## Load Data ##
###############

data_path <- "/project/ourminsk/gallup/exports/for_mediation_analyses/dfg_rs_med_data_all_years.csv"

dfg <- 
  read_csv(
    data_path,
    col_types = c("ddddddddddddddddddffdffff")
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
      year,
      fips_code
    ),
    all_vars(!is.na(.))
  )

################
##  Mediation ##
################

ptm <- proc.time()

boot_data <- foreach(i= 1:1000, .combine = cbind, .packages='lme4') %dopar% {
  indices <- sample(1:nrow(dfg), nrow(dfg), replace = T)
  ie <- indirect_effect(dfg, indices) #calling a function
  ie 
}

#stop cluster
stopCluster(cl)

print(proc.time() - ptm)

##################
## Save Results ##
##################

boot_data <- data.frame(res = t(boot_data))

write_csv(boot_data, paste0("/project/ourminsk/gallup/results/mediation/fv_pwb_mediation_boot_p2.csv"))

