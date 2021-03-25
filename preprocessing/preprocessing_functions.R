###################
## Load Packages ##
###################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven
)


###############
## Set Paths ##
###############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

data_path <- "D:/data/gallup/raw/US Daily SPSS Files/"

####################
## Load Functions ##
####################

source(paste0(machine_path, "research/projects/gallup/gallup_analysis_1/relative_status_analysis/munge_data/gallup_data_select_function.R"))
source(paste0(machine_path, "research/projects/gallup/gallup_analysis_1/relative_status_analysis/munge_data/gallup_join_median_earnings_census_functions.R"))
source(paste0(machine_path, "research/projects/gallup/gallup_analysis_1/relative_status_analysis/munge_data/gallup_join_median_income_gallup_functions.R"))
source(paste0(machine_path, "research/projects/gallup/gallup_analysis_1/relative_status_analysis/munge_data/gallup_join_census_data_function.R"))
source(paste0(machine_path, "research/projects/gallup/gallup_analysis_1/relative_status_analysis/munge_data/bind_chr_data.R"))

gallup_preprocess_master <-function(data_path, data_year) {
  
  # Read data
  df <-
    read_sav(data_path) %>% 
    mutate(
      year = data_year
    )
  
  # Rename and munge variables
  df <-
    gallup_munge(df)
  
  # Select variables
  df <- gallup_select(df)
  print(paste0("Select variables for ", df$year[1], " complete."))
  
  df <- median_earnings_all_census_function(df)
  print(paste0("Generate census median earnings for ", df$year[1], " complete."))

  df <- median_income_gallup_function(df)
  print(paste0("Generate gallup median income for ", df$year[1], " complete."))

  df <- gallup_join_census_data_function(df, df$year[1])
  print(paste0("Attach general census data for ", df$year[1], " complete."))
  
  df <- bind_chr(df, df$year[1])
  print(paste0("Attach CHR ranking data for ", df$year[1], " complete."))

  gallup_write(df)
  print(paste0("Writing data data for ", df$year[1], " complete."))

  
}

## This function writes munged gallup data to disk

gallup_write <- function(df){
  sinfo <- data.frame(Sys.info())
  machine <- sinfo$Sys.info..[4]
  
  machine_path <- 
    ifelse(
      machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
      "/Users/djolear/Google Drive/", 
      "G:/My Drive/"
    )
  
  data_path <- "research/projects/gallup/gallup_data/relative_status/"

  write_rds(df, paste0("D:/data/gallup/exports/", "df_", df$year[1], ".rds"))
}

gallup_munge <- function(df) {
  df <- 
    df %>% 
    mutate(
      WP46 = ifelse(year %in% c(2008:2009), WP46, NA),
      WP10200 = ifelse(year %in% c(2009:2017), WP10200, NA),
      WP10202 = ifelse(year %in% c(2009:2017), WP10202, NA),
      WP9081 = ifelse(year %in% c(2009), WP9081, NA),
      WP8859 = ifelse(year %in% c(2009), WP8859, NA),
      HWB19 = ifelse(year %in% c(2014:2017), HWB19, NA),
      HWB1 = ifelse(year %in% c(2014:2017), HWB1, NA),
      HWB20 = ifelse(year %in% c(2014:2017), HWB20, NA),
      HWB11 = ifelse(year %in% c(2014:2017), HWB11, NA),
      HWB2 = ifelse(year %in% c(2014:2017), HWB2, NA),
      HWB21 = ifelse(year %in% c(2014:2016), HWB21, NA),
      WP83 = ifelse(year %in% c(2014:2016), WP83, NA),
      HWB9 = ifelse(year %in% c(2014:2016), HWB9, NA),
      HWB22 = ifelse(year %in% c(2014:2016), HWB22, NA),
      HWB23 = ifelse(year %in% c(2014:2016), HWB23, NA),
      HWB18 = ifelse(year %in% c(2014:2016), HWB18, NA),
      HWB10 = ifelse(year %in% c(2014:2016), HWB10, NA),
      WP40 = ifelse(year %in% c(2014:2017), WP40, NA),
      HWB5 = ifelse(year %in% c(2014:2017), HWB5, NA),
      HWB6 = ifelse(year %in% c(2014:2017), HWB6, NA),
      M1 = ifelse(year %in% c(2014:2016), M1, NA),
      HWB17 = ifelse(year %in% c(2014:2017), HWB17, NA),
      HWB14 = ifelse(year %in% c(2014:2017), HWB14, NA),
      HWB15 = ifelse(year %in% c(2014:2017), HWB15, NA),
      HWB3 = ifelse(year %in% c(2014:2017), HWB3, NA),
      HWB4 = ifelse(year %in% c(2014:2017), HWB4, NA)
    )
  
  df <-
    df %>% 
    mutate(
      
      # Psychological
      comp_satis_std_liv = ifelse(year %in% c(2014:2017), HWB17, NA),
      std_living = WP30,
      econ = M30,
      sr_health = H36,
      enough_money = ifelse(year %in% c(2014:2017), HWB5, NA),
      goals = ifelse(year %in% c(2014:2017), HWB20, NA),
      little_pleasure = ifelse(year %in% c(2014:2017), H45, NA),
      active_prod = ifelse(year %in% c(2014:2017), HWB7, NA),
      drugs_relax = ifelse(year %in% c(2014:2016), H46, NA),
      ladder_now = ifelse(year %in% c(2014:2016), WP16, ifelse(year %in% c(2018), LAD1, NA)),
      ladder_5yrs = ifelse(year %in% c(2014:2016), WP18, ifelse(year %in% c(2018), LAD2, NA)),
      cl_diff = ladder_5yrs - ladder_now,
      economy_getting_better = 4 - WP148,
      enjoyment = WP67,
      worry = WP69,
      sadness = ifelse(year %in% c(2017, 2018), NA, WP70),
      stress = WP71,
      happiness = ifelse(year %in% c(2018), NA, WP6878),
      PURPOSE = ifelse(year %in% c(2014:2018), PURPOSE, NA),
      COMMUNITY = ifelse(year %in% c(2014:2018), COMMUNITY, NA),
      PHYSICAL = ifelse(year %in% c(2014:2018), PHYSICAL, NA),
      FINANCIAL = ifelse(year %in% c(2014:2018), FINANCIAL, NA),
      SOCIAL = ifelse(year %in% c(2014:2018), SOCIAL, NA),
      WELL_BEING_INDEX = ifelse(year %in% c(2014:2018), WELL_BEING_INDEX, NA),
      BAI = ifelse(year %in% c(2008:2013), BAI, NA),
      EHI = ifelse(year %in% c(2008:2013), EHI, NA),
      HBI = ifelse(year %in% c(2008:2013), HBI, NA),
      PHI = ifelse(year %in% c(2008:2013), PHI, NA),
      WEI = ifelse(year %in% c(2008:2013), WEI, NA),
      THRIVING = ifelse(year %in% c(2008:2013), THRIVING, NA),
      
      # Health behaviors  
      fruits_veggies = as.numeric(ifelse(H12B < 8, H12B, NA)),
      exercise = as.numeric(ifelse(H12A < 8, H12A, NA)),
      eat_healthy = as.numeric(M16 == 1, 1, ifelse(M16 == 2, 0, NA)),
      smoke = as.numeric(H11 == 1, 1, ifelse(H11 == 2, 0, NA)),
      num_alc = ifelse(year %in% c(2014:2016), ALCO1, NA),

      # Health
      bmi = as.numeric(BMI),
      obese = as.factor(OBESE),
      sr_health = H36,
      hbp = H4A,
      diabetes = H4C,
      depression = H4D,
      height = HEIGHT,
      
      # Demographics
      income = ifelse(INCOME_SUMMARY < 11, INCOME_SUMMARY, NA),
      raw_income = 
        case_when(
          income == 1 ~ 360,
          income == 2 ~ 3360,
          income == 3 ~ 9000,
          income == 4 ~ 18000,
          income == 5 ~ 30000,
          income == 6 ~ 42000,
          income == 7 ~ 54000,
          income == 8 ~ 75000,
          income == 9 ~ 105000,
          income == 10 ~ 120000,
        ),
      education = ifelse(EDUCATION < 7, EDUCATION, NA),
      education_fac = as.factor(education),
      married = 
        as.factor(
          ifelse(
            WP1223 == 6 | WP1223 == 7,
            NA,
            WP1223
          )
        ),
      age = ifelse(WP1220 < 100, WP1220, NA),
      race = as.factor(RACE),
      sex = SC7,
      children = H17,
      adults = D9,
      political_party = as.factor(P1),
      party_lean = PARTY,
      employment10_fac = as.factor(ifelse(year >= 2010, as.factor(EMPLOYMENT2010), NA)),
      employment_all = 
        as.factor(
          ifelse(
            year %in% c(2008:2009) & !is.na(WP46) & WP46 == 1,
            1,
            ifelse(
              year %in% c(2008:2009) & !is.na(WP46) & WP46 == 2,
              0,
              ifelse(
                year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 1,
                1,
                ifelse(
                  year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 2 & WP10202 == 1,
                  1, 
                  ifelse(
                    year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 2 & WP10202 == 2,
                    0,
                    ifelse(
                      year == 2009 & !is.na(WP9081) & WP9081 == 1, 
                      1,
                      ifelse(
                        year == 2009 & !is.na(WP9081) & WP9081 == 2, 
                        0,
                        ifelse(
                          year == 2009 & !is.na(WP8859) & WP8859 == 1, 
                          1,
                          ifelse(
                            year == 2009 & !is.na(WP8859) & WP8859 == 2, 
                            0,
                            NA
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
      
      # Geography
      zipcode = as.factor(ZIPCODE),
      census_region = as.factor(ZIPCENSUSREGION),
      msa = as.factor(MSACODE),
      fips_code = as.character(ifelse(year %in% c(2008:2012), fips_code, as.character(FIPS_CODE))),
      COMB_WEIGHT = ifelse(year %in% c(2018), WB_WEIGHT, ifelse(year %in% c(2008:2017), COMB_WEIGHT, NA))
      
    )
  
  return(df)
}
