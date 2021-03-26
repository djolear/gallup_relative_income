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

source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/munge_data_function.R"))
source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/select_variables_function.R"))
source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/demographically_derived_median_income_function.R"))
source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/join_census_data_function.R"))
source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/join_chr_data_function.R"))
source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/write_data_function.R"))

preprocess_master <-function(data_path, data_year) {
  
  # Read data
  df <-
    read_sav(data_path) %>% 
    mutate(
      year = data_year
    )
  
  
  # Rename and munge variables
  df <- munge_data(df)
  print(paste0("Munge data for ", df$year[1], " complete."))
  
  # Select variables
  df <- select_variables(df)
  print(paste0("Select variables for ", df$year[1], " complete."))

  
  df <- calculate_median_income(df)
  print(paste0("Generate gallup median income for ", df$year[1], " complete."))

  
  df <- join_census_data(df, df$year[1])
  
  print(paste0("Attach general census data for ", df$year[1], " complete."))
  
  
  df <- join_chr_data(df, df$year[1])
  
  print(paste0("Attach CHR ranking data for ", df$year[1], " complete."))

  write_data(df)
  print(paste0("Writing data data for ", df$year[1], " complete."))
  
  return(df)

}




