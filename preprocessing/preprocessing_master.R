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

source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/preprocessing_functions.R"))

#####################
## Preprocess Data ##
#####################

# list all data files
file_list <- 
  data.frame(
    file_list = list.files(path = paste0(data_path))
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".SAV")
  ) 

# run data preprocessing
for(i in 1:length(file_list$file_list)) {
  
  # read each file and apply functions
  assign(
    
    # create name for file
    paste0("dfg_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    
    # run function
    preprocess_master(
      paste0(data_path, file_list$file_list[i]), 
      as.numeric(str_extract(file_list$file_list[i], "[[:digit:]]+"))
    )
    
  )
  
  # print status
  print(paste0("dfg_", str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}
