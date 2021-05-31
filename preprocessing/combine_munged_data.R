

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

data_path <- "research/projects/gallup/gallup_data/relative_status/"

####################
## Load Functions ##
####################

source(paste0(machine_path, "research/projects/gallup_rs/preprocessing/standardize_variables_function.R"))


###############
## Load Data ##
###############

file_list <- 
  data.frame(
    file_list = list.files(path = "D:/data/gallup/exports/")
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".rds")
  )  %>% 
  dplyr::slice(1:10)

for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("dfg_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    read_rds(paste0("D:/data/gallup/exports/", file_list$file_list[i]))
  )
  print(paste0("load dfg_", str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}


##################
## Combine Data ##
##################

dfg_rs <-
  bind_rows(
    dfg_2008,
    dfg_2009,
    dfg_2010,
    dfg_2011,
    dfg_2012,
    dfg_2013,
    dfg_2014,
    dfg_2015,
    dfg_2016,
    dfg_2017
  )


###################
## Extra Munging ##
###################

dfg_rs <-
  dfg_rs %>% 
  mutate_at(
    vars(
      year,
      employment10_fac,
      employment_all
    ),
    as.factor
  ) %>% 
  mutate(
    enjoyment = ifelse(enjoyment == 1, -1, ifelse(enjoyment == 2, 1, NA)),
    happiness = ifelse(happiness == 1,- 1, ifelse(happiness == 2, 1, NA)),
    stress = ifelse(stress == 1, 1, ifelse(stress == 2, -1, NA)),
    worry = ifelse(worry == 1, 1, ifelse(worry == 2, -1, NA)),
    sadness = ifelse(sadness == 1, 1, ifelse(sadness == 2, -1, NA)),
    diabetes = ifelse(diabetes == 1, 1, ifelse(diabetes == 2, 0, NA)),
    hbp = ifelse(hbp == 1, 1, ifelse(hbp == 2, 0, NA)),
    depression = ifelse(depression == 1, 1, ifelse(depression == 2, 0, NA)),
    obese = ifelse(obese == 1, 0, ifelse(obese == 0, 1, NA))
  ) %>% 
  mutate(
    neg_aff = enjoyment + happiness + stress + worry + sadness
  )

dfg_rs <- standardize_variables(dfg_rs)


write_rds(dfg_rs, paste0("D:/data/gallup/exports/", "dfg_rs.rds"))
write_csv(dfg_rs, paste0("D:/data/gallup/exports/", "dfg_rs.csv"))
