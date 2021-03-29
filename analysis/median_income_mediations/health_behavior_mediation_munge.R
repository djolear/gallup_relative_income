data_path <- "D:/data/gallup/results/mediation"

########################
## FRUITS AND VEGGIES ##
########################

### Main WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "fv_mediation_main_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

fv_med_main <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


### Alt. WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "fv_mediation_alt_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

fv_med_alt <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


#################
## EAT HEALTHY ##
#################

### Main WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "eh_mediation_main_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

eh_med_main <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


### Alt. WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "eh_mediation_alt_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

eh_med_alt <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


#############
## SMOKING ##
#############

### Main WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "smoking_mediation_main_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

smoking_med_main <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


### Alt. WB

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "smoking_mediation_alt_wb")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

smoking_med_alt <-
  map_dfr(.x = file_list$file_list, .f = read_csv)
