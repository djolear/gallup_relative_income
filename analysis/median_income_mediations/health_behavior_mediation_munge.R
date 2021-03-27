data_path <- "D:/data/gallup/results/mediation"

########################
## FRUITS AND VEGGIES ##
########################

### Terms

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

fv_med <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


###########
## SMOKE ##
###########

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "smoke_mediation")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

smoke_med <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


#################
## EAT HEALTHY ##
#################

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "eh_mediation")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

eh_med <-
  map_dfr(.x = file_list$file_list, .f = read_csv)

