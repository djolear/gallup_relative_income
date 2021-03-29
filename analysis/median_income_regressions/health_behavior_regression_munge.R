data_path <- "D:/data/gallup/results/regression"

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
    str_detect(file_list, ".csv") & str_detect(file_list, "fv_mi")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

fv_terms <-
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
    str_detect(file_list, ".csv") & str_detect(file_list, "smoking")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

smoke_terms <-
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
    str_detect(file_list, ".csv") & str_detect(file_list, "eh_mi")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

eh_terms <-
  map_dfr(.x = file_list$file_list, .f = read_csv)



#############
## PURPOSE ##
#############

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "purpose")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

pur_terms <-
  map_dfr(.x = file_list$file_list, .f = read_csv)



###############
## FINANCIAL ##
###############

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "financial")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

fin_terms <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


###############
## COMMUNITY ##
###############

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "community")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

com_terms <-
  map_dfr(.x = file_list$file_list, .f = read_csv)


############
## SOCIAL ##
############

### Terms

file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )


file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv") & str_detect(file_list, "social")
  ) %>% 
  mutate(
    file_list = paste0(data_path, "/", file_list)
  )

soc_terms <-
  map_dfr(.x = file_list$file_list, .f = read_csv)

