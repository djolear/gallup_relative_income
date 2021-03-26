years <-
  dfg_rs %>% 
  count(year)

for(i in 1:length(years$year)){
  df <-
    dfg_rs %>% 
    filter(year == years$year[i])
  
  write_csv(df, paste0("D:/data/gallup/exports/for_regression_analyses/dfg_rs_reg_", years$year[i], ".csv"))
}
