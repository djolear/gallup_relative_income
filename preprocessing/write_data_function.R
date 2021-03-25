write_data <- function(df){
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