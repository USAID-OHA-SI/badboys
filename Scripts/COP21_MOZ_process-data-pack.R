# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  tidy MOZ data pack
# LICENSE:  MIT
# DATE:     2021-05-18
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tameDP)
  library(readr)
  library(stringr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  fldr <- "Data"
  
  file <- "Mozambique_20210507_050902.xlsx"

# FILE PATHS --------------------------------------------------------------
  
  path_in <- file.path(fldr, file)
  
  path_out <- file.path(fldr,
                        paste0("COP21_tamedp_",
                               str_replace(file, "xlsx", "csv")))
  
  path_zip <- file.path(str_replace(path_out, "csv", "zip"))
  
  
# TIDY DATA PACK ----------------------------------------------------------
  
  
  df_dp <- tame_dp(path_in, map_names = FALSE)
  
  df_dp <- get_names(df_dp)
  
  write_csv(df_dp, path_out, na = "")
  
  zip(path_zip, path_out)
  
  unlink(path_out)
  