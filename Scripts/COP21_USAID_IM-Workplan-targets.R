# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP21 Target for USAID workplans
# LICENSE:  MIT
# DATE:     2021-06-11
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(fs)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  #global material folder
  glbl_id <- "1LSqjgZlhl28yaJpsBtZFSR2ME3eqB3Rr"
  
  #upload folder
  fldr_id <- "1JbZnP2k5OdIf_R6VCQ5fzTfOFFzvSjyU"
  
# FUNCTION ----------------------------------------------------------------

  print_targets <- function(mech){
    df_mech <- df_totals %>% 
      filter(mech_code == mech)
    
    meta <- df_mech %>% 
      distinct(country, mech_code) %>%
      mutate(country = str_remove_all(country, " |'"),
             name = glue("COP21IMTargets/COP21_{country}_{mech_code}_Targets.csv"))
    
    print(glue("Printing...{meta$country}-{meta$mech_code}"))
    
    write_csv(df_mech, file.path(meta$name), na = "")
  }

# IMPORT ------------------------------------------------------------------
  
  #compiled data packs by ER team
  #https://drive.google.com/drive/u/0/folders/1CD3Asd5Uror4YEGUpcXzv-ah0_UV3dkU
  df_dp <- read_csv("../../../Downloads/Datapack_Master_06_09.csv")

# MUNGE -------------------------------------------------------------------

  #agregate totals
  df_totals <- df_dp %>% 
    filter(!(indicator %in% c("HTS_RECENT", "HTS_TST", "HTS_TST_POS", "PrEP_NEW", "PrEP_CURR",
                              "TX_CURR", "TX_NEW", "TX_PVLS") & disagg == "KeyPop"),
           fundingagency == "USAID") %>%
    clean_indicator() %>% 
    mutate(country = ifelse(operatingunit == countryname, operatingunit, glue("{operatingunit}-{countryname}"))) %>% 
    group_by(country, mech_code, mech_name, primepartner, fiscal_year, indicator) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(country, mech_code, indicator)

# CREATE TARGET FILES -----------------------------------------------------

  #list of mechanism
  mechs <- df_totals %>% 
    distinct(mech_code) %>% 
    pull()
  
  #create folder for storing IM target files  
  dir_create("COP21IMTargets")
  
  #create IM target files
  walk(mechs, print_targets)
  

# MOVE TO DRIVE -----------------------------------------------------------

  #create folder for upload
  drive_mkdir("Test",
              path = as_id(glbl_id))
  
  #identify list of   
  local_files <- list.files("COP21IMTargets", full.names = TRUE)
  
  #push to drive
  walk(local_files,
       ~ drive_upload(.x,
                      path = as_id(fldr_id),
                      name = basename(.x),
                      type = "spreadsheet"))
  
  #remove all local files
  unlink("COP21IMTargets", recursive = TRUE)
  
  