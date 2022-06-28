# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP21 Target for USAID workplans
# LICENSE:  MIT
# DATE:     2021-06-11
# UPDATED:  2022-06-28

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(fs)
  library(googledrive)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  #global material folder
  glbl_id <- "1aA71E4BET48-XF8qNG8u9KA1vS2s1GEw"
  
  #upload folder
  fldr_id <- "1JbZnP2k5OdIf_R6VCQ5fzTfOFFzvSjyU"
  
  #COP FY
  fy <- 2023
  cop_yr <- fy - 1 - 2000
  
# FUNCTION ----------------------------------------------------------------

  print_targets <- function(mech, cntry, output_folder){
    df_mech <- df_totals %>% 
      filter(mech_code == mech,
             country == cntry,
             targets > 0)
    
    meta <- df_mech %>% 
      distinct(country, mech_code) %>%
      mutate(country = str_remove_all(country, " |'"),
             name = glue("COP{cop_yr}_Targets_{country}_{mech_code}.csv"))
    
    print(glue("Printing...{meta$country}-{meta$mech_code}"))
    
    write_csv(df_mech, file.path(output_folder, meta$name), na = "")
  }

# IMPORT ------------------------------------------------------------------
  
  #MSD as of FY22Q2c included COP22/FY23 targets
  df_msd <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_msd()   
  

# MUNGE -------------------------------------------------------------------

  #available indicators with targets
  inds <- df_msd %>% 
    filter(fiscal_year == fy,
           funding_agency == "USAID") %>%
    pluck_totals() %>% 
    distinct(indicator) %>% 
    pull()
  
  #aggregate totals
  df_totals <- df_msd %>% 
    filter(funding_agency == "USAID",
           fiscal_year == fy,
           indicator %in% inds) %>%
    pluck_totals() %>% 
    clean_indicator() %>% 
    mutate(country = ifelse(operatingunit == country, operatingunit, glue("{operatingunit}-{country}"))) %>% 
    count(country, mech_code, mech_name, prime_partner_name, fiscal_year, indicator, wt = targets, name = "targets") %>% 
    arrange(country, mech_code, indicator) %>% 
    mutate(targets = na_if(targets, 0))

# CREATE TARGET FILES -----------------------------------------------------

  #list of mechanism
  mechs <- df_msd %>% 
    filter(fiscal_year == fy,
           funding_agency == "USAID") %>%
    pluck_totals() %>% 
    distinct(mech_code, operatingunit, country) %>%
    mutate(country = ifelse(operatingunit == country, operatingunit, glue("{operatingunit}-{country}"))) %>% 
    select(mech_code, country)
  
  #create folder for storing IM target files
  temp_folder()
  
  #create IM target files
  mechs %>%
    pwalk(~print_targets(..1, ..2, folderpath_tmp))
  

# MOVE TO DRIVE -----------------------------------------------------------

  #create folder for upload
  drive_mkdir(glue("COP{cop_yr} Mech Target Tables"),
              path = as_id(glbl_id))
  
  fldr_id <- drive_ls(path = as_id(glbl_id), glue("COP{cop_yr} Mech Target Tables"))$id
  
  #identify list of   
  local_files <- list.files(folderpath_tmp, full.names = TRUE)
  
  #push to drive
  walk(local_files,
       ~ drive_upload(.x,
                      path = as_id(fldr_id),
                      name = basename(.x),
                      type = "spreadsheet"))
  
  #share folder
  drive_share(as_id(fldr_id), role = "reader",
              type = "domain", domain = "usaid.gov")
