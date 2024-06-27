# PROJECT:  badboys
# PURPOSE:  Provide countries with FY25 Target tables for workplans
# AUTHOR:   A.Chafetz | USAID
# REF ID:   a04ab439 
# LICENSE:  MIT
# DATE:     2024-06-27
# UPDATED: 


# GENIE METADATA ----------------------------------------------------------

# OU By IM
# 
# DATIM data as of: 6/14/2024, 20:20:11 UTC
# Genie report updated: 6/24/2024, 05:20:34 UTC
# Current period(s): 2023 Q1, 2023 Q2, 2023 Q3, 2023 Q4, 2023 Target, 2024 Q1, 2024 Q2, 2024 Target, 2025 Target
# 
# Operating Unit: Cameroon, Haiti
# Daily/Frozen: Daily
# Funding Agency: USAID
# Standardized Disaggregate: Total Numerator, Total Denominator
# Fiscal Year: 2025


# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(googledrive)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "a04ab439"  #a reference to be places in viz captions 
  
  path_genie <- return_latest("Data", "Genie-OUByIMs")
  
  meta <- get_metadata(path_genie)  #extract MSD metadata
  
  #target FY
  fy <- 2025
  
  #cop year
  cop_yr <- "COP23y2"
  
  #output folder
  folderpath_out <- "Dataout"
  
  #gdrive workplan folder
  drive_folder <- as_id("1Simrf2tgulJ1SpcMjt1YDosmgZtWmqji")
  
# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(path_genie)
  

# MUNGE -------------------------------------------------------------------

  df_msd <- df_msd %>% 
    filter(funding_agency == "USAID",
           fiscal_year == fy) %>% 
    pluck_totals() %>% 
    clean_indicator()
  
  df_msd <- df_msd %>% 
    mutate(period = str_replace(fiscal_year, "20", "FY"))
  
  df_targets <- df_msd %>% 
    count(operatingunit, country, mech_code, mech_name, period, indicator, 
          wt = targets, name = "targets")

# FUNCTION - PRINT TARGET FILES -------------------------------------------
  
  print_targets <- function(df, mech, cntry, output_folder){
    #filter for mechanism
    df_mech <- df %>% 
      filter(mech_code == mech,
             country == cntry)
    
    #pull out info for file naming
    meta <- df_mech %>% 
      distinct(country, mech_code) %>%
      mutate(country = str_remove_all(country, " |'"),
             name = glue("{cop_yr}-Targets_{country}_{mech_code}.csv"))
    
    print(glue("Printing...{meta$country}-{meta$mech_code}"))
    
    #export file to be then later be uploaded to Gdrive
    write_csv(df_mech, file.path(output_folder, meta$name), na = "")
  } 
  
  # CREATE TARGET FILES -----------------------------------------------------
  
  #list of mechanisms to iterate over for export
  mechs <- df_targets %>%
    distinct(mech_code, country) %>%
    arrange(country, mech_code)
  
  #export IM target files
  mechs %>%
    pwalk(~print_targets(df_targets, 
                         ..1, ..2,
                         folderpath_out)) #folderpath_tmp
  
  

# MAP GDRIVE --------------------------------------------------------------

  #OU cop folder id
  master_folder <- drive_ls(path = as_id("1Simrf2tgulJ1SpcMjt1YDosmgZtWmqji")) %>% 
    filter(name %in% unique(mechs$country)) %>% 
    rename(ou_name = name,
           ou_id = id,
           drive_resource_ou = drive_resource) 

  list_folders <- function(folder_id){
    
    #OU cop folder id
    cop_folder_id <- drive_ls(folder_id, pattern = cop_yr) %>% 
      pull(id)
    
    #look within sub folders to get IM target table folders; add parent id back to df
    sub_folders <- drive_ls(cop_folder_id) %>% 
      filter(name %in% c("IM Target Tables")) 
    
    # add check here if folders is null, create folder
    if(nrow(sub_folders) == 0) {
      drive_mkdir("IM Target Tables",
                  path = as_id(cop_folder_id))
      
      sub_folders <- drive_ls(cop_folder_id) %>% 
        filter(name %in% c("IM Target Tables"))
    } else {
      print("IM Target Tables exists")
    }
    
    sub_folders <- sub_folders %>% 
      filter(name %in% c("IM Target Tables")) %>% 
      mutate(parent_folder_id = folder_id)
    
    return(sub_folders)
  }
 
  
  #apply function
  df_sub <- map(master_folder$ou_id, list_folders) %>% 
    list_rbind()
  
  #left join back the OU folder info
  df_drive_folder <- df_sub %>% 
    left_join(master_folder, by = c("parent_folder_id" = "ou_id"))%>% 
    select(ou_name, id)
  
  
  #identify exported target files and assoicate with GDrive folder
  local_files <- list.files(folderpath_out, 
                            pattern = cop_yr,
                            full.names = TRUE) %>%
    tibble(filepath = .) %>%
    mutate(basename = basename(filepath),
           ou_name = basename %>%
             str_extract("(?<=_).*(?=_)") %>%
             str_remove("-.*")) %>%
    left_join(df_drive_folder) %>%
    select(filepath, id, basename)
  

# UPLOAD TO GDRIVE --------------------------------------------------------

  #upload
  local_files %>% 
    pwalk(~ drive_upload(..1,
                         path = as_id(..2),
                         name = basename(.x),
                         type = "spreadsheet",
                         overwrite = TRUE))

  #remove local files
  unlink(local_files$filepath)
  