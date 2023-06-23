# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP23 Targets for USAID workplans
# REF ID:   162fcdc4 
# LICENSE:  MIT
# DATE:     2023-06-21
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(grabr)
  library(tictoc)
  library(vroom)
  library(googledrive)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  baseurl <- "https://final.datim.org/"
  
  #target FY
  fy <- 2024
   
  #generate a temp folder for saving temp outputs
  temp_folder()
  
# IDENTIFY COP INDICATORS -------------------------------------------------
  
  #grab UIDS for COP **indicators**
  df_ind_uids <- glue("{baseurl}api/indicators?paging=false") %>% 
    datim_execute_query(datim_user(), datim_pwd(), flatten = TRUE) %>% 
    purrr::pluck("indicators") %>% 
    tibble::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(indicators = display_name) %>% 
    filter(str_detect(indicators, glue("COP{fy-1-2000}")))
  
  #uid list for non-HTS indicators
  v_ind_uids <- df_ind_uids %>% 
    filter(str_detect(indicators, "HTS_TST", negate = TRUE)) %>%
    pull(id) %>% 
    str_flatten(";")
  
  #uid list for HTS indicators (separate pull since status needed to be used)
  v_hts_uids <- df_ind_uids %>% 
    filter(str_detect(indicators, "HTS_TST")) %>% 
    pull(id) %>% 
    str_flatten(";")

# IDENTIFY HTS MODALITIES -------------------------------------------------

  # v_modality_uids <- datim_dim_items("HTS Modality (USE ONLY for FY23 Results/FY24 Targets)") %>% 
  #   pull(id) %>% 
  #   str_flatten(";")

  #uids for HIV status excluding known pos and unknow (VMMC)
  v_status_uids <- datim_dim_items("HIV Test Status (Specific)") %>% 
    filter(str_detect(item, "(Known|Unknown)", negate = TRUE)) %>% 
    pull(id) %>% 
    str_flatten(";")
  

# FUNCTION - EXTRACTION API -----------------------------------------------

  extract_targets <- function(cntry_name, cntry_uid, cntry_iso, 
                              org_lvl, folderpath_export){
    
    print(glue("Running DATIM API for...{cntry_name}...{Sys.time()}"))
    
    #API url
    url_core <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=pe:", fy-1, "Oct&", #period
             "dimension=ou:LEVEL-", org_lvl, ";", cntry_uid, "&", #level and ou
             "filter=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency - USAID
             "dimension=SH885jaRe0o&", #Funding Mechanism
             # "dimension=BOyWrF33hiR&", #Implementing Partner
             "displayProperty=SHORTNAME&skipMeta=false")
    
    #add data elements for non HTS indicators to url
    url_nonhts <- paste0(url_core, "&",
                         "dimension=dx:", v_ind_uids)
    
    #start clocking run time
    tic("API run time") 
    
    #extract non-HTS targets
    df_nonhts <- datim_process_query(url_nonhts)
    # toc()
    
    #add data elements for HTS indicators to url
    url_hts <- paste0(url_core, "&",
                      "dimension=dx:", v_hts_uids, "&",
                      # "dimension=fmxSIyzexmb&", #HTS Modality (USE ONLY for FY23 Results/FY24 Targets)
                      "dimension=bDWsPYyXgWP:", v_status_uids) #HIV Test Status (Specific)
    
    #extract non-HTS targets
    df_hts <- datim_process_query(url_hts)
    
    #end run time
    toc() 
    
    #bind datasets
    df_targets <- bind_rows(df_nonhts, df_hts)
    
    #export
    if(!missing(folderpath_export))
      write_csv(df_targets, glue("{folderpath_export}/COP{fy-1-2000}-targets_{cntry_iso}.csv"),
                na = "")
    
    invisible(df_targets)
    
  }
  
  
# RUN API -----------------------------------------------------------------

  #country table for UIDs and names
  df_cntry_info <- get_outable() %>% 
    arrange(country) %>% 
    select(starts_with("country"))
  
  #run API to extract DATIM tables and store locally
  df_cntry_info %>% 
    pwalk(~extract_targets(..1, ..2, ..3, ..4, "Data"))
    

# IMPORT DATA -------------------------------------------------------------

  #read in all local files
  df_targets <- list.files("Data", "targets", full.names = TRUE) %>% 
    map_dfr(read_csv)
      
# MUNGE -------------------------------------------------------------------

  #clean up period and column naming
  df_targets <- df_targets %>% 
    convert_datim_pd_to_qtr() %>% 
    select(country = `Organisation unit`,
           mech = `Funding Mechanism`,
           # prime_partner_name = `Implementing Partner`,
           period = Period,
           ind = Data,
           hivstatus = `HIV Test Status (Specific)`,
           targets = Value
           )
  
  #separate out mech info
  df_targets <- df_targets %>% 
    separate_wider_delim(mech, names = c(NA, "mech_code", "mech_name"), 
                         delim = " - ",
                         too_many = "merge") %>% 
    mutate(mech_name = str_trim(mech_name))
  
  #clean up indicator to exclude extraneous details including age
  df_targets <- df_targets %>%
      mutate(ind = ind %>%
               str_trim() %>% 
               str_replace("3T", "3 T") %>% 
               str_remove(" \\(N\\)") %>%
               str_replace(" \\(D\\)", "_D")) %>%
      separate_wider_delim(ind,
                           names = c(NA, NA, "indicator", "age_coarse"),
                           delim = " ",
                           too_few = "align_start")
  
  #clean up age to reflect MSD (<15 not 15-)
   df_targets <- df_targets %>% 
      mutate(age_coarse = str_replace(age_coarse, "(15|18)-", "<\\1"))
  
   #aggregate data to have only one line per indicator
   df_targets <- df_targets %>% 
     group_by(country, mech_code, mech_name, period, indicator) %>%
     summarise(targets = sum(targets),
               .groups = "drop") %>%
     arrange(country, mech_code, indicator)
 
 
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
            name = glue("COP{fy-1-2000}-Targets_{country}_{mech_code}.csv"))
   
   print(glue("Printing...{meta$country}-{meta$mech_code}"))
   
   #export file to be then later be uploaded to Gdrive
   write_csv(df_mech, file.path(output_folder, meta$name), na = "")
 } 
 
 # CREATE TARGET FILES -----------------------------------------------------
 
   #merge on OU since files in Gdrive directory are stored under OU & match DRC in Drive
   df_targets <- df_targets %>% 
     left_join(pepfar_country_list %>% 
                 select(operatingunit, country)) %>% 
     mutate(country = case_match(country, 
                                 "Democratic Republic of the Congo" ~ "DRC",
                                 .default = country),
            operatingunit = case_match(operatingunit, 
                                 "Democratic Republic of the Congo" ~ "DRC",
                                 .default = operatingunit)) %>% 
     relocate(operatingunit, .before = 1) %>% 
     mutate(country = ifelse(operatingunit == country, operatingunit, glue("{operatingunit}-{country}")))
 
   #list of mechanisms to iterate over for export
   mechs <- df_targets %>%
     distinct(mech_code, country) %>%
     arrange(country, mech_code)
   
   #export IM target files
   mechs %>%
     pwalk(~print_targets(df_targets, 
                          ..1, ..2,
                          folderpath_tmp))

# UPLOAD FILES ------------------------------------------------------------

   #import Gdrive id mapping table & align to target table names
   df_xwalk <- read_csv("Data/COP_workplan_upload_crosswalk.csv") %>% 
     mutate(ou_name = ou_name %>% 
              str_remove_all(" ") %>% 
              str_remove("'") %>% 
              str_remove("al$"))
 
 
   #identify exported target files and assoicate with GDrive folder  
   local_files <- list.files(folderpath_tmp, full.names = TRUE) %>% 
     tibble(filepath = .) %>% 
     mutate(basename = basename(filepath),
            ou_name = basename %>% 
              str_extract("(?<=_).*(?=_)") %>% 
              str_remove("-.*")) %>% 
     left_join(df_xwalk) %>%
     select(filepath, id, basename)
 
   #push to target tables to Gdrive
     local_files %>% 
       pwalk(~ drive_upload(..1,
                            path = as_id(..2),
                            name = basename(.x),
                            type = "spreadsheet"))

# CHECKS ------------------------------------------------------------------

 # df_targets %>% 
 #   count(indicator, wt = targets) %>% 
 #   arrange(indicator)
 # 
 # 
 # library(tameDP)
 # 
 # df_dp <- tame_dp("../../../Downloads/PSNUxIM_Malawi_ver12May2023.xlsx",
 # # df_dp <- tame_dp("../../../Downloads/Final_PSNUxIM_Botswana_20230512_170337_MD512.xlsx",
 #                  type = "PSNUxIM")
 # 
 # df_dp %>% 
 #   clean_indicator() %>% 
 #   filter((str_detect(standardizeddisaggregate, "(KeyPop|Age/Sex/KnownNewResult)", negate = TRUE) | indicator == "KP_PREV"),
 #          mech_code %in% unique(df_targets$mech_code),
 #          !(indicator == "PrEP_CT" & standardizeddisaggregate == "Age/Sex/HIVStatus")) %>% 
 #   count(indicator, wt = targets,
 #         name = "targets_tdp")
 # 
 # df_dp %>% 
 #   clean_indicator() %>% 
 #   filter((str_detect(standardizeddisaggregate, "(KeyPop|Age/Sex/KnownNewResult)", negate = TRUE) | indicator == "KP_PREV"),
 #          mech_code %in% unique(df_targets$mech_code),
 #          !(indicator == "PrEP_CT" & standardizeddisaggregate == "Age/Sex/HIVStatus")) %>% 
 #   count(mech_code, indicator, wt = targets,
 #         name = "targets_tdp") %>% 
 #   full_join(df_targets, .) %>% 
 #   mutate(match = targets == targets_tdp) %>% 
 #   View()
   
 
 