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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "162fcdc4" #id for adorning to plots, making it easier to find on GH

  load_secrets()
  baseurl <- "https://final.datim.org/"
  
  #target FY
  fy <- 2024
   
  

# IDENTIFY COP INDICATORS -------------------------------------------------
  
  df_ind_uids <- glue("{baseurl}api/indicators?paging=false") %>% 
    datim_execute_query(datim_user(), datim_pwd(), flatten = TRUE) %>% 
    purrr::pluck("indicators") %>% 
    tibble::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(indicators = display_name) %>% 
    filter(str_detect(indicators, glue("COP{fy-1-2000}")))
  
  v_ind_uids <- df_ind_uids %>% 
    filter(str_detect(indicators, "HTS_TST", negate = TRUE)) %>%
    pull(id) %>% 
    str_flatten(";")
  
  v_hts_uids <- df_ind_uids %>% 
    filter(str_detect(indicators, "(HTS_TST|HTS_INDEX|PMTCT_STAT|TB_STAT|VMMC_CIRC) ")) %>% 
    pull(id) %>% 
    str_flatten(";")

# IDENTIFY HTS MODALITIES -------------------------------------------------

  v_modality_uids <- datim_dim_items("HTS Modality (USE ONLY for FY23 Results/FY24 Targets)") %>% 
    pull(id) %>% 
    str_flatten(";")

  datim_dimensions() %>% 
    arrange(dimension) %>% 
    prinf()
  
  v_status_uids <- datim_dim_items("HIV Test Status (Specific)") %>% 
    filter(str_detect(item, "(Known|Unknown)", negate = TRUE)) %>% 
    pull(id) %>% 
    str_flatten(";")
  
# IDENTIFY COUNTRY UIDS ---------------------------------------------------

  df_cntry_info <- get_outable() %>% 
      arrange(country) %>% 
      select(country, country_uid, country_lvl)
     
# IMPORT ------------------------------------------------------------------
  
  cntry_name <- "Malawi"
  cntry_uid <- "lZsCb6y0KDX"
  org_lvl <- "3"
  
  print(paste("Running DATIM API for", cntry_name,  Sys.time(),
              sep = " ... "))
  
  url_core <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:", fy-1, "Oct&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", cntry_uid, "&", #level and ou
           "filter=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency - USAID
           "dimension=SH885jaRe0o&", #Funding Mechanism
           # "dimension=BOyWrF33hiR&", #Implementing Partner
           "displayProperty=SHORTNAME&skipMeta=false")
           
  url_nonhts <- paste0(url_core, "&",
                        "dimension=dx:", v_ind_uids)
  tictoc::tic()
  df_nonhts <- datim_process_query(url_nonhts)
  tictoc::toc()
  
  url_hts <- paste0(url_core, "&",
                    "dimension=dx:", v_hts_uids, "&",
                    # "dimension=fmxSIyzexmb&", #HTS Modality (USE ONLY for FY23 Results/FY24 Targets)
                    "dimension=bDWsPYyXgWP:", v_status_uids) #HIV Test Status (Specific)
  tictoc::tic()
  df_hts <- datim_process_query(url_hts)
  tictoc::toc()
  
  df_hts <- df_hts %>% 
    mutate(Data = str_replace(Data, "(?<=Targets ).*(?= \\()", "HTS_TST"))
    
  
  df_targets <- bind_rows(df_nonhts, df_hts)
  
# MUNGE -------------------------------------------------------------------

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
  
  df_targets <- df_targets %>% 
    separate_wider_delim(mech, names = c(NA, "mech_code", "mech_name"), 
                         delim = " - ",
                         too_many = "merge") %>% 
    mutate(mech_name = str_trim(mech_name))
  
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
  
  
  df_targets <- df_targets %>% 
    bind_rows(df_targets %>% 
                filter(str_detect(hivstatus, "Positive")) %>% 
                mutate(indicator = "HTS_TST_POS")) %>% 
    select(-hivstatus)
  
  
 df_targets <- df_targets %>% 
    mutate(age_coarse = str_replace(age_coarse, "(15|18)-", "<\\1"))
  
 df_targets <- df_targets %>% 
   group_by(country, mech_code, mech_name, period, indicator) %>%
   summarise(targets = sum(targets),
             .groups = "drop") %>%
   arrange(country, mech_code, indicator)
 
 
 # CREATE TARGET FILES -----------------------------------------------------
 
 
 write_csv(df_targets,
           "Data/test.csv",
           na = "")
 
 # #list of mechanism
 # mechs <- df_targets %>%
 #   distinct(mech_code, country) %>%
 #   arrange(country, mech_code) %>% 
 #   # mutate(country = ifelse(operatingunit == country, operatingunit, glue("{operatingunit}-{country}"))) %>%
 #   select(mech_code, country)
 # 
 # #create folder for storing IM target files
 # temp_folder()
 # 
 # #create IM target files
 # mechs %>%
 #   pwalk(~print_targets(..1, ..2, folderpath_tmp))


# CHECK -------------------------------------------------------------------


 
 #USAID/ou level testing check
 
 df_targets %>% 
   count(indicator, wt = targets) %>% 
   arrange(indicator)
 
 
 
 
 library(tameDP)

 df_dp <- tame_dp("../../../Downloads/PSNUxIM_Malawi_ver12May2023.xlsx",
 # df_dp <- tame_dp("../../../Downloads/Final_PSNUxIM_Botswana_20230512_170337_MD512.xlsx",
                  type = "PSNUxIM")
 
 df_dp %>% 
   clean_indicator() %>% 
   filter((str_detect(standardizeddisaggregate, "(KeyPop|Age/Sex/KnownNewResult)", negate = TRUE) | indicator == "KP_PREV"),
          mech_code %in% unique(df_targets$mech_code),
          !(indicator == "PrEP_CT" & standardizeddisaggregate == "Age/Sex/HIVStatus")) %>% 
   count(indicator, wt = targets,
         name = "targets_tdp")
 
 df_dp %>% 
   clean_indicator() %>% 
   filter((str_detect(standardizeddisaggregate, "(KeyPop|Age/Sex/KnownNewResult)", negate = TRUE) | indicator == "KP_PREV"),
          mech_code %in% unique(df_targets$mech_code),
          !(indicator == "PrEP_CT" & standardizeddisaggregate == "Age/Sex/HIVStatus")) %>% 
   count(mech_code, indicator, wt = targets,
         name = "targets_tdp") %>% 
   full_join(df_targets, .) %>% 
   mutate(match = targets == targets_tdp) %>% 
   View()
   
 
 df_dp %>% 
   filter(indicator == "PrEP_CT") %>% 
   count(standardizeddisaggregate, wt = targets)
 
 df_msd <- si_path() %>% 
   return_latest("OU_IM") %>% 
   read_psd()   

 df_msd %>% 
   filter(indicator == "PrEP_CT",
          fiscal_year == 2023) %>% 
   count(standardizeddisaggregate, wt = cumulative)
 
 
 df_dp %>% 
   filter(str_detect(standardizeddisaggregate, "(KeyPopAge/Sex/KnownNewResult)", negate = TRUE)) %>% 
   count(indicator, standardizeddisaggregate, wt = targets) %>% 
   arrange(indicator, standardizeddisaggregate) %>% 
   group_by(indicator) %>% 
   mutate(rows = n())
 