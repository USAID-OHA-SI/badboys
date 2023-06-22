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

# IDENTIFY COUNTRY UIDS ---------------------------------------------------

  df_cntry_info <- get_outable() %>% 
      arrange(country) %>% 
      select(country, country_uid, country_lvl)
     
# IMPORT ------------------------------------------------------------------
  
  cntry_name <- "Botswana"
  cntry_uid <- "l1KFEXKI4Dg"
  org_lvl <- "3"
  
  print(paste("Running DATIM API for", cntry_name,  Sys.time(),
              sep = " ... "))
  
  url_core <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:", fy-1, "Oct&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", cntry_uid, "&", #level and ou
           "filter=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency - USAID
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=BOyWrF33hiR&", #Implementing Partner
           # "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets / Results - targets
           "displayProperty=SHORTNAME&skipMeta=false")
           
  url_nonhts <- paste0(url_core, "&",
                        "dimension=dx:", v_ind_uids)
  tictoc::tic()
  df_nonhts <- datim_process_query(url_nonhts)
  tictoc::toc()
  
  url_hts <- paste0(url_core, "&",
                    "dimension=dx:", v_hts_uids, "&",
                    # "dimension=fmxSIyzexmb&", #HTS Modality (USE ONLY for FY23 Results/FY24 Targets)
                    # "filter=fmxSIyzexmb:JyHpOg0Iu5a;UKsxJneeEKv;T5c3iO27ZXo;KBywuUyKKkt;ffRRNlcJcDm;JhqbE6DaPHa;u4O4VoYcpsY;Pwr47IqyUOv;UO19EkMqGSv;U82Nff6n18Z;jdknuTGPngF;MvNJB6wzMyI;sRbK6bY4Ovz;vrZ765PIJXR;iqox5fMeLYE;ZvodeG6VDz4;mnAHV80OjSs;eVDKHRK7jCS;TR2heMhJZDD;XpawfSjdgiS;cVAqTMhzlvE;JF0YmkyBHz7;DbdhNI4vars&dimension=dx%3APBSj3jctjNH;MEwHLgltuUu;NFJBeaZnRi7;wRWdn96180W;v5bfqQwbt3a;gn3MmK1K6kI;g7njYDJYNJq;iXE6hq0GOVQ;bPobyyvv6b8;LDeOYoqrutf;J9aO4FuubsD;newAhTXirTN;jZHSDieHo9w;brp6rRBQXey;mHcr32IGNoB;UFw1hrglrGm;SgqljkXlVGl;OuuakKg2UMY;Yrv1NvxziHD;BaPIehJK2l7;X4afMjXUaG6;Ad8fKCBHcby;cOd9wGwiiyL;MG5NlhPq6EK;oLm2BRnu9mb;X5Nc1Ae2rY2;wdiPHReljP1;ro6dYIiog4T;ZlRRL8wjvfy;Pb2MfODeGi2;WZiqIeyR6lQ;ts9khEv4uMY;VKKuPE3h8GI;rQW3olZ8hyl;zxsvwaENBI8;J4uNnN08ohQ;yUotfy43BWY;ca7QmUTl13A;LBQINrmB9gs;cbXDrqRcRzv;QbVoRzXCGyV;MMHmOW51JNY;HaXrHFdlRy7;AUeOrHw0JAG;JRzebVm6tvS&", #HTS Modality (USE ONLY for FY23 Results/FY24 Targets)
                    "dimension=ipBFu42t2sJ:l5mQOWpLybR;ntdVDkWUOj9;Qnfnp6VzPcP") #HIV Test Status (Inclusive)
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
           prime_partner_name = `Implementing Partner`,
           period = Period,
           ind = Data,
           hivstatus = `HIV Test Status (Inclusive)`,
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
    filter(is.na(hivstatus)) %>% 
    bind_rows(df_targets %>% 
                filter(hivstatus == "HIV Positive (Inclusive)") %>% 
                mutate(indicator = "HTS_TST_POS")) %>% 
    select(-hivstatus)
  
  
 df_targets <- df_targets %>% 
    mutate(age_coarse = str_replace(age_coarse, "(15|18)-", "<\\1"))
  
 df_targets <- df_targets %>% 
   group_by(country, mech_code, mech_name, period, indicator) %>% 
   summarise(targets = sum(targets),
             .groups = "drop")
 
 
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

  
  datim_dimensions() %>%
    arrange(dimension) %>%
    prinf()
  #
  datim_dim_items("Funding Partner") %>%
    pull(id) %>%
    str_flatten(";")
  