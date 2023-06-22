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
  
  v_ind_uids <- glue("{baseurl}api/indicators?paging=false") %>% 
    datim_execute_query(datim_user(), datim_pwd(), flatten = TRUE) %>% 
    purrr::pluck("indicators") %>% 
    tibble::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(indicators = display_name) %>% 
    filter(str_detect(indicators, "COP23")) %>% 
    pull(id) %>% 
    str_flatten(";")
  

# IDENTIFY HTS MODALITIES -------------------------------------------------

  v_hts_uids <- datim_dim_items("HTS Modality (USE ONLY for FY23 Results/FY24 Targets)") %>% 
    pull(id) %>% 
    str_flatten(";")

# IDENTIFY COUNTRY UIDS ---------------------------------------------------

  df_cntry_info <- get_outable() %>% 
      arrange(country) %>% 
      select(country, country_uid, country_lvl)
     
# IMPORT ------------------------------------------------------------------
  
 
  ou_uid <- "l1KFEXKI4Dg"
  org_lvl <- "3"
  
  
  print(paste("Running DATIM API for", ou_name,  Sys.time(),
              sep = " ... "))
  
  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=pe:", fy-1, "Oct&", #period
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets / Results - targets
           "dimension=dx:", v_ind_uids, "&",
           "displayProperty=SHORTNAME&skipMeta=false")
  
  
  df <- datim_process_query(core_url)
  #TODO remove HTS
  
  hts_url <- paste0(baseurl,"api/29/analytics?",
         "dimension=pe:", fy-1, "Oct&", #period
         "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
         "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets / Results - targets
         "dimension=fmxSIyzexmb%3AJyHpOg0Iu5a%3BUKsxJneeEKv%3BT5c3iO27ZXo%3BKBywuUyKKkt%3BffRRNlcJcDm%3BJhqbE6DaPHa%3Bu4O4VoYcpsY%3BPwr47IqyUOv%3BUO19EkMqGSv%3BU82Nff6n18Z%3BjdknuTGPngF%3BMvNJB6wzMyI%3BsRbK6bY4Ovz%3BvrZ765PIJXR%3Biqox5fMeLYE%3BZvodeG6VDz4%3BmnAHV80OjSs%3BeVDKHRK7jCS%3BTR2heMhJZDD%3BXpawfSjdgiS%3BcVAqTMhzlvE%3BJF0YmkyBHz7%3BDbdhNI4vars&dimension=dx%3APBSj3jctjNH%3BMEwHLgltuUu%3BNFJBeaZnRi7%3BwRWdn96180W%3Bv5bfqQwbt3a%3Bgn3MmK1K6kI%3Bg7njYDJYNJq%3BiXE6hq0GOVQ%3BbPobyyvv6b8%3BLDeOYoqrutf%3BJ9aO4FuubsD%3BnewAhTXirTN%3BjZHSDieHo9w%3Bbrp6rRBQXey%3BmHcr32IGNoB%3BUFw1hrglrGm%3BSgqljkXlVGl%3BOuuakKg2UMY%3BYrv1NvxziHD%3BBaPIehJK2l7%3BX4afMjXUaG6%3BAd8fKCBHcby%3BcOd9wGwiiyL%3BMG5NlhPq6EK%3BoLm2BRnu9mb%3BX5Nc1Ae2rY2%3BwdiPHReljP1%3Bro6dYIiog4T%3BZlRRL8wjvfy%3BPb2MfODeGi2%3BWZiqIeyR6lQ%3Bts9khEv4uMY%3BVKKuPE3h8GI%3BrQW3olZ8hyl%3BzxsvwaENBI8%3BJ4uNnN08ohQ%3ByUotfy43BWY%3Bca7QmUTl13A%3BLBQINrmB9gs%3BcbXDrqRcRzv%3BQbVoRzXCGyV%3BMMHmOW51JNY%3BHaXrHFdlRy7%3BAUeOrHw0JAG%3BJRzebVm6tvS&",
         "dimension=ipBFu42t2sJ&",
         "displayProperty=SHORTNAME&skipMeta=false")
  
  df_hts <- datim_process_query(hts_url)
  #TODO remove *_POS
  #TODO create HTS_TST indicator
  #TODO create
  
  
  # datim_dimensions() %>% 
  #   arrange(dimension) %>% 
  #   prinf()
  # 
  # datim_dim_items("HIV Test Status (Inclusive)")
  
# MUNGE -------------------------------------------------------------------
  

