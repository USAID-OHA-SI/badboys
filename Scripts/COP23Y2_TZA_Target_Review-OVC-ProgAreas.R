# PROJECT:  badboys
# PURPOSE:  
# AUTHOR:   A.Chafetz | USAID
# REF ID:   51a80221 
# LICENSE:  MIT
# DATE:     2024-04-11
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(tameDP)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(gt)
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gtExtras)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "51a80221"  #a reference to be places in viz captions 
  
  path_tst <-  "Data/PSNUxIM_Tanzania_031524_final submission.xlsx"

# IMPORT ------------------------------------------------------------------
  
  df_tst <- tame_dp(path_tst, "PSNUxIM", map_names = TRUE)
  

# MUNGE -------------------------------------------------------------------

  df_ovc <- df_tst %>% 
    filter(indicator == "OVC_SERV") %>%
    clean_agency() %>% 
    mutate(mech = glue("{mech_code} [{funding_agency}]: {prime_partner_name}")) %>% 
    mutate(prog_area = ifelse(is.na(otherdisaggregate), standardizeddisaggregate, otherdisaggregate) %>% 
             str_remove("Age/Sex/")) %>% 
    count(mech, prog_area, fiscal_year, wt = targets) %>% 
    pivot_wider(names_from = prog_area,
                values_from = n)

# VIZ ---------------------------------------------------------------------
  
  source <- glue("Source: {basename(path_tst)} [downloaded 2024-04-11] | Ref id: {ref_id}")
  
  df_ovc %>% 
    select(-fiscal_year) %>% 
    gt() %>% 
    sub_missing(-c(mech), missing_text = "-") %>% 
    fmt_number(-c(mech),
               decimals = 0) %>% 
    cols_align("left", mech) %>% 
    tab_header(title = "COP23 Year2 Targets (FY25)",
               subtitle = "OVC_SERV Targets by Mech and Program Area") %>% 
    tab_footnote(source) %>% 
    gt_theme_nytimes()
