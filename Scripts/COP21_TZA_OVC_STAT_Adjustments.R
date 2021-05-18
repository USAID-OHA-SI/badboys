# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Adjustments for OVC_SERVER PACT targets
# LICENSE:  MIT
# DATE:     2021-05-17
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
library(tameDP)
library(janitor)
library(readxl)


# GLOBALS -----------------------------------------------------------------

  path <- "C:/Users/achafetz/Downloads/PEPFAR Tanzania - DP -05162021-clean-1330 (1)_ahc.xlsx"

  mech_pact <- "17358"
  
  d_trgt <- 
  d_trgt_dreams <- 

# IMPORT ------------------------------------------------------------------

  #PSNUxIM tab
    df_psnuxim <- tame_dp(path, map_names = TRUE)

  #OVC tab
    df_ovc <- read_excel(path, "OVC", skip = 13) %>% 
      clean_names()

# MUNGE -------------------------------------------------------------------

  #add row number
    df_ovc <- df_ovc %>% 
      mutate(row = row_number() + 14)
  
  #separate psnu uid
    df_ovc <- df_ovc %>% 
      mutate(psnu = str_remove(psnu, " \\[#DREAMS]")) %>% 
      separate(psnu, c("psnu", NA, "psnuuid"), sep = " \\[") %>% 
      mutate(psnu = str_remove(psnu, "]"))
    
  #filter psnuxim to ovc
    df_psnuxim_ovc <- df_psnuxim %>% 
      mutate(row = row_number() + 14) %>% 
      filter(indicator == "OVC_SERV")
    
  #pact target
    trgt_pact <- df_psnuxim_ovc %>% 
      filter(mech_code == mech_pact) %>% 
      count(wt = targets) %>% 
      pull()
  
  #target gap
    to_distribute <- d_trgt - trgt_pact
    
  #psnus to focus on
  df_new_pact <- df_psnuxim_ovc %>% 
    filter(mech_code == mech_pact) %>%
    # group_by(psnu, psnuuid, mech_code, age, sex, otherdisaggregate) %>% 
    # summarise(across(targets, sum, na.rm = TRUE)) %>% 
    # ungroup() %>% 
    mutate(overall_share = targets/sum(targets),
           new_targets = round(to_distribute * overall_share))
  
  df_psnuxim_ovc_adj <- df_psnuxim_ovc %>% 
    filter(mech_code != mech_pact) %>% 
    bind_rows(df_new_pact) %>% 
    arrange(row)

  df_psnuxim_ovc_adj <- df_psnuxim_ovc_adj %>% 
    bind_rows(df_psnuxim_ovc_adj %>%
                mutate(otherdisaggregate = "total")) %>% 
    filter(mech_code != "00000") %>% 
    group_by(psnuuid, age, sex, otherdisaggregate) %>%
    summarise(across(c(targets, new_targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(new_targets = targets + new_targets) %>% 
    select(-targets) %>% 
    pivot_wider(names_from = otherdisaggregate,
                names_prefix = "new_ovc_",
                values_from = new_targets) %>% 
    rename_all(tolower) %>% 
    select(psnuuid, age, sex, new_ovc_total, new_ovc_dreams, new_ovc_prev, new_ovc_grad, new_ovc_act)
    
  
  df_ovc_adj <- df_ovc %>% 
    tidylog::left_join(df_psnuxim_ovc_adj)
  
  
  df_ovc_adj <-  df_ovc_adj %>% 
    mutate(new_growth = (new_ovc_total /ovc_serv_t_1)-1,
           new_dreams = new_ovc_dreams / new_ovc_total,
           new_prev = new_ovc_prev/new_ovc_total,
           new_grad = new_ovc_grad/(new_ovc_total*ovc_serv_comp_rt_t)) 
  
  df_ovc_adj %>% 
    arrange(row) %>% 
    select(row,id, new_ovc_total:new_ovc_act, new_growth:new_grad) %>%
    write_csv("Dataout/COP21_TZA_OVCSTAT_adj.csv", na = "")
  
  # df_ovc_adj %>% 
  #   summarise(across(starts_with("new_ovc"), sum, na.rm = TRUE))
  
  
  
  
  
  
  
  
# DREAMS ALLOCATION -------------------------------------------------------

  path <- "C:/Users/achafetz/Downloads/PEPFAR Tanzania - DP -05162021-clean-1330 (1)_ahc_out.xlsx"
  
  #PSNUxIM tab
  df_psnuxim <- tame_dp(path, map_names = TRUE)
  
  #OVC tab
  df_ovc <- read_excel(path, "OVC", skip = 13) %>% 
    clean_names()
  
  #add row number
  df_ovc <- df_ovc %>% 
    mutate(row = row_number() + 14)
  
  #separate psnu uid
    df_ovc <- df_ovc %>% 
      mutate(psnu = str_remove(psnu, " \\[#DREAMS]")) %>% 
      separate(psnu, c("psnu", NA, "psnuuid"), sep = " \\[") %>% 
      mutate(psnuuid = str_remove(psnuuid, "\\]"))
      
  
  #filter psnuxim to ovc
  df_psnuxim_ovc <- df_psnuxim %>% 
    mutate(row = row_number() + 14) %>% 
    filter(indicator == "OVC_SERV")
  
  ovc_dreams <- df_ovc %>% 
    filter(dreams_snu_flag == "Y") %>% 
    distinct(psnuuid) %>% 
    pull()
  
  psnus_pact <- df_psnuxim_ovc %>%
    filter(mech_code == mech_pact) %>% 
    distinct(psnuuid) %>% 
    pull()

  
  df_new_pact <- df_psnuxim_ovc %>% 
    filter(mech_code == mech_pact,
           psnuuid %in% ovc_dreams,
           age == "10-14",
           sex == "Female") %>% 
    mutate(overall_share = targets/sum(targets),
           new_targets = round(d_trgt_dreams * overall_share))
  
  df_new_pact_dreams <- df_new_pact %>% 
    mutate(otherdisaggregate = "DREAMS",
           fiscal_year = as.character(fiscal_year)) %>% 
    select(-row, -overall_share) %>% 
    group_by(across(where(is.character))) %>% 
    summarise(new_targets = sum(new_targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fiscal_year = as.double(fiscal_year))
    
  df_new_pact <- df_new_pact %>% 
    mutate(new_targets = targets - new_targets)
    
  df_psnuxim_ovc_adj <- df_psnuxim_ovc %>% 
    filter(mech_code != c("00000", mech_pact),
           psnuuid %in% ovc_dreams,
           age == "10-14",
           sex == "Female") %>% 
    mutate(new_targets = targets) %>% 
    bind_rows(df_new_pact, df_new_pact_dreams) %>% 
    arrange(row)
  
  
  df_psnuxim_ovc_adj <- df_psnuxim_ovc_adj %>% 
    bind_rows(df_psnuxim_ovc_adj %>%
                mutate(otherdisaggregate = "total")) %>% 
    filter(mech_code != "00000") %>% 
    group_by(psnuuid, age, sex, otherdisaggregate) %>%
    summarise(across(new_targets, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = otherdisaggregate,
                names_prefix = "new_ovc_",
                values_from = new_targets) %>% 
    rename_all(tolower) %>% 
    select(psnuuid, age, sex, new_ovc_total, new_ovc_dreams, new_ovc_grad, new_ovc_act)
  
  
  df_ovc_adj <- df_ovc %>% 
    tidylog::left_join(df_psnuxim_ovc_adj)
  
  df_ovc_adj <-  df_ovc_adj %>% 
    mutate(new_dreams = ifelse(is.na(new_ovc_total), ovc_serv_dreams_rt_t, new_ovc_dreams / new_ovc_total),
           new_dreams = ifelse(!is.na(new_ovc_total) & new_dreams > 1, 1, new_dreams),
           new_grad = ifelse(is.na(new_ovc_total), ovc_serv_grad_rt_t,  new_ovc_grad/(new_ovc_total*ovc_serv_comp_rt_t)),
           new_grad = ifelse(!is.na(new_ovc_total) & new_grad < 0, 0, new_grad))
  
  df_ovc_adj %>% 
    arrange(row) %>% 
    select(row,id, new_ovc_total:new_ovc_act, new_dreams:new_grad) %>%
    write_csv("Dataout/COP21_TZA_OVCSTAT_adj_dreams.csv", na = "")
  
  
  df_ovc_adj %>%
    summarise(across(starts_with("new_ovc"), sum, na.rm = TRUE))
  
  
  
      
  
  df_psnuxim_ovc %>% 
    filter(mech_code != "00000",
           age == "10-14") %>% 
    count(primepartner, otherdisaggregate, wt= targets) %>% 
    spread(otherdisaggregate, n)
  
  df_psnuxim_adj <- df_psnuxim_ovc %>%
    mutate(dreams = psnuuid %in% ovc_dreams) %>% 
    filter(mech_code != "00000",
           psnuuid %in% psnus_pact,
           age == "10-14",
           dreams == TRUE) %>% 
    count(psnu, psnuuid, mech_code, age, sex, otherdisaggregate, wt = targets, name = "targets") %>% 
    spread(otherdisaggregate, targets)
    mutate(overall_share = targets/sum(targets, na.rm = TRUE),
           dreams_target = round(d_trgt_dreams * overall_share),
           adj_target = targets - dreams_target)
    
  df_psnuxim_adj_dreams <- df_psnuxim_adj %>% 
    mutate(otherdisaggregate = "DREAMS") %>%
    group_by(psnu, psnuuid, mech_code, age, sex, otherdisaggregate) %>% 
    summarise(across(c(targets, dreams_target), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(targets_new = dreams_target)
  
  df_psnuxim_adj_other <- df_psnuxim_adj %>%
    group_by(psnu, psnuuid, mech_code, age, sex, otherdisaggregate) %>% 
    summarise(across(c(targets, adj_target), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(targets_new = adj_target)
  
  df_psnuxim_adj <- bind_rows(df_psnuxim_adj_dreams, df_psnuxim_adj_other)
  
  df_psnuxim_full <- df_psnuxim_ovc %>%
    mutate(dreams = psnuuid %in% ovc_dreams) %>% 
    filter(!mech_code %in% c("00000", mech_pact),
           psnuuid %in% psnus_pact,
           age == "10-14",
           dreams == TRUE) %>% 
    count(psnu, psnuuid, mech_code, age, sex, otherdisaggregate, wt = targets, name = "targets") %>% 
    bind_rows(df_psnuxim_adj)
    

  df_psnuxim_full %>% 
    group_by(mech_code, otherdisaggregate) %>% 
    summarise(across(c(targets, targets_new), sum, na.rm = TRUE))

  df_psnuxim_ovc %>% 
    filter(psnuuid %in% psnus_pact) %>% 
    distinct(mech_code)
  

  
  
  
  
  
  
  df_ovc %>% 
    filter(mech_code == mech_pact) %>% 
    count(mech_code, age, otherdisaggregate, wt = targets) 
    
  
  df_psnuxim_ovc %>% 
    filter(mech_code != "00000") %>%
    group_by(mech_code, psnu) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    spread(mech_code, targets)
  
  
  
  
  
  df_ovc %>% 
  filter(mech_code != "00000") %>%
  count(otherdisaggregate, wt = targets)

df_ovc %>% 
  filter(mech_code != "00000") %>% 
  group_by(fundingagency, primepartner, age, otherdisaggregate) %>% 
  summarise(across(targets, sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(age, targets) %>% 
  adorn_totals(c("row", "col"))
  prinf()
  
  
  df_ovc %>% 
    filter(mech_code != "00000") %>% 
    group_by(fundingagency, primepartner) %>% 
    summarise(across(targets, sum, na.rm = TRUE)) %>% 
    ungroup()
  
  df_ovc %>% 
    filter(mech_code != "00000",
           otherdisaggregate == "DREAMS") %>% 
    group_by(fundingagency, primepartner, age, otherdisaggregate) %>% 
    summarise(across(targets, sum, na.rm = TRUE)) %>% 
    ungroup()
  

# OVC_ADJ -----------------------------------------------------------------



