# PROJECT:  bad boys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Final review of TZA DP to COP Approval Memo
# LICENSE:  MIT
# DATE:     2022-05-05
# UPDATED: 
# NOTE:     updated from COP21_TZA_COP-Approval-Memo_Review

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(systemfonts)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(vroom)
  library(tameDP)
  library(openxlsx)
  
# GLOBAL VARIABLES --------------------------------------------------------
  
  #setup folders in this repo if they don't exist already
  folder_setup()

  folderpath_output <- "Dataout"
    
  peds_ages <- c("<01","01-04", "05-09", "01-09", "10-14", 
               "<15", "02 - 12 Months", "<=02 Months")
  
  tbl_ind <- c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", "HTS_SELF", "HTS_TST", 
               "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX", "PMTCT_STAT",
               "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", "TB_STAT", 
               "TB_ART", "TB_PREV", "TX_TB", "TX_TB_D", "VMMC_CIRC", "KP_PREV",
               "KP_MAT", "PrEP_NEW", "PrEP_CT", "CXCA_SCRN", "PP_PREV",
               "OVC_SERV", "OVC_HIVSTAT", "GEND_GBV", "AGYW_PREV")
  
  tbl_age_order <- c("<15", "15+", "<18", "18+", "Total")

    
# IMPORT ------------------------------------------------------------------
  
  df_dp <- tame_dp("Data/PSNUxIM_Haiti_20240327_173928_updated.xlsx", 
                   type = "PSNUxIM")
  
  df_dp <- rename_official(df_dp, datim_user(), datim_pwd())
  
# MUNGE -------------------------------------------------------------------

  #create total numerators/denoms for each indicator
  df_totals <- df_dp %>% 
      mutate(indicator = str_replace(indicator, "PREP", "PrEP")) %>% 
      filter(!(indicator %in% c("HTS_RECENT", "HTS_SELF", "HTS_TST", "HTS_TST_POS", "PrEP_NEW", "PrEP_CT", "TX_CURR", "TX_NEW", "TX_PVLS") & str_detect(standardizeddisaggregate, "KeyPop")) & 
             standardizeddisaggregate != c("Total Denominator", "Total Numerator"),
             !(indicator == "PrEP_CT" & standardizeddisaggregate == "Age/Sex/HIVStatus")) %>% 
      select(-c(ageasentered, sex, modality, statushiv, otherdisaggregate, cumulative)) %>% 
      mutate(standardizeddisaggregate = ifelse(numeratordenom == "N", "Total Numerator", "Total Denominator"),
             fiscal_year = as.character(fiscal_year)) %>% 
      group_by(across(where(is.character))) %>% 
      summarise(across(where(is.double), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>% 
      mutate(fiscal_year = as.integer(fiscal_year))
      
  df_dp_full <- df_dp %>% 
    bind_rows(df_totals)

  #add _D to denominator indicators
  df_dp_full <- clean_indicator(df_dp_full)

  #create age groups to match COP Memo tables
  df_dp_full <- df_dp_full %>% 
    mutate(age_group = case_when(str_detect(standardizeddisaggregate, "^Total") ~ "Total",
                                 is.na(ageasentered) ~ NA_character_,
                                 # str_detect(standardizeddisaggregate, "KeyPop") ~ NA_character_,
                                 indicator == "OVC_SERV" & ageasentered %in% c(peds_ages, "15-17") ~ "<18",
                                 ageasentered %in% c("18+", "18-20") ~ "18+",
                                 ageasentered %in% peds_ages ~ "<15",
                                 !ageasentered %in% peds_ages ~ "15+")) %>% 
    filter(!is.na(age_group))


  
# MEMO VALUE CHECKS -------------------------------------------------------
  
  
  df_dp_full %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_PVLS_D", "TX_PVLS"),
           fiscal_year == 2025) %>% 
    pluck_totals() %>% 
    count(indicator, wt = targets)
  
# TABLE COMPARISON --------------------------------------------------------

#create a workbook to export to
  wb <- createWorkbook()

#FY 2023 Target Summary
  df_summary <- df_dp_full %>% 
    filter(indicator %in% tbl_ind) %>% 
    mutate(indicator = factor(indicator, tbl_ind),
           age_group = factor(age_group, tbl_age_order),
           snuprioritization = ifelse(psnu == "_Military Haiti", "mil", "Scale Up")) %>% 
    count(indicator, age_group, snuprioritization, wt = targets) %>%
    group_by(indicator,age_group) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = snuprioritization,
                values_from = n) %>% 
    relocate(total, .after = everything()) %>% 
    rename_all(~str_remove(., "[:digit:]+ - ")) %>% 
    arrange(indicator, age_group) 

  addWorksheet(wb, "summary")
  writeData(wb, "summary", df_summary)
  
  #COP22 Implementing Agency Target Table
  df_agency <- df_dp_full %>% 
    filter(indicator %in% tbl_ind) %>% 
    mutate(indicator = factor(indicator, tbl_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    count(indicator, age_group, funding_agency, wt = targets) %>%
    group_by(indicator,age_group) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = funding_agency,
                values_from = n) %>% 
    relocate(total, .after = everything()) %>% 
    arrange(indicator, age_group)
  
  addWorksheet(wb, "agency")
  writeData(wb, "agency", df_agency)

  saveWorkbook(wb, file.path(folderpath_output, "COP23Y2_HTI_Table-Check.xlsx"), overwrite = TRUE)
    
    
    

  