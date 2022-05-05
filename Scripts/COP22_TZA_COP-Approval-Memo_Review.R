# PROJECT:  bad boys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Final review of TZA DP to COP Approval Memo
# LICENSE:  MIT
# DATE:     2022-05-05
# UPDATED: 
# NOTE:     updated from COP21_TZA_COP-Approval-Memo_Review

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(vroom)
  library(tameDP)
  library(clipr)
  

# GLOBAL VARIABLES --------------------------------------------------------

  peds_ages <- c("<01","01-04", "05-09", "10-14", 
               "<15", "02 - 12 Months", "<=02 Months")
  
  tbl_ind <- c("TX_NEW", "TX_CURR", "TX_PVLS", "HTS_SELF", "HTS_TST", 
               "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX", "PMTCT_STAT",
               "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", "TB_STAT", 
               "TB_ART", "TB_PREV", "TX_TB", "TX_TB_D", "VMMC_CIRC", "KP_PREV",
               "KP_MAT", "PrEP_NEW", "PrEP_CT", "CXCA_SCRN", "PP_PREV",
               "OVC_SERV", "OVC_HIVSTAT", "GEND_GBV", "AGYW_PREV")
  
  tbl_age_order <- c("<15", "15+", "<18", "18+", "Total")
  
  ptnr_tbl1_ind <- c("TX_NEW", "TX_CURR", "TX_PVLS", "HTS_SELF")
  ptnr_tbl2_ind <- c("HTS_TST", "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX" )
  ptnr_tbl3_ind <- c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID")
  ptnr_tbl4_ind <- c("TB_STAT", "TB_ART", "TB_PREV", "TX_TB_D")
  ptnr_tbl5_ind <- c("VMMC_CIRC", "KP_PREV", "KP_MAT", "PrEP_NEW", "PrEP_CT")
  ptnr_tbl6_ind <- c("CXCA_SCRN", "PP_PREV", "OVC_SERV", "OVC_HIVSTAT", "GEND_GBV")
    
# IMPORT ------------------------------------------------------------------
  
  df_dp <- tame_dp("Data/Tanzania_datapack_May 3rd 1415hrs_V2.xlsx",
                   "PSNUxIM", map_names = TRUE)
  
  # write_csv(df_dp, "Dataout/COP22_TZA_DataPack_tidy.csv", na = "")
  
  # df_dp <- vroom("Dataout/COP22_TZA_DataPack_tidy.csv",
  #                col_types = c(fiscal_year = "i",
  #                              targets = "d",
  #                              cumulative = "d",
  #                              .default = "c"))
  # df_msd <- si_path() %>% 
  #   return_latest("OU_IM") %>% 
  #   read_msd()   


# MUNGE -------------------------------------------------------------------

df_totals <- df_dp %>% 
    mutate(indicator = str_replace(indicator, "PREP", "PrEP")) %>% 
    filter(!(indicator %in% c("HTS_RECENT", "HTS_SELF", "HTS_TST", "HTS_TST_POS", "PrEP_NEW", "PrEP_CT") & str_detect(standardizeddisaggregate, "KeyPop")),
           indicator != "OVC_HIVSTAT") %>% 
    select(-c(ageasentered, sex, modality, statushiv, otherdisaggregate, cumulative)) %>% 
    mutate(standardizeddisaggregate = ifelse(numeratordenom == "N", "Total Numerator", "Total Denominator"),
           fiscal_year = as.character(fiscal_year)) %>% 
    group_by(across(where(is.character))) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fiscal_year = as.integer(fiscal_year))
    

df_dp <- df_dp %>% 
  bind_rows(df_totals)

df_dp <- df_dp %>%
  # filter(mech_code != "00000") %>%
  clean_indicator()

df_dp <- df_dp %>% 
  mutate(age_group = case_when(str_detect(standardizeddisaggregate, "^Total") ~ "Total",
                               is.na(ageasentered) ~ NA_character_,
                               # str_detect(standardizeddisaggregate, "KeyPop") ~ NA_character_,
                               indicator == "OVC_SERV" & ageasentered %in% c(peds_ages, "15-17") ~ "<18",
                               ageasentered == "18+" ~ "18+",
                               ageasentered %in% peds_ages ~ "<15",
                               !ageasentered %in% peds_ages ~ "15+")) %>% 
  filter(!is.na(age_group))


df_dp_ip <- df_dp %>% 
  filter(indicator %in% c("HTS_RECENT", "PMTCT_EID", ptnr_tbl5_ind, 
                          "CXCA_SCRN", "OVC_HIVSTAT", "GEND_GBV"),
         age_group == "Total") %>% 
  bind_rows(df_dp %>% 
              filter(!indicator %in% c("HTS_RECENT", "PMTCT_EID", ptnr_tbl5_ind, 
                                       "CXCA_SCRN", "OVC_HIVSTAT", "GEND_GBV"),
                     age_group != "Total")) %>% 
  count(fundingagency, primepartner, mech_code, indicator, age_group, wt = targets) 

# TABLE COMPARISON --------------------------------------------------------


#FY 2023 Target Summary
  df_dp %>% 
    filter(indicator %in% tbl_ind) %>% 
    mutate(indicator = factor(indicator, tbl_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    count(indicator, age_group, snuprioritization, wt = targets) %>%
    group_by(indicator,age_group) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = snuprioritization,
                values_from = n) %>% 
    relocate(total, .after = everything()) %>% 
    rename_all(~str_remove(., "[:digit:]+ - ")) %>% 
    arrange(indicator, age_group) %>% 
    write_clip()

  #COP22 Implementing Agency Target Table
  df_dp %>% 
    filter(indicator %in% tbl_ind) %>% 
    mutate(indicator = factor(indicator, tbl_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    count(indicator, age_group, fundingagency, wt = targets) %>%
    group_by(indicator,age_group) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = fundingagency,
                values_from = n) %>% 
    relocate(total, .after = everything()) %>% 
    arrange(indicator, age_group) %>% 
    write_clip()

  #COP22 Implementing Partner & IM Target Tables
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl1_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl1_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl2_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl2_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl3_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl3_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl4_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl4_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl5_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl5_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  
  df_dp_ip %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ptnr_tbl6_ind) %>% 
    unite(ind_age, c(indicator, age_group), remove = FALSE) %>%
    mutate(indicator = factor(indicator, ptnr_tbl6_ind),
           age_group = factor(age_group, tbl_age_order)) %>% 
    arrange(indicator, age_group, fundingagency, primepartner) %>%
    select(-c(indicator, age_group)) %>% 
    pivot_wider(names_from = ind_age,
                values_from = n) %>% 
    arrange(primepartner) %>%
    write_clip()
  