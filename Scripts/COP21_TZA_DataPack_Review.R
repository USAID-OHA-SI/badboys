# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Target summary
# LICENSE:  MIT
# DATE:     2021-04-30
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
  

# GLOBAL VARIABLES --------------------------------------------------------

dp_path <- "Data/Data Pack_Tanzania_Apr232021-2100-GA.xlsx"  

# IMPORT ------------------------------------------------------------------
  
  df_dp <- tame_dp(dp_path)

  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()
  
  

# MUNGE -------------------------------------------------------------------

  df_dp_sel <- df_dp %>% 
    filter(mech_code != "00000")

  df_dp_sel <- df_dp_sel %>% 
    filter(indicator == "KP_PREV" | disagg != "KeyPop") %>% 
    count(fundingagency, indicator, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022)
  
  
  lst_trgts <- df_dp_sel %>% 
    distinct(indicator, numeratordenom)
  
  df_tza <- df %>% 
    filter(operatingunit == "Tanzania",
           fundingagency != "Dedup",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    semi_join(lst_trgts, by = c("indicator", "numeratordenom")) %>% 
    group_by(fiscal_year, fundingagency, indicator, numeratordenom) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")
  
  
  df_full <- df_tza %>% 
    bind_rows(df_dp_sel)
  
  df_full <- df_full %>% 
    clean_agency() %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  df_full <- df_full %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_ex <- df_full %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement))
  
  write_csv(df_ex, "Dataout/COP21_TZA_Target_Comp.csv", na = "")
  

# HTS ---------------------------------------------------------------------

  df_dp_mods <- df_dp %>% 
    filter(mech_code != "00000",
           indicator == "HTS_TST_POS",
           disagg != "KeyPop") %>% 
    count(fundingagency, indicator, modality, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022)

  
  df_tza_mods <- df %>% 
    filter(operatingunit == "Tanzania",
           fundingagency != "Dedup",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
    group_by(fiscal_year, fundingagency, indicator,numeratordenom, modality) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")

  df_full_mods <- df_tza_mods %>% 
    bind_rows(df_dp_mods)
  
  
  df_full_mods <- df_full_mods %>% 
    clean_agency() %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  df_full_mods <- df_full_mods %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_ex_mods <- df_full_mods %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement))
  
  write_csv(df_ex_mods, "Dataout/COP21_TZA_Target_Comp_HTS_POS.csv", na = "")
  