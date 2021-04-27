# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  PMTCT indicators
# LICENSE:  MIT
# DATE:     2021-04-27
# UPDATED: 


# GENIE INFO --------------------------------------------------------------

# Site By IM Extract
# DATIM data as of: 04/26/2021 00:22:27 UTC
# Genie report updated: 04/27/2021 01:50:10 UTC
# 
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2 
# Daily/Frozen: Daily
# Indicator: PMTCT_STAT, PMTCT_STAT_POS, PMTCT_ART, PMTCT_EID, PMTCT_HEI_POS, PMTCT_FO
# Fiscal Year: 2022, 2021,2020

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(ICPIutilities)
library(tameDP)


# IMPORT ------------------------------------------------------------------

  df_genie <- list.files("Data", "Genie.*(PMTCT|HTS)",full.names = TRUE) %>% 
    map_dfr(read_msd)

  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  dp_path <- "Data/Tanzania_20210423_122727-1530-GA.xlsx"
  df_dp <- tame_dp(dp_path, map_names = FALSE)


# MUNGE -------------------------------------------------------------------

  df_genie <- df_genie %>% 
    filter(fiscal_year == 2021,
           indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS", "HTS_TST"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator", "Modality/Age/Sex/Result")) %>% 
    mutate(indicator = case_when(numeratordenom == "D" ~ glue("{indicator}_D"),
                                 indicator == "HTS_TST" ~ glue("{indicator}_{str_remove_all(modality, ' ')}"),
                                 TRUE ~ indicator)) %>%
    group_by(fiscal_year, indicator, orgunituid) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_longer(c(cumulative, targets), names_to = "type") %>% 
    filter(value != 0) %>% 
    group_by(fiscal_year, indicator, type) %>% 
    summarise(across(c(value), sum, na.rm = TRUE),
              site_count = n(),
              .groups = "drop") %>% 
    mutate(type = glue("FY{str_sub(fiscal_year, -2)} {str_to_sentence(type)}"),
           site_count = ifelse(type == "FY21 Targets", NA_integer_, site_count)) %>% 
    select(-fiscal_year) %>% 
    arrange(type, indicator) %>% 
    mutate(source = "DATIM GENIE 2021-04-27")
    
  
  df_dp <- df_dp %>% 
    filter(indicator %in% c("PMTCT_STAT", "PMTCT_ART", "PMTCT_EID") |
           (indicator == "HTS_TST" & modality == "Post ANC 1"),
           mech_code != "00000"
           ) %>% 
    mutate(indicator = case_when(numeratordenom == "D" ~ glue("{indicator}_D"),
                                 indicator == "HTS_TST" ~ glue("{indicator}_{str_remove_all(modality, ' ')}"),
                                 TRUE ~ indicator)) %>% 
    group_by(indicator) %>% 
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>% 
    mutate(type = "FY22/COP21 Targets",
           source = basename(dp_path))

  df_combo <- bind_rows(df_genie, df_dp)  
  
  
  df_combo %>% 
    select(-c(site_count, source)) %>% 
    pivot_wider(names_from = type)
  
  df_combo %>% 
    select(-c(value, source)) %>% 
    filter(!is.na(site_count)) %>% 
    pivot_wider(names_from = type,
                values_from = "site_count")
  

    
  