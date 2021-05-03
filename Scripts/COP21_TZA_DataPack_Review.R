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

dp_path <- "Data/Tanzania_20210503_104926- NEW (pending fixes)-1410-GA.xlsx"  

peds_ages <- c("<01","01-04", "05-09", "10-14", 
               "<15", "02 - 12 Months", "<=02 Months")

age_disaggs <- tribble(
     ~indicator, ~numeratordenom,                              ~standardizeddisaggregate,
    "CXCA_SCRN",             "N",       "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
     "GEND_GBV",             "N",                                      "Total Numerator",
    "HTS_INDEX_FAC",         "N",                                     "4:Age/Sex/Result",
    "HTS_INDEX_COM",         "N",                                     "4:Age/Sex/Result",
   "HTS_RECENT",             "N",                      "Modality/Age/Sex/RTRI/HIVStatus",
     "HTS_SELF",             "N",                                  "Age/Sex/HIVSelfTest",
      "HTS_TST",             "N",                   "Modality/Age Aggregated/Sex/Result",
      "HTS_TST",             "N",                              "Modality/Age/Sex/Result",
  "HTS_TST_POS",             "N",                   "Modality/Age Aggregated/Sex/Result",
  "HTS_TST_POS",             "N",                              "Modality/Age/Sex/Result",
      "KP_PREV",             "N",                                      "Total Numerator",
  "OVC_HIVSTAT",             "N",                                      "Total Numerator",
     "OVC_SERV",             "N",                                "Age/Sex/ProgramStatus",
    "PMTCT_ART",             "N",                     "Age/NewExistingArt/Sex/HIVStatus",
    "PMTCT_EID",             "N",                                                  "Age",
   "PMTCT_STAT",             "D",                                              "Age/Sex",
   "PMTCT_STAT",             "N",                               "Age/Sex/KnownNewResult",
      "PP_PREV",             "N",                                              "Age/Sex",
    "PrEP_CURR",             "N",                                              "Age/Sex",
     "PrEP_NEW",             "N",                                              "Age/Sex",
       "TB_ART",             "N",                     "Age/Sex/NewExistingArt/HIVStatus",
      "TB_PREV",             "D",                     "Age/Sex/NewExistingArt/HIVStatus",
      "TB_PREV",             "N",                     "Age/Sex/NewExistingArt/HIVStatus",
      "TB_STAT",             "D",                                              "Age/Sex",
      "TB_STAT",             "N",                               "Age/Sex/KnownNewPosNeg",
      "TX_CURR",             "N",                        "Age/Sex/ARVDispense/HIVStatus",
      "TX_CURR",             "N",                         "Age Aggregated/Sex/HIVStatus",
      "TX_CURR",             "N",                                    "Age/Sex/HIVStatus",
       "TX_NEW",             "N",                                    "Age/Sex/HIVStatus",
       "TX_NEW",             "N",                         "Age Aggregated/Sex/HIVStatus",
      "TX_PVLS",             "D",                         "Age/Sex/Indication/HIVStatus",
      "TX_PVLS",             "D",              "Age Aggregated/Sex/Indication/HIVStatus",
      "TX_PVLS",             "N",                         "Age/Sex/Indication/HIVStatus",
      "TX_PVLS",             "N",              "Age Aggregated/Sex/Indication/HIVStatus",
        "TX_TB",             "D", "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus"
  )


# FUNCTIONS ---------------------------------------------------------------

  group_ages <- function(df){
    mutate(df,
           age = case_when(is.na(age) ~ NA_character_,
                           indicator == "OVC_SERV" & age %in% c(peds_ages, "15-17") ~ "<18",
                           indicator == "OVC_SERV" ~ "18+",
                           age %in% peds_ages ~ "<15",
                           TRUE~ "15+"))
  }

  clean_partner <- function(df){
    mutate(df, primepartner = case_when(str_detect(primepartner, "Elizabeth") ~ "EGPAF",
                               str_detect(primepartner, "DELOITTE") ~ "Deloitte",
                               str_detect(primepartner, "BAYLOR|Baylor") ~ "Baylor",
                               str_detect(primepartner, "Family") ~ "Epic",
                               str_detect(primepartner, "Pact") ~ "Pact",
                               str_detect(primepartner, "TANZANIA HEALTH") ~ "Police and Prisons Activity"))
  }
  
  
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
  
  
  

# PARTNER -----------------------------------------------------------------

  df_dp_ptnr <- df_dp %>% 
    filter(fundingagency == "USAID",
           (indicator == "KP_PREV" | disagg != "KeyPop")) %>%
    group_ages() %>% 
    clean_partner() %>%
    count(primepartner, indicator, age, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022) %>% 
    arrange(primepartner, indicator, numeratordenom)
  
  
  df_msd_usaid <- df %>% 
    filter(operatingunit == "Tanzania",
           fundingagency == "USAID") %>% 
    mutate(trendscoarse = na_if(trendscoarse, "Unknown Age"),
           indicator = case_when(indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
                                 indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
                                 TRUE ~ indicator)) %>% 
    clean_partner() %>%
    filter(!is.na(primepartner)) %>% 
    semi_join(age_disaggs, by = c("indicator", "numeratordenom", "standardizeddisaggregate")) %>% 
    group_by(fiscal_year, primepartner, indicator, age = trendscoarse, numeratordenom) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")
  
  
  df_full_ptnr <- df_msd_usaid %>% 
    bind_rows(df_dp_ptnr)
  
  df_full_ptnr <- df_full_ptnr %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  
  df_full_ptnr <- df_full_ptnr %>% 
    mutate(age = "Total") %>% 
    group_by(fiscal_year, primepartner, indicator, age) %>% 
    summarize(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    bind_rows(df_full_ptnr)
  
  df_full_ptnr <- df_full_ptnr %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_pntr_ex <- df_full_ptnr %>% 
    filter(age %in% c("Total", "<15")) %>% 
    arrange(primepartner, desc(age), indicator) %>% 
    select(-cumulative) %>% 
    pivot_wider(names_from = fiscal_year,
                values_from = c(targets, achievement))
  
  write_csv(df_pntr_ex, "Dataout/COP21_TZA_Target_Comp_Partner.csv", na = "")
  
  

# SNU1 --------------------------------------------------------------------

  
  df_dp_ptnrsnu <- df_dp %>% 
    filter(fundingagency == "USAID",
           (indicator == "KP_PREV" | disagg != "KeyPop")) %>%
    mutate(indicator = case_when(indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
                                 indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
                                 TRUE ~ indicator)) %>% 
    group_ages() %>% 
    clean_partner() %>%
    count(primepartner, psnu, indicator, age, numeratordenom, wt = targets, name = "targets") %>% 
    mutate(fiscal_year = 2022) %>% 
    arrange(primepartner, indicator, numeratordenom)
  
  
  df_dp_ptnrsnu <- df_dp_ptnrsnu %>% 
    clean_indicator() %>% 
    select(-numeratordenom)
  
  
  df_dp_ptnrsnu <- df_dp_ptnrsnu %>% 
    mutate(age = "Total") %>% 
    group_by(fiscal_year, primepartner, psnu, indicator, age) %>% 
    summarize(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    bind_rows(df_dp_ptnrsnu)
  
  ind_lst <- df_dp_ptnrsnu %>% 
    distinct(indicator) %>% 
    arrange(indicator) %>% 
    pull()
  
  df_dp_ptnrsnu_ex <- df_dp_ptnrsnu %>% 
    filter(age %in% c("Total", "<15")) %>% 
    arrange(primepartner, desc(age), psnu, indicator) %>%  
    pivot_wider(names_from = indicator,
                values_from = targets) %>% 
    select(fiscal_year:age, ind_lst)
  
  write_csv(df_dp_ptnrsnu_ex, "Dataout/COP21_TZA_Target_Comp_SNU.csv", na = "")
  