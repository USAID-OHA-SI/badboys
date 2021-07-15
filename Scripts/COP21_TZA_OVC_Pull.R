# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  compile OVC data  
# LICENSE:  MIT
# DATE:     2021-05-24
# UPDATED:  2021-07-15

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
  library(googlesheets4)
  library(googledrive)
  library(tameDP)


# NOTES -------------------------------------------------------------------

# - C/ALHIV estimates
# - FY21 TX_CURR<20y/o targets
# - FY21 TX_CURR<20y/o results per Q2 preliminary data
# - VLC per Q2 preliminary data
# - VLS per Q2 preliminary data
# - PMTCT_ART (all ages) per Q2 preliminary data
# - PMTCT_ART 0-19y/o per Q2 preliminary data
# - OVC_HIVSTAT_POS per Q2 preliminary data
# - OVC_HIVSTAT_POS, i.e. C/ALHIV enrollment, projected for Q4 (assumption is at least 90% of TX_CURR<20y/o result for Q2)


# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  dp_path <- "Data/PEPFAR TZ DataPack 052121 final.xlsx"

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   

  df_plhiv <- import_plhiv(dp_path)
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds()  
  
# MUNGE -------------------------------------------------------------------

  df_plhiv <- df_plhiv %>% 
    filter(indicator == "PLHIV",
           age %in% c("<01", "01-04", "05-09", "10-14")
           ) %>% 
    group_by(snu1, psnu, psnuuid) %>%
    summarise(FY22_PLHIV_UNDER_15 = sum(targets, na.rm = TRUE)) %>% 
    ungroup()
  
  df_subnat <- df_subnat %>% 
    filter(operatingunit == "Tanzania",
           fiscal_year == 2021,
           indicator == "PLHIV",
           ageasentered %in% c("<01", "01-04", "05-09", "10-14")) %>% 
    count(snu1, psnu, psnuuid, wt = targets, name = "FY21_PLHIV_UNDER_15")
  
  df_tza <- df %>% 
    filter(operatingunit == "Tanzania")
  
  df_ovc <- df_tza %>% 
    filter((indicator %in% c("TX_CURR", "TX_PVLS", "PMTCT_ART") &
              standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex/Indication/HIVStatus", "Age/NewExistingArt/Sex/HIVStatus")) |
            (indicator %in% c("OVC_HIVSTAT_POS") & standardizeddisaggregate == "Total Numerator"))
  
  df_ovc_u15 <- df_ovc %>% 
    filter(indicator %in% c("TX_CURR", "TX_PVLS"),
           trendscoarse == "<15") %>% 
    mutate(indicator = glue("{indicator}_UNDER_15"))
  
  df_ovc_u20 <- df_ovc %>% 
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "PMTCT_ART"),
           (trendscoarse == "<15" | ageasentered == "15-19")) %>% 
    mutate(indicator = glue("{indicator}_UNDER_20"))
  
  df_ovc <- df_ovc %>% 
    filter(!indicator %in% c("TX_CURR", "TX_PVLS")) %>% 
    bind_rows(df_ovc_u15) %>% 
    bind_rows(df_ovc_u20) %>% 
    clean_indicator() %>%
    mutate(targets = case_when(indicator %in% c("TX_CURR_UNDER_15", "TX_CURR_UNDER_20") ~ targets)) %>% 
    group_by(fiscal_year, snu1, psnu, psnuuid, snuprioritization, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd()
  

  df_vl <- df_ovc %>% 
    filter(indicator %in% c("TX_CURR_UNDER_15", "TX_PVLS_UNDER_15", "TX_PVLS_UNDER_15_D",
                            "TX_CURR_UNDER_20", "TX_PVLS_UNDER_20", "TX_PVLS_UNDER_20_D"),
           period_type == "results") %>% 
    select(-period_type) %>% 
    spread(indicator, value) %>% 
    arrange(snu1, psnu, period) %>% 
    group_by(psnuuid) %>% 
    mutate(VLC_UNDER_15 = TX_PVLS_UNDER_15_D / lag(TX_CURR_UNDER_15, 2, order_by = period),
           VLS_UNDER_15 = TX_PVLS_UNDER_15/TX_PVLS_UNDER_15_D,
           VLC_UNDER_20 = TX_PVLS_UNDER_20_D / lag(TX_CURR_UNDER_20, 2, order_by = period),
           VLS_UNDER_20 = TX_PVLS_UNDER_20/TX_PVLS_UNDER_20_D) %>% 
    ungroup() %>% 
    filter(period == max(period)) %>% 
    pivot_wider(names_from = period,
                values_from = c("VLC_UNDER_15", "VLS_UNDER_15",
                                "VLC_UNDER_20", "VLS_UNDER_20"),
                names_glue = "{period}_{.value}") %>% 
    select(snu1, psnu, psnuuid, starts_with("FY"))
  
  
  df_wide <- df_ovc %>% 
    mutate(indicator = ifelse(period_type == "targets", glue("{indicator}_TARGETS"), indicator)) %>% 
    filter(period_type %in% c("cumulative", "targets"),
           str_detect(indicator, "TX_PVLS", negate = TRUE)) %>% 
    select(-period_type) %>% 
    filter(period == max(period)) %>% 
    pivot_wider(names_from = c(period, indicator)) %>% 
    left_join(df_vl)

  df_wide <- df_wide %>% 
    full_join(df_plhiv) %>% 
    full_join(df_subnat)
  
  df_wide <- df_wide %>% 
    select(snu1, psnu, psnuuid, snuprioritization,
           contains("PLHIV"),
           contains("TX_CURR"),
           contains("VL"),
           contains("ART"),
           contains("OVC_HIVSTAT_POS")
           ) 
  
  df_wide <- df_wide %>% 
    arrange(desc(FY22_PLHIV_UNDER_15)) %>% 
    mutate(snuprioritization = str_remove(snuprioritization, "^[:digit:]{1,} - ")) %>% 
    mutate(ovc_psnu = !is.na(FY21_OVC_HIVSTAT_POS), .after = snuprioritization)
  
# EXPORT ------------------------------------------------------------------

  # (ss <- gs4_create("COP21_TZA_OVC-planning"))
  ss <- "11lWghp7kjuEhpjKRhOKGkveFE2Orht6GG7mwBthkPb0"
  
  sheet_write(df_wide, ss, "Data")
  
  drive_share(as_id(ss), role = "writer",
              type = "domain", domain = "usaid.gov", verbose = FALSE)
  
  drive_browse(ss)
