# PROJECT:  bad boys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Final review of TZA DP to COP Approval Memo
# LICENSE:  MIT
# DATE:     2021-06-10
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
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------

  peds_ages <- c("<01","01-04", "05-09", "10-14", 
               "<15", "02 - 12 Months", "<=02 Months")
  
  tbl_ind <- c("HTS_INDEX", "HTS_TST", "HTS_TST_POS",
               "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
               "CXCA_SCRN", "OVC_SERV", "OVC_HIVSTAT", 
               "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART",
               "PMTCT_EID", "PP_PREV", "KP_PREV", "KP_MAT", "VMMC_CIRC",
               "HTS_SELF", "PrEP_NEW", "PrEP_CURR", "TB_STAT",
               "TB_ART", "TB_PREV", "TX_TB_D", "GEND_GBV", "AGWY_PREV")
  
  ptnr_tbl1_ind <- c("HTS_INDEX", "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")
  ptnr_tbl2_ind <- c("TX_PVLS", "CXCA_SCRN", "OVC_SERV", "OVC_HIVSTAT", "PMTCT_STAT", "PMTCT_STAT_POS")
  ptnr_tbl3_ind <- c("PMTCT_ART", "PMTCT_EID", "PP_PREV", "KP_PREV", "KP_MAT", "VMMC_CIRC", "HTS_SELF")
  ptnr_tbl4_ind <- c("PrEP_NEW", "PrEP_CURR", "TB_STAT", "TB_ART", "TB_PREV", "TX_TB_D", "GEND_GBV")
    
# IMPORT ------------------------------------------------------------------
  
  df_dp <- vroom("Dataout/COP21_TZA_tameDP_20210521.csv",
                 col_types = c(fiscal_year = "i",
                               targets = "d",
                               .default = "c"))
  df_msd <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   

  
  

# MUNGE -------------------------------------------------------------------

df_totals <- df_dp %>% 
    filter(!(indicator %in% c("HTS_RECENT", "HTS_TST", "HTS_TST_POS", "PrEP_NEW", "PrEP_CURR") & disagg == "KeyPop"),
           indicator != "OVC_HIVSTAT") %>% 
    select(-c(age, sex, modality, statushiv, otherdisaggregate)) %>% 
    mutate(disagg = "Total",
           fiscal_year = as.character(fiscal_year)) %>% 
    group_by(across(where(is.character))) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fiscal_year = as.integer(fiscal_year))
    

df_dp <- df_dp %>% 
  bind_rows(df_totals) %>% 
  mutate(indicator = ifelse(indicator %in% c("HTS_INDEX_COM", "HTS_INDEX_FAC"), "HTS_INDEX", indicator))
  

df_dp <- df_dp %>% 
  filter(mech_code != "00000") %>% 
  clean_indicator()

df_dp <- df_dp %>% 
  mutate(age_group = case_when(disagg == "Total" ~ "Total",
                               disagg == "KeyPop" ~ NA_character_,
                               indicator == "OVC_SERV" & age %in% c(peds_ages, "15-17") ~ "<18",
                               age == "18+" ~ "18+",
                               age %in% peds_ages ~ "<15",
                               !age %in% peds_ages ~ "15+")) %>% 
  filter(!is.na(age_group))


# SUMMARIZE ---------------------------------------------------------------


df_dp %>% 
  filter(indicator == "TX_NEW",
         disagg == "Total") %>% 
  count(indicator, wt = targets)


df_dp %>% 
  filter(indicator %in% c("TX_PVLS", "TX_PVLS_D"),
         disagg == "Total") %>% 
  count(indicator, wt = targets)


df_dp %>% 
  filter(indicator == "VMMC_CIRC",
         disagg == "Age/Sex",
         !age %in% peds_ages
         ) %>% 
  count(indicator, wt = targets)


df_dp %>% 
  filter(indicator == "VMMC_CIRC",
         disagg == "Age/Sex",
         !age %in% c(peds_ages, "15-19", "20-24")
  ) %>% 
  count(indicator, wt = targets)

df_dp %>% 
  filter(indicator == "CXCA_SCRN",
         disagg == "Total"
  ) %>% 
  count(indicator, wt = targets)



df_dp %>% 
  filter(indicator %in% tbl_ind) %>% 
  mutate(age_group = case_when(disagg == "Total" ~ "Total",
                               age %in% peds_ages ~ "<15",
                               !age %in% peds_ages ~ "15+"),
         age_group = factor(age_group, c("<15", "15+", "Total")),
         indicator = factor(indicator, c(tbl_ind))) %>% 
  count(indicator, age_group, wt = targets) %>% 
  mutate(n = comma(n, 1)) %>% 
  prinf()

df_dp %>% 
  count(indicator) %>% 
  arrange(indicator) %>% 
  prinf()

# TABLE COMPARISON --------------------------------------------------------

df_im_tb <- df_dp %>% 
  filter(indicator %in% tbl_ind,
         fundingagency == "USAID") %>% 
  count(primepartner, indicator, age_group, wt = targets) %>% 
  pivot_wider(names_from = c(indicator, age_group),
              names_sep = " ",
              values_from = n) 
#table 1
df_im_tb %>% 
  select(primepartner,
         `HTS_INDEX <15`,
         `HTS_INDEX 15+`,
         `HTS_TST <15`,
         `HTS_TST 15+`,
         `HTS_TST_POS <15`,
         `HTS_TST_POS 15+`,
         `TX_NEW <15`,
         `TX_NEW 15+`,
         `TX_CURR <15`,
         `TX_CURR 15+`
)

#table 2
df_im_tb %>% 
  select(primepartner, 
         `TX_PVLS <15`,
         `TX_PVLS 15+`,
         `CXCA_SCRN Total`,
         `OVC_SERV <18`,
         `OVC_SERV 18+`,
         `OVC_HIVSTAT Total`,
         `PMTCT_STAT <15`,
         `PMTCT_STAT 15+`)

df_dp %>% 
  filter(fundingagency == "USAID",
         indicator == "PMTCT_STAT",
         disagg == "Age/Sex",
         otherdisaggregate %in% c("NewPos", "KnownPos")
         ) %>%
  mutate(age_group = case_when(age %in% peds_ages ~ "<15",
                               !age %in% peds_ages ~ "15+")) %>% 
  count(primepartner, age_group, wt = targets)

#table 3
df_im_tb %>% 
  select(primepartner,
         `PMTCT_ART <15`,
         `PMTCT_ART 15+`,
         `PMTCT_EID Total`,
         `PP_PREV <15`,
         `PP_PREV 15+`,
         `KP_PREV Total`,
         `KP_MAT`,
         `VMMC_CIRC Total`,
         `HTS_SELF <15`,
         `HTS_SELF 15+`)


#table 4
df_im_tb %>% 
  select(primepartner,
         `PrEP_NEW Total`,
         `PrEP_CURR Total`,
         `TB_STAT <15`,
         `TB_STAT 15+`,
         `TB_ART <15`,
         `TB_ART 15+`,
         `TB_PREV <15`,
         `TB_PREV 15+`,
         `TX_TB_D <15`,
         `TX_TB_D 15+`,
         `GEND_GBV Total`)




# TARGET TRENDS -----------------------------------------------------------

df_msd_agg <- df_msd %>% 
  clean_indicator() %>% 
  filter(operatingunit == "Tanzania",
         fundingagency == "USAID",
         indicator %in% unique(df_dp$indicator),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  count(fundingagency, fiscal_year, indicator, wt = targets)

df_dp_agg <- df_dp %>% 
  filter(fundingagency == "USAID",
         disagg == "Total") %>% 
  count(fundingagency, fiscal_year, indicator, wt = targets)

df_msd_agg %>% 
  bind_rows(df_dp_agg) %>% 
  mutate(n = na_if(n, 0)) %>% 
  pivot_wider(names_from = fiscal_year, values_from = n)
