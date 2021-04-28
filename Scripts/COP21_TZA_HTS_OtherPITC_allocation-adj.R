# PROJECT:  COP21 Data Pack
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  adjust issues due to rounding on HTS tab
# LICENSE:  MIT
# DATE:     2021-04-20
# UPDATED:  2021-04-28

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(readxl)


# GLOBAL ------------------------------------------------------------------

path_dp <- "Data/Data Pack_Tanzania_Apr2821-PMTCT Test DP V2 1158 am.xlsx" 
  
# IMPORT ------------------------------------------------------------------
  
#read in data pack
  df_dp <- read_excel(path_dp,
                      sheet = "HTS",
                      skip = 13) 
  
#identify how many rows there are issues 
  df_dp %>% 
    filter(!is.na(ID)) %>%
    count(HTS_TST.Pos.Diff != 0)
  
#limit scope of data frame
  df_fix <- df_dp %>% 
    filter(!is.na(ID)) %>% 
    select(SNU1:ID, 
           HTS_TST.Pos.T,
           HTS_TST.Other.Pos.Share,
           HTS_TST.Other.Pos.T, 
           HTS_TST.Pos.Original, 
           HTS_TST.Pos.Total,
           HTS_TST.Pos.Diff)
  
#flag wher ethere are issues and create the new share (cannot resolve where Other PITC < diff)
  df_fix <- df_fix %>%
    mutate(across(starts_with("HTS"), ~ ifelse(is.na(.), 0, .)),
           is_off = HTS_TST.Pos.Diff != 0,
           is_resolved_now = case_when(is_off ~ HTS_TST.Other.Pos.T >= HTS_TST.Pos.Diff),
           new_other_pos = case_when(is_off ~ HTS_TST.Other.Pos.T - HTS_TST.Pos.Diff),
           new_other_pos_share = ifelse(is_off & is_resolved_now, new_other_pos/HTS_TST.Pos.T,
                                    HTS_TST.Other.Pos.Share))
#export
  df_fix %>%
    mutate(dp_row = row_number() +14, .before =1) %>% 
    select(dp_row:ID, HTS_TST.Other.Pos.Share, is_off,is_resolved_now, new_other_pos_share) %>% 
    write_csv("Dataout/hts_realignment.csv", na = "")  
  
#review
  df_fix %>% 
    filter(is_off) %>% 
    count(is_resolved_now)
  
  janitor::tabyl(df_fix, is_off, is_resolved_now) %>% janitor::adorn_totals(c("row", "col"))
  