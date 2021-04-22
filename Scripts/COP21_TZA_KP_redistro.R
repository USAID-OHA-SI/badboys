# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  redistribute KP targets by age/sex
# LICENSE:  MIT
# DATE:     2021-04-22
# UPDATED: 

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
  library(readxl)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  file <- "Data/PREP Integration of KP into PREP Tab.xlsx"
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")
  
  df_kp <- read_excel(file,
                    sheet = "Sheet1") %>% 
    select(-starts_with("..."))
  
  df_age <- read_excel(file,
                       sheet = "Sheet2",
                       skip = 1)

# MUNGE -------------------------------------------------------------------

  #use PreP_NEW to create a age/sex distro from FY21, distibution btwn large age bands
  df_disto <- df %>% 
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate == "Age/Sex",
           fiscal_year == 2021) %>% 
    count(sex, ageasentered, wt = cumulative) %>% 
    group_by(sex) %>% 
    mutate(share = n/sum(n)) %>% 
    ungroup() %>%
    filter(share >.1) %>% 
    group_by(sex) %>% 
    mutate(share = n/sum(n)) %>% 
    select(Sex = sex, Age = ageasentered, share) %>% 
    mutate(SNU1 = "NATIONAL", 
           PSNU = "NATIONAL")
  
  write_csv(df_disto, "Dataout/1_Nat_distro.csv", na = "")
    
  #reshape the KP targes long and assign sex
  df_kp <- df_kp %>% 
    rename(`PrEP_CURR.T` = `PREP CURR`,
           `PrEP_NEW.T` = `PREP NEW`) %>% 
    pivot_longer(c(`PrEP_CURR.T`, `PrEP_NEW.T`), 
                 names_to = "indicator",
                 values_drop_na = TRUE) %>% 
    mutate(Sex = case_when(KeyPop == "FSW" ~ "Female",
                           KeyPop == "MSM" ~ "Male"))
  
  #split non FSW/MSM sex between males and females
  df_kp_other <- df_kp %>% 
    filter(is.na(Sex)) %>% 
    mutate(Male = ifelse((value %% 2) == 0, value/2, (value-1)/2),
           Female = ifelse((value %% 2) == 0, value/2, (value+1)/2)) %>% 
    select(-c(Sex, value)) %>% 
    pivot_longer(c(Male, Female), names_to = "Sex")
    
  #add the nonFSW/MSM back onto the KP table
  df_kp <- df_kp %>% 
    filter(!is.na(Sex)) %>% 
    bind_rows(df_kp_other)
  
  #reshape the age df long
  df_age <- df_age %>% 
    pivot_longer(c(`PrEP_CURR.T`, `PrEP_NEW.T`), 
                 names_to = "indicator",
                 values_drop_na = TRUE)
  
  #bind the age/sex groups onto the psnus and ensure all grouping exist 
  df_full <- df_kp %>%
    select(SNU1, PSNU, indicator) %>% 
    bind_rows(df_disto) %>% 
    complete(Sex, Age, indicator, nesting(SNU1, PSNU)) %>% 
    select(-share) %>% 
    filter(!is.na(indicator),
           !is.na(Sex),
           !is.na(Age),
           !is.na(SNU1),
           !is.na(PSNU),
           PSNU != "NATIONAL")
  
  #remove placeholder PNSU for 1st merge to apply to all PSNUs
  df_distro_lim <- df_disto %>% 
    select(-c(SNU1, PSNU))
  
  #join the complete df with the natl distro and then kp total values
  df_full <- df_full %>% 
    left_join(df_distro_lim) %>% 
    left_join(df_kp)
  
  #create the new KP total for each age/sex band
  df_full <- df_full %>% 
    mutate(kp_spread = round(value * share))
  
  write_csv(df_full, "Dataout/2_complete_psnu-age-sex.csv", na = "")
  
  #aggregate up, removing the KP group
  df_agg <- df_full %>% 
    group_by(SNU1, PSNU, Age, Sex, indicator) %>% 
    summarise(kp_spread = sum(kp_spread, na.rm =T)) %>% 
    ungroup()
  
  #append original age and the new distrubuted KP age/sex totals
  df_complete <- bind_rows(df_age, df_agg) %>% 
    group_by(SNU1, PSNU, Age, Sex, indicator) %>% 
    summarise(across(c(value, kp_spread), sum, na.rm =T)) %>% 
    ungroup()
  
  #create at new value to use in DP
  df_complete <- df_complete %>%
    rename(orig_agesex_value = value) %>% 
    mutate(new_total = orig_agesex_value + kp_spread)
  
  #clean for export
  df_export <- df_complete %>% 
    select(SNU1:indicator, new_total) %>% 
    filter(new_total>0) %>% 
    pivot_wider(names_from = indicator, values_from = new_total)
  
  write_csv(df_export, "Dataout/4_final.csv", na = "")

# CHECK -------------------------------------------------------------------

  
  df_export_check <- df_export %>% 
    group_by(PSNU) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_longer(-PSNU, names_to = "indicator") %>% 
    rename(final_total = value)
  
  
  df_age_check <- df_age %>% 
    group_by(indicator, PSNU) %>% 
    summarise(orig_val = sum(value, na.rm = TRUE)) %>% 
    ungroup() 
  
  df_kp_check <- df_kp %>% 
    group_by(indicator, PSNU) %>% 
    summarise(kp_val = sum(value, na.rm = TRUE)) %>% 
    ungroup() 

  df_final_check <- df_kp_check %>% 
    tidylog::left_join(df_age_check) %>% 
    mutate(across(c(kp_val, orig_val), ~ ifelse(is.na(.), 0, .)),
           total = kp_val + orig_val) %>% 
    tidylog::left_join(df_export_check) %>% 
    mutate(delta = final_total/total)
  
  write_csv(df_final_check, "Dataout/3_check.csv", na = "")
  
