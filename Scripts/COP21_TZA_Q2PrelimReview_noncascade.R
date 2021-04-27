# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Prelim Q2 review for TZA in non-cascade areas
# LICENSE:  MIT
# DATE:     2021-04-22
# UPDATED: 


# GENIE INFO --------------------------------------------------------------

# PSNU By IM
# DATIM data as of: 04/21/2021 00:08:22 UTC
# Genie report updated: 04/22/2021 01:45:02 UTC
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2
# Daily/Frozen: Daily
# Indicator: AGYW_PREV, CXCA_SCRN, CXCA_TX, HTS_TST, HTS_TST_POS, KP_PREV,
#            OVC_HIVSTAT, OVC_HIVSTAT_POS, OVC_SERV, PrEP_CURR, PrEP_NEW, 
#            TB_ART, TB_PREV, TB_STAT, TB_STAT_POS, TX_CURR, TX_NEW
# Fiscal Year: 2021,2020


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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  genie_file <- "Data/Genie-PSNUByIMs-Tanzania-Daily-2021-04-22.zip"
  
  genie_extract_date <- str_extract(genie_file, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")
  
  default_source <- glue("Source: DATIM Genie (pulled {genie_extract_date})")

# IMPORT ------------------------------------------------------------------
  
  df_genie <- read_msd(genie_file)
  

# OVC ---------------------------------------------------------------------
  
  df_ovc <- df_genie %>% 
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2021) %>% 
    group_by(snu1) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(achievement = cumulative/targets,
           achv_lab = case_when(achievement > 0 ~ percent(achievement, 1)))

  df_ovc %>% 
    ggplot(aes(y = fct_reorder(snu1, targets))) +
    geom_col(aes(targets), alpha = .8, fill = trolley_grey_light) +
    geom_col(aes(cumulative), fill = genoa) +
    geom_errorbar(aes(xmin = targets, xmax = targets), color = trolley_grey) + 
    geom_text(aes(x = 2500, label = achv_lab,
                   family = "Source Sans Pro")) +
    scale_x_continuous(label = comma,
                       expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "OVC_SERV HAS LARGELY NOT BEEN REPORTED FOR FY21 AS OF YET",
         subtitle = "FY21Q2 OVC_SERV Results and Targets",
         caption = glue("Note: FY21Q2 data are preliminary!
                        {default_source}")) +
    si_style_xgrid()
  
    si_save("Images/FY21Q2Prelim_TZA_OVCSERV.png")
  
  # df_ovc %>% 
  #   ggplot(aes(achievement, fct_reorder(snu1, achievement))) +
  #   geom_segment(aes(x = 0, xend = achievement, yend = fct_reorder(snu1, achievement))) +
  #   geom_point() +
  #   scale_x_continuous(label = percent) +
  #   labs(x = NULL, y = NULL,
  #        caption = glue("Note: FY21Q2 data are preliminary!
  #                       {default_source}")) +
  #   si_style_xgrid()
  

# DREAMS -------------------------------------------------------------------

  
    df_dreams <- df_genie %>% 
      filter(indicator == "AGYW_PREV",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2021) %>% 
      group_by(snu1) %>% 
      summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(achievement = cumulative/targets,
             achv_lab = case_when(achievement > 0 ~ percent(achievement, 1)))

# TB ----------------------------------------------------------------------

    df_art <- df_genie %>% 
      filter(indicator %in% c("TB_STAT", "TB_ART"),
             standardizeddisaggregate %in% c("Total Denominator","Total Numerator")) %>% 
      group_by(fiscal_year, snu1, indicator, numeratordenom) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd() %>% 
      pivot_wider(names_from = numeratordenom,
                  values_from = value) %>% 
      mutate(N = ifelse(is.na(N), 0, N),
             coverage = N/D,
             flag = coverage < .9)
    
    
    df_art %>% 
      filter(indicator == "TB_STAT") %>% 
      mutate(max_val = ifelse(period == max(period), D, 0)) %>% 
      arrange(snu1, period) %>% 
      group_by(snu1) %>% 
      mutate(decline = case_when(period == max(period) ~ coverage < lag(coverage, n = 1, order_by = period)),
             ln_clr = ifelse(max(decline, na.rm = TRUE) == 1, burnt_sienna, scooter)) %>%
      ungroup() %>%
      ggplot(aes(period, coverage, group = snu1, color = ln_clr)) +
      geom_hline(yintercept = .9, linetype = "dashed", color = trolley_grey) +
      geom_path() +
      geom_point() +
      facet_wrap(~fct_reorder(snu1, max_val, max, .desc = TRUE)) +
      scale_y_continuous(label = percent_format(1),
                         limits = c(.8, 1.1),
                         oob = squish) +
      scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1")) +
      scale_color_identity() +
      labs(x = NULL, y = NULL,
           title = "WITH A FEW EXCEPTIONS, TB STAT COVERAGE REMAIN CONSTANT",
           subtitle = "Trends in TB Coverage (TB_STAT/TB_STAT_D)",
           caption = glue("Note: FY21Q2 data are preliminary!
                        {default_source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "lines"))
    
    
    si_save("Images/FY21Q2Prelim_TZA_TB-Coverage.png")
    
    df_art %>% 
      filter(indicator == "TB_STAT") %>% 
      mutate(max_val = ifelse(period == max(period), D, 0)) %>% 
      arrange(snu1, period) %>% 
      ggplot(aes(period, D, group = snu1)) +
      geom_hline(yintercept = .9, linetype = "dashed", color = trolley_grey) +
      geom_path() +
      geom_point() +
      facet_wrap(~fct_reorder(snu1, max_val, max, .desc = TRUE), scale = "free_y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1")) +
      labs(x = NULL, y = NULL,
           title = "WITH A FEW EXCEPTIONS, ",
           subtitle = "FY21Q2 OVC_SERV Results and Targets",
           caption = glue("Note: FY21Q2 data are preliminary!
                        {default_source}")) +
      si_style_ygrid()
    
  

# CERVICAL CANCER ---------------------------------------------------------

  df_genie %>% 
      filter(indicator == "CXCA_SCRN",
             standardizeddisaggregate == "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
             fiscal_year == 2021) %>% 
      count(standardizeddisaggregate, wt = cumulative)
    