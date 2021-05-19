# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  OVC Trends
# LICENSE:  MIT
# DATE:     2021-05-19
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
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------

dodge <- position_dodge(width=.5)

# IMPORT & STORE DP -------------------------------------------------------

  # dp_tza <-  "../../../Downloads/PEPFAR Tanzania - DP -05162021-clean-1330 (1)_ahc_out2.xlsx"
  # df_dp <- tame_dp(dp_tza, FALSE)
  # df_dp <- get_names(df_dp)
  # write_csv(df_dp, "Dataout/COP21_TZA_tameDP_20210519.csv", na = "")
  
# IMPORT ------------------------------------------------------------------
  
  df_dp <- vroom("Dataout/COP21_TZA_tameDP_20210519.csv",
                 col_types = c(fiscal_year = "i",
                               targets = "d",
                               .default = "c"))
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   
  

# MUNGE -------------------------------------------------------------------

  df_msd <- df_msd %>% 
    filter(operatingunit == "Tanzania",
           fiscal_year == 2021,
           indicator %in% c("TX_CURR", "OVC_SERV"),
           standardizeddisaggregate %in% c("Age/Sex/ProgramStatus",
                                           "Age/Sex/HIVStatus"),
           ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-17", "18+", "15-19"))
  
  df_msd_ovc <- df_msd %>% 
    group_by(fiscal_year, snu1, fundingagency, indicator, otherdisaggregate) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = indicator,
                values_from = targets) %>% 
    rename_all(tolower)
  
  df_snus <- df_msd %>% 
    filter(operatingunit == "Tanzania") %>% 
    distinct(snu1, psnuuid)
  
  df_dp <- left_join(df_dp, df_snus)
  
  df_dp <- df_dp %>% 
    filter(mech_code != "00000",
           indicator %in% c("OVC_SERV", "TX_CURR"),
           disagg == "Age/Sex",
           age %in% c("<01", "01-04", "05-09", "10-14", "15-17", "18+", "15-19"))

  df_dp_ovc <- df_dp %>% 
    group_by(fiscal_year, snu1, fundingagency, indicator, otherdisaggregate) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = indicator,
                values_from = targets) %>% 
    rename_all(tolower) %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate, 
                                      "Act" = "Active",
                                      "Grad" = "Graduated",
                                      "Prev" = "Prevention"))

  df_ovc <- bind_rows(df_msd_ovc, df_dp_ovc)
  

# VIZ ---------------------------------------------------------------------

#TOTAL SHIFT FROM COP20-21  
  df_ovc_tot <- df_ovc %>% 
    group_by(fiscal_year, snu1, fundingagency) %>% 
    summarise(across(c(ovc_serv, tx_curr), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    clean_agency()

  df_ovc_tot <- df_ovc_tot %>% 
    filter(ovc_serv > 0) %>%
    group_by(snu1) %>% 
    mutate(ovc_snu = ifelse(fiscal_year == 2022, sum(ovc_serv, na.rm = TRUE), 0),
           ovc_snu = max(ovc_snu)) %>% 
    ungroup() %>% 
    mutate(snu_agency = glue("{snu1}\n{fundingagency}"),
           fiscal_year = as.character(fiscal_year),
           fundingagency = factor(fundingagency, c("DOD","CDC", "USAID")),
           left_val = case_when(fiscal_year == "2021" ~ ovc_serv),
           right_val = case_when(fiscal_year == "2022" ~ ovc_serv),
           lab = glue("{round(ovc_serv/1000,0)}k"),
           agency_col = case_when(fundingagency == "USAID" ~ denim,
                                  fundingagency == "CDC" ~ scooter,
                                  fundingagency == "DOD" ~ genoa_light)) %>% 
    arrange(ovc_snu, fundingagency) %>% 
    mutate(snu_agency = fct_inorder(snu_agency)) 
  
  df_ovc_tot %>% 
    ggplot(aes(fiscal_year, ovc_serv, color = agency_col, fill = agency_col, group = fundingagency)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "gray90") +
    geom_area(alpha = .2) +
    geom_text(aes(y = right_val, label = lab), color = "gray50", hjust = -.3,
              family = "Source Sans Pro", size = 8/.pt, na.rm = TRUE)+
    geom_text(aes(y = left_val, label = lab), color = "gray50", hjust = 1,
              family = "Source Sans Pro", size = 8/.pt, na.rm = TRUE)+
    facet_wrap(~ fct_rev(snu_agency), ncol = 10) +
    scale_y_continuous(label = comma) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = "DECLINING OVC_SERV TARGETS PLANNED FOR FY22",
         caption = "Source: FY21Q1c MSD + 
         Data Pack (PEPFAR Tanzania - DP -05162021-clean-1330 (1)_ahc_out2.xlsx)") +
    si_style_xgrid() +
    theme(panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(1, "lines"),
          axis.text.y = element_blank())
  
  si_save("Images/OVC_SERV-Targets.png")
  
  
#SHIFT BY STATUS BETWEEN COP20-21   
  df_ovc_disagg <- df_ovc %>% 
    clean_agency() %>% 
    filter(ovc_serv > 0) %>%
    group_by(snu1) %>% 
    mutate(ovc_snu = ifelse(fiscal_year == 2022, sum(ovc_serv, na.rm = TRUE), 0),
           ovc_snu = max(ovc_snu)) %>% 
    ungroup() %>% 
    mutate(snu_agency = glue("{snu1} - {fundingagency}"),
           fiscal_year = as.character(fiscal_year),
           fundingagency = factor(fundingagency, c("DOD","CDC", "USAID")),
           agency_col = case_when(fundingagency == "USAID" ~ denim,
                                  fundingagency == "CDC" ~ scooter,
                                  fundingagency == "DOD" ~ genoa_light)) %>% 
    arrange(ovc_snu, fundingagency) %>% 
    mutate(snu_agency = fct_inorder(snu_agency)) %>% 
    group_by(snu_agency, otherdisaggregate) %>%
    mutate(min = min(ovc_serv, na.rm = TRUE),
           max = max(ovc_serv, na.rm = TRUE)) %>% 
    ungroup()
  
  
  df_ovc_disagg %>% 
    ggplot(aes(ovc_serv, snu_agency, shape = fiscal_year, group = snu_agency, size = fiscal_year,
               color = fiscal_year, fill = fiscal_year)) +
    geom_vline(xintercept = 0, color = "gray30") +
    geom_line(size = .9, color = trolley_grey_light) +
    geom_point(stroke = .8, fill = "white") +
    scale_x_continuous(label = comma) + 
    scale_shape_manual(values = c(21, 18)) +
    scale_size_manual(values = c(3, 4)) +
    scale_color_manual(values = c(trolley_grey_light, scooter), aesthetics = c("color", "fill")) +
    labs(x = NULL, y = NULL, shape = NULL, color = NULL, fill = NULL, size = NULL,
         title = "GROWTH IN OVC FOR DREAM + PREVENTION DOES NOT OFFSET DECLINE IN OVC ACTIVE",
         caption = "Source: FY21Q1c MSD + 
         Data Pack (PEPFAR Tanzania - DP -05162021-clean-1330 (1)_ahc_out2.xlsx)") +
    facet_grid(~fct_reorder(otherdisaggregate, ovc_serv, sum, na.rm = TRUE, .desc = TRUE)) +
    si_style_xgrid()
    
  si_save("Images/OVC_SERV-Targets-Disaggs.png")
  