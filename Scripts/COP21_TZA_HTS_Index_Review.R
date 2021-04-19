# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COP21 support - HTS review for data pack
# LICENSE:  MIT
# DATE:     2021-04-19
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
  library(ICPIutilities)
  

# GLOBAL VARIABLES --------------------------------------------------------
  

# IMPORT ------------------------------------------------------------------
  
  df_hts <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result")


# MUNGE -------------------------------------------------------------------

  df_hts <- df_hts %>% 
    mutate(type_loc = ifelse(str_detect(modality, "Mod"), "Community", "Facility"),
           type_mod = ifelse(str_detect(modality, "Index"), "Index", "Other")
           )
  

# SHARE OF POS FROM INDEX -------------------------------------------------

  df_pos_share <- df_hts %>% 
    bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
    group_by(fiscal_year, snu1, indicator, type_mod) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop_last") %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    mutate(share = cumulative/sum(cumulative)) %>% 
    ungroup()


  df_pos_share <- df_pos_share %>% 
    group_by(snu1, fiscal_year) %>% 
    mutate(snu_total = ifelse(fiscal_year == 2021, sum(cumulative), 0)) %>% 
    group_by(snu1) %>% 
    mutate(snu_total = max(snu_total),
           snu_index = ifelse(fiscal_year == 2021 & type_mod == "Index", cumulative, 0),
           snu_index = max(snu_index),
           area_fill = ifelse(snu1 == "NATIONAL", burnt_sienna, scooter),
           facet_title = glue("{snu1}\nFY21Q1: {comma(snu_index)}/{comma(snu_total)}")) %>% 
    ungroup()
  
  
  df_pos_share %>% 
    filter(type_mod == "Index",
           snu_total > 500) %>% 
    ggplot(aes(fiscal_year, share, fill = area_fill, color = area_fill)) +
    geom_area(alpha = .6, size = 1.1) +
    facet_wrap(~fct_reorder(facet_title, snu_index, median, .desc = TRUE)) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks = c(2019, 2020, 2021)) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = "NATIONALLY JUST OVER 50% OF FY21Q1 POSITIVES ARE FROM INDEX TESTING",
         subtitle = "Share of positive index test out of all positive tests",
         caption = "Note: Plots from the six regions with less than 500 positive tests in FY21Q1 are removed
         Source: FY21Q1c MSD
         OHA/SIEI graphic: Aaron Chafetz") +
    theme(panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Images/COP21_TZA_Index_Share.png")

# SHARE OF COMMUNITY VS FACILITY INDEX POS --------------------------------

  df_pos_index_share <- df_hts %>% 
    bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
    group_by(fiscal_year, snu1, indicator, type_mod, type_loc) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop_last") %>% 
    filter(indicator == "HTS_TST_POS",
           type_mod == "Index") %>% 
    mutate(share = cumulative/sum(cumulative),
           share = ifelse(type_loc == "Community", -share, share)) %>% 
    ungroup()
  
  df_pos_index_share <- df_pos_index_share %>% 
    complete(type_loc, nesting(fiscal_year, snu1), fill = list(cumulative = 0, share = 0)) %>% 
    mutate(fiscal_year = as.character(fiscal_year) %>% fct_rev,
           area_fill = ifelse(type_loc == "Community", denim, scooter),
           background = ifelse(type_loc == "Community", -1, 1)) %>% 
    group_by(snu1, fiscal_year) %>% 
    mutate(snu_total = ifelse(fiscal_year == 2021, sum(cumulative), 0)) %>% 
    group_by(snu1) %>% 
    mutate(snu_total = max(snu_total),
           snu_comm = ifelse(fiscal_year == 2021 & type_loc == "Community", cumulative, 0),
           snu_comm = max(snu_comm),
           snu_fac = ifelse(fiscal_year == 2021 & type_loc == "Facility", cumulative, 0),
           snu_fac = max(snu_fac),
           facet_title = glue("{snu1}<br>FY21Q1: <span style = 'color:#2057a7;'>{comma(snu_comm)}</span>/<span style = 'color:#1e87a5;'>{comma(snu_fac)}")) %>% 
    ungroup()
  
  df_pos_index_share %>% 
    filter(snu_total > 200) %>% 
    ggplot(aes(share, fiscal_year, fill = area_fill)) +
    geom_col(aes(background), alpha = .2, fill = "gray80") +
    geom_col(alpha = .8) +
    geom_vline(xintercept = c(-1, 0, 1), color = "gray60") +
    geom_vline(xintercept = c(-.5, .5), color = "gray60", linetype = "dashed") +
    facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE)) +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(-1, 0, 1),
                       labels = percent(c(1, 0, 1))) +
    si_style_nolines() +
    labs(x = NULL, y = NULL,
         title = glue("NATIONALLY ABOUT A THIRD OF POSITIVE INDEX TESTS ARE IN <span style = 'color:#2057a7;'>COMMUNITIES</span> AS OPPOSED TO <span style = 'color:#1e87a5;'>FACILITIES</span>"),
         subtitle = "Share of positive index test by location",
         caption = "Note: Plots from the six regions with less than 200 positive index tests in FY21Q1 are removed
         Source: FY21Q1c MSD
         OHA/SIEI graphic: Aaron Chafetz") +
    theme(panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          strip.text = element_markdown(),
          plot.title = element_markdown())
  
  si_save("Images/COP21_TZA_Index_Comm-v-Fac.png")  

# INDEX AGE/SEX POSITIVITY ------------------------------------------------

  df_index_positivity <-  df_hts %>% 
    bind_rows(df_hts %>% mutate(snu1 = "NATIONAL")) %>% 
    group_by(fiscal_year, snu1, indicator, ageasentered, sex, type_mod, type_loc) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") %>% 
    filter(type_mod == "Index",
           fiscal_year == 2021) %>% 
    pivot_wider(names_from = indicator, values_from = cumulative, values_fill = 0) %>% 
    mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
    ungroup()  
    
  df_index_positivity <- df_index_positivity %>% 
    group_by(snu1) %>% 
    mutate(snu_total = sum(HTS_TST_POS)) %>% 
    group_by(snu1, type_loc) %>% 
    mutate(snu_type = sum(HTS_TST_POS),
           facet_title = glue("{snu1}<br>FY21Q1: {comma(snu_type)}")) %>% 
    ungroup()
  
  df_index_positivity %>% 
    filter(type_loc == "Facility",
           !ageasentered %in% c("<01", "01-04", "05-09", "10-14", "Unknown Age"),
           snu_total > 200) %>% 
    ggplot(aes(positivity, ageasentered, color = sex, fill = sex,size = HTS_TST_POS)) +
    geom_point(shape = 21, alpha = .3) +
    facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE), ncol = 9) +
    scale_x_continuous(label = percent) +
    scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa),
                      aesthetics = c("colour", "fill")) +
    scale_size_continuous(label = comma) +
    labs(x = NULL, y = NULL, fill = NULL, color = NULL,
         title = "FACILITY INDEX POSITIVITY BY AGE/SEX",
         caption = "Note: Plots from the six regions with less than 200 index positive tests in FY21Q1 are removed
         Source: FY21Q1c MSD
         OHA/SIEI graphic: Aaron Chafetz") +
    si_style() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          axis.text.x = element_text(size = 7),
          strip.text = element_markdown())
  
  si_save("Images/COP21_TZA_Index_Positivity_Fac.png") 
  
  df_index_positivity %>% 
    filter(type_loc == "Community",
           !ageasentered %in% c("<01", "01-04", "05-09", "10-14", "Unknown Age"),
           snu_total > 200) %>% 
    ggplot(aes(positivity, ageasentered, color = sex, fill = sex, size = HTS_TST_POS)) +
    geom_point(shape = 21, alpha = .3) +
    facet_wrap(~fct_reorder(facet_title, snu_total, max, .desc = TRUE), ncol = 9) +
    scale_x_continuous(label = percent) +
    scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa),
                      aesthetics = c("colour", "fill")) +
    scale_size_continuous(label = comma) +
    labs(x = NULL, y = NULL, fill = NULL, color = NULL,
         title = "COMMUNITY INDEX POSITIVITY BY AGE/SEX",
         caption = "Note: Plots from the six regions with less than 200 index positive tests in FY21Q1 are removed
         Source: FY21Q1c MSD
         OHA/SIEI graphic: Aaron Chafetz") +
    si_style() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          axis.text.x = element_text(size = 7),
          strip.text = element_markdown())
  
  si_save("Images/COP21_TZA_Index_Positivity_Comm.png")
    
  