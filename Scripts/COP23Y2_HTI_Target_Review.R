# PROJECT:  badboys  
# PURPOSE:  review HTI targets (based on ZAM review)
# AUTHOR:   A.Chafetz | USAID
# REF ID:   424a3885 
# LICENSE:  MIT
# DATE:     2024-06-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(glue)
  library(vroom)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# SOURCE ------------------------------------------------------------------

# PAW - https://workspace.pepfar-panorama.org/MicroStrategy/asp/Main.aspx
# Workspaces Daily>Shared Reports>Analytic Workspaces>COP Dossiers>COP23/FY25 & ROP24/FY25-26 Target Setting Tool Dossier
# Country: Haiti
# Target Setting Tool Last Loaded: 2024-04-15 03:42:38 PM
# Target Summary: Total Population and KP

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "424a3885"  #a reference to be places in viz captions 
  
  path_paw <- c("Data/COP23_FY25 & ROP24_FY25-26 Target Setting Tool Dossier_HTI.csv",
                "Data/COP23_FY25 & ROP24_FY25-26 Target Setting Tool Dossier_HTI_KP.csv")
  
  file.exists(path_paw)  
  
# IMPORT ------------------------------------------------------------------
  
  df_paw <- map_dfr(path_paw, read_csv)
  

# MUNGE -------------------------------------------------------------------

  df_paw <- df_paw %>% 
    filter(!is.na(Indicator)) %>% 
    select(-`Operating Unit`) %>% 
    pivot_longer(starts_with("20"), 
                 names_to = "fiscal_year",
                 values_to = "targets",
                 # values_transform = list(targets = as.double)
                 ) %>% 
    rename_all(tolower) %>%
    rename(numeratordenom = `numerator (operand)`,
           funding_agency = `fiscal year`,
           keypop_disagg = `key population`) %>% 
    mutate(targets = targets %>% 
             str_remove_all(",") %>% 
             as.integer(),
           is_kp = !is.na(keypop_disagg)) %>% 
    clean_indicator() %>% 
    select(-numeratordenom) %>% 
    filter(!is.na(targets))


  df_agg <- df_paw %>% 
    mutate(funding_agency = ifelse(funding_agency %in% c("USAID", "Default"), funding_agency, "Other")) %>% 
    count(fiscal_year, funding_agency, indicator, is_kp, wt = targets, name = "targets") 
  
  
  df_agg <- df_agg %>%
    filter(indicator %ni% c("POP_EST", "PLHIV", "DIAGNOSED_SUBNAT", "PrEP_CURR",
                            "HTS_INDEX_NEWNEG", "HTS_INDEX")) %>% 
    group_by(fiscal_year, indicator, is_kp) %>% 
    mutate(share = targets / sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, indicator, is_kp) %>% 
    mutate(delta = targets - lag(targets, order_by = fiscal_year),
           pct_change = delta / lag(targets, order_by = fiscal_year)) %>% 
    ungroup() %>% 
    mutate(pct_change = case_when(is.infinite(pct_change) ~ 1,
                                   is.nan(pct_change) ~ NA,
                                   .default = pct_change))
 
  
# VIZ ---------------------------------------------------------------------
  
  df_viz <- df_agg %>% 
    filter(fiscal_year == 2025,
           is_kp == FALSE,
           !is.na(targets)
           ) %>%
    group_by(indicator) %>% 
    mutate(sort_order = case_when(funding_agency == "Default" ~ -Inf,
                                  funding_agency != "Other" & is_kp == FALSE ~ pct_change,
                                  # TRUE ~ 0
                                  ),
           usaid_value = case_when(funding_agency != "Other" & is_kp == FALSE ~ targets)) %>%
    fill(usaid_value, .direction = "updown") %>% 
    fill(sort_order, .direction = "updown") %>% 
    ungroup() %>% 
    mutate(fill_color = case_when(funding_agency == "USAID" & delta >= 0 ~ hw_electric_indigo,
                                  funding_agency == "USAID" ~ hw_orchid_bloom,
                                  TRUE ~ hw_slate),
           fill_alpha = ifelse(funding_agency == "USAID", .9, .3),
           indicator_val = case_when(indicator == "PrEP_CT" ~ glue("{indicator} [FY25 USAID Targets: {label_comma(1)(usaid_value)}]"),
                                     indicator %in% c("AGYW_PREV", "AGYW_PREV_D") ~ glue("{indicator} [FY25 PEPFAR Targets: {label_comma(1)(usaid_value)}]"),
                                     TRUE ~ glue("{indicator} [{label_comma(1)(usaid_value)}]")),
           indicator_val = fct_reorder(indicator_val, sort_order, .fun = max, 
                                   .na_rm = TRUE))

# MUNGE SHARES ------------------------------------------------------------

  
  df_viz %>% 
    ggplot(aes(pct_change, indicator_val,
               color = fill_color, alpha = fill_alpha)) +
    geom_vline(aes(xintercept = 0), color = "#505050") +
    geom_segment(aes(x = 0, xend = pct_change),
                 position = position_dodge(.5)) +
    geom_point(color = "white", position = position_dodge(.5), alpha = 1) +
    geom_point(position = position_dodge(.5)) +
    # facet_wrap(~is_kp) +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(-1, 1),
                       breaks = seq(-1, 1, .25),
                       oob = oob_squish,
                       label = percent_format(1)) +
    labs(x = NULL, y = NULL,
         title = glue("FY25 USAID TARGETS TEND TO SEE SIMILAR <span style='color: {hw_orchid_bloom};'>DECLINES</span> COMPARED TO FY23 TARGETS"),
         subtitle = glue("<span style='color:{hw_slate};'>Other agencies'</span> target change from last year are depicted in the background"),
         caption = glue("Calculation: Target Growth = (FY25 Targets/FY24 Targets) - 1
         Source: COP23_FY25 & ROP24_FY25-26 Target Setting Tool Dossier [2024-04-10] | Ref id: {ref_id}")) +
    si_style_xgrid() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown())

  si_preview()
  # si_save("Images/COP23Y2_TZA_target-delta.png")
  si_save("Graphics/COP23Y2_TZA_target-delta.svg")
  
  df_viz %>% 
    arrange(sort_order) %>%
    prinf()
  
  
  
  df_agg %>% 
    filter(funding_agency == "USAID",
           is_kp == FALSE,
           fiscal_year >= 2024) %>%
    group_by(indicator) %>% 
    mutate(delta = share - lag(share, order_by = fiscal_year)) %>% 
    fill(delta, .direction = "up") %>% 
    ungroup() %>% 
    mutate(fill_color = case_when(delta <= -.05 ~ hw_orchid_bloom,
                                  delta >= .05 ~ hw_electric_indigo,
                                  TRUE ~ hw_slate)) %>% 
    ggplot(aes(fiscal_year, share, fill = fill_color,
               group = indicator, label = percent_format(1)(share))) +
    geom_area(alpha = .2) +
    geom_text(family = "Source Sans Pro", size = 7/.pt,
              vjust = -.4,
              color = matterhorn) +
    facet_wrap(~fct_reorder2(indicator, fiscal_year, share)) +
    scale_y_continuous(limits = c(0, 1), 
                       expand = c(0.005, .005)) +
    scale_x_discrete(expand = c(0.005, .005)) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("USAID TARGETS TEND TO <span style='color: {hw_slate};'>FLATLINE</span> OR <span style='color: {hw_orchid_bloom};'>DECLINE</span> AS A SHARE OF TOTAL PEFPAR FY25 TARGETS"),
         subtitle = "USAID has the major share of PEPFAR/Tanzania's targets only in  OVC",
         caption = glue("Source: COP23_FY25 & ROP24_FY25-26 Target Setting Tool Dossier [2024-04-10] | Ref id: {ref_id}")) +
    si_style_nolines() +
    theme(panel.spacing.x = unit(2, "lines"),
          panel.spacing.y = unit(1, "lines"),
          strip.text = element_text(size = 9, hjust = .5),
          axis.text.y = element_blank(),
          plot.title = element_markdown())
  
  si_preview()
  si_save("Images/COP23Y2_TZA_target-share.png")
  