## PROJECT:  badboys
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  TZA COP21 Support - Index Testing
## LICENSE:  MIT
## DATE:     2021-01-26


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glamr)
library(glitr)
library(extrafont)
library(scales)
library(glue)
library(ggtext)
library(ggrepel)
library(gisr)
library(sf)



# IMPORT DATA -------------------------------------------------------------

df_tza <- si_path() %>% 
  list.files("PSNU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")


shp_tza <- read_sf("GIS/Tanzania_PROD_4_Region_RegionLsib_2019_May.shp")

terr <- terrain_map("United Republic of Tanzania", terr_path = si_path("path_raster"), mask = TRUE)



# Index Quarterly Trends --------------------------------------------------
  
df_qtr <- df_tza %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           snu1 != "_Military Tanzania") %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
    group_by(fiscal_year, indicator, mod_type) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE)

df_qtr %>% 
  mutate(lab = case_when(mod_type == "Index" ~ glue("{comma(val/1000, 1)}k"))) %>% 
  ggplot(aes(period, val, fill = fct_rev(mod_type))) +
  geom_col(alpha = .7) +
  geom_text(aes(label = lab),
            family = "Source Sans Pro", color = "#505050",
            vjust = -.5) +
  facet_grid(indicator ~ ., scales = "free_y") +
  scale_fill_si("denims") +
  scale_y_continuous(label = comma) +
  scale_x_discrete(breaks = c("FY18Q1", "FY18Q3", "FY19Q1", "FY19Q3", "FY20Q1", "FY20Q3")) +
  labs(x = NULL, y = NULL,
       title = "INCREASE IN <span style = 'color:#002065'>INDEX TESTING</span> SHARE OF <span style = 'color:#bfddff'>ALL POSITIVE</span> TEST",
       subtitle = "Tanzania | USAID",
       caption = "Source: FY20Q4c MSD") +
  si_style_ygrid() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_text(face = "bold"))
  
  si_save("COP21_TZA_Index-share.png", path = "Images")


# Index share of tests/positives

  df_share_reg <- df_tza %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2020,
           snu1 != "_Military Tanzania") %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
    group_by(snu1, indicator, mod_type) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(cumulative > 0) %>% 
    group_by(snu1, indicator) %>% 
    mutate(share = cumulative/sum(cumulative)) %>% 
    ungroup() %>% 
    group_by(indicator) %>% 
    mutate(share_total = cumulative/sum(cumulative)) %>% 
    ungroup()
  
  df_share_reg %>%
    filter(mod_type == "Index") %>%
    group_by(snu1) %>% 
    mutate(max = max(share),
           highlight = max > .5,
           lab_hts = case_when(max > .5 & indicator == "HTS_TST" ~ percent(share, 1)),
           lab_pos = case_when(max > .5 & indicator == "HTS_TST_POS" ~ glue("{percent(share, 1)} {snu1}"))) %>% 
    ungroup() %>% 
    ggplot(aes(indicator, share, group = snu1, color = highlight)) +
    geom_vline(xintercept = c(1, 2), color = "#d3d3d3") +
    geom_hline(yintercept = 0, color = "#d3d3d3") +
    geom_text_repel(aes(label = lab_hts), hjust = 1.2, direction = "y",
              family = "Source Sans Pro", size = 3, color = "#505050") +
    geom_text_repel(aes(label = lab_pos), hjust = -1.2, direction = "y",
                    family = "Source Sans Pro", size = 3, color = "#505050") +
    geom_path(data = . %>% filter(highlight == FALSE), size = .9) +
    geom_point(data = . %>% filter(highlight == FALSE), aes(size = share_total)) +
    # geom_point(data = . %>% filter(highlight == FALSE), size = 4) +
    geom_path(data = . %>% filter(highlight == TRUE), size = .9) +
    geom_point(data = . %>% filter(highlight == TRUE), aes(size = share_total)) +
    # geom_point(data = . %>% filter(highlight == TRUE), size = 4) +
    scale_x_discrete(expand = c(.05, .05)) +
    scale_color_manual(values = c(trolley_grey_light, burnt_sienna)) +
    labs(x = NULL, y = NULL,
         title = "SIX REGIONS HAD INDEX TESTING CONTRIBUTE MORE THAN 50% OF POSITIVES",
         subtitle = "Tanzania | USAID FY20 | Index testing share of all tests",
         caption = "Source: FY20Q4c MSD") +
    si_style() +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank())
  
  si_save("COP21_TZA_Index-slope.png", path = "Images")
  
# What does achievement look like across partners?
  
  df_achv <- df_tza %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other"),
           partner = recode(primepartner,
                            "Baylor College of Medicine" = "Baylor",
                            "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor",
                            "DELOITTE CONSULTING LIMITED" = "Deloitte",
                            "Elizabeth Glaser Pediatric AIDS Foundation" = "EGPAF",
                            "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                            "JSI Research And Training Institute, INC." = "JSI",
                            "JHPIEGO CORPORATION" = "JHPIEGO",
                            "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" = "THPS",
                            "Family Health International" = "EpiC"
           )) %>% 
    group_by(fiscal_year, partner, indicator, mod_type) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(achievement = cumulative/targets)
  
   
  
  df_achv <- df_achv %>% 
    mutate(fy20_val = ifelse(fiscal_year == "2020" & mod_type == "Index", cumulative, 0))
  
  df_achv %>% 
    filter(mod_type == "Index") %>% 
    ggplot(aes(fiscal_year)) +
    geom_col(aes(y = cumulative), fill = scooter) +
    geom_errorbar(aes(ymin = targets, ymax = targets), color = golden_sand, size = 1) +
    facet_wrap(~ fct_reorder(partner, fy20_val, max, .desc = TRUE), scales = "free_y") +
    scale_y_continuous(label = comma) +
    labs(x = NULL, y = NULL, 
         title = "EGPAF NOT ABLE TO REACH INDEX POS TARGETS LAST 2 YEARS",
         subtitle = "Tanzania | USAID | FY Target Achievement",
         caption = "Source: FY20Q4c MSD") +
    si_style_ygrid()
  
  si_save("COP21_TZA_Index-partner.png", path = "Images")
  
# Where do we have low achievement - region?

  df_achv_reg <- df_tza %>% 
    filter(fundingagency == "USAID",
           fiscal_year != 2021,
           indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
    group_by(fiscal_year, snu1, snu1uid, indicator, mod_type) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(achievement = cumulative/targets,
           achievement = na_if(achievement, Inf))
  
  df_map <- df_achv_reg %>% 
    full_join(shp_tza, by = c("snu1uid" = "uid"))
  
  df_map <- df_map %>% 
    filter(!is.na(fiscal_year)) %>% 
    mutate(group = case_when(achievement < .75 ~ burnt_sienna,
                             achievement < .9 ~ golden_sand,
                             achievement >= .9 ~ genoa_light),
           group_lab = case_when(achievement < .75 ~ "<75%",
                                  achievement < .9 ~ "75-89%",
                                  achievement >= .9 ~ "+90%"),
           group_lab = factor(group_lab, c("<75%", "75-89%", "+90%")),
           snu_lab = case_when(achievement < .75 ~ snu1))
  
  
  terr + 
    geom_sf(data = df_map, aes(geometry = geometry, fill = group_lab), alpha = .7) +
    geom_sf_text(data = df_map, aes(geometry = geometry, label = snu_lab), size = 2,
                 family = "Source Sans Pro", color = "#505050") +
    facet_grid(mod_type ~ fiscal_year, switch = "y") +
    scale_fill_manual(values = c(burnt_sienna, golden_sand, genoa_light)#,
                      # na.value = NULL
                      ) +
    labs(x = NULL, y = NULL, fill = "FY20 Target Achievement",
         title = "MANY REGIONS FEEL SHORT OF INDEX TESTING TARGETS IN FY20",
         subtitle = "Tanzania | USAID",
         caption = "Source: FY20Q4c MSD") +
    si_style() +
    theme(strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold", vjust = 0, hjust = 0),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank()
    )
  
  si_save("COP21_TZA_Index-regional-map.png", path = "Images")
  
# Are there differences by age/sex?

  df_agesex <- df_tza %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year == 2020) %>% 
    mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
    group_by(fiscal_year, ageasentered, sex, indicator, mod_type) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(mod_type, cumulative)

  df_agesex %>% 
    filter(sex != "Unknown Sex",
           ageasentered != "Unknown Age") %>% 
    mutate(value = ifelse(sex == "Male", -Index, Index),
           lab = glue("{comma(Index/1000, .1)}k")) %>% 
    ggplot(aes(value, ageasentered, fill = sex)) +
    geom_col() +
    geom_text(aes(label = lab, hjust = ifelse(sex == "Male", 1.2, -.4)),
              family = "Source Sans Pro", size = 4, color = "#505050") +
    geom_vline(xintercept = 0) +
    expand_limits(x = c(-6000,6000)) +
    labs(x = NULL, y = NULL,
         title = "LESS POSITIVE INDEX TESTS FOR <span style = 'color:#287c6f'>MALES</span> THAN <span style = 'color:#8980cb'>FEMALES</span> in FY20",
         subtitle = "Tanzania | USAID",
         caption = "Source: FY20Q4c MSD") +
    scale_x_continuous(breaks = seq(-6000,6000, 1000)) +
    scale_fill_manual(values = c(moody_blue, genoa)) +
    si_style() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_markdown()
          )
  
  si_save("COP21_TZA_Index-age-sex.png", path = "Images")
  
    