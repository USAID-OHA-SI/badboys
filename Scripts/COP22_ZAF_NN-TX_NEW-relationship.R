# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  investigate NN and TX_NEW in ZAF
# LICENSE:  MIT
# DATE:     2022-02-22
# UPDATED:  2022-02-23

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)


# IMPORT ------------------------------------------------------------------

  #download
  # googledrive::drive_download(googledrive::as_id("16sJBYfzaJl5c_1FTeEqm1l1K0ufIH9lJ/edit#gid=1524990347"),
  #                             path = si_path("path_downloads"))  
  
  df_dp <- si_path("path_downloads") %>% 
    return_latest("Data Pack_South Africa") %>% 
    read_excel(sheet = "Cascade", skip = 13)  

# MUNGE -------------------------------------------------------------------

  df_tx <- df_dp %>% 
    group_by(SNU1, PSNU) %>% 
    summarise(across(c(TX_CURR.New.T, TX_EVLT.T, TX_NEW.T, TX_CURR.T, TX_NET_NEW.T),
                     sum, na.rm = TRUE), .groups = "drop")
  
  #does TX_NEW match TX_CURR.NEW?
  df_tx %>% 
    ggplot(aes(TX_NEW.T, TX_CURR.New.T)) +
    geom_blank(aes(TX_CURR.New.T, TX_NEW.T)) +
    geom_abline(slope = 1) +
    geom_point()
  
  df_tx %>% 
    filter(TX_NET_NEW.T!= 0) %>% 
    mutate(ratio = TX_NET_NEW.T/TX_NEW.T) %>% 
    summary(ratio)
  
  #how does TX_NEW compare with NET_NEW
  df_tx %>% 
    ggplot(aes(TX_NEW.T, TX_NET_NEW.T)) +
    geom_blank(aes(TX_NET_NEW.T, TX_NEW.T)) +
    geom_abline(slope = 1, linetype = "dashed") +
    geom_abline(slope = .75, linetype = "dotted") +
    geom_abline(slope = .95, linetype = "dotted") +
    geom_point(size = 4, alpha = .7, color = "#909090") +
    geom_point(data = . %>% filter(str_detect(PSNU, "Sed")),
               color = old_rose, size = 5, alpha = .9) +
    geom_text(data = . %>% filter(str_detect(PSNU, "Sed")),
              aes(label = "Sedibeng District"), hjust = -.1, vjust = 1.2,
              family = "Source Sans Pro", color = matterhorn) +
    scale_x_continuous(label = comma) +
    scale_y_continuous(label = comma) +
    labs(title = "NET_NEW falls in the range of 75%-95% of TX_NEW for South Africa PSNUs" %>% toupper,
         subtitle = "In Sedibeng District NET_NEW is less than 60% of TX_NEW",
         caption = "Source: MASTER_Data Pack_South Africa_20220121124345_v02.18 16h28 LATEST.xlsx") +
    si_style()
  
  
  #how does TX_NEW compare with NET_NEW
  df_tx %>% 
    filter(str_detect(PSNU, "Sed")) %>% 
    select(PSNU, TX_NET_NEW.T, TX_NEW.T) %>% 
    mutate(delta = (TX_NET_NEW.T/TX_NEW.T)) %>% 
    ggplot(aes(delta, PSNU)) +
    geom_segment(aes(x = 1, xend = delta, y = PSNU, yend = PSNU)) +
    geom_point() +
    geom_vline(xintercept = 1)
  
  
  #how does TX_NEW compare with NET_NEW
  df_tx %>% 
    ggplot(aes(TX_EVLT.T, TX_NEW.T)) +
    geom_blank(aes(TX_NEW.T, TX_EVLT.T)) +
    geom_abline(slope = 1) +
    geom_point() 
  
  
  #inspect Sedibeng District
  df_sedi <- df_dp %>% 
    filter(str_detect(PSNU, "Sed"),
           Age != "<01") %>% 
    select(PSNU, TX_CURR.T_1, TX_CURR.New.T, TX_EVLT.T, TX_NEW.T:TX_CURR.T, TX_NET_NEW.T)
  
  df_sedi %>% 
    group_by(PSNU) %>% 
    summarise(across(c(TX_NEW.T, TX_NET_NEW.T),
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_longer(-PSNU,
                 names_to = "indicator") %>% 
    ggplot(aes(value, indicator)) +
    geom_blank(aes(x = value *1.1)) +
    geom_col() +
    geom_text(aes(label = comma(value)), hjust = 1.2,
              family = "Source Sans Pro SemiBold", color = "white") +
    labs(title = "Large disconnect between TX_NEW and NET_NEW in Sedibeng District" %>% toupper,
         subtitle = "aggregated age/sex excluding <01",
         caption = "Source: MASTER_Data Pack_South Africa_20220121124345_v02.18 16h28 LATEST.xlsx") +
    si_style_xgrid() +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank())
  
  df_sedi %>% 
    mutate(`Retention Loss` = round((TX_CURR.T_1 * (1-TX_RET.Already.T)) + (TX_NEW.T * (1-TX_RET.New.T)))) %>% 
    group_by(PSNU) %>% 
    summarise(across(c(TX_NEW.T, TX_NET_NEW.T, `Retention Loss`),
                     sum, na.rm = TRUE), .groups = "drop") %>% 
    pivot_longer(-PSNU,
                 names_to = "indicator") %>% 
    mutate(disp_value = ifelse(indicator == "Retention Loss", 2600 + value, value),
           hidden = case_when(indicator == "Retention Loss" ~ 2600)) %>% 
    ggplot(aes(disp_value, indicator)) +
    geom_blank(aes(x = value *1.1)) +
    geom_col() +
    geom_col(data = . %>% filter(indicator == "Retention Loss"),
             aes(disp_value), fill = old_rose) +
    geom_col(aes(hidden), fill = "white") +
    geom_text(aes(label = comma(value)), hjust = 1.2,
              family = "Source Sans Pro SemiBold", color = "white") +
    labs(title = "Disconnect the result of new and already on retention rate" %>% toupper,
         subtitle = "aggregated age/sex excluding <01",
         caption = "Source: MASTER_Data Pack_South Africa_20220121124345_v02.18 16h28 LATEST.xlsx") +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank())
  
  
  df_dp %>% 
    group_by(PSNU) %>% 
    summarise(vlt_planned = sum(TX_PVLS.D.Routine.T, na.rm = TRUE)/sum(TX_EVLT.T, na.rm = TRUE), 
              tx_curr = sum(TX_CURR.T, na.rm = TRUE),
              .groups = "drop") %>% 
    arrange(vlt_planned) %>% 
    # prinf()
    ggplot(aes(tx_curr, vlt_planned)) +
    geom_point(size = 4, alpha = .7, na.rm = TRUE) +
    geom_point(data = . %>% filter(str_detect(PSNU, "Sed")),
               color = old_rose, size = 5, alpha = .9)
  
  
  #how does the ratio of TX_NEW to TX_CURR compare across PSNUs?
  df_tx %>% 
    filter(TX_CURR.T != 0) %>% 
    mutate(share = TX_NEW.T/TX_CURR.T,
           PSNU = str_extract(PSNU, "(?<=[:alnum:]{2} ).*(?= \\[#S)") %>% str_remove_all(" (Municipality|District|Metropolitan)")) %>% 
    ggplot(aes(share, fct_reorder(PSNU, share), size = TX_CURR.T)) +
    geom_point(alpha = .7, na.rm = TRUE) +
    geom_point(data = . %>% filter(str_detect(PSNU, "Sed")),
               color = old_rose, alpha = .9) +
    scale_size(label = label_number_si()) +
    labs(x = NULL, y = NULL,
         title = "",
         subtitle = "TX_NEW share of TX_CURR") +
    scale_x_continuous(label = percent_format(1)) +
    si_style()
