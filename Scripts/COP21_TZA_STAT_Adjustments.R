# PROJECT:  badboys
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  identify and export PSNUs where D != N
# LICENSE:  MIT
# DATE:     2021-05-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(tameDP)
  library(janitor)
  library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------

path <- "C:/Users/achafetz/Downloads/PSNU x IM Fixes - May62021.xlsx"


psnu_pmtct <- c("Moshi MC", "Arusha CC", "Babati TC", "Bukoba MC", 
                "Mbeya CC", "Sumbawanga MC", "Mpanda MC", "Rungwe DC", 
                "Njombe TC", "Songea MC", "Iringa MC", "Mtwara MC", 
                "Singida MC", "Karagwe DC", "Morogoro DC", "Nyamagana MC", 
                "Dodoma MC", "Geita TC", "Lindi MC")

psnu_tb <- c("Moshi MC", "Singida MC", "Arusha CC", "Monduli DC", "Tabora MC", 
             "Temeke MC", "Ilala MC", "Kinondoni MC", "Songwe DC", "Mbeya CC", 
             "Mbozi DC", "Nyamagana MC", "Kahama TC", "Njombe TC", "Songea MC", 
             "Morogoro MC", "Bariadi TC", "Tanga CC", "Rungwe DC", "Iringa MC",
             "Mtwara MC", "Kibaha TC")

# IMPORT ------------------------------------------------------------------

  df_dp <- import_dp(path) %>% 
    glimpse()

# MUNGE -------------------------------------------------------------------

df_dp <- df_dp %>% 
  mutate(row_n = row_number()+14)

df_dp <- df_dp %>% 
  mutate(psnu = str_extract(psnu, "^.*(?= \\[#)"))


# PMTCT -------------------------------------------------------------------

df_pmtct <- df_dp %>% 
  filter(str_detect(indicator_code, "^PMTCT_STAT"),
         psnu %in% psnu_pmtct) %>% 
  select(row_n, psnu, indicator_code, age, sex, datapacktarget, ends_with("share")) %>% 
  remove_empty("cols") %>% 
  mutate(across(c(datapacktarget, ends_with("share")), as.numeric),
         numeratordenom = ifelse(str_detect(indicator_code, "\\.D\\."), "D", "N")) %>% 
  rowwise() %>% 
  mutate(lrg_ptnr_share = sum(`18060_dsd_share`, `18237_dsd_share`, `18627_dsd_share`, `80095_dsd_share`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(psnu, numeratordenom) %>% 
  mutate(max_value = case_when(datapacktarget == max(datapacktarget)~TRUE)) %>% 
  ungroup()

write_csv(df_pmtct, "Dataout/COP21_TZA_PMTCT-adj.csv", na = "")


# TB ----------------------------------------------------------------------

df_tb <- df_dp %>% 
  filter(str_detect(indicator_code, "^TB_STAT"),
         psnu %in% psnu_tb) %>% 
  select(row_n, psnu, indicator_code, age, sex, datapacktarget, ends_with("share")) %>% 
  remove_empty("cols") %>% 
  mutate(across(c(datapacktarget, ends_with("share")), as.numeric),
         numeratordenom = ifelse(str_detect(indicator_code, "\\.D\\."), "D", "N")) %>% 
  rowwise() %>% 
  mutate(lrg_ptnr_share = sum(`18060_dsd_share`, `18237_dsd_share`, `18488_dsd_share`, 
                              `18627_dsd_share`, `80095_dsd_share`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(psnu, numeratordenom) %>% 
  mutate(max_value = case_when(datapacktarget == max(datapacktarget)~TRUE)) %>% 
  ungroup()

write_csv(df_tb, "Dataout/COP21_TZA_TB-adj.csv", na = "")



# CREATE FULL SET TO REPLACE ----------------------------------------------

sheets <- excel_sheets("Dataout/COP21_TZA_STAT-adj.xlsx")
df_adj <- map_dfr(sheets,
                  ~read_excel("Dataout/COP21_TZA_STAT-adj.xlsx",
                              sheet = .x, skip = 2))

df_adj <- df_adj %>% 
  filter(`adjusted?` == TRUE) %>% 
  select(row_n, psnu, indicator_code, lrg_ptnr_orig_sh = lrg_ptnr_share, 
         large_adj, adjusted = `adjusted?`)

df_dp_adj <- df_dp %>% 
  select(psnu:id, ends_with("share"), row_n) %>% 
  inner_join(df_adj)

df_dp_adj <- df_dp_adj %>% 
  rename(`82164_orig_sh` = `82164_dsd_share`) %>% 
  mutate(across(ends_with("share"), ~ case_when(!is.na(.) ~ large_adj))) %>% 
  mutate(`82164_dsd_share` = 1-large_adj, .before = `82164_orig_sh`) %>% 
  select(-large_adj)

df_dp_adj_full <- df_dp %>% 
  filter(!row_n %in% unique(df_dp_adj$row_n)) %>%
  select(psnu:id, ends_with("share"), row_n) %>% 
  mutate(across(ends_with("share"), as.double)) %>% 
  bind_rows(df_dp_adj) %>% 
  arrange(row_n)

write_csv(df_dp_adj_full, "Dataout/COP21_TZA_STAT-adj_allshares.csv", na = "")
