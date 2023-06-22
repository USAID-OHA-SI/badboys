# PROJECT:  badboys
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  COP23 Targets for USAID workplans - push to google drive
# REF ID:   ade25da8 
# LICENSE:  MIT
# DATE:     2023-06-21
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(glue)
library(grabr)
library(googledrive)

ref_id <- "ade25da8"

# IMPORT -----------------------------------------------------------------

#read in parent folder and filter out non country folders
master_folder <- drive_ls(path = as_id("1Simrf2tgulJ1SpcMjt1YDosmgZtWmqji")) %>% 
  filter(name %ni% c("*COP23/FY24 Work Plans - Global Materials",
                     "*Previous Year Work Plans - Global Materials",
                     "IP Work Planning")) %>% 
  rename(ou_name = name,
         ou_id = id,
         drive_resource_ou = drive_resource)


# PUSH TO DRIVE -----------------------------------------------------------

#Get unique list of OU folder ids
id_list <- master_folder$ou_id

# Create function to recursively list folders in each OU folder
list_folders <- function(folder_id) {
  
  #look within OU folder and filter to COP23 folder
  folders <- drive_ls(folder_id) %>% 
    filter(name %in% c("COP23 / FY24", "COP23/FY24"))
  
  # add check here if folders is null, create folder
  if(nrow(folders) == 0) {
    drive_mkdir("COP23 / FY24",
                path = as_id(folder_id))
    
    folders <- drive_ls(folder_id) %>% 
      filter(name %in% c("COP23 / FY24", "COP23/FY24"))
  } else {
    print("COP23 / FY24 Folder exists")
  }
  
  #grab those subfolder ids
  cop23_ids <- folders$id
  
  #look within sub folders to get IM target table folders; add parent id back to df
  sub_folders <- drive_ls(cop23_ids) %>% 
    filter(name %in% c("IM Target Tables")) 
  
  # add check here if folders is null, create folder
  if(nrow(sub_folders) == 0) {
    drive_mkdir("IM Target Tables",
                path = as_id(cop23_ids))
    
    sub_folders <- drive_ls(cop23_ids) %>% 
      filter(name %in% c("IM Target Tables"))
  } else {
    print("IM Target Tables exists")
  }
  
  sub_folders <- sub_folders %>% 
    filter(name %in% c("IM Target Tables")) %>% 
    mutate(parent_folder_id = folder_id)
  
  return(sub_folders)

}

#apply function
df_sub <- map_dfr(id_list, list_folders)

#left join back the OU folder info
df_folder <- df_sub %>% 
  left_join(master_folder, by = c("parent_folder_id" = "ou_id"))


# push to drive based on OU name and associated ID


# # REGIONAL ---------------------------------------
# 
# #get OU level g_ids
# regional_folder <- master_folder %>% 
#   filter(str_detect(ou_name, "Region"))
# 
# id_list <- regional_folder$ou_id
# 
# # Function to recursively list folders in each OU folder
# list_regional_folders <- function(folder_id) {
#   
#   #look within OU folder and filter to COP23 folder
#   folders <- drive_ls(folder_id) %>% 
#     filter(name %in% c("COP23 / FY24", "COP23/FY24"))
#   
#   #grab those subfolder ids
#   cop23_ids <- folders$id
#   
#   #look within sub folders to get IM target table folders; add parent id back to df
#   sub_cntry_folders <- drive_ls(cop23_ids) %>% 
#     #filter(name %in% c("IM Target Tables")) %>% 
#     mutate(ou = str_extract(name, paste0(".*?(?=", " ROP23 IP Workplan", ")"))) %>% 
#     rename(cntry_id = id)
#   
#   cntry_map <- sub_cntry_folders %>% 
#     select(ou, cntry_id)
#   
#   
#   # #grab those subfolder ids
#   # sub_cntry_ids <- sub_cntry_folders$cntry_id
#   # 
#   # #look within sub folders to get IM target table folders; add parent id back to df
#   # regional_folders <- drive_ls(sub_cntry_ids) %>% 
#   #   filter(name %in% c("IM Target Tables")) %>% 
#   #   mutate(parent_folder_id = sub_cntry_ids)
#   
#   return(cntry_map)
#   
# }

# #apply function
# df_sub <- map_dfr(id_list, list_regional_folders)
# 
# #left join back the OU folder info
# df_folder <- df_sub %>% 
#   left_join(master_folder, by = c("parent_folder_id" = "ou_id"))



