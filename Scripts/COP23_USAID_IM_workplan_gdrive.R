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

df_folder %>% 
  select(id, ou_name) %>% 
  write_csv("Dataout/COP_workplan_upload_crosswalk.csv")


# push to drive based on OU name and associated ID


