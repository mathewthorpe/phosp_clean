# PHOSP-COVID analysis: Import True Colours data
## Centre for Medical Informatics, Usher Institute, University of Edinburgh 2021
## These are received via an encrypted data transfer and stored in the data folder. 

# Packages ----------------------------------------
library(tidyverse)
library(lubridate)

# Import data -------------------------------------
path = "/home/common/phosp/truecolours/tcphosp.20220511/"
import_files = list.files(path, full.names = TRUE)

phosp = readRDS("/home/common/phosp/cleaned/full/phosp_2022-07-15_0400_full.rds")

#ldf <- lapply(import_files, read.csv)

new_tc = import_files %>% 
  map(~ read_csv(.) %>% 
        rename_with(~ paste0(.x, "_tc")) %>% # Add _tc to each variable name
        rename("phosp_id" = 1,#               # Bring back phosp_id and make new trucolours date common across all
               "date_tc" = 2) %>% 
        mutate(date_tc = as_date(date_tc)) %>% # Get rid of time
        distinct(phosp_id, date_tc, .keep_all = TRUE)
  )

tc = import_files %>% 
  map(~ read_csv(.) %>% 
        rename_with(~ paste0(.x, "_tc")) %>% # Add _tc to each variable name
        rename("phosp_id" = 1,#               # Bring back phosp_id and make new trucolours date common across all
               "date_tc" = 2) %>% 
        mutate(date_tc = as_date(date_tc)) %>% # Get rid of time
        distinct(phosp_id, date_tc, .keep_all = TRUE) # For now, only keep the first submission where more than one on a day
  )

# Clean --------------------------------------------
# Only exmaple of a single instrument being done before midnight, and everything else after
tc[[10]][which(tc[[10]]$phosp_id == "070-00368"),]

tc[[10]] = tc[[10]] %>% 
  mutate(
    date_tc = if_else(phosp_id == "070-00368" & date_tc == ymd("2021-04-18"), ymd("2021-04-19"), date_tc)
  )

# Join/collapse all tables --------------------------
tc = tc %>% 
  reduce(full_join, by = c("phosp_id", "date_tc")) %>% 
  arrange(phosp_id, date_tc)

# Extract study_id from phosp
phosp_study_id = phosp %>% select(study_id, phosp_id) %>% 
  distinct(phosp_id, .keep_all = TRUE) %>% 
  drop_na() # Check with distinct that one-to-one relationship
  
# Add require variables for matching ----------------
tc = tc %>% 
  inner_join(phosp_study_id) %>% # Only keep TrueColours data if patient is on REDCap
  mutate(redcap_repeat_instrument = "True Colours") %>% 
  group_by(study_id) %>% 
  mutate(redcap_repeat_instance = row_number()) %>% 
  relocate(study_id, redcap_repeat_instrument, redcap_repeat_instance)

# Join phosp ----------------------------------------
# phosp = phosp %>% 
#   full_join(tc)

# For now export as separate object
phosp_tc = tc

# Clean up -----------------------------------------
rm(tc, path, import_files, phosp_study_id)

# Not run ----
# tc %>% 
#   select(c(
#     phosp_id,
#     date_tc,
#     gad7_summary_tc,
#     phq9_summary_tc,
#     pcl5_summary_tc,
#     d12_summary_tc,
#     facit_v4_summary_tc)
#   ) %>% 
#   drop_na()
# 163


new_tc[1]
