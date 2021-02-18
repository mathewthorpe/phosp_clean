# PHOSP-COVID analysis: DATA PREP
# Cleaning and preparation of variables
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# Packages
library(finalfit)
library(tidyverse)
library(lubridate)

# Define tier 2 --------------------------------------------------------------------------
tier2_study_id = phosp %>% 
  filter(!is.na(crf3b_visit)) %>% 
  distinct(study_id) %>% 
  pull(study_id)

# Variable definitions-------------------------------------------------------------------
phosp = phosp %>% 
  mutate(
    
    # Tier 
    tier = if_else(study_id %in% tier2_study_id, 2, 1),
    
    # Explanatory variables ----
    ## Ethnicity
    crf1b_eth_5levels = case_when(
      crf1b_eth == "(1) White - English / Welsh / Scottish / Northern Irish / British" |                           
        crf1b_eth == "(2) White - Irish" | 
        crf1b_eth == "(3) White - Gypsy or Irish Traveller" |
        crf1b_eth == "(4) White - Any other White background" ~ "White", 
      crf1b_eth == "(5) Mixed/ Multiple Ethnic Backgrounds - White and Black Caribbean" |
        crf1b_eth == "(6) Mixed/ Multiple Ethnic Backgrounds - White and Black African" |
        crf1b_eth == "(7) Mixed/ Multiple Ethnic Backgrounds  - White and Asian" |
        crf1b_eth == "(8) Mixed/ Multiple Ethnic Backgrounds  - Any other Mixed/ Multiple ethnic background" ~ "Mixed", 
      crf1b_eth == "(9) Asian/ Asian British -  Indian" |
        crf1b_eth == "(10) Asian/ Asian British - Pakistani" | 
        crf1b_eth == "(11) Asian/ Asian British - Bangladeshi" |
        crf1b_eth == "(12) Asian/ Asian British - Chinese" |
        crf1b_eth == "(13) Asian/ Asian British - Any other Asian background" ~ "Asian", 
      crf1b_eth == "(14) Black/ African/ Caribbean/ Black British - African" |
        crf1b_eth == "(15) Black/ African/ Caribbean/ Black British - Caribbean" |
        crf1b_eth == "(16) Black/ African/ Caribbean/ Black British - Any other Black/ African/ Caribbean background" ~ "Black",
      crf1b_eth == "(17) Other ethnic group - Arab" |
        crf1b_eth == "(18) Other ethnic group - Any other ethnic group" ~ "Other",
      crf1b_eth == "Prefer not to say" ~ NA_character_
    ) %>% 
      ff_label("Ethinicity"),
    
    ## Diabetes yes no
    crf1a_com_mer_diab_yn = if_else(crf1a_com_mer_diab == "Type 1" | crf1a_com_mer_diab == "Type 2", "Yes", "No") %>% 
      ff_label("Diabetes"),
    
    ## Malignancy
    crf1a_com_mh_stm_yn = if_else(crf1a_com_mh_stm == "Localised" | crf1a_com_mh_stm == "Metastatic", "Yes", "No") %>% 
      ff_label("Malignancy"),
    
    ## Symptoms - change N/K to missing
    across(crf1a_fever_history:crf1a_bleeding, fct_recode, NULL = "N/K")
    
  ) %>% 
  
  mutate( # (new mutate needed to utilise diabetes and malig variables)
    
    ## Comorbidity count (no missings here)
    no_comorbid = select(., 
                         # Count these variables
                         crf1a_com_card_mi:crf1a_com_id_other_yn,
                         crf1a_com_mer_diab_yn,
                         crf1a_com_mh_stm_yn,
                         
                         # But don't count these, note not counting the "others"
                         -c(crf1a_com_card_catia,
                            crf1a_com_card_other_yn,
                            crf1a_com_card_other,
                            crf1a_com_neupsy_other,
                            crf1a_com_res_other_yn,
                            crf1a_com_res_other,
                            crf1a_com_rheu_other_yn,
                            crf1a_com_rheu_other,
                            crf1a_com_gast_ld,
                            crf1a_com_gast_other_yn,
                            crf1a_com_gast_other,
                            crf1a_com_mer_other_yn,
                            crf1a_com_mer_ckd_stage,
                            crf1a_com_mer_other,
                            crf1a_com_mer_diab,
                            crf1a_com_mer_diab_com,
                            crf1a_com_mh_stm,
                            crf1a_com_mh_stm_detail,
                            crf1a_com_mh_leuk_detail,
                            crf1a_com_mh_lymp,
                            crf1a_com_mh_other,
                            crf1a_com_id_cvh,
                            crf1a_com_id_mt,
                            crf1a_com_id_other_yn)
    ) %>% 
      {. == "Yes"} %>% 
      rowSums(na.rm = TRUE) %>% 
      ff_label("Number of comorbidities"), 
    
    # Dates ----
    ## Symptom duration
    crf1a_symptom_duration = (ymd(crf1a_date_adm) - ymd(crf1a_date_first_symptoms)) %>% 
      as.numeric("days") %>% 
      ff_label("Symptom duration (days)"),
    
    ## Admission duration
    crf1a_admission_duration = (ymd(crf1a_date_dis) - ymd(crf1a_date_adm)) %>% 
      as.numeric("days") %>% 
      ff_label("Admission duration (days)"),
    
    
    # Outcomes ----
    ## Respiratory support
    crf1a_resp_support_8levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "ECMO", 
      crf1a_treat_rrt == "Yes" ~ "RRT",
      crf1a_o2_imv == "Yes" ~ "IMV", 
      crf1a_o2_hfn == "Yes" ~ "HFN", 
      crf1a_o2_blniv == "Yes" ~ "BIPAP", 
      crf1a_o2_cpapv == "Yes" ~ "CPAP", 
      crf1a_o2_supp == "Yes" ~ "O2",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "No respiratory support"
    ) %>% 
      factor() %>% 
      fct_relevel("No respiratory support", "O2", "CPAP", "BIPAP", "HFN", "IMV", "RRT", "ECMO") %>% 
      ff_label("Maximal organ support"),
    
    
    crf1a_resp_support_4levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "IMV/RRT/ECMO", 
      crf1a_treat_rrt == "Yes" ~ "IMV/RRT/ECMO",
      crf1a_o2_imv == "Yes" ~ "IMV/RRT/ECMO", 
      crf1a_o2_hfn == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_blniv == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_cpapv == "Yes" ~ "CPAP/BIPAP/HFN", 
      crf1a_o2_supp == "Yes" ~ "O2",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "No respiratory support"
    ) %>% 
      factor() %>% 
      fct_relevel("No respiratory support", "O2", "CPAP/BIPAP/HFN", "IMV/RRT/ECMO") %>% 
      ff_label("Maximal organ support")
    
  ) %>% 
  ff_relabel_df(phosp)


# Hospital discharge event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_hosp = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "Hospital Discharge") %>% 
  purrr::discard(~all(is.na(.)))

# 6 week event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_six = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "6 Weeks")%>% 
  purrr::discard(~all(is.na(.)))

# Temp out for testing -----------------------------------------------
# saveRDS(phosp_hosp, "phosp_hosp.rds")
# saveRDS(phosp_six, "phosp_six.rds")

