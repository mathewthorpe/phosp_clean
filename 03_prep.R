# PHOSP-COVID analysis: DATA PREP
# Cleaning and preparation of variables
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# Packages
library(finalfit)
library(tidyverse)
library(lubridate)
library(rspiro)

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
    
    crf1b_eth_pft = case_when(
      crf1b_eth == "(1) White - English / Welsh / Scottish / Northern Irish / British" |
        crf1b_eth == "(2) White - Irish" |
        crf1b_eth == "(3) White - Gypsy or Irish Traveller" |
        crf1b_eth == "(4) White - Any other White background" ~ 1,
      crf1b_eth == "(5) Mixed/ Multiple Ethnic Backgrounds - White and Black Caribbean" |
        crf1b_eth == "(6) Mixed/ Multiple Ethnic Backgrounds - White and Black African" |
        crf1b_eth == "(7) Mixed/ Multiple Ethnic Backgrounds  - White and Asian" |
        crf1b_eth == "(8) Mixed/ Multiple Ethnic Backgrounds  - Any other Mixed/ Multiple ethnic background" ~ 5,
      crf1b_eth == "(9) Asian/ Asian British -  Indian" |
        crf1b_eth == "(10) Asian/ Asian British - Pakistani" |
        crf1b_eth == "(11) Asian/ Asian British - Bangladeshi" ~ 4,
      crf1b_eth == "(12) Asian/ Asian British - Chinese" ~ 3,
      crf1b_eth == "(13) Asian/ Asian British - Any other Asian background" ~ 5,
      crf1b_eth == "(14) Black/ African/ Caribbean/ Black British - African" |
        crf1b_eth == "(15) Black/ African/ Caribbean/ Black British - Caribbean" |
        crf1b_eth == "(16) Black/ African/ Caribbean/ Black British - Any other Black/ African/ Caribbean background" ~ 2,
      crf1b_eth == "(17) Other ethnic group - Arab" |
        crf1b_eth == "(18) Other ethnic group - Any other ethnic group" ~ 5,
      crf1b_eth == "Prefer not to say" ~ NA_real_
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
    crf1a_symptom_duration = (crf1a_date_adm - crf1a_date_first_symptoms) %>% 
      as.numeric("days") %>% 
      ff_label("Symptom duration (days)"),
    
    ## Admission duration
    crf1a_admission_duration = (crf1a_date_dis - crf1a_date_adm) %>% 
      as.numeric("days") %>% 
      ff_label("Admission duration (days)"),
    
    # Anthro values ----
    ## Height (character)
    crf3a_rest_height = parse_number(crf3a_rest_height),
    crf3a_rest_weight = parse_number(crf3a_rest_weight),
    crf3a_bmi = (crf3a_rest_weight / (crf3a_rest_height / 100)^2) %>% 
      ff_label("BMI"),
    
    # PROMs
    eq5d5l_summary_pre = parse_number(eq5d5l_summary_pre),
    eq5d5l_summary = parse_number(eq5d5l_summary),
    eq5d5l_summary_delta = (eq5d5l_summary - eq5d5l_summary_pre) %>% 
      ff_label("How good or bad is your health overall? 3 months vs pre-covid"),
    
    
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

# Within phosp, fill within patients across rows  ---------------------------------------
## This can be changed to joins in the future if causes any issues
## Currently this is for PFT calculations
phosp = phosp %>%  
  group_by(study_id) %>% 
  fill(age_admission, crf1a_sex,  crf1b_eth_pft, crf3a_rest_height, 
       .direction = "downup") %>% 
  ungroup() %>% 
  ff_relabel_df(phosp)


# Temporarily remove out-of-range values for certain variables --------------------------
phosp = phosp %>% 
  mutate(
    age_admission = case_when(
      age_admission < 18 ~ NA_real_,
      age_admission > 120 ~ NA_real_,
      TRUE ~ age_admission
      ),
      
      crf3a_rest_height = case_when(
        crf3a_rest_height < 100 ~ NA_real_,
        crf3a_rest_height > 250 ~ NA_real_,
        TRUE ~ crf3a_rest_height
      )
    ) %>% 
  ff_relabel_df(phosp)


# PFTs ----------------------------------------------------------------------------------------
phosp = phosp %>% 
  mutate(    
    ### These should be numeric
    across(c(pft_tlco_reading1, pft_tlco_reading2, pft_kco_reading1, pft_kco_reading2, 
             pft_mip_reading, pft_mip_reading2, pft_mep_reading1, pft_mep_reading2), parse_number)
  ) %>% 
  
  mutate(
    
    ### FEV1:FVC ratio reported as percentage normal by some, recalculate from raw values
    pft_fev1_fvc_r1 = pft_fev1_r1a / pft_fvc_r1a,
    pft_fev1_fvc_r2 = pft_fev1_r2a / pft_fvc_r2a,
    pft_fev1_fvc_r3 = pft_fev1_r3a / pft_fvc_r3a,
    
    ### Means of three readings
    pft_fev1 = select(., starts_with("pft_fev1_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FEV1"),
    pft_fvc = select(., starts_with("pft_fvc_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FVC"),
    pft_fev1_fvc = select(., starts_with("pft_fev1_fvc")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FEV1/FVC"),
    pft_tlco = select(., pft_tlco_reading1, pft_tlco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("TLCO"),
    pft_kco = select(., pft_kco_reading1, pft_kco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("KCO"),
    pft_mip = select(., pft_mip_reading, pft_mip_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MIP"), 
    pft_mep = select(., pft_mep_reading1, pft_mep_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MEP"),
    
    pft_fev1_pred = pred_GLI(age_admission, crf3a_rest_height / 100, crf1a_sex, crf1b_eth_pft, param = c("FEV1")),
    pft_fvc_pred = pred_GLI(age_admission, crf3a_rest_height / 100, crf1a_sex, crf1b_eth_pft, param = c("FVC")),
    
    pft_fev1_perc_pred = (100 * pft_fev1 / pft_fev1_pred) %>% ff_label("FEV1 % predicted"), 
    pft_fvc_perc_pred = (100 * pft_fvc / pft_fvc_pred) %>% ff_label("FVC % predicted")
  ) %>% 
  
  mutate(
    
    ### FEV1:FVC ratio reported as percentage normal by some, recalculate from raw values
    pft_fev1_fvc_r1 = pft_fev1_r1a / pft_fvc_r1a,
    pft_fev1_fvc_r2 = pft_fev1_r2a / pft_fvc_r2a,
    pft_fev1_fvc_r3 = pft_fev1_r3a / pft_fvc_r3a,
    
    ### Means of three readings
    pft_fev1 = select(., starts_with("pft_fev1_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FEV1"),
    pft_fvc = select(., starts_with("pft_fvc_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FVC"),
    pft_fev1_fvc = select(., starts_with("pft_fev1_fvc")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FEV1/FVC"),
    pft_tlco = select(., pft_tlco_reading1, pft_tlco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("TLCO"),
    pft_kco = select(., pft_kco_reading1, pft_kco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("KCO"),
    pft_mip = select(., pft_mip_reading, pft_mip_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MIP"), 
    pft_mep = select(., pft_mep_reading1, pft_mep_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MEP"),
  )


# Hospital discharge event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_hosp = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "Hospital Discharge") %>% 
  purrr::discard(~all(is.na(.)))

# 6 week event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_6w = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "6 Weeks")%>% 
  purrr::discard(~all(is.na(.)))

# 3 month event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_3m = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "3 Months (1st Research Visit)")%>% 
  purrr::discard(~all(is.na(.)))

# Define patients for 1st 1000 -  discharge before 30112020
study_id_before_end_nov = phosp %>% 
  filter(crf1a_date_dis <= ymd(20201130)) %>% 
  pull(study_id)


# Temp out for testing -----------------------------------------------
saveRDS(phosp, "phosp.rds")
saveRDS(data, "data.rds")
saveRDS(phosp_hosp, "phosp_hosp.rds")
saveRDS(phosp_6w, "phosp_6w.rds")
saveRDS(phosp_3m, "phosp_3m.rds")
save(study_id_before_end_nov, file = "helpers.rda")

