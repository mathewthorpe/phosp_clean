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
    ## Age
    age_admission_factor = case_when(
      age_admission < 20 ~ "<20",
      age_admission < 30 ~ "20-29",
      age_admission < 40 ~ "30-39",
      age_admission < 50 ~ "40-49",
      age_admission < 60 ~ "50-59",
      age_admission < 70 ~ "60-69",
      age_admission < 80 ~ "70-79",
      age_admission < 90 ~ "80-89",
      is.na(age_admission) ~ NA_character_,
      TRUE ~ "90+"
      ),
    
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
        crf1b_eth == "(13) Asian/ Asian British - Any other Asian background" ~ "South Asian", 
      crf1b_eth == "(12) Asian/ Asian British - Chinese" ~ "East Asian",
      crf1b_eth == "(14) Black/ African/ Caribbean/ Black British - African" |
        crf1b_eth == "(15) Black/ African/ Caribbean/ Black British - Caribbean" |
        crf1b_eth == "(16) Black/ African/ Caribbean/ Black British - Any other Black/ African/ Caribbean background" ~ "Black",
      crf1b_eth == "(17) Other ethnic group - Arab" |
        crf1b_eth == "(18) Other ethnic group - Any other ethnic group" ~ "Other",
      crf1b_eth == "Prefer not to say" ~ NA_character_
    ) %>% 
      fct_relevel("White", "South Asian", "East Asian", "Black", "Mixed", "Other") %>% 
      ff_label("Ethnicity"),
    
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
      ff_label("Ethnicity"),
    
    ## Diabetes yes no
    crf1a_com_mer_diab_yn = if_else(crf1a_com_mer_diab == "Type 1" | crf1a_com_mer_diab == "Type 2", "Yes", "No") %>% 
      ff_label("Diabetes"),
    
    ## Malignancy
    crf1a_com_mh_stm_yn = if_else(crf1a_com_mh_stm == "Localised" | crf1a_com_mh_stm == "Metastatic", "Yes", "No") %>% 
      ff_label("Malignancy"),
    
    ## Symptoms - change N/K to missing
    across(crf1a_fever_history:crf1a_bleeding, fct_recode, NULL = "N/K"),
    
    ## Symptom scales - convert to numeric
    across(starts_with("psq_scale_"), ~ as.character(.) %>% parse_number()),
    
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
    
    no_comorbid_3levels = case_when(
      no_comorbid == 0 ~ "No comorbidity",
      no_comorbid == 1 ~ "1 comorbidity", 
      no_comorbid > 1 ~ "2 or more comorbidities+"
    ) %>% 
      factor() %>% 
      fct_relevel("No comorbidity") %>% 
      ff_label("Number of comorbidities (factor)"), 
    
    # Dates ----
    ## Symptom duration
    crf1a_symptom_duration = (crf1a_date_adm - crf1a_date_first_symptoms) %>% 
      as.numeric("days") %>% 
      ff_label("Symptom duration (days)"),
    
    ## Admission duration
    crf1a_admission_duration = (crf1a_date_dis - crf1a_date_adm) %>% 
      as.numeric("days") %>% 
      ff_label("Admission duration (days)"),
    
    
    # Comorbidities ----
    ## Make summary variables
    crf1a_com_card = rowSums(select(., dplyr::starts_with("crf1a_com_card"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_res = rowSums(select(., dplyr::starts_with("crf1a_com_res"), -c(contains("other"), crf1a_com_res_ild)) %>% 
                              mutate(across(everything(), ~ . == "Yes")),
                            na.rm = TRUE),
    crf1a_com_gast = rowSums(select(., dplyr::starts_with("crf1a_com_gast"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_neupsy = rowSums(select(., dplyr::starts_with("crf1a_com_neupsy"), -c(contains("other"), crf1a_com_res_ild)) %>% 
                                 mutate(across(everything(), ~ . == "Yes")),
                               na.rm = TRUE),
    crf1a_com_rheu = rowSums(select(., dplyr::starts_with("crf1a_com_rheu"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_mer = rowSums(select(., dplyr::starts_with("crf1a_com_mer"), -c(contains("other"),
                                                                              crf1a_com_mer_diab,
                                                                              crf1a_com_mer_diab_com)) %>% 
                              mutate(across(everything(), ~ . == "Yes")),
                            na.rm = TRUE),
    crf1a_com_mh = rowSums(select(., dplyr::starts_with("crf1a_com_mh"), -c(contains("other"), 
                                                                            contains("detail"), 
                                                                            crf1a_com_mh_lymp)) %>% 
                             mutate(across(everything(), ~ . == "Yes")),
                           na.rm = TRUE),
    crf1a_com_id = rowSums(select(., dplyr::starts_with("crf1a_com_id"), -contains("other")) %>% 
                             mutate(across(everything(), ~ . == "Yes")),
                           na.rm = TRUE),
    
    # Special case diabetes separated out - need to compare this with crf1a_com_mer_diab_yn for missingness
    crf1a_com_diab = fct_explicit_na(crf1a_com_mer_diab, na_level = "No"),

    across(c(crf1a_com_card, crf1a_com_res,
             crf1a_com_gast, crf1a_com_neupsy,
             crf1a_com_rheu, crf1a_com_mer, 
             crf1a_com_mh, crf1a_com_id), 
           ~ case_when(
             . > 0 ~ "Yes",
             TRUE ~ "No") %>% 
             factor()
    ),
    
    crf1a_com_card = ff_label(crf1a_com_card, "Cardiovascular"), 
    crf1a_com_res = ff_label(crf1a_com_res, "Respiratory"),
    crf1a_com_gast = ff_label(crf1a_com_gast, "Gastrointestinal"), 
    crf1a_com_neupsy = ff_label(crf1a_com_neupsy, "Neurological and psychiatric"),
    crf1a_com_rheu = ff_label(crf1a_com_rheu, "Rheumatological"), 
    crf1a_com_mer = ff_label(crf1a_com_mer, "Metabolic/endocrine/renal"), 
    crf1a_com_mh = ff_label(crf1a_com_mh, "Malignancy"), 
    crf1a_com_id = ff_label(crf1a_com_id, "Infectious disease"), 
    
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
    
    ## EQ5D sum across domains. as.numeric makes lowest == 1, so subtract 1 for zero as reference
    eq5d5l_total_pre = rowSums(select(., eq5d5l_q1_pre:eq5d5l_q5_pre) %>% 
                                 mutate(across(everything(), ~ as.numeric(.) %>% {. - 1})),
                               na.rm = TRUE) %>% 
      ff_label("EQ5D sum of domains pre-covid"),
    
    eq5d5l_total = rowSums(select(., eq5d5l_q1:eq5d5l_q5) %>% 
                             mutate(across(everything(), ~ as.numeric(.) %>% {. - 1})),
                           na.rm = TRUE) %>% 
      ff_label("EQ5D sum of domains"),
    
    
    across(starts_with("eq5d5l_q"), ~ as.numeric(.) - 1, .names = "{.col}_numeric"),
    eq5d5l_q1_delta = eq5d5l_q1_numeric - eq5d5l_q1_pre_numeric,
    eq5d5l_q2_delta = eq5d5l_q1_numeric - eq5d5l_q2_pre_numeric,
    eq5d5l_q3_delta = eq5d5l_q1_numeric - eq5d5l_q3_pre_numeric,
    eq5d5l_q4_delta = eq5d5l_q1_numeric - eq5d5l_q4_pre_numeric,
    eq5d5l_q5_delta = eq5d5l_q1_numeric - eq5d5l_q5_pre_numeric,
    
    across(matches("eq5d5l_q.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    across(starts_with("patient_sq_l_"), ~ as.numeric(.) - 1, .names = "{.col}_numeric"),
    patient_sq_l_t_seeing_delta = patient_sq_l_t_seeing_numeric - patient_sq_l_b_seeing_numeric,
    patient_sq_l_t_hearing_delta = patient_sq_l_t_hearing_numeric - patient_sq_l_b_hearing_numeric,
    patient_sq_l_t_walking_delta = patient_sq_l_t_walking_numeric - patient_sq_l_b_walking_numeric,
    patient_sq_l_t_remembering_delta = patient_sq_l_t_remembering_numeric - patient_sq_l_b_remembering_numeric,
    patient_sq_l_t_self_care_delta = patient_sq_l_t_self_care_numeric - patient_sq_l_b_self_care_numeric,
    patient_sq_l_t_communicate_delta = patient_sq_l_t_communicate_numeric - patient_sq_l_b_communicate_numeric,
    
    across(matches("patient_sq_l_t_.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    # Want if_all and if_any here to keep the regex column names select, but on-going issues
    # https://github.com/tidyverse/dplyr/issues/5782
    
    # % with a disability
    patient_sq_l_t_disability = case_when(
      patient_sq_l_t_seeing == "Yes - a lot difficulty" |
        patient_sq_l_t_hearing == "Yes - a lot difficulty" |
        patient_sq_l_t_walking == "Yes - a lot difficulty" |
        patient_sq_l_t_remembering == "Yes - a lot difficulty" |
        patient_sq_l_t_self_care  == "Yes - a lot difficulty" |
        patient_sq_l_t_communicate == "Yes - a lot difficulty" ~ "Yes",
      
      is.na(patient_sq_l_t_seeing) &
        is.na(patient_sq_l_t_hearing) &
        is.na(patient_sq_l_t_walking) &
        is.na(patient_sq_l_t_remembering) &
        is.na(patient_sq_l_t_self_care) &
        is.na(patient_sq_l_t_communicate) ~ NA_character_,
      TRUE ~ "No") %>% 
      factor(),
    
    psq_scale_blness_delta = psq_scale_blness_24hrs - psq_scale_blness_pre,
    psq_scale_fatigue_delta = psq_scale_fatigue_24hrs - psq_scale_fatigue_pre,
    psq_scale_cough_delta = psq_scale_cough_24hrs - psq_scale_cough_pre,
    psq_scale_pain_delta = psq_scale_pain_24hrs - psq_scale_pain_pre,
    psq_scale_sleep_delta = psq_scale_sleep_24hrs - psq_scale_sleep_pre,
    
    across(matches("psq_scale_.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    
    
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
## Doesn't work at the moment for comorbidity, as that sums across columns for all rows
## See joint below. 
## Currently this is for PFT calculations
phosp = phosp %>%  
  group_by(study_id) %>% 
  fill(age_admission, age_admission_factor, crf1a_sex,  crf1b_eth_pft, crf3a_rest_height,
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

# IMD ------------------------------------------------------------------------------------
post_code_main_lookup = read_csv('http://argonaut.is.ed.ac.uk/public/lookup/NSPL_FEB_2020_UK.csv')

pcode_data = phosp %>% 
  select(study_id, post_code) %>% 
  mutate(length_pcode = str_length(post_code),
         post_code = toupper(post_code),
         number_digits = str_count(post_code, "[0-9]"),
         number_alphanum = str_count(post_code, "[A-Z]")) %>% 
  filter(number_digits >= 1 & number_alphanum >=1) %>% 
  mutate(post_code = gsub(' O', ' 0', post_code),
         post_code = gsub('C0', 'CO', post_code),
         post_code = gsub('S0', 'SO', post_code),
         post_code = ifelse(number_digits > 4, sub(" .*", "", post_code), post_code),
         post_code = gsub('ZZ.*', '', post_code),
         post_code = gsub(' ', '', post_code),
         half_post_code = gsub('.{3}$', '', post_code))

post_code_main_lookup = post_code_main_lookup %>%
  mutate(country = ifelse(startsWith(ccg, 'S'), 'Scotland', NA),
         country = ifelse(startsWith(ccg, 'E'), 'England', country),
         country = ifelse(startsWith(ccg, 'W'), 'Wales', country) %>% 
           factor(),
         pcds = gsub(' ', '', pcds),
         half_pcds = gsub('.{3}$', '', pcds)) %>% 
  group_by(country) %>% 
  mutate(imd_quintile = ntile(imd, 5)) %>% 
  select(pcds, half_pcds, country, ccg, imd, imd_quintile) %>% 
  ungroup()

post_code_supp_lookup = post_code_main_lookup %>% 
  group_by(half_pcds) %>% 
  mutate(median_imd_quintile = median(imd_quintile) %>% floor()) %>% 
  ungroup() %>% 
  distinct(half_pcds, .keep_all = T) %>% 
  select(-imd_quintile)

pcode_data = pcode_data %>% 
  left_join(post_code_main_lookup, by = c('post_code' = 'pcds')) %>% 
  left_join(post_code_supp_lookup, by = c('half_post_code' = 'half_pcds')) %>%
  mutate(imd = ifelse(is.na(imd.x), imd.y, imd.x)) %>% 
  mutate(imd_quintile = ifelse(is.na(imd_quintile), median_imd_quintile, imd_quintile)) %>% 
  select(study_id, imd, imd_quintile)  

phosp = phosp %>% 
  left_join(pcode_data %>% 
              select(study_id, imd, imd_quintile), by = 'study_id') %>% 
  mutate(
    imd_quintile = factor(imd_quintile,
                          levels = c("1", "2", "3", "4", "5"),
                          labels = c("1 - most deprived", "2", "3", "4", "5 - least deprived")
    ) %>% 
      ff_label("Index of muliple deprivation")
  )

rm(pcode_data, post_code_main_lookup, post_code_supp_lookup)

# Hospital discharge event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_hosp = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "Hospital Discharge") %>% 
  purrr::discard(~all(is.na(.)))

# Join back in admission only variables that cannot be filled --------------------------
phosp = phosp %>% 
  select(-c(no_comorbid, no_comorbid_3levels)) %>% 
  left_join(phosp_hosp %>% select(study_id, no_comorbid, no_comorbid_3levels))

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

