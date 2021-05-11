####################################################################

# Code authors: Luke Daines and Steven Kerr
### Predicted median values for TLCO and KCO  ###
####################################################################

library(tidyverse)
library(lubridate)

################### IMPORT DATA ###############################
# datadir = "/home/common/phosp/cleaned/full/"
# timestamp = "2021-03-02_0400"
# 
# phosp_hosp   = read_rds(paste0(datadir, "phsop_hosp_", timestamp, "_full.rds"))

#Read Mspline lookup table
# Mspline <- read.csv('/home/skerr/Git/phosp_analysis/tlco_kco_prediction/Mspline.csv')
Mspline <- read.csv('/home/eharrison/phosp_clean/Mspline.csv')

#################################################################

MsplineLookup <- function(age, sex, variable, units) {
  age <- ifelse(age < 5, 5, age)
  age <- ifelse(age > 90, 90, age)

  if( any(is.na(c(age, sex, variable, units)))  ){
    return(NA)
  }
  else{
    col <- paste('Mspline_', variable, units, '_', sex, sep=''  )
    return(  as.numeric(Mspline[ which(Mspline['age'] == plyr::round_any(age, 0.25)) , col] ) )
  }
}

# Vectorise Mspline lookup for use in mutate
MsplineLookupV <- Vectorize(MsplineLookup)

# Add columns to data
phosp_kco = phosp_hosp %>% 
  select(study_id, crf1a_sex, crf3a_rest_height, age_admission) %>% 
  mutate(
    tlcoM_SI_pred =  case_when( 
      crf1a_sex == 'Male' ~ exp(-8.129189 + 2.018368*log(crf3a_rest_height) - 0.012425*log(age_admission)
                                + MsplineLookupV(age_admission, crf1a_sex, 'tlco', 'SI') ),
      crf1a_sex == 'Female' ~ exp(-6.253720 + 1.618697*log(crf3a_rest_height) - 0.015390*log(age_admission) 
                                  + MsplineLookupV(age_admission, crf1a_sex, 'tlco', 'SI')) ),
    tlcoM_Tr_pred =  case_when( 
      crf1a_sex == 'Male' ~ exp(-7.034920 + 2.018368*log(crf3a_rest_height) - 0.012425*log(age_admission) 
                                + MsplineLookupV(age_admission, crf1a_sex, 'tlco', 'Tr') ),
      crf1a_sex == 'Female' ~ exp(-5.159451 + 1.618697*log(crf3a_rest_height) - 0.015390*log(age_admission)  
                                  +  MsplineLookupV(age_admission, crf1a_sex, 'tlco', 'Tr')   )), 
    kcoM_SI_pred =  case_when( 
      crf1a_sex == 'Male' ~ exp(2.994137 - 0.415334*log(crf3a_rest_height) - 0.113166*log(age_admission)  
                                +  MsplineLookupV(age_admission, crf1a_sex, 'kco', 'SI') ),
      crf1a_sex == 'Female' ~ exp(4.037222 - 0.645656*log(crf3a_rest_height) - 0.097395*log(age_admission)
                                  + MsplineLookupV(age_admission, crf1a_sex, 'kco', 'SI')) ),
    kcoM_Tr_pred =  case_when( 
      crf1a_sex == 'Male' ~ exp(4.088408 - 0.415334*log(crf3a_rest_height) - 0.113166*log(age_admission)
                                + MsplineLookupV(age_admission, crf1a_sex, 'kco', 'Tr')),
      crf1a_sex == 'Female' ~ exp(5.131492 - 0.645656*log(crf3a_rest_height) - 0.097395*log(age_admission) 
                                  + MsplineLookupV(age_admission, crf1a_sex, 'kco', 'Tr'))),
    vaM_SI_pred =  case_when( 
      crf1a_sex == 'Male' ~ exp(-11.086573 + 2.43002*log(crf3a_rest_height) + 0.097047*log(age_admission)
                                + MsplineLookupV(age_admission, crf1a_sex, 'va', 'SI')),
      crf1a_sex == 'Female' ~ exp(-9.873970 + 2.182316*log(crf3a_rest_height) + 0.082868*log(age_admission)
                                  + MsplineLookupV(age_admission, crf1a_sex, 'va', 'SI')))
  )
