#-----------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b u i l d   s c r i p t   #
#                                                                             #
# Author: Alex                                                                #
# Date created: 2024-09-16                                                    #
#-----------------------------------------------------------------------------#



source("G:/Alex Harley/Audit_2023_onwards/My R functions/MySummary.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/lintestOR.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/tidyoutput.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/checkSame.R")

library(tidyverse)
library(psych)
library(lme4)
library(finalfit)

# options(max.print = 10000)


dat_old <- read.csv("G:/Alex Harley/Audit_2023_onwards/2022-2023/PR/Data/rawData/PR004-Bristol/PR004-Bristol.csv", header = TRUE, 
                    stringsAsFactors = TRUE, na.strings = c("NA", ""))

dat <- read.csv("G:/Alex Harley/Audit_2023_onwards/2023-2024/PR/Data/rawData/NRAP-PR-2304-2403-Imperial2.csv", header = TRUE, 
                stringsAsFactors = TRUE, na.strings = c("NA", ""))

colnames(dat_old)
colnames(dat)

checkSame(dat, dat_old) %>% arrange(variable)

# Quite a lot of things are different: 
# - impairments
# - conditions
# - EQ5D
# supervised PR programme is no longer in 2 versions, some variable names slightly changed
# - EQ5D at discharge

# dat$X5.1a.Start.Date <- as.Date(dat$X5.1a.Start.Date, format = "%d/%m/%Y")
# 
# dat %>% filter(is.na(X1.7.Primary.Condition)) %>% select(X5.1a.Start.Date) %>% summary()
# dat %>% filter(!is.na(X1.7.Primary.Condition)) %>% select(X5.1a.Start.Date) %>% summary()
# 
# dat$X2.3.Initial.PR.Assessment.Appointment <- as.Date(dat$X2.3.Initial.PR.Assessment.Appointment, format = "%d/%m/%Y")
# 
# dat %>% filter(is.na(X1.7.Primary.Condition)) %>% select(X2.3.Initial.PR.Assessment.Appointment) %>% summary()
# dat %>% filter(!is.na(X1.7.Primary.Condition)) %>% select(X2.3.Initial.PR.Assessment.Appointment) %>% summary()
# 
# table(dat$X5.2a.Home.based.delivery.method, dat$X5.2.Supervised.PR.programme...Home, useNA = "ifany")
# table(dat$X5.2a.Home.based.delivery.method...Other.digital.communications, useNA = "ifany")
# table(dat$X5.2a.Home.based.delivery.method...Other.digital.communications, useNA = "ifany")

# sort out column names etc

dat <- dat %>% select(study_ID = RowID,
                      patient_ID = PatientID,
                      country = Country,
                      region = Region,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      org_name = OrgName,
                      org_code = Org.Service,
                      #   nhs_number_valid = NHS.Number.Valid,
                      LSOA = lsoa11,
                      age = X.AgeAtAssessment,
                      gender = X1.3.Gender,
                      ethnicity = X1.5.Ethnicity,
                      # impairments come in here
                      impairments_none = X1.6.Impairments...No.none,
                      impairments_anxiety = X1.6.Impairments...Anxiety,
                      impairments_depression = X1.6.Impairments...Depression,
                      impairments_severe_mental_illness = X1.6.Impairments...Severe.mental.illness,
                      impairments_dementia_mci = X1.6.Impairments...Dementia.mild.cognitive.impairment,
                      impairments_other = X1.6.Impairments...Other,
                      impairments_NR = X1.6.Impairments...Not.recorded, 
                      # conditions
                      primary_condition = X1.7.Primary.Condition, # introduced in november
                      secondary_conditions_all = X1.7a.Other.Conditions,
                      secondary_conditions_asthma = X1.7a.Other.Conditions...Asthma,
                      secondary_conditions_bronchiectasis = X1.7a.Other.Conditions...Bronchiectasis,
                      secondary_conditions_COPD = X1.7a.Other.Conditions...COPD,
                      secondary_conditions_CHF = X1.7a.Other.Conditions...Chronic.heart.failure,
                      secondary_conditions_ILD = X1.7a.Other.Conditions...Interstitial.lung.disease,
                      secondary_conditions_covid = X1.7a.Other.Conditions...Long.covid,
                      secondary_conditions_none = X1.7a.Other.Conditions...No.None,
                      secondary_conditions_NR = X1.7a.Other.Conditions...Not.recorded,
                      secondary_conditions_other_CRD = X1.7a.Other.Conditions...Other.chronic.respiratory.disease,
                      secondary_conditions_hypertension = X1.7a.Other.Conditions...Pulmonary.hypertension,
                      secondary_conditions_thoracic_surgery = X1.7a.Other.Conditions...Thoracic.surgery,
                      ref_date = X2.1.Referral.Date,
                      ref_date_NR = X2.1.1.Not.recorded,
                      ref_location = X2.2.Referred.From,
                      assess_date = X2.3.Initial.PR.Assessment.Appointment,
                      #  smoke_status = X3.1.Smoking,
                      FEV1_percpred = X3.1.FEV1...predicted.,
                      FEV1_NR = X3.1x.Not.recorded,
                      FEV1FVC = X3.2.FEV1.FVC.Ratio,
                      FEV1FVC_NR = X3.2x.Not.recorded,
                      #  BMI = X3.4.Patient.s.body.mass.index..BMI.,
                      #  BMI_NR = X3.4.1.Not.recorded,
                      MRC_score_init = X3.3.MRC.Score,
                      #  CVD_history_orig = X3.6.Cardiovascular.Disease,
                      #  musc_skel_history_orig = X3.7.Lower.Limb.Musculoskeletal.Disorder,
                      #  mental_history_orig = X3.8.Mental.Health,
                      #  mental_history_combined_illness = X3.8a.Mental.Health,
                      #  anxiety_bin = X3.8a.Mental.Health...Anxiety,
                      #  depression_bin = X3.8a.Mental.Health...Depression,
                      #  SMI_bin = X3.8a.Mental.Health...Severe.mental.illness,
                      test_type_init = X4.1.Initial.Assessment.Tests,
                      test_value_init = X4.1a.Value.in.metres,
                      prac_test_init = X4.1b.Practice.test.at.initial.assessment,
                      ESWT_at_init = X4.2.ESWT.at.initial.assessment,
                      ESWT_value_init = X4.2a.Value.in.seconds,
                      CRQ_init = X4.3.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_init = X4.3a.Dyspnoea.score,
                      CRQ_fatigue_init = X4.3b.Fatigue.score,
                      CRQ_emotion_init = X4.3c.Emotion.score,
                      CRQ_mastery_init = X4.3d.Mastery.score,
                      CAT_init = X4.4.COPD.Assessment.Test..CAT.,
                      CAT_score_init = X4.4a.Total.CAT.score,
                      
                      # add in EQ5D at assessment, brought in in November
                      EQ5D_init = X4.5.EQ5D,
                      EQ5D_mobility_init = X4.5a.EQ5D.Mobility, # 1 is you feel fine, 5 is you feel terrible,
                      EQ5D_self_care_init = X4.5b.EQ5D.Self.Care,
                      EQ5D_activities_init = X4.5c.EQ5D.Activities,
                      EQ5D_pain_init = X4.5d.EQ5D.Pain,
                      EQ5D_depression_init = X4.5e.EQ5D.Depression,
                      EQ5D_VAS_init = X4.5f.EQ5D.Health.Today, # 100 is you feel great, 0 is you feel awful
                                                               # VAS = Visual analogue score
                      
                      # Q5
                      
                      enrolled = X5.1.Enrolled.onto.PR.programme.,
                      start_date = X5.1a.Start.Date,
                    
                      #2023-24 Q5:
                      centre_based = X5.2.Supervised.PR.programme...Centre,
                      home_based = X5.2.Supervised.PR.programme...Home,
                      # X5.2a.Home.based.delivery.method
                      home_based_in_person = X5.2a.Home.based.delivery.method...In.person,
                      home_based_other = X5.2a.Home.based.delivery.method...Other.digital.communications,
                      home_based_phone = X5.2a.Home.based.delivery.method...Phone.calls,
                      home_based_video = X5.2a.Home.based.delivery.method...Video.coferencing,
                      scheduled_sess = X5.3.Supervised.sessions.scheduled,
                      rec_sess_group = X5.4a.Supervised.group.sessions.received,
                      rec_sess_indiv = X5.4b.Supervised.face.to.face.sessions.received, # despite the variable name this is 1 to 1, not face to face
                      
                      discharge_assess = X6.1.Discharge.assessment.performed.,
                      discharge_date = X6.1a.Discharge.assessment.date,
                      exercise_plan = X6.1b.Discharge.exercise.plan.provided,
                      MRC_score_dis = X7.1.MRC.Score.At.Discharge,
                      test_type_dis = X7.2.Walking.test.at.discharge.assessment.,
                      test_value_dis = X7.2a.Value.in.metres,
                      ESWT_at_dis = X7.3.Did.you.also.record.the.ESWT.at.discharge.,
                      ESWT_value_dis = X7.3a.Value.in.seconds,
                      CRQ_dis = X7.4.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_dis = X7.4a.Dyspnoea.score,
                      CRQ_fatigue_dis = X7.4b.Fatigue.score,
                      CRQ_emotion_dis = X7.4c.Emotion.score,
                      CRQ_mastery_dis = X7.4d.Mastery.score,
                      CAT_dis = X7.5.COPD.Assessment.Test..CAT.,
                      CAT_score_dis = X7.5a.Total.CAT.score,
                      
                      # EQ5D at discharge
                      # add in EQ5D at assessment
                      EQ5D_dis = X7.6.Dischange.EQ5D,
                      EQ5D_mobility_dis = X7.6a.Dischange.EQ5D.Mobility, # 1 is you feel fine, 5 is you feel terrible,
                      EQ5D_self_care_dis = X7.6b.Dischange.EQ5D.Self.Care,
                      EQ5D_activities_dis = X7.6c.Dischange.EQ5D.Activities,
                      EQ5D_pain_dis = X7.6d.Dischange.EQ5D.Pain,
                      EQ5D_depression_dis = X7.6e.Dischange.EQ5D.Depression,
                      EQ5D_VAS_dis = X7.6f.Dischange.EQ5D.Health.Today) # 100 is you feel great, 0 is you feel awful
                      
colnames(dat)

table(dat$enrolled)
table(dat$enrolled, dat$rec_sess_group)


# dat %>% filter(enrolled != "Yes") %>% summary() # check that these are all missing

# this is confusing - looks like it has been simplified to just primary / secondary / self-referral


table(dat$ref_location) 

# old column names

# dat <- dat %>% select(study_ID = RowID,
#                       patient_ID = PatientID,
#                       country = Country,
#                       region = Region,
#                       trust_code = Tcode.Now,
#                       trust_name = Trust.Now,
#                       org_name = OrgName,
#                       org_code = Org.Service,
#                    #   nhs_number_valid = NHS.Number.Valid,
#                       LSOA = LSOA11,
#                       age = X.AgeAtAssessment,
#                       gender = X1.3.Gender,
#                       ethnicity = X1.5.Ethnicity,
#                       ref_date = X2.1.Referral.Date,
#                       ref_date_NR = X2.1.1.Not.recorded,
#                       ref_location = X2.2.Referred.From,
#                       assess_date = X2.3.Initial.PR.Assessment.Appointment,
#                     #  smoke_status = X3.1.Smoking,
#                       FEV1_percpred = X3.1.FEV1...predicted.,
#                       FEV1_NR = X3.1x.Not.recorded,
#                       FEV1FVC = X3.2.FEV1.FVC.Ratio,
#                       FEV1FVC_NR = X3.2x.Not.recorded,
#                     #  BMI = X3.4.Patient.s.body.mass.index..BMI.,
#                     #  BMI_NR = X3.4.1.Not.recorded,
#                       MRC_score_init = X3.3.MRC.Score,
#                     #  CVD_history_orig = X3.6.Cardiovascular.Disease,
#                     #  musc_skel_history_orig = X3.7.Lower.Limb.Musculoskeletal.Disorder,
#                     #  mental_history_orig = X3.8.Mental.Health,
#                     #  mental_history_combined_illness = X3.8a.Mental.Health,
#                     #  anxiety_bin = X3.8a.Mental.Health...Anxiety,
#                     #  depression_bin = X3.8a.Mental.Health...Depression,
#                     #  SMI_bin = X3.8a.Mental.Health...Severe.mental.illness,
#                       test_type_init = X4.1.Initial.Assessment.Tests,
#                       test_value_init = X4.1a.Value.in.metres,
#                       prac_test_init = X4.1b.Practice.test.at.initial.assessment,
#                       ESWT_at_init = X4.2.ESWT.at.initial.assessment,
#                       ESWT_value_init = X4.2a.Value.in.seconds,
#                       CRQ_init = X4.3.Chronic.Respiratory.Questionnaire..CRQ.,
#                       CRQ_dyspnoea_init = X4.3a.Dyspnoea.score,
#                       CRQ_fatigue_init = X4.3b.Fatigue.score,
#                       CRQ_emotion_init = X4.3c.Emotion.score,
#                       CRQ_mastery_init = X4.3d.Mastery.score,
#                       CAT_init = X4.4.COPD.Assessment.Test..CAT.,
#                       CAT_score_init = X4.4a.Total.CAT.score,
#                       enrolled = X5.1.Enrolled.onto.PR.programme.,
#                       start_date = X5.1a.Start.Date,
#                       centre_based_V1 = X5.2.centre.based.PR.programme..V1.,
#                     # Start of V1s
#                     #  prog_type = X5.2a.Programme.Type,
#                       scheduled_sess_centre_no_V1 = X5.2b.Centre.based.PR.Sessions.Scheduled..V1.,
#                       rec_sess_centre_group_no_V1 = X5.2c.a..Group.centre.based.sessions.received..V1.,
#                       rec_sess_centre_indiv_no_V1 = X5.2c.b..1.1.centre.based.sessions.received..V1.,
#                       home_based_V1 = X5.3.Home.based.PR.programme..V1.,
#                       scheduled_sess_home_no_V1 = X5.3a.Home.based.PR.Sessions.Scheduled..V1.,
#                       rec_sess_home_in_person_no_V1 = X5.3b.a..Home.based.PR.Sessions...In.Person...V1.,
#                       rec_sess_home_video_group_no_V1 = X5.3b.b..Home.based.PR.Sessions...Video.conferencing...group..V1.,
#                       rec_sess_home_video_indiv_no_V1 = X5.3b.c..Home.based.PR.Sessions...Video.conferencing...1.1...V1.,
#                       rec_sess_home_phone_no_V1 = X5.3b.d..Home.based.PR.Sessions...Phone.calls..V1.,
#                       rec_sess_home_other_no_V1 = X5.3b.e..Home.based.PR.Sessions...Other.Digital.Communications..V1.,
# 
#                     # Start of V2s
# 
#                     centre_based_V2 = X5.2.Supervised.PR.programme...Centre,
#                     home_based_V2 = X5.2.Supervised.PR.programme...Home,
#                     home_based_in_person_V2 = X5.2a.Home.based.delivery.method...Person,
#                     home_based_video_V2 = X5.2a.Home.based.delivery.method...Video,
#                     home_based_phone_V2 = X5.2a.Home.based.delivery.method...Phone,
#                     home_based_other_V2 = X5.2a.Home.based.delivery.method...Digital,
#                     scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED., # I've checked it - it's legit
#                     rec_sess_group_V2 = X5.4a.Supervised.PR.group.sessions.received..V2.,
#                     rec_sess_indiv_V2 = X5.4b.Supervised.PR.face.to.face.sessions.received..V2., # check this is indiv and not F2F!!!
# 
#                       discharge_assess = X6.1.Discharge.assessment.performed.,
#                       discharge_date = X6.1a.Discharge.assessment.date,
#                       exercise_plan = X6.1b.Discharge.exercise.plan.provided,
#                       MRC_score_dis = X7.1.MRC.Score.At.Discharge,
#                       test_type_dis = X7.2.Walking.test.at.discharge.assessment.,
#                       test_value_dis = X7.2a.Value.in.metres,
#                       ESWT_at_dis = X7.3.Did.you.also.record.the.ESWT.at.discharge.,
#                       ESWT_value_dis = X7.3a.Value.in.seconds,
#                       CRQ_dis = X7.4.Chronic.Respiratory.Questionnaire..CRQ.,
#                       CRQ_dyspnoea_dis = X7.4a.Dyspnoea.score,
#                       CRQ_fatigue_dis = X7.4b.Fatigue.score,
#                       CRQ_emotion_dis = X7.4c.Emotion.score,
#                       CRQ_mastery_dis = X7.4d.Mastery.score,
#                       CAT_dis = X7.5.COPD.Assessment.Test..CAT.,
#                       CAT_score_dis = X7.5a.Total.CAT.score,
#                       dataset = Dataset)

# Make sure that those who aren't enrolled are missing for the values below:

dat %>% filter(enrolled == "Yes") %>% filter(is.na(rec_sess_indiv)) %>% nrow()



# mainly fine, but have to sort out the question 5 stuff.

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))
                      

# It's strange but these do all have different decimal point levels
head(dat$CAT_score_init, 30) # 0 dp
head(dat$CRQ_fatigue_init, 30) # 2 dp
head(dat$CRQ_dyspnoea_init, 30) # 1 dp
head(dat$CRQ_emotion_init, 30) # 2dp
head(dat$CRQ_mastery_init, 30) # 2dp




# Read in the IMD quintiles
# let's use the joint quintiles seeing as we no longer have scotland

IMDeng_wal <- read.csv("G:/Alex Harley/Audit_2023_onwards/General UK data/IMD/2019_England_and_Wales_Income_Employment_IMD_clean.csv",
                       header = TRUE, stringsAsFactors = FALSE)

summary(IMDeng_wal$new_IMD_quintile)

IMDeng_wal <- IMDeng_wal %>% select(LSOA = LSOA_code_2011, IMD_quintile = new_IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng_wal, by = "LSOA")
summary(dat$IMD_quintile)

dat$IMD_quintile <- as.character(dat$IMD_quintile)
dat$IMD_quintile[is.na(dat$IMD_quintile)] <- "Missing IMD quintile"
dat$IMD_quintile <- factor(dat$IMD_quintile, levels = c("1", "2", "3", "4", "5", "Missing IMD quintile"))



# dat <- dat %>% mutate(anyIMD = factor(ifelse(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Wal) &
#                                                is.na(IMD_quintile_Scot), "No IMD", "IMD present")))

dat <- dat %>% mutate(anyIMD = factor(ifelse(IMD_quintile == "Missing IMD quintile", "No IMD", "IMD present")))


dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)
summary(dat$IMD_quintile)
summary(dat$anyIMD)

# Country already in the dataset so can skip this bit:

# # Add country variable
# 
# country <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_CYP_asthma_R_format.csv",
#                     stringsAsFactors = FALSE)
# 
# # country <- country %>% rename(country = Country, hosp_code = HospCode, trust_code = TrustCode)
# 
# # Drop the provided trust code because the trust code in Tim's provided spreadsheet should be the most
# # up-to-date
# 
# dat <- dat %>% select(-trust_code)
# 
# dat <- left_join(dat, country, by = "hosp_code")








# relevel gender

dat$gender <- relevel(dat$gender, ref = "Male")



# dat %>% filter(assess_date < "2019-06-01") %>% filter(home_based == "Yes") %>% select(centre_based:discharge_assess)

# # Next thing is to remove the brackets form the study and patient IDs - very unhelpful. 
# 
# dat <- dat %>% mutate(study_ID = str_remove_all(study_ID, "[()]"))
# dat <- dat %>% mutate(patient_ID = str_remove_all(patient_ID, "[()]"))
# 
# # Okay that was easier than I thought. I think the square bracket means 'remove all characters within me'.

#Need to convert dates to dates

dat <- dat %>% mutate(ref_date = as.Date(ref_date, format = "%d/%m/%Y"), 
                      assess_date = as.Date(assess_date, format = "%d/%m/%Y"),
                      start_date = as.Date(start_date, format = "%d/%m/%Y"),
                      discharge_date = as.Date(discharge_date, format = "%d/%m/%Y"))


# Create the 'date to date' variables

# should we just create some more here?

dat <- dat %>% mutate(ref_to_start_days = as.numeric(start_date - ref_date),
                      assess_to_start_days = as.numeric(start_date - assess_date),
                      assess_to_discharge_days = as.numeric(discharge_date - assess_date),
                      ref_to_assess_days = as.numeric(assess_date - ref_date),
                      start_to_discharge_days = as.numeric(discharge_date - start_date))



#//////////// this section is now very confusing - come back to it after PR variable talk

# ref to start days needs to be for stable COPD

table(dat$ref_location, useNA = "ifany")

# All refs location has to be converted now to 'hospitalised AECOPD' vs everything else

# let's create a new variable for COPD vs non-COPD
# This will need to change with new dataset as primary condition should no longer be missing

table(dat$primary_condition, useNA = "ifany")

dat$primary_COPD <- "Other disease"
dat$primary_COPD[dat$primary_condition == "COPD" | is.na(dat$primary_condition)] <- "COPD"
dat$primary_COPD <- factor(dat$primary_COPD, levels = c("COPD", "Other disease"))

table(dat$primary_COPD, useNA = "ifany")

table(dat$ref_location)

dat <- dat %>% mutate(ref_location = fct_recode(ref_location,
                                                `Primary care` = "Primary/Community - post treatment for AECOPD",
                                                `Primary care` = "Primary/Community - stable COPD",
                                                `Primary care` = "Primary Care",
                                                `Secondary care - post AECOPD` = "Post AECOPD",
                                                `Secondary care - post AECOPD` = "Secondary Care - post admission for AECOPD",
                                                `Secondary care - stable COPD or other disease` = "Secondary Care",
                                                `Secondary care - stable COPD or other disease` = "Secondary Care - stable COPD",
                                                `Self-referral` = "Self-referral",
                                                `Self-referral` = "Self referral"))

table(dat$ref_location, useNA = "ifany")
table(dat$ref_location, dat$primary_COPD, useNA = "ifany")

# ref to start stable COPD

dat <- dat %>% mutate(ref_to_start_days_stable_COPD = ifelse(
    ref_location %in% c("Secondary care - stable COPD or other disease",
                        "Primary care",
                        "Self-referral") &
      primary_COPD == "COPD",
    ref_to_start_days, NA))

summary(dat$ref_to_start_days_stable_COPD)

dat %>% select(ref_location, primary_COPD) %>% summary()
dat %>% filter(is.na(ref_to_start_days_stable_COPD)) %>% select(ref_location, primary_COPD) %>% summary()
dat %>% filter(!is.na(ref_to_start_days_stable_COPD)) %>% select(ref_location, primary_COPD) %>% summary()

summary(dat$enrolled)

# assess to start days stable COPD

dat <- dat %>% mutate(assess_to_start_days_stable_COPD = ifelse(
  ref_location %in% c("Secondary care - stable COPD or other disease",
                      "Primary care",
                      "Self-referral") &
    primary_COPD == "COPD",
  assess_to_start_days, NA))

summary(dat$assess_to_start_days_stable_COPD)

# Start date within 90 days for stable COPD

dat <- dat %>% mutate(ninety_day_referral_to_start_for_stable_COPD = factor(
  ifelse(ref_to_start_days_stable_COPD < 90, "<90 days", ">=90 days"),
  levels = c("<90 days", ">=90 days"))) 



# ref to start AECOPD

dat <- dat %>% mutate(ref_to_start_days_AECOPD = ifelse(
  ref_location == "Secondary care - post AECOPD" &
    primary_COPD == "COPD",
  ref_to_start_days, NA))

summary(dat$ref_to_start_days_AECOPD)


# assess to start days AECOPD

dat <- dat %>% mutate(assess_to_start_days_AECOPD = ifelse(
  ref_location == "Secondary care - post AECOPD" &
    primary_COPD == "COPD",
  assess_to_start_days, NA))

summary(dat$assess_to_start_days_AECOPD)

# 30 day referral for AECOPD

dat <- dat %>% mutate(thirty_day_referral_to_start_for_AECOPD = factor(
  ifelse(ref_to_start_days_AECOPD < 30, "<30 days", ">=30 days"),
  levels = c("<30 days", ">=30 days"))) 



#//////////////////////////////////////////////////


# Make a variable that says whether or not referral date was recorded
                      
dat <- dat %>% mutate(ref_date_rec = as.character(ref_date_NR))
dat$ref_date_rec[is.na(dat$ref_date_rec)] <- "Known"
dat$ref_date_rec <- factor(dat$ref_date_rec)
summary(dat$ref_date_rec)

# We created a more useful variable so we can drop the other one:

dat$ref_date_NR <- NULL



# Make a variable that says whether or not referral date was recorded

dat %>% select(FEV1_percpred, FEV1_NR) %>% table(useNA = "ifany")

table(dat$FEV1_NR)


dat %>% filter(is.na(FEV1_NR)) %>% select(FEV1_percpred) %>% summary()
dat %>% filter(is.na(FEV1_NR)) %>% filter(is.na(FEV1_percpred)) %>% select(primary_condition) %>% table()


dat <- dat %>% mutate(FEV1_percpred_rec = as.character(FEV1_NR),
                      FEV1FVC_rec = as.character(FEV1FVC_NR))

# dat$FEV1_percpred_rec[is.na(dat$FEV1_percpred_rec)] <- "Recorded" # this way no longer works at non-COPD don't have spiro
# dat$FEV1FVC_rec[is.na(dat$FEV1FVC_rec)] <- "Recorded"

dat$FEV1_percpred_rec[!is.na(dat$FEV1_percpred)] <- "Recorded" # need to use presence of spiro value instead
dat$FEV1FVC_rec[!is.na(dat$FEV1FVC)] <- "Recorded"

dat <- dat %>% mutate(FEV1_percpred_rec = factor(FEV1_percpred_rec),
                      FEV1FVC_rec = factor(FEV1FVC_rec))


dat$FEV1_NR <- NULL
dat$FEV1FVC_NR <- NULL

summary(dat$FEV1_percpred_rec)
summary(dat$FEV1FVC_rec)




# Sort out tests

# Initial tests

dat <- dat %>% mutate(all_3_test_types_init = ifelse(test_type_init == "6MWT" & ESWT_at_init == "No", "6MWT only",
                                              ifelse(test_type_init == "ISWT" & ESWT_at_init == "No", "ISWT only",
                                              ifelse(test_type_init == "None" & ESWT_at_init == "No", "None",
                                              ifelse(test_type_init == "Remote", "Remote",
                                                     ifelse(test_type_init == "6MWT" & ESWT_at_init == "Yes", "6MWT and ESWT",
                                                     ifelse(test_type_init == "ISWT" & ESWT_at_init == "Yes", "ISWT and ESWT",
                                                     ifelse(test_type_init == "None" & ESWT_at_init == "Yes", "ESWT only",
                                                                          "check this variable"))))))))
table(dat$all_3_test_types_init)

dat$all_3_test_types_init <- factor(dat$all_3_test_types_init, levels = c("6MWT only", "6MWT and ESWT", "ISWT only", "ISWT and ESWT",
                                                                    "ESWT only", "None", "Remote"))

summary(dat$all_3_test_types_init)



# Test value and practice test variable broken down by test type
dat <- dat %>% mutate(test_value_6MWT_init = ifelse(test_type_init == "6MWT", test_value_init, NA),
                      test_value_ISWT_init = ifelse(test_type_init == "ISWT", test_value_init, NA))

dat <- dat %>% mutate(prac_test_6MWT_init = factor(ifelse(test_type_init == "6MWT",
                                                          as.character(prac_test_init), NA)),
                      prac_test_ISWT_init = factor(ifelse(test_type_init == "ISWT",
                                                          as.character(prac_test_init), NA)))
 
summary(dat$test_value_6MWT_init)
summary(dat$test_value_ISWT_init)

summary(dat$prac_test_6MWT_init)

dat %>% filter(discharge_assess == "Yes") %>%  nrow() # summary()

# Discharge

dat <- dat %>% mutate(all_3_test_types_dis = ifelse(test_type_dis == "6MWT" & (ESWT_at_dis == "No" | is.na(ESWT_at_dis)), "6MWT only",
                                             ifelse(test_type_dis == "ISWT" & (ESWT_at_dis == "No" | is.na(ESWT_at_dis)), "ISWT only",
                                             ifelse(test_type_dis == "None" & (ESWT_at_dis == "No" | is.na(ESWT_at_dis)), "None",
                                             ifelse(test_type_dis == "Remote", "Remote",
                                                    ifelse(test_type_dis == "6MWT" & ESWT_at_dis == "Yes", "6MWT and ESWT",
                                                    ifelse(test_type_dis == "ISWT" & ESWT_at_dis == "Yes", "ISWT and ESWT",
                                                    ifelse(test_type_dis == "None" & ESWT_at_dis == "Yes", "ESWT only",
                                                           "check this variable"))))))))
table(dat$all_3_test_types_dis, dat$all_3_test_types_init, useNA = "ifany")

table(dat$test_type_dis, dat$ESWT_at_dis, useNA = "ifany")

# it shouldn't be possible for someone to be missing a test type at discharge but to also have an ESWT at discharge

summary(dat$test_type_dis)
summary(dat$ESWT_at_dis)


dat$all_3_test_types_dis <- factor(dat$all_3_test_types_dis, levels = c("6MWT only", "6MWT and ESWT", "ISWT only", "ISWT and ESWT",
                                                                          "ESWT only", "None", "Remote"))

summary(dat$all_3_test_types_dis)

# note that you can only input data at discharge if the patient had data inputted for walking test at initial assessment


dat <- dat %>% mutate(test_value_6MWT_dis = ifelse(test_type_dis == "6MWT", test_value_dis, NA),
                      test_value_ISWT_dis = ifelse(test_type_dis == "ISWT", test_value_dis, NA))




dat %>% filter(discharge_assess == "Yes") %>% select(test_type_dis, test_type_init) %>% table(useNA = "ifany")


# No practise test info for discharge


# Where are patients enrolled?

table(dat$centre_based, dat$home_based, useNA = "ifany")


dat <- dat %>% mutate(PR_location = NA)
dat$PR_location[dat$centre_based == 1 & dat$home_based == 0] <- "Centre-based"
dat$PR_location[dat$centre_based == 0 & dat$home_based == 1] <- "Home-based"
dat$PR_location[dat$centre_based == 1 & dat$home_based == 1] <- "Both"


dat$PR_location <- factor(dat$PR_location, levels = c("Centre-based", "Home-based", "Both"))

table(dat$PR_location)

# discharge assess bin

summary(dat$discharge_assess)

dat <- dat %>% mutate(discharge_assess_bin = NA)
dat$discharge_assess_bin[dat$discharge_assess == "Yes"] <- "Yes"
dat$discharge_assess_bin[dat$discharge_assess == "No - DNA" | 
                         dat$discharge_assess == "No - drop-out - health reasons" |
                         dat$discharge_assess == "No - drop-out - patient choice" ] <- "No"

# this is temporarily a character variable in order to make it easier to make the variables
# that are combined with programme type, but at the end I will convert it to a factor

table(dat$discharge_assess, dat$discharge_assess_bin, useNA = "ifany")

# Change the discharge assessment variable...


# Not sure we need this variable...
# 
# 
# dat <- dat %>% rename(discharge_assess_no_reason = discharge_assess)
# dat$discharge_assess_no_reason[dat$discharge_assess_no_reason == "Yes"] <- NA
# dat <- dat %>% mutate(discharge_assess_no_reason = fct_drop(discharge_assess_no_reason))
# 
# table(dat$discharge_assess_no_reason, dat$discharge_assess_bin, useNA = "ifany")

# colnames(dat)
# 
# summary(dat$discharge_assess_bin)
# summary(dat$enrolled)

# Create the variables of discharge assessment by programme type




dat <- dat %>% mutate(discharge_assess_bin_by_centre = ifelse(PR_location == "Centre-based",
                                                            discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_home = ifelse(PR_location == "Home-based",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_both = ifelse(PR_location == "Both",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_both, dat$PR_location, useNA = "ifany")


head(dat)

# Also create the variable exercise plan by programme type
# To do this, I need to convert exercise plan to a character variable first

dat <- dat %>% mutate(exercise_plan = as.character(exercise_plan))




dat <- dat %>% mutate(exercise_plan_by_centre = ifelse(PR_location == "Centre-based",
                                                              exercise_plan, NA))
table(dat$exercise_plan_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_home = ifelse(PR_location == "Home-based",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_both = ifelse(PR_location == "Both",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_both, dat$PR_location, useNA = "ifany")


dat$PR_location <- factor(dat$PR_location)
summary(dat$PR_location)

dat <- dat %>% mutate(exercise_plan = factor(exercise_plan, levels = c("No", "Yes")))

  

dat <- dat %>% mutate(MRC_score_both = "Both known")
dat$MRC_score_both[dat$MRC_score_init == "Not recorded" | 
                     dat$MRC_score_dis == "Not recorded"] <- "1 or more not known"
dat$MRC_score_both[is.na(dat$MRC_score_init) | is.na(dat$MRC_score_dis)] <- NA


table(dat$MRC_score_both, useNA = "ifany")
table(dat$MRC_score_dis, useNA = "ifany")
table(dat$MRC_score_init, useNA = "ifany")

dat %>% select(MRC_score_init, MRC_score_dis) %>% table()

# Done.

# Now, to create the variable that says whether MRC score has changed.
# to do this safely, need to explicitly give the levels for the MRC score.

levels(dat$MRC_score_init) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")
levels(dat$MRC_score_dis) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")

dat <- dat %>% mutate(MRC_score_init_num = as.numeric(MRC_score_init))
dat$MRC_score_init_num[dat$MRC_score_init_num == 6] <- NA

dat <- dat %>% mutate(MRC_score_dis_num = as.numeric(MRC_score_dis))
dat$MRC_score_dis_num[dat$MRC_score_dis_num == 6] <- NA

table(dat$MRC_score_init, dat$MRC_score_init_num)

# This could be useful
dat <- dat %>% mutate(MRC_change_value = MRC_score_dis_num - MRC_score_init_num)

dat <- dat %>% mutate(MRC_change_factor = NA)
dat$MRC_change_factor[dat$MRC_change_value < 0] <- "Improved"
dat$MRC_change_factor[dat$MRC_change_value == 0] <- "Same"
dat$MRC_change_factor[dat$MRC_change_value > 0] <- "Worse"
dat$MRC_change_factor <- factor(dat$MRC_change_factor)

summary(dat$MRC_score_init_num)
summary(dat$MRC_score_dis_num)
summary(dat$MRC_change_value)
table(dat$MRC_change_value)
table(dat$MRC_change_factor)

# Create a value for difference in walking tests and health status questionnaires

dat <- dat %>% mutate(test_value_ISWT_diff = test_value_ISWT_dis - test_value_ISWT_init)
dat <- dat %>% mutate(test_value_6MWT_diff = test_value_6MWT_dis - test_value_6MWT_init)
dat <- dat %>% mutate(test_value_ESWT_diff = ESWT_value_dis - ESWT_value_init)

dat <- dat %>% mutate(CAT_score_diff = CAT_score_dis - CAT_score_init)

dat <- dat %>% mutate(CRQ_dyspnoea_diff = CRQ_dyspnoea_dis - CRQ_dyspnoea_init)
dat <- dat %>% mutate(CRQ_fatigue_diff = CRQ_fatigue_dis - CRQ_fatigue_init)
dat <- dat %>% mutate(CRQ_emotion_diff = CRQ_emotion_dis - CRQ_emotion_init)
dat <- dat %>% mutate(CRQ_mastery_diff = CRQ_mastery_dis - CRQ_mastery_init)


# Create MCID binary values

# MCID is now 35m for ISWT (was 48m)
dat <- dat %>% mutate(MCID_ISWT = NA)
dat$MCID_ISWT[dat$test_value_ISWT_diff < 35] <- "MCID not met"  # changed from 48m
dat$MCID_ISWT[dat$test_value_ISWT_diff >= 35] <- "MCID met"     # changed from 48m
dat$MCID_ISWT <- factor(dat$MCID_ISWT)
summary(dat$MCID_ISWT)

# MCID is 30 for 6MWT
dat <- dat %>% mutate(MCID_6MWT = NA)
dat$MCID_6MWT[dat$test_value_6MWT_diff < 30] <- "MCID not met"
dat$MCID_6MWT[dat$test_value_6MWT_diff >= 30] <- "MCID met"
dat$MCID_6MWT <- factor(dat$MCID_6MWT)
summary(dat$MCID_6MWT)

dat <- dat %>% mutate(MCID_N_ISWT_6MWT = NA)
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_6MWT)] <- 1
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_ISWT)] <- 1


# MCID for CAT is -2
dat <- dat %>% mutate(MCID_CAT = NA)
dat$MCID_CAT[dat$CAT_score_diff <= -2] <- "MCID met"
dat$MCID_CAT[dat$CAT_score_diff > -2] <- "MCID not met"
dat$MCID_CAT <- factor(dat$MCID_CAT)
summary(dat$MCID_CAT)

# MCID for each CRQ is 0.5
dat <- dat %>% mutate(MCID_CRQ_dyspnoea = NA)
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_dyspnoea <- factor(dat$MCID_CRQ_dyspnoea)
summary(dat$MCID_CRQ_dyspnoea)

dat <- dat %>% mutate(MCID_CRQ_fatigue = NA)
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_fatigue <- factor(dat$MCID_CRQ_fatigue)
summary(dat$MCID_CRQ_fatigue)

dat <- dat %>% mutate(MCID_CRQ_emotion = NA)
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_emotion <- factor(dat$MCID_CRQ_emotion)
summary(dat$MCID_CRQ_emotion)

dat <- dat %>% mutate(MCID_CRQ_mastery = NA)
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_mastery <- factor(dat$MCID_CRQ_mastery)
summary(dat$MCID_CRQ_mastery)


dat <- dat %>% mutate(MCID_N_CAT_CRQ = NA)
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CAT)] <- 1
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CRQ_fatigue)] <- 1



dat <- dat %>% mutate(CAT_CRQ_dis_N = NA)
dat$CAT_CRQ_dis_N[!is.na(dat$CAT_dis)] <- 1
dat$CAT_CRQ_dis_N[!is.na(dat$CRQ_dis)] <- 1


dat <- dat %>% mutate(CAT_CRQ_score_diff_N = NA)
dat$CAT_CRQ_score_diff_N[!is.na(dat$CAT_score_diff)] <- 1
dat$CAT_CRQ_score_diff_N[!is.na(dat$CRQ_score_diff)] <- 1


# recode as factors the variables we left before

dat$discharge_assess_bin <- factor(dat$discharge_assess_bin)

# Let's create some benchmarking variables

dat <- dat %>% mutate(BM_start_90 = ifelse(ref_to_start_days_stable_COPD < 90, 1,
                                    ifelse(ref_to_start_days_stable_COPD >= 90, 0, NA)))



# The bit of code below is just to reassure myself that the median for practice test really is 8% with an upper
# quartile of 82%.

# dat %>% group_by(org_code) %>% summarise(practest = sum(BM_prac_test, na.rm = TRUE)/sum(!is.na(BM_prac_test))*100) %>% 
#   arrange(practest) %>% print(., n = 300) 

dat <- dat %>% mutate(BM_prac_test = ifelse(prac_test_init == "No", 0,
                                     ifelse(prac_test_init == "Yes", 1, NA))) 

dat <- dat %>% mutate(BM_discharge_assess = ifelse(discharge_assess_bin == "No", 0,
                                            ifelse(discharge_assess_bin == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_exercise_plan = ifelse(exercise_plan == "No", 0,
                                         ifelse(exercise_plan == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_MCID_exercise = 0)
dat$BM_MCID_exercise[is.na(dat$MCID_6MWT) & is.na(dat$MCID_ISWT)] <- NA
dat$BM_MCID_exercise[dat$MCID_6MWT == "MCID met"] <- 1
dat$BM_MCID_exercise[dat$MCID_ISWT == "MCID met"] <- 1



# table(dat$BM_MCID_exercise, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, dat$MCID_ISWT, useNA = "ifany")
# table(dat$MCID_ISWT, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, useNA = "ifany")


dat <- dat %>% mutate(BM_MCID_CAT_CRQ = 0)
dat$BM_MCID_CAT_CRQ[is.na(dat$MCID_CAT) & is.na(dat$MCID_CRQ_dyspnoea)] <- NA

dat$BM_MCID_CAT_CRQ[dat$MCID_CAT == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_dyspnoea == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_emotion == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_fatigue == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_mastery == "MCID met"] <- 1


table(dat$BM_MCID_CAT_CRQ, dat$MCID_CAT)
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_fatigue, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_emotion, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_dyspnoea, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_mastery, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, useNA = "ifany")


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_exercise_cat = BM_MCID_exercise)
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 0] <- "MCID not met"
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 1] <- "MCID met"
dat$MCID_exercise_cat <- factor(dat$MCID_exercise_cat)
dat$MCID_exercise_cat <- relevel(dat$MCID_exercise_cat, ref = "MCID not met")
summary(dat$MCID_exercise_cat)


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_CAT_CRQ_cat = BM_MCID_CAT_CRQ)
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 0] <- "MCID not met"
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 1] <- "MCID met"
dat$MCID_CAT_CRQ_cat <- factor(dat$MCID_CAT_CRQ_cat)
dat$MCID_CAT_CRQ_cat <- relevel(dat$MCID_CAT_CRQ_cat, ref = "MCID not met")
summary(dat$MCID_CAT_CRQ_cat)



dat <- dat %>% mutate(discharge_assess_bin_by_MRC1_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                          ifelse(MRC_score_init != "Grade 1", NA,
                                                                 as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC2_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 2", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC3_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 3", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC4_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 4", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC5_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 5", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC_NR_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Not recorded", NA,
                                                                               as.character(discharge_assess_bin)))))


dat %>% select(discharge_assess_bin, MRC_score_init) %>% table()

summary(dat$discharge_assess_bin_by_MRC1_init)
summary(dat$discharge_assess_bin_by_MRC2_init)
summary(dat$discharge_assess_bin_by_MRC3_init)
summary(dat$discharge_assess_bin_by_MRC4_init)
summary(dat$discharge_assess_bin_by_MRC5_init)
summary(dat$discharge_assess_bin_by_MRC_NR_init)


# I've checked that all these variables are okay, and correspond to the number of missing values.

summary(dat$ref_location)


# Can split into centre/home-based/both, if required:

# mediSumRound(dat, "scheduled_sess", 0), # good
# # need rec_sess_group and rec_sess indiv.
# # No way to tell where these sessions were based
# mediSumRound(dat, "rec_sess_group", 0),
# mediSumRound(dat, "rec_sess_indiv", 0),

dat$scheduled_sess_by_centre <- dat$scheduled_sess
dat$scheduled_sess_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_group_by_centre <- dat$rec_sess_group
dat$rec_sess_group_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_indiv_by_centre <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_centre[dat$PR_location != "Centre-based"] <- NA

dat$scheduled_sess_by_home <- dat$scheduled_sess
dat$scheduled_sess_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_group_by_home <- dat$rec_sess_group
dat$rec_sess_group_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_indiv_by_home <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_home[dat$PR_location != "Home-based"] <- NA

dat$scheduled_sess_by_both <- dat$scheduled_sess
dat$scheduled_sess_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_group_by_both <- dat$rec_sess_group
dat$rec_sess_group_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_indiv_by_both <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_both[dat$PR_location != "Both"] <- NA


summary(dat$scheduled_sess_by_centre)
summary(dat$rec_sess_group_by_centre)
summary(dat$rec_sess_indiv_by_centre)

summary(dat$scheduled_sess_by_home)
summary(dat$rec_sess_group_by_home)
summary(dat$rec_sess_indiv_by_home)

summary(dat$scheduled_sess_by_both)
summary(dat$rec_sess_group_by_both)
summary(dat$rec_sess_indiv_by_both)

summary(dat$PR_location)



# For the benchmarking, only those with COPD are of interest.


dat %>% select(starts_with("BM")) %>% summary()
dat$BM_start_90[dat$primary_COPD == "Other disease"] <- NA 
dat$BM_prac_test[dat$primary_COPD == "Other disease"] <- NA 
dat$BM_discharge_assess[dat$primary_COPD == "Other disease"] <- NA 
dat$BM_exercise_plan[dat$primary_COPD == "Other disease"] <- NA 
dat$BM_MCID_exercise[dat$primary_COPD == "Other disease"] <- NA 
dat$BM_MCID_CAT_CRQ[dat$primary_COPD == "Other disease"] <- NA 
dat %>% select(starts_with("BM")) %>% summary()

table(dat$discharge_assess)

table(dat$primary_COPD)

# Now, we sort out the new variables we have

table(dat$primary_condition, useNA = "ifany")
table(dat$primary_condition, useNA = "ifany")

table(dat$primary_condition, dat$primary_COPD, useNA = "ifany")

dat$primary_other <- as.character(dat$primary_condition)
dat$primary_other[dat$primary_other == "COPD"] <- NA
dat$primary_other <- factor(dat$primary_other)

table(dat$primary_condition, dat$primary_other, useNA = "ifany")

table(dat$secondary_conditions_asthma, useNA = "ifany")
# table(dat$primary_condition, dat$secondary_conditions_all, useNA = "ifany")

seccon <- dat %>% select(starts_with("secondary_condition")) %>% select(-secondary_conditions_all) %>% colnames()

# let's make sure that our secondary condition doesn't match our primary condition

# Also - 'Lung' is such a stupid category name. Change it.

dat$primary_condition <- fct_recode(dat$primary_condition, `ILD` = "Lung")

# check here whether people have been given the same primary and secondary conditions
# dat %>% mutate(across(starts_with("secondary_condition"), ~as.factor(.))) %>% 
#   summary_factorlist(dependent = "primary_condition", explanatory = seccon)

# for this run, we can see that some people have COPD and ILD as a primary and secondary condition, therefore we 
# recode these ones

table(dat$secondary_conditions_COPD)
dat$secondary_conditions_COPD[dat$primary_condition == "COPD"] <- 0
table(dat$secondary_conditions_COPD)

table(dat$secondary_conditions_ILD)
dat$secondary_conditions_ILD[dat$primary_condition == "ILD"] <- 0
table(dat$secondary_conditions_ILD)


dat %>% filter(assess_date == "2023-04-01") %>% select(impairments_anxiety) %>% table(useNA = "ifany")

# it's annoying but I'm just going to leave this as is it - 34 people missing for impairments 
# probably due to dataset change delay

# no inconsistent impairments with NR or none
dat %>% filter(impairments_none == 1 | impairments_NR == 1) %>% select(starts_with("impairments")) %>% summary()


# the EQ5D thing is quite complicated.
# need to convert the 1-5 score into something different that is then extracted.
# I've used the paper from Devlin et al (2017) to get the scores.

#  I've triple-checked these numbers

# initial EQ5D converted
dat$EQ5D_mobility_init_conv[dat$EQ5D_mobility_init == 1] <- 0
dat$EQ5D_mobility_init_conv[dat$EQ5D_mobility_init == 2] <- 0.058
dat$EQ5D_mobility_init_conv[dat$EQ5D_mobility_init == 3] <- 0.076
dat$EQ5D_mobility_init_conv[dat$EQ5D_mobility_init == 4] <- 0.207
dat$EQ5D_mobility_init_conv[dat$EQ5D_mobility_init == 5] <- 0.274

dat$EQ5D_self_care_init_conv[dat$EQ5D_self_care_init == 1] <- 0
dat$EQ5D_self_care_init_conv[dat$EQ5D_self_care_init == 2] <- 0.050
dat$EQ5D_self_care_init_conv[dat$EQ5D_self_care_init == 3] <- 0.080
dat$EQ5D_self_care_init_conv[dat$EQ5D_self_care_init == 4] <- 0.164
dat$EQ5D_self_care_init_conv[dat$EQ5D_self_care_init == 5] <- 0.203

dat$EQ5D_activities_init_conv[dat$EQ5D_activities_init == 1] <- 0
dat$EQ5D_activities_init_conv[dat$EQ5D_activities_init == 2] <- 0.050
dat$EQ5D_activities_init_conv[dat$EQ5D_activities_init == 3] <- 0.063
dat$EQ5D_activities_init_conv[dat$EQ5D_activities_init == 4] <- 0.162
dat$EQ5D_activities_init_conv[dat$EQ5D_activities_init == 5] <- 0.184

dat$EQ5D_pain_init_conv[dat$EQ5D_pain_init == 1] <- 0
dat$EQ5D_pain_init_conv[dat$EQ5D_pain_init == 2] <- 0.063
dat$EQ5D_pain_init_conv[dat$EQ5D_pain_init == 3] <- 0.084
dat$EQ5D_pain_init_conv[dat$EQ5D_pain_init == 4] <- 0.276
dat$EQ5D_pain_init_conv[dat$EQ5D_pain_init == 5] <- 0.335

dat$EQ5D_depression_init_conv[dat$EQ5D_depression_init == 1] <- 0
dat$EQ5D_depression_init_conv[dat$EQ5D_depression_init == 2] <- 0.078
dat$EQ5D_depression_init_conv[dat$EQ5D_depression_init == 3] <- 0.104
dat$EQ5D_depression_init_conv[dat$EQ5D_depression_init == 4] <- 0.285
dat$EQ5D_depression_init_conv[dat$EQ5D_depression_init == 5] <- 0.289


# discharge EQ5D converted

dat$EQ5D_mobility_dis_conv[dat$EQ5D_mobility_dis == 1] <- 0
dat$EQ5D_mobility_dis_conv[dat$EQ5D_mobility_dis == 2] <- 0.058
dat$EQ5D_mobility_dis_conv[dat$EQ5D_mobility_dis == 3] <- 0.076
dat$EQ5D_mobility_dis_conv[dat$EQ5D_mobility_dis == 4] <- 0.207
dat$EQ5D_mobility_dis_conv[dat$EQ5D_mobility_dis == 5] <- 0.274

dat$EQ5D_self_care_dis_conv[dat$EQ5D_self_care_dis == 1] <- 0
dat$EQ5D_self_care_dis_conv[dat$EQ5D_self_care_dis == 2] <- 0.050
dat$EQ5D_self_care_dis_conv[dat$EQ5D_self_care_dis == 3] <- 0.080
dat$EQ5D_self_care_dis_conv[dat$EQ5D_self_care_dis == 4] <- 0.164
dat$EQ5D_self_care_dis_conv[dat$EQ5D_self_care_dis == 5] <- 0.203

dat$EQ5D_activities_dis_conv[dat$EQ5D_activities_dis == 1] <- 0
dat$EQ5D_activities_dis_conv[dat$EQ5D_activities_dis == 2] <- 0.050
dat$EQ5D_activities_dis_conv[dat$EQ5D_activities_dis == 3] <- 0.063
dat$EQ5D_activities_dis_conv[dat$EQ5D_activities_dis == 4] <- 0.162
dat$EQ5D_activities_dis_conv[dat$EQ5D_activities_dis == 5] <- 0.184

dat$EQ5D_pain_dis_conv[dat$EQ5D_pain_dis == 1] <- 0
dat$EQ5D_pain_dis_conv[dat$EQ5D_pain_dis == 2] <- 0.063
dat$EQ5D_pain_dis_conv[dat$EQ5D_pain_dis == 3] <- 0.084
dat$EQ5D_pain_dis_conv[dat$EQ5D_pain_dis == 4] <- 0.276
dat$EQ5D_pain_dis_conv[dat$EQ5D_pain_dis == 5] <- 0.335

dat$EQ5D_depression_dis_conv[dat$EQ5D_depression_dis == 1] <- 0
dat$EQ5D_depression_dis_conv[dat$EQ5D_depression_dis == 2] <- 0.078
dat$EQ5D_depression_dis_conv[dat$EQ5D_depression_dis == 3] <- 0.104
dat$EQ5D_depression_dis_conv[dat$EQ5D_depression_dis == 4] <- 0.285
dat$EQ5D_depression_dis_conv[dat$EQ5D_depression_dis == 5] <- 0.289

# Then, we do the differences using the normal score as this is more easily understandable

dat <- dat %>% mutate(EQ5D_mobility_diff = EQ5D_mobility_dis - EQ5D_mobility_init)
dat <- dat %>% mutate(EQ5D_self_care_diff = EQ5D_self_care_dis - EQ5D_self_care_init)
dat <- dat %>% mutate(EQ5D_activities_diff = EQ5D_activities_dis - EQ5D_activities_init)
dat <- dat %>% mutate(EQ5D_pain_diff = EQ5D_pain_dis - EQ5D_pain_init)
dat <- dat %>% mutate(EQ5D_depression_diff = EQ5D_depression_dis - EQ5D_depression_init)

dat <- dat %>% mutate(EQ5D_mobility_diff = EQ5D_mobility_dis - EQ5D_mobility_init)
dat <- dat %>% mutate(EQ5D_self_care_diff = EQ5D_self_care_dis - EQ5D_self_care_init)
dat <- dat %>% mutate(EQ5D_activities_diff = EQ5D_activities_dis - EQ5D_activities_init)
dat <- dat %>% mutate(EQ5D_pain_diff = EQ5D_pain_dis - EQ5D_pain_init)
dat <- dat %>% mutate(EQ5D_depression_diff = EQ5D_depression_dis - EQ5D_depression_init)

# Then this is the difference in the visual analogue score
dat <- dat %>% mutate(EQ5D_VAS_diff = EQ5D_VAS_dis - EQ5D_VAS_init)

# And these are the combined scores (i.e. utility indices)
dat <- dat %>% mutate(EQ5D_utility_index_init = 1 - EQ5D_mobility_init_conv - EQ5D_self_care_init_conv - 
                        EQ5D_activities_init_conv - EQ5D_pain_init_conv - EQ5D_depression_init_conv)

dat <- dat %>% mutate(EQ5D_utility_index_dis = 1 - EQ5D_mobility_dis_conv - EQ5D_self_care_dis_conv - 
                        EQ5D_activities_dis_conv - EQ5D_pain_dis_conv - EQ5D_depression_dis_conv)

summary(dat$EQ5D_utility_index_init)
summary(dat$EQ5D_utility_index_dis)

hist(dat$EQ5D_utility_index_init) # not normally distributed
hist(dat$EQ5D_utility_index_dis)  # not normally distributed


dat %>% filter(EQ5D_utility_index_init == 1) %>% select(starts_with("EQ5D")) %>% summary()
dat %>% filter(EQ5D_utility_index_init < 0) %>% select(starts_with("EQ5D")) %>% summary()


# create the difference in overall health state

dat <- dat %>% mutate(EQ5D_utility_index_diff = EQ5D_utility_index_dis - EQ5D_utility_index_init)
summary(dat$EQ5D_utility_index_diff)


# and create the MCIDs

dat <- dat %>% mutate(MCID_EQ5D_utility_index = NA)
dat$MCID_EQ5D_utility_index[dat$EQ5D_utility_index_diff >= 0.051] <- "MCID met"
dat$MCID_EQ5D_utility_index[dat$EQ5D_utility_index_diff < 0.051] <- "MCID not met"
dat$MCID_EQ5D_utility_index <- factor(dat$MCID_EQ5D_utility_index)
summary(dat$MCID_EQ5D_utility_index)

dat <- dat %>% mutate(MCID_EQ5D_VAS = NA)
dat$MCID_EQ5D_VAS[dat$EQ5D_VAS_diff >= 6.9] <- "MCID met"
dat$MCID_EQ5D_VAS[dat$EQ5D_VAS_diff < 6.9] <- "MCID not met"
dat$MCID_EQ5D_VAS <- factor(dat$MCID_EQ5D_VAS)
summary(dat$MCID_EQ5D_VAS)

dat$MCID_EQ5D_UI_or_VAS <- NA
dat$MCID_EQ5D_UI_or_VAS[dat$MCID_EQ5D_utility_index == "MCID not met" &
                          dat$MCID_EQ5D_VAS == "MCID not met"] <- "MCID not met for UI or VAS"
dat$MCID_EQ5D_UI_or_VAS[dat$MCID_EQ5D_utility_index == "MCID met" |
                          dat$MCID_EQ5D_VAS == "MCID met"] <- "MCID met for UI or VAS"
dat$MCID_EQ5D_UI_or_VAS <- factor(dat$MCID_EQ5D_UI_or_VAS)

table(dat$MCID_EQ5D_utility_index, dat$MCID_EQ5D_VAS)
table(dat$MCID_EQ5D_utility_index, dat$MCID_EQ5D_VAS, dat$MCID_EQ5D_UI_or_VAS)


#////////////////////////////// Do some data cleaning

# sort out the people who have an ESWT at discharge but nothing recorded for their other walking tests at discharge

# Total number of admissions in dataset:

nrow(dat)

summary(dat$assess_date)
summary(dat$discharge_date)



# How many people are in the dataset who were assessed before 1st April 2023?

dat %>% filter(assess_date < "2023-04-01") %>% nrow()

# How many people are assessed after the 31st March 2024?

dat %>% filter(assess_date > "2024-03-31") %>% nrow()


nrow(dat)

# Now we do some data cleaning. Is there anyone who is inconsistent in their dates?

dat %>% filter(assess_date < ref_date) %>% nrow()
dat %>% filter(start_date < assess_date) %>% nrow()
dat %>% filter(discharge_date < start_date) %>% nrow()

# if people have an obviously wrong referral date they should be dropped as not clear when 
# their actual referral date was (looks like 2 birthdays have been inputted erroneously)
nrow(dat)
dat <- dat %>% filter(ref_date > "2017-12-31" | is.na(ref_date))
nrow(dat)


dat <- dat %>% filter(assess_date >= ref_date | is.na(assess_date) | is.na(ref_date)) 

# does anyone receive their start date before their assessment date?

dat %>% filter(start_date < assess_date) %>% nrow()
dat <- dat %>% filter(start_date >= assess_date | is.na(assess_date) | is.na(start_date)) 

# Is anyone discharged before their start date?
dat %>% filter(discharge_date < start_date) %>% nrow()
dat <- dat %>% filter(discharge_date >= start_date | is.na(discharge_date) | is.na(start_date)) 

nrow(dat)


# Is anyone marked as missing something who has that same thing encoded?

# referral date
dat %>% filter(is.na(ref_date) & ref_date_rec == "Known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()

dat %>% filter(is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Known") %>% nrow()

dat <- dat %>% filter(!(is.na(ref_date) & ref_date_rec == "Known"))
dat <- dat %>% filter(!(!is.na(ref_date) & ref_date_rec == "Not known"))

nrow(dat)

# This is all fine.

table(dat$FEV1_percpred)
table(dat$FEV1_percpred_rec)

dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% select(contains("FEV")) %>% summary()

colnames(dat)

# recording state is now based on whether or not they have a value, so this is redundant

# FEV1 perc pred
dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()
dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()

# FEV1FVC
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()



# assess whether there are duplicate records. Done based on:
# country, trust_code, patient_ID, org_code, LSOA, age, gender, ethnicity, ref_date, 
# ref_location, assess_date, spirometry, and test scores at assessment. This many duplicates:

dat %>% select(country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
               ref_location, assess_date, FEV1_percpred, FEV1FVC, ends_with("init")) %>% duplicated() %>% sum()

nrow(dat)

# After duplicates removed, we have this many people:
dat <- dat[!duplicated(select(dat, country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
                              ref_location, assess_date, FEV1_percpred, FEV1FVC, ends_with("init"))), ]

nrow(dat)



# We have a few columns that are character when they should be factor, so we convert them to factor,
# but we leave 'study_ID', 'patient_ID' and 'LSOA' as character.

dat %>% select_if(is.character) %>% colnames()

dat <- dat %>% mutate_if(is.character, factor) %>% 
           mutate_at(c("study_ID", "patient_ID", "LSOA"), as.character)


saveRDS(dat, "G:/Alex Harley/Audit_2023_onwards/2023-2024/PR/Data/tidyData/PR_clinical_2023-24_cleaned.RDS")


# done! 

