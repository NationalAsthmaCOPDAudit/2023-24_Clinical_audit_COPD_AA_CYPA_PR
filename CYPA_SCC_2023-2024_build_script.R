#                                                             #
# CYPA SCC 2023-24 build script                               #
#                                                             #
# Author: Alex Adamson                                        #
# Date created:  31st May 2024                                #
# Added DB selected elements 4th September 2024               #


library(tidyverse)

checkSame <- function(newdata, olddata) {
  
  library(dplyr)
  
  newdat <- data.frame(variable = colnames(newdata), in_new = 1, class_new = NA) 
  
  for (i in 1:ncol(newdata)) {
    newdat$class_new[i] <- paste(class(newdata[[i]]), collapse = " ")
  }
  
  
  olddat <- data.frame(variable = colnames(olddata), in_old = 1, class_old = NA)
  
  for (i in 1:ncol(olddata)) {
    olddat$class_old[i] <- paste(class(olddata[[i]]), collapse = " ")
  }
  
  
  samesame <- dplyr::full_join(newdat, olddat, by = "variable")
  samesame$in_both <- 0
  samesame$in_both[samesame$in_new == 1 & samesame$in_old == 1] <- 1
  return(samesame)
  
}

dat_old <- read.csv("G:/Alex Harley/Audit_2023_onwards/2022-2023/CYPA/Data/rawData/NACAP-CYPA-2204-2303-v103+LSOA-NDO-Imperial.csv",
                header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

dat <- read.csv("G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Data/rawData/CYPA-2405-Imperial.csv",
                    header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

checkSame(dat, dat_old)


dat_old <- dat_old %>% select(study_ID = STUDYID,
                      patient_ID = PATIENTID,
                      LSOA = lsoa11,
                      hosp_code = Org,
                    #  hosp_name = OrgName,
                      trust_code = TrustCode,
                    #  trust_name = Trust.Now,
                    #  region = Region,
                      country = Country,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                      ambulance = X1.2.Arrive.by.Ambulance,
                      age = X.1.2a.Age.At.Arrival,
                      gender = X2.3.Gender,
                      smoke_status = X3.1.Smoking,
                      SH_smoke = X3.2.Smoking.exposure,
                      heart_rate = X4.1.Heart.Rate..BPM.,
                      resp_rate = X4.2.Respiratory.Rate..BPM.,
                      oxygen_sat_value = X4.3.Oxygen.Saturation....,
                      oxygen_sat_recorded = X4.3.1.OxygenSaturation.NR,
                      oxygen_sat_measurement_type = X4.3a.Oxygen.Measurement,
                      # oxygen_supp = X4.3b.Supplementary.oxygen,
                      PEF_init_value = X4.4.Peak.Flow..Post.Arrival.,
                      PEF_init_recorded = X4.4.1.Peak.Flow.NR,
                      PEF_prev_value = X4.4a.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X4.4a.1.Previous.Best.Peak.Flow.NR,
                      PEF_predict_value = X4.4b.Predicted.Peak.Flow,
                      PEF_predict_recorded = X4.4b.1.Predicted.Peak.Flow.NR,
                      RSR = X5.1.MDT.Specialist.Review,
                      steroids_24hr_prev = X5.2.Steroids.Pre.Arrival,
                      steroids_admin = X5.3.Steroids,
                      steroids_admin_date = X5.3a.Steroids.Date,
                      steroids_admin_time = X5.3b.Steroids.Time,
                      b2a_1hr_prev = X5.4.Agonists.B2.Pre.Arrival,
                      b2a_admin = X5.5.Agonists.B2.On.Arrival,
                      b2a_admin_date = X5.5a.Agonists.B2.Date,
                      b2a_admin_time = X5.5b.Agonists.B2.Time,
                      IV_med_total = X5.6.Intravenous.Medications,
                      IV_med_aminophylline = X5.6.Intravenous.Medications...Aminophylline,
                      IV_med_ketamine = X5.6.Intravenous.Medications...Ketamine,
                      IV_med_mag_sulphate = X5.6.Intravenous.Medications...Magnesium.sulphate,
                      IV_med_b2a = X5.6.Intravenous.Medications...B2.agonists,
                      IV_med_none = X5.6.Intravenous.Medications...No,
                      crit_care_total = X5.7.Critical.Care.Transfer,
                      crit_care_HDU = X5.7.Critical.Care.Transfer...Yes...HDU,
                      crit_care_ICU = X5.7.Critical.Care.Transfer...Yes...ICU,
                      crit_care_none = X5.7.Critical.Care.Transfer...No,
                      life_status = X6.1.Discharge.Life.Status,
                      discharge_date = X6.2a.Discharge.Date,
                      discharge_time = X6.2b.Discharge.Time,
                      discharge_bundle = X6.3.Discharge.Bundle,
                      discharge_elements_all = X6.4.Discharge.Elements, # completely useless: drop.
                      DB_inhaler = X6.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X6.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X6.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X6.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X6.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X6.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_parent_smoke = X6.4.Discharge.Elements...Parent.carer.tobacco.dependency.addressed,
                      DB_comm_FU_2_days =  X6.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
                      DB_asthma_clinic_4_weeks = X6.4.Discharge.Elements...Paediatric.asthma.clinic.requested.within.4.weeks,
                      DB_RSR_if_LT = X6.4.Discharge.Elements...Paediatric.respiratory.specialist.review.if.there.have.been.life.threatening.features,
                      DB_none = X6.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X6.5.Inhaled.Steroids.At.Discharge,
                      #  oral_steroids_dis = X7.2.Oral.Steroids.at.Discharge,
                      oral_steroids_rescue_history = X6.6.Rescue.Oral.Steroids...2.in.12m,
                      referred_for_FU = X6.7.Referred.for.Followup,
                      overseas = X..Overseas.or.Non.NHS)
#  completion_status = Completion..)
# Dataset.Version   # useless - drop


# dat <- dat %>% rename(study_ID = ROWID,  # first use rename, then use select when all verified
                      
dat <- dat %>% rename(study_ID = ROWID,
                      patient_ID = PATIENTID,
                      LSOA = lsoa11,
                      hosp_code = Org,
                      hosp_name = OrgName,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      region = Region,
                      country = Country,
                      integrated_care_system = ICS,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                    #  ambulance = X1.2.Arrive.by.Ambulance,
                      age = X.1.2a.Age.At.Arrival,
                      gender = X2.3.Gender,
                    ethnicity = X2.5.Ethnicity,
                    impairments_none = X..2.6.Impairments...No.None,
                    impairments_anxiety = X..2.6.Impairments...Anxiety,
                    impairments_depression = X..2.6.Impairments...Depression,
                    impairments_self_harm = X..2.6.Impairments...Self.harm,
                    impairments_eating_disorder = X..2.6.Impairments...Eating.disorder,
                    impairments_MHS = X..2.6.Impairments...Known.to.MHS,
                    impairments_severe_mental_illness = X..2.6.Impairments...Severe.mental.illness,
                    impairments_other = X..2.6.Impairments...Other,
                    impairments_NR = X..2.6.Impairments...Not.recorded, 
                    #smoke_status = X2.5.Smoking.Status,
                    smoke_tobacco = X3.1a.Smoking.Tobacco, # new
                    smoke_shisha = X3.1b.Smoking.Shisha, # new
                    smoke_cannabis = X3.1c.Smoking.Cannabis, # new
                    smoke_other = X3.1d.Smoking.Other, # new
                    vaping_status = X3.2.Vaping.status, # new
                      SH_smoke = X3.3.Smoking.exposure,
                      heart_rate = X4.1.Heart.Rate..BPM.,
                      resp_rate = X4.2.Respiratory.Rate..BPM.,
                      oxygen_sat_value = X4.3.Oxygen.Saturation....,
                      oxygen_sat_recorded = X4.3.1.OxygenSaturation.NR,
                      oxygen_sat_measurement_type = X4.3a.Supplementary.Oxygen.Measurement,
                      # oxygen_supp = X4.3b.Supplementary.oxygen,
                      PEF_init_value = X4.4.Peak.Flow..Post.Arrival.,
                      PEF_init_recorded = X4.4.1.Peak.Flow.NR,
                      PEF_prev_value = X4.4a.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X4.4a.1.Previous.Best.Peak.Flow.NR,
                      PEF_predict_value = X4.4b.Predicted.Peak.Flow,
                      PEF_predict_recorded = X4.4b.1.Predicted.Peak.Flow.NR,
                    symptoms_breathless = X..4.5.Symptoms...Breathlessness,                                               
                    symptoms_silent_chest = X..4.5.Symptoms...Silent.chest,                                                 
                    symptoms_cyanosis = X..4.5.Symptoms...Cyanosis,                                                     
                    symptoms_poor_respiratory_effort = X..4.5.Symptoms...Poor.respiratory.effort,                                      
                    symptoms_hypotension = X..4.5.Symptoms...Hypotension,                                                  
                    symptoms_exhaustion = X..4.5.Symptoms...Exhaustion,                                                   
                    symptoms_confusion = X..4.5.Symptoms...Confusion,                                      
                    symptoms_none = X..4.5.Symptoms...None, 
                      RSR = X5.1.MDT.Specialist.Review,
                      steroids_24hr_prev = X5.2.Steroids.Pre.Arrival,
                      steroids_admin = X5.3.Systemic.Steroids.After.Arrival,
                      steroids_admin_date = X5.3a.Steroids.Date,
                      steroids_admin_time = X5.3b.Steroids.Time,
                      b2a_1hr_prev = X5.4.Agonists.B2.Pre.Arrival,
                      b2a_admin = X5.5.Agonists.B2.On.Arrival,
                      b2a_admin_date = X5.5a.Agonists.B2.Date,
                      b2a_admin_time = X5.5b.Agonists.B2.Time,
                      IV_med_total = X5.6.Intravenous.Medications,
                      IV_med_aminophylline = X5.6.Intravenous.Medications...Aminophylline,
                      IV_med_ketamine = X5.6.Intravenous.Medications...Ketamine,
                      IV_med_mag_sulphate = X5.6.Intravenous.Medications...Magnesium.sulphate,
                      IV_med_b2a = X5.6.Intravenous.Medications...B2.agonists,
                      IV_med_none = X5.6.Intravenous.Medications...No,
                      crit_care_total = X5.7.Critical.Care.Transfer,
                      crit_care_HDU = X5.7.Critical.Care.Transfer...Yes...HDU,
                      crit_care_ICU = X5.7.Critical.Care.Transfer...Yes...ICU,
                      crit_care_none = X5.7.Critical.Care.Transfer...No,
                      life_status = X6.1.Discharge.Life.Status,
                      discharge_date = X6.2a.Discharge.Date,
                      discharge_time = X6.2b.Discharge.Time,
                      discharge_bundle = X6.3.Discharge.Bundle.Completed,
                      discharge_elements_all = X6.4.Discharge.Elements, # completely useless: drop.
                      DB_inhaler = X6.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X6.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X6.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X6.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X6.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X6.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_parent_smoke = X6.4.Discharge.Elements...Parent.carer.tobacco.dependency.addressed,
                      DB_comm_FU_2_days =  X6.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
                      DB_asthma_clinic_4_weeks = X6.4.Discharge.Elements...Paediatric.asthma.clinic.requested.within.4.weeks,
                      DB_RSR_if_LT = X6.4.Discharge.Elements...Paediatric.respiratory.specialist.review.if.there.have.been.life.threatening.features,
                      DB_none = X6.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X6.5.Inhaled.Steroids.At.Discharge,
                      #  oral_steroids_dis = X7.2.Oral.Steroids.at.Discharge,
                      oral_steroids_rescue_history = X6.6.Rescue.Oral.Steroids...2.in.12m,
                      referred_for_FU = X6.7.Referred.for.Followup)
                  #    overseas = X..Overseas.or.Non.NHS)
#  completion_status = Completion..)

colnames(dat)

# - smoking status -> shisha/tobacco/cannabis/other, current/ex/never/NR
# ethnicity
# impairments
# asthma additional symptoms - doesn't need to be directly reported  but can be used to properly
# classify asthma severity


# removed: arrival by ambulance



# No. records in original dataset:
nrow(dat)

# nlc("No. records that are drafts or test hospitals:")
# dat %>% filter(completion_status != "100%" | hosp_code == "YYY") %>% nrow()

# No. records that are drafts or test hospitals:
dat %>% filter(hosp_code == "YYY") %>% nrow()

# dat <- dat %>% filter(completion_status == "100%" & hosp_code != "YYY")
# nlc("No. records after draft records and test hospitals removed:")
# nrow(dat)

dat <- dat %>% filter(hosp_code != "YYY")
# No. records after draft records and test hospitals removed:
nrow(dat)

# # remove that column because we don't need it any more
# dat <- dat %>% select(-completion_status)


# # Remove overseas patients
# nlc("Number of overseas patients:") # bear in mind this has gone down because some overseas 
#                                     # were from test hospitals
# dat %>% filter(overseas == 1) %>% nrow()
# 
# dat <- dat %>% filter(overseas != 1)
# nlc("No. records after overseas removed:")
# nrow(dat)
# 
# 
# # remove that column because we don't need it any more
# dat <- dat %>% select(-overseas)

# Need to add in the empty gender factors and smoking status factors, and while we're doing it we might as well
# put it in the correct order

summary(dat$smoke_status)
summary(dat$gender)

dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Other", "Not recorded/Preferred not to say"))


table(dat$smoke_tobacco)
table(dat$smoke_shisha)
table(dat$smoke_cannabis)
table(dat$smoke_other)
table(dat$vaping_status)

dat$smoke_tobacco <- factor(dat$smoke_tobacco, levels = c("Never", "Ex", "Current", "Not recorded"))
dat$smoke_shisha <- factor(dat$smoke_shisha, levels = c("Never", "Ex", "Current", "Not recorded"))
dat$smoke_cannabis <- factor(dat$smoke_cannabis, levels = c("Never", "Ex", "Current", "Not recorded"))
dat$smoke_other <- factor(dat$smoke_other, levels = c("Never", "Ex", "Current", "Not recorded"))
dat$vaping_status <- factor(dat$vaping_status, levels = c("Never", "Ex", "Current", "Not recorded"))

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))

# Everything seems to be alright at this point.

# Now would probably be a good point to link to the LSOAs.

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

summary(dat$IMD_quintile)

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

dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)


# Having to bind the rows together has made all of our date and time variables character. Let's change them back to factor.




dat$arrival_date <- as.Date(dat$arrival_date, "%d/%m/%Y")
dat$steroids_admin_date <- as.Date(dat$steroids_admin_date, "%d/%m/%Y")
dat$b2a_admin_date <- as.Date(dat$b2a_admin_date, "%d/%m/%Y")
dat$discharge_date <- as.Date(dat$discharge_date, "%d/%m/%Y")

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)

summary(dat$arrival_date)
summary(dat$arrival_time)
summary(dat$steroids_admin_date)
summary(dat$steroids_admin_time)
summary(dat$b2a_admin_date)
summary(dat$b2a_admin_time)
summary(dat$discharge_date)
summary(dat$discharge_time)

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)

# dat$arrival_time[dat$arrival_time != ""] <- paste0(dat$arrival_time[dat$arrival_time != ""]
#                                                    
#                                                    
#                                                    
# dat$steroids_admin_time[dat$steroids_admin_time)
# dat$b2a_admin_time[dat$b2a_admin_time)
# dat$discharge_time[dat$discharge_time)
# 
# 
head(dat$arrival_time)



# dat <- dat %>% mutate(arrival_time = ifelse(arrival_time == "", "", 
#                                      paste0(arrival_time, ":00")),
#                       steroids_admin_time = ifelse(steroids_admin_time == "", "", 
#                                             paste0(steroids_admin_time, ":00")),
#                       b2a_admin_time = ifelse(b2a_admin_time == "", "", 
#                                             paste0(b2a_admin_time, ":00")),
#                       discharge_time = ifelse(discharge_time == "", "", 
#                                             paste0(discharge_time, ":00")))


head(dat$steroids_admin_time)
head(dat$arrival_time)
head(dat$arrival_date)
head(dat$discharge_time)
head(dat$discharge_date)

# We can try converting times to 'difftime' when we try and create the table of when people are given
# particular things.


dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M"),
                      steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time), 
                                                     format="%Y-%m-%d %H:%M"),
                      b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M"),
                      discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M"))

head(dat$arrival_DT)
head(dat$steroids_admin_DT)
summary(dat$steroids_admin_DT)

# Life status
summary(dat$life_status)

# dat %>% filter(life_status == "Died as inpatient") 

dat <- dat %>% mutate(life_status = recode_factor(life_status, Yes = "Alive"))
dat$life_status <- factor(dat$life_status, levels = c(levels(dat$life_status), "Died as inpatient"))

summary(dat$life_status)

# head(dat)


# Time to b2a minutes

dat <- dat %>% mutate(arrival_to_b2a_minutes = difftime(b2a_admin_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_b2a_minutes)
dat$arrival_to_b2a_minutes <- as.integer(dat$arrival_to_b2a_minutes)

# Time to steroids in hours

dat <- dat %>% mutate(arrival_to_steroids_hours = difftime(steroids_admin_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_steroids_hours)
dat$arrival_to_steroids_hours <- as.numeric(dat$arrival_to_steroids_hours)

summary(dat$arrival_to_steroids_hours)

# length of stay hours

dat <- dat %>% mutate(LOS_hours = difftime(discharge_DT, arrival_DT, units = "hours"))
head(dat$LOS_hours)
dat$LOS_hours <- as.numeric(dat$LOS_hours)

# Need to remove those who were transferred from this

summary(dat$discharge_bundle)

dat <- dat %>% mutate(LOS_hours = ifelse(discharge_bundle == "Patient transferred to another hospital" | 
                                           life_status == "Died as inpatient", 
                                         NA, LOS_hours))


dat$DB_FU_any <- dat$DB_comm_FU_2_days
dat$DB_FU_any[dat$DB_spec_review_4_weeks == 1] <- 1

table(dat$DB_FU_any, useNA = "ifany")

# # # # 

# # # # NOTE THIS HAS CHANGED NOW THAT DAYTIME/NIGHT TIME AUTOMATICALLY INCLUDED.

# also here, we split into daytime and night time

# discharge weekday/weekend daytime/night time.


colnames(dat)



# # # # # #



# day of arrival and day of discharge


dat <- dat %>% mutate(arrival_day_of_week = weekdays(arrival_date, abbreviate = FALSE))

dat$arrival_day_of_week <- ordered(dat$arrival_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

table(dat$arrival_day_of_week)


dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))

dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                     "Friday", "Saturday", "Sunday"))


# Those who transferred need to be removed from the discharge day of week

# Using ifelse here changes them to factor levels so I'm doing it the old school method

# dat <- dat %>% mutate(discharge_day_of_week = ifelse(discharge_bundle == "Patient transferred to another hospital", 
#                                          NA, discharge_day_of_week))

dat$discharge_day_of_week[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA
dat$discharge_day_of_week[dat$life_status == "Died as inpatient"] <- NA

summary(dat$discharge_day_of_week)

# time of arrival for the time of arival table.
# basically times are so annoying to work with, so what I've done is given each time an associated date of 
# 1970-01-01 (the common origin date) and then subtracted the time from 1970-01-01 00:00:00 to give minutes
# into the day. Seems to have worked well

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.POSIXct(paste("1970-01-01", arrival_time))) %>%
  mutate(arrival_time_mins_from_midnight = difftime(arrival_time_mins_from_midnight, 
                                                    "1970-01-01 00:00:00", units = "mins"))
head(dat$arrival_time_mins_from_midnight)

dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(arrival_time_mins_from_midnight) %>% head(10)
dat %>% select(arrival_time, arrival_time_mins_from_midnight) %>% 
  arrange(desc(arrival_time_mins_from_midnight)) %>% head(10)

dat <- dat %>% mutate(arrival_time_mins_from_midnight = as.integer(arrival_time_mins_from_midnight))



summary(dat$arrival_time_mins_from_midnight)

dat <- dat %>% mutate(weekday_weekend_arrival = factor(ifelse( (arrival_day_of_week == "Friday" & arrival_time_mins_from_midnight >= round((18.5*60), 0)) |
                                                                   arrival_day_of_week == "Saturday" |
                                                                   arrival_day_of_week == "Sunday" |
                                                                   (arrival_day_of_week == "Monday" & arrival_time_mins_from_midnight < round(8*60, 0)),
                                                                 "Weekend arrival", 
                                                                 "Weekday arrival"),
                                                         levels = c("Weekday arrival", "Weekend arrival")))

#             "Weekend arrival (18:00 Friday to 07:59 Monday)", "Weekday arrival (08:00 Monday to 17:59 Friday)"),


dat$daynight_arrival <- "Day time"
dat$daynight_arrival[dat$arrival_time_mins_from_midnight < round(8*60, 0)] <- "Night time"
dat$daynight_arrival[dat$arrival_time_mins_from_midnight >= round((18.5*60), 0)] <- "Night time"

dat$daynight_arrival <- factor(dat$daynight_arrival, levels = c("Day time", "Night time"))


dat$weekday_weekend_daynight_arrival <- "Weekday day time"
dat$weekday_weekend_daynight_arrival[dat$weekday_weekend_arrival == "Weekday arrival" & dat$daynight_arrival == "Night time"] <- "Weekday night time"
dat$weekday_weekend_daynight_arrival[dat$weekday_weekend_arrival == "Weekend arrival" & dat$daynight_arrival == "Day time"] <- "Weekend day time"
dat$weekday_weekend_daynight_arrival[dat$weekday_weekend_arrival == "Weekend arrival" & dat$daynight_arrival == "Night time"] <- "Weekend night time"

dat$weekday_weekend_daynight_arrival <- factor(dat$weekday_weekend_daynight_arrival, levels = c("Weekday day time", "Weekday night time", 
                                                                                                    "Weekend day time", "Weekend night time"))
summary(dat$weekday_weekend_daynight_arrival)

summary(dat$daynight_arrival)

table(dat$daynight_arrival, dat$weekday_weekend_arrival, dat$arrival_day_of_week)

# and for discharge


dat <- dat %>% mutate(discharge_time_mins_from_midnight = as.POSIXct(paste("1970-01-01", discharge_time))) %>%
  mutate(discharge_time_mins_from_midnight = difftime(discharge_time_mins_from_midnight, 
                                                    "1970-01-01 00:00:00", units = "mins"))
head(dat$discharge_time_mins_from_midnight)

dat %>% select(discharge_time, discharge_time_mins_from_midnight) %>% 
  arrange(discharge_time_mins_from_midnight) %>% head(10)
dat %>% select(discharge_time, discharge_time_mins_from_midnight) %>% 
  arrange(desc(discharge_time_mins_from_midnight)) %>% head(10)

dat <- dat %>% mutate(discharge_time_mins_from_midnight = as.integer(discharge_time_mins_from_midnight))



summary(dat$discharge_time_mins_from_midnight)

dat <- dat %>% mutate(weekday_weekend_discharge = factor(ifelse( (discharge_day_of_week == "Friday" & discharge_time_mins_from_midnight >= round((18.5*60), 0)) |
                                                                 discharge_day_of_week == "Saturday" |
                                                                 discharge_day_of_week == "Sunday" |
                                                                 (discharge_day_of_week == "Monday" & discharge_time_mins_from_midnight < round(8*60, 0)),
                                                               "Weekend discharge", 
                                                               "Weekday discharge"),
                                                       levels = c("Weekday discharge", "Weekend discharge")))

#             "Weekend discharge (18:00 Friday to 07:59 Monday)", "Weekday discharge (08:00 Monday to 17:59 Friday)"),


dat$daynight_discharge <- "Day time"
dat$daynight_discharge[dat$discharge_time_mins_from_midnight < round(8*60, 0)] <- "Night time"
dat$daynight_discharge[dat$discharge_time_mins_from_midnight >= round((18.5*60), 0)] <- "Night time"

dat$daynight_discharge <- factor(dat$daynight_discharge, levels = c("Day time", "Night time"))


dat$weekday_weekend_daynight_discharge <- "Weekday day time"
dat$weekday_weekend_daynight_discharge[dat$weekday_weekend_discharge == "Weekday discharge" & dat$daynight_discharge == "Night time"] <- "Weekday night time"
dat$weekday_weekend_daynight_discharge[dat$weekday_weekend_discharge == "Weekend discharge" & dat$daynight_discharge == "Day time"] <- "Weekend day time"
dat$weekday_weekend_daynight_discharge[dat$weekday_weekend_discharge == "Weekend discharge" & dat$daynight_discharge == "Night time"] <- "Weekend night time"

dat$weekday_weekend_daynight_discharge <- factor(dat$weekday_weekend_daynight_discharge, levels = c("Weekday day time", "Weekday night time", 
                                                                                                "Weekend day time", "Weekend night time"))
summary(dat$weekday_weekend_daynight_discharge)

summary(dat$daynight_discharge)

table(dat$daynight_discharge, dat$weekday_weekend_discharge, dat$discharge_day_of_week)


# Peak flow things
summary(dat$PEF_init_recorded)
summary(dat$PEF_init_value)

dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% filter(!is.na(PEF_prev_value)) %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% 
  table(useNA = "ifany")

# Invalid PEF values? No.
dat %>% filter(PEF_init_value < 30) %>% nrow()
dat %>% filter(PEF_init_value > 800) %>% nrow()

dat %>% filter(PEF_prev_value < 30) %>% nrow()
dat %>% filter(PEF_prev_value > 800) %>% nrow()

dat %>% filter(PEF_predict_value < 30) %>% nrow()
dat %>% filter(PEF_predict_value > 800) %>% nrow()

# PEF / recorded inconsistencies? No

dat %>% filter(!is.na(PEF_init_value)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & !is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value) & is.na(PEF_init_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% select(PEF_init_recorded) %>% table()

dat %>% filter(!is.na(PEF_prev_value)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_prev_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_prev_value) & is.na(PEF_prev_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_predict_value)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & !is.na(PEF_predict_recorded)) %>% nrow()
dat %>% filter(!is.na(PEF_predict_value) & is.na(PEF_predict_recorded)) %>% nrow()

dat %>% filter(!is.na(PEF_prev_value) & !is.na(PEF_predict_value)) %>% nrow()

summary(dat$PEF_init_recorded)

# Make the 'recorded' variable clearer, by adding in 'recorded' for people that have an associated value

dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)

dat$PEF_init_recorded[(!is.na(dat$PEF_init_value))] <- "Recorded"
dat$PEF_prev_recorded[(!is.na(dat$PEF_prev_value))] <- "Recorded"
dat$PEF_predict_recorded[(!is.na(dat$PEF_predict_value))] <- "Recorded"

dat$PEF_init_recorded <- factor(dat$PEF_init_recorded)
dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded)
dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded)

summary(dat$PEF_init_recorded)
summary(dat$PEF_prev_recorded)
summary(dat$PEF_predict_recorded)


dat %>% filter(age > 5) %>% select(PEF_init_recorded) %>% table(useNA = "ifany")
dat %>% filter(age < 6) %>% select(PEF_init_recorded) %>% table(useNA = "ifany")


dat %>% filter(age > 5) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(age > 5) %>% filter(PEF_prev_recorded == "Not recorded") %>% 
  select(PEF_predict_recorded) %>% table(useNA = "ifany")

# No one over the age of 5 has missing data for this question

dat %>% filter(age > 5) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(age > 5) %>% filter(PEF_prev_recorded == "Not recorded") %>% 
  select(PEF_predict_recorded) %>% table(useNA = "ifany")

# No invalid PEFs! Just doing it here while I sort out that variable


# Now to make the % predicted

dat <- dat %>% mutate(PEF_percent_pred = round(ifelse(!is.na(PEF_prev_value), PEF_init_value/PEF_prev_value,
                                                PEF_init_value/PEF_predict_value)*100, 0))

summary(dat$PEF_percent_pred)
dat %>% filter(PEF_init_recorded == "Recorded" & 
                 (PEF_prev_recorded == "Recorded" | PEF_predict_recorded == "Recorded")) %>% nrow()

# Split into <75% and => 75%

dat <- dat %>% mutate(PEF_percpred_75 = factor(ifelse(PEF_percent_pred < 75, "<75%",
                                       ifelse(PEF_percent_pred >= 75, ">= 75%", NA))))

# Seems to work
summary(dat$PEF_percpred_75)

# Matches the expected nummber

# Cleaning

# No inconsistent steroids or beta agonists administration

dat %>% filter(is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_time)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% nrow()
dat %>% select(steroids_admin) %>% table()
dat %>% filter(steroids_admin == "Yes") %>% filter(is.na(steroids_admin_DT)) %>% nrow()


dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()




# Discharge bundle consistency check

dat %>% select(discharge_bundle, DB_none) %>% table(useNA = "ifany") 
table(dat$DB_none)

# people received a discharge bundle but were marked as 'none' for each element.

colnames(dat)

# Discharge bundle consistency check - How many patients marked as having no discharge bundle elements,
# but actually did have at least 1?

dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_RSR_if_LT), any_vars(. == 1)) %>% 
  nrow()

# For reference, this many people had 'none' ticked and truly had none:
dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_RSR_if_LT), all_vars(. == 0)) %>% 
  nrow()

dat %>% select(discharge_bundle, life_status) %>% table(useNA = "ifany")




# Filter out invalid dates (N = 0)

dat %>% filter(discharge_date < "2023-04-01") %>% nrow()
dat %>% filter(discharge_date > "2024-03-31") %>% nrow()

# Was anyone discharged before they arrived, or received medication before they arrived, or received medication
# after they left? No.

dat %>% filter(discharge_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(b2a_admin_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(steroids_admin_DT - arrival_DT < 0) %>% nrow()

dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - steroids_admin_DT < 0) %>% nrow()


# Filter out invalid heart rates (0-250) or resp rates (0-80) or oxygen sats (60-100). None present.

dat %>% filter(heart_rate < 0) %>% nrow()
dat %>% filter(heart_rate > 250) %>% nrow()

dat %>% filter(resp_rate < 0) %>% nrow()
dat %>% filter(resp_rate > 80) %>% nrow()

dat %>% filter(oxygen_sat_value < 60) %>% nrow()
dat %>% filter(oxygen_sat_value > 100) %>% nrow()


dat %>% filter(is.na(oxygen_sat_value)) %>% nrow()
dat %>% select(oxygen_sat_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(oxygen_sat_value) & is.na(oxygen_sat_recorded)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value) & !is.na(oxygen_sat_recorded)) %>% nrow()

summary(dat$oxygen_sat_recorded)
dat <- dat %>% mutate(oxygen_sat_recorded = fct_explicit_na(oxygen_sat_recorded, "Recorded"))
summary(dat$oxygen_sat_recorded)





# Need to classify the oxygen saturation
head(dat$oxygen_sat_value)

dat <- dat %>% mutate(oxygen_sat92 = ifelse(oxygen_sat_value < 92, "<92",
                                            ifelse(oxygen_sat_value > 91, ">=92", NA)))


table(dat$oxygen_sat_measurement_type)
table(dat$oxygen_sat92, useNA = "ifany")

# It's worked

# no supplementary oxygen any more

# # Also need to create a new variable for the hypoxic children
# 
# table(dat$oxygen_supp, useNA = "ifany")
# 
# dat <- dat %>% mutate(oxygen_supp_hypoxic_only = oxygen_supp)
# dat$oxygen_supp_hypoxic_only[dat$oxygen_sat92 == ">=92"] <- NA
# 
# dat %>% select(oxygen_sat92, oxygen_supp_hypoxic_only) %>% table(useNA = "ifany")

# More on steroids

summary(dat$steroids_24hr_prev)




dat$steroids_admin_or_24hr_prev <- "No"
dat$steroids_admin_or_24hr_prev[dat$steroids_admin == "Yes" | dat$steroids_24hr_prev == "Yes"] <- "Yes"
dat$steroids_admin_or_24hr_prev <- factor(dat$steroids_admin_or_24hr_prev, levels = c("Yes", "No"))

dat <- dat %>% mutate(steroids_1hour = factor(ifelse(steroids_admin == "Not administered", "Not administered",
                                                     ifelse(steroids_admin == "Not recorded", "Not recorded",
                                                     ifelse(arrival_to_steroids_hours >= 1, ">=1 hour", "<1 hour"))),
                                              levels = c("<1 hour", ">=1 hour", "Not administered", "Not recorded")))
dat$steroids_1hour[dat$steroids_24hr_prev == "Yes"] <- NA

summary(dat$steroids_24hr_prev)
summary(dat$steroids_1hour)
summary(dat$steroids_admin)

dat$arrival_to_steroids_hours[dat$steroids_24hr_prev == "Yes"] <- NA



dat$b2a_admin_or_1hr_prev <- "No"
dat$b2a_admin_or_1hr_prev[dat$b2a_admin == "Yes" | dat$b2a_1hr_prev == "Yes"] <- "Yes"
dat$b2a_admin_or_1hr_prev <- factor(dat$b2a_admin_or_1hr_prev, levels = c("Yes", "No"))


dat <- dat %>% mutate(b2a_1hour = factor(ifelse(b2a_admin == "Not administered", "Not administered",
                                                ifelse(b2a_admin == "Not recorded", "Not recorded",
                                                ifelse(arrival_to_b2a_minutes/60 >= 1, ">=1 hour", "<1 hour"))),
                                         levels = c("<1 hour", ">=1 hour", "Not administered", "Not recorded")))
dat$b2a_1hour[dat$b2a_1hr_prev == "Yes"] <- NA

summary(dat$b2a_admin)
summary(dat$b2a_admin_or_1hr_prev)
summary(dat$b2a_1hour)
table(dat$b2a_1hr_prev)

dat$arrival_to_b2a_minutes[dat$b2a_1hr_prev == "Yes"] <- NA


dat <- dat %>% mutate(DB_smoke_NR_included = DB_smoke)



dat$DB_smoke[dat$smoke_tobacco != "Current"] <- NA
dat$DB_smoke[is.na(dat$smoke_tobacco)] <- NA


table(dat$DB_smoke, dat$smoke_shisha, useNA = "ifany")
# And we do the same for parents' smoking status

dat$DB_parent_smoke[dat$SH_smoke != "Yes"] <- NA
dat %>% select(SH_smoke, DB_parent_smoke) %>% table(useNA = "ifany")


# Going to create a new variable for transferred and not transferred, and then recode transferred as missing
# for the discharge bundle variable. Doing it right at the end because I know I use that variable previously.


dat %>% filter(life_status == "Died as inpatient")


# And now we change discharge bundle so that the transferreds are missing

dat$discharge_bundle[dat$life_status == "Died as inpatient"] <- NA

dat$discharge_bundle <- factor(dat$discharge_bundle)


# I also need to create a variable for discharge bundle yes/no for the subanalysis section

dat <- dat %>% mutate(discharge_bundle_yes_no = discharge_bundle)
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Parental/carer/self-discharge"] <- "No"
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Patient transferred to another hospital"] <- NA
dat$discharge_bundle_yes_no <- factor(dat$discharge_bundle_yes_no, levels = c("No", "Yes"))

summary(dat$discharge_bundle_yes_no)


# Okay and now I think we're ready to go!



summary(dat$SH_smoke)
dat$SH_smoke <- factor(dat$SH_smoke, levels = c("Yes", "No", "Not recorded"))
summary(dat$SH_smoke)

summary(dat$oxygen_sat_recorded)
dat$oxygen_sat_recorded <- factor(dat$oxygen_sat_recorded, levels = c("Recorded", "Not recorded"))
summary(dat$oxygen_sat_recorded)

summary(dat$oxygen_sat_measurement_type)
dat$oxygen_sat_measurement_type <- factor(dat$oxygen_sat_measurement_type, levels = c("Yes", "No - room air",
                                                                                      "Not recorded"))
summary(dat$oxygen_sat_measurement_type)

summary(dat$PEF_init_recorded)
dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded",
                                                                  "Patient too unwell", "Not recorded"))
summary(dat$PEF_init_recorded)

summary(dat$steroids_24hr_prev)
dat$steroids_24hr_prev <- factor(dat$steroids_24hr_prev, levels = c("Yes", "Not recorded", "No"))
summary(dat$steroids_24hr_prev)

summary(dat$steroids_admin)
dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered", "Not recorded"))
summary(dat$steroids_admin)

summary(dat$b2a_1hr_prev)
dat$b2a_1hr_prev <- factor(dat$b2a_1hr_prev, levels = c("Yes", "Not recorded", "No"))
summary(dat$b2a_1hr_prev)

summary(dat$b2a_admin)
dat$b2a_admin <- factor(dat$b2a_admin, levels = c("Yes", "Not recorded", "Not administered"))
summary(dat$b2a_admin)

summary(dat$crit_care_total)

dat$crit_care_total <- as.character(dat$crit_care_total)
dat$crit_care_total[dat$crit_care_total == "Yes - HDU;Yes - ICU"] <- "Yes - HDU,Yes - ICU"
dat$crit_care_total <- factor(dat$crit_care_total, levels = c("Yes - ICU", "Yes - HDU",  # check this one
                                                              "Yes - HDU,Yes - ICU", "No"))
summary(dat$crit_care_total)


summary(dat$discharge_bundle)
dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Parental/carer/self-discharge"))
summary(dat$discharge_bundle)

summary(dat$inhaled_steroids_dis)
dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis, 
                                   levels = c("Yes", "No - not medically indicated", "No - reason not given",
                                              "Offered but patient/parent/carer declined"))
summary(dat$inhaled_steroids_dis)

# dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No - not medically indicated", 
#                                 "No - reason not given"))

summary(dat$oral_steroids_rescue_history)
dat$oral_steroids_rescue_history <- factor(dat$oral_steroids_rescue_history, levels = c("Yes", "No", 
                                                                                        "Not recorded"))
summary(dat$oral_steroids_rescue_history)

summary(dat$referred_for_FU)
dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No - not medically indicated", 
                                                              "Not recorded", 
                              "Patient/parent/carer declined", "Already being seen in secondary care clinic"))
summary(dat$referred_for_FU)



# We need to create some asthma severity levels. Depends on age.
# Using BTS / SIGN guidelines 2009 Annex 8



# - - - - - - - - - - - - - -#
#  Defining asthma severity  #
# - - - - - - - - - - - - - -#

# More difficult because different definitions of severity according to the different ages.
# <1 not included

# For 1-5:
# Acute severe asthma
# . SpO2 <92%  <<<<<
# .                                    (Too breathless to talk or eat)
# . Heart rate >140/min  <<<<<<<
# . Respiratory rate >40/min  <<<<<<<
# .                                    (Use of accessory neck muscles)

# Life-threatening asthma
# SpO2 <92% plus any of:
#   . Silent chest
# . Poor respiratory effort
# . Agitation
# . Confusion
# . Cyanosis



# For 6+
# Acute severe asthma
# . SpO2 <92%  <<<<<<<<<<
# . PEF 33-50% best or predicted  <<<<<<<<<
# . Heart rate >125/min   <<<<<<<
# . Respiratory rate >30/min   <<<<<<<<<
# .                                       (Use of accessory neck muscles)


# Life-threatening asthma
# SpO2 <92% plus any of:
#   . PEF <33% best or predicted
# . Silent chest
# . Poor respiratory effort
# . Confusion
# . Cyanosis



# So, we use oxygen saturation, heart rate, respiratory rate, and PEF if >5

# For oxygen saturation, it's the same for all ages (apart from <2)

# Also applying Jenni's criteria of heart rate < 30 or resp rate < 10
# (Note - this changes 4-5 people into the severe asthma group)


table(dat$oxygen_sat92, useNA = "ifany")

dat <- dat %>% mutate(oxygen_sat_sev = fct_recode(oxygen_sat92, Normal = ">=92",
                                                  Low = "<92"))



# For PEF it's quite straight forward as those under 6 didn't get any PEF

dat %>% select(PEF_init_recorded, age) %>% table()



# Don't need to worry about floating point rounding errors for this 
# But can't use cut because of the definitions the left and the right are both < and >. 

dat <- dat %>% mutate(PEF_percent_pred_sev = NA)


dat$PEF_percent_pred_sev[dat$PEF_percent_pred > 50] <- "Moderate"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred <= 50] <- "Severe"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred < 33] <- "Life-threatening"
dat$PEF_percent_pred_sev[dat$PEF_init_recorded == "Patient too unwell"] <- "Severe"
dat$PEF_percent_pred_sev <- factor(dat$PEF_percent_pred_sev)

summary(dat$PEF_percent_pred_sev) # All adds up

# Resp rate rate
# Let's create it in the old school way - probably easier

dat <- dat %>% mutate(resp_rate_sev = NA)

# For age 1-5


# resp rate > 40 for ages 2-5

dat$resp_rate_sev[dat$resp_rate > 40 & dat$age %in% (1:5)] <- "High"
dat$resp_rate_sev[dat$resp_rate <= 40 & dat$age %in% (1:5)] <- "Normal"

# resp rate > 30 for 6+

dat$resp_rate_sev[dat$resp_rate > 30 & dat$age %in% (6:18)] <- "High"
dat$resp_rate_sev[dat$resp_rate <= 30 & dat$age %in% (6:18)] <- "Normal"

dat$resp_rate_sev <- factor(dat$resp_rate_sev, levels = c("Normal", "High"))

# Heart rate

dat <- dat %>% mutate(heart_rate_sev = NA)

# Heart rate >140 in those aged 1-5

dat$heart_rate_sev[dat$heart_rate > 140 & dat$age %in% (1:5)] <- "High"
dat$heart_rate_sev[dat$heart_rate <= 140 & dat$age %in% (1:5)] <- "Normal"

# Heart rate >125 in those aged 6+

dat$heart_rate_sev[dat$heart_rate > 125 & dat$age %in% (6:18)] <- "High"
dat$heart_rate_sev[dat$heart_rate <= 125 & dat$age %in% (6:18)] <- "Normal"

dat$heart_rate_sev <- factor(dat$heart_rate_sev, levels = c("Normal", "High"))


dat %>% select(heart_rate_sev, age) %>% table(useNA = "ifany")
dat %>% select(heart_rate_sev, heart_rate) %>% table(useNA = "ifany")

# symptoms

dat %>% select(starts_with("symptoms")) %>% colnames()

dat <- dat %>% mutate(symptom_sev = ifelse(symptoms_silent_chest == 1 |
                                             symptoms_cyanosis == 1 |
                                             symptoms_poor_respiratory_effort == 1 |
                                             symptoms_hypotension == 1 |
                                             symptoms_exhaustion == 1 |
                                             symptoms_confusion == 1, 
                                           "Life-threatening", 
                                           ifelse(symptoms_breathless == 1, "Severe", "Moderate")))

table(dat$symptoms_breathless)
dat$symptom_sev <- factor(dat$symptom_sev, levels = c("Moderate", "Severe", "Life-threatening"))
table(dat$symptom_sev)

# I'm just going to define asthma severity as one thing now, which doesn't account for whether a measurement it missing.
# This one only uses high resp/heart rates for severity.


dat$asthma_sev <- "Moderate"
dat$asthma_sev[dat$resp_rate_sev == "High"] <- "Severe"
dat$asthma_sev[dat$heart_rate_sev == "High"] <- "Severe"
dat$asthma_sev[dat$PEF_percent_pred_sev == "Severe"] <- "Severe"
dat$asthma_sev[dat$symptom_sev == "Severe"] <- "Severe"
dat$asthma_sev[dat$symptom_sev == "Life-threatening"] <- "Life-threatening"
dat$asthma_sev[dat$PEF_percent_pred_sev == "Life-threatening"] <- "Life-threatening"
dat$asthma_sev[dat$oxygen_sat_sev == "Low"] <- "Life-threatening"



dat$asthma_sev <- factor(dat$asthma_sev, levels = c("Moderate", "Severe", "Life-threatening"))

summary(dat$asthma_sev, useNA = "ifany")

# check everything is as expected

dat %>% filter(asthma_sev == "Life-threatening") %>% select(ends_with("sev")) %>%
  summary()

dat %>% filter(symptom_sev == "Life-threatening" | PEF_percent_pred_sev == "Life-threatening" |
                 oxygen_sat_sev == "Low") %>% select(ends_with("sev")) %>%
  summary()

dat %>% filter(asthma_sev == "Moderate") %>% select(ends_with("sev")) %>%
  summary()



# Remove the 'day of discharge' of anyone who died.


dat$discharge_day_of_week[dat$life_status != "Alive"] <- NA
summary(dat$discharge_day_of_week)




colnames(dat)

summary(dat$discharge_day_of_week)


# we do RSR by asthma severity

dat %>% select(RSR, asthma_sev) %>% table(useNA = "ifany")

dat$RSR_if_severe_or_LT <- dat$RSR
dat$RSR_if_severe_or_LT[dat$asthma_sev == "Moderate"] <- NA

dat %>% select(RSR, RSR_if_severe_or_LT) %>% table(useNA = "ifany")
# remove duplicates

# and we do DB_comm_FU if severe as well

dat$DB_comm_FU_2_days_if_severe_or_LT <- dat$DB_comm_FU_2_days
dat$DB_comm_FU_2_days_if_severe_or_LT[dat$asthma_sev == "Moderate"] <- NA

dat %>% select(DB_comm_FU_2_days, DB_comm_FU_2_days_if_severe_or_LT) %>% table(useNA = "ifany")



# assess whether there are duplicate records. Done based on:
# country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date

# We have this many duplicated records:

dat %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% duplicated() %>% 
  sum() 

dat <- dat %>% arrange(patient_ID, arrival_DT)



dat <- dat[!duplicated(select(dat, country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)), ]


# These are removed to leave this many records:
nrow(dat)

summary(dat$asthma_sev)

table(dat$DB_RSR_if_LT)

table(dat$RSR_if_severe_or_LT, useNA = "ifany")

# and, the additional discharge bundle part:

dat$DB_selected_elements <- 0

dat$DB_selected_elements[dat$DB_inhaler == 1 & 
                         dat$DB_PAAP == 1 &
                         dat$DB_FU_any == 1 & 
                         (dat$RSR_if_severe_or_LT == "Yes" | is.na(dat$RSR_if_severe_or_LT))] <- 1

dat$DB_selected_elements[is.na(dat$DB_inhaler)] <- NA

table(dat$DB_inhaler, useNA = "ifany")
table(dat$DB_selected_elements, useNA = "ifany")

saveRDS(dat, "G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Data/tidyData/CYPA_SCC_2023-24_clean_data.RDS")

