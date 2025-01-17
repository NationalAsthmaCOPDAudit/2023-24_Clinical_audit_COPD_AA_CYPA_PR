#                                                             #
# AA SCC 2023-24                                              #
#                                                             #
# Author: Alex Adamson                                        #
# Date created:  17th July 2024                                #
#                                                             #


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


# Rename the columns so that they are easier to read and if possible match up to the adult asthma column
# names:

# Safer to use rename here first, so I know which columns I include and which ones I drop...
# But after doing that, it's fine




dat_old <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/AA/Data/rawData/NACAP-AA-2204-2303-v102+LSOA-NDO-Imperial.csv",
                header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Data/rawData/AA-2405-Imperial.csv",
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
                      hospital_transfer = X1.1c.Hospital.Transfer,
                      first_department = X1.2.First.Department,
                      age = X.Age.At.Arrival,
                      gender = X2.3.Gender,
                      smoke_status = X2.5.Smoking,
                      heart_rate = X3.1.Heart.Rate.BPM,
                      resp_rate = X3.2.Respiratory.Rate.BPM,
                      oxygen_sat_value = X3.3.Oxygen.Saturation.,
                      oxygen_sat_recorded = X3.3nr.Oxygen.Saturation.NR,
                      oxygen_sat_measurement_type = X3.3a.Oxygen.Measurement,
                      PEF_init_value = X3.4a.First.Recorded.Peak.Flow,
                      PEF_init_recorded = X3.4.Peak.Flow.Measurement,
                      PEF_init_date = X3.4b.Peak.Flow.Date,
                      #                      PEF_init_date_recorded = X3.4anr.Peak.Flow.Recoding.Date.Not.Recorded,
                      PEF_init_time = X3.4c.Peak.Flow.Time,
                      PEF_init_time_recorded = X3.4nr.Peak.Flow.Time.NR,
                      PEF_prev_value = X3.5.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X3.5nr.Previous.Peak.Flow.Not.Recorded,
                      PEF_predict_value = X3.5a.Predicted.Peak.Flow,
                      PEF_predict_recorded = X3.5anr.Predicted.Peak.Flow.Not.Recorded,
                      RSR = X4.1.Specialist.Respiratory.Review,
                      RSR_date = X4.1a.Date.of.respiratory.review,
                      RSR_time = X4.1b.Time.of.respiratory.review,
                      oxygen_prescribed = X4.2.Oxygen.Prescribed,
                      oxygen_date = X4.2a.Oxygen.Prescribed.Date, # don't actually need this variable
                      oxygen_time = X4.2b.Oxygen.Prescribed.Time, # don't actually need this variable
                      # X4.2.Oxygen.Prescribed...No # drop
                      # X4.2.Oxygen.Prescribed...Prescribed # drop
                      # X4.2.Oxygen.Prescribed...Administered # drop
                      oxygen_admin = X4.3.Oxygen.Administered,
                      steroids_admin = X4.4.Steroids.Administered,
                      steroids_admin_date = X4.4a.Steroids.Date,
                      steroids_admin_time = X4.4b.Steroids.Time,
                      steroids_24hr_prev = X4.5.Steroids.Administered.24.Hours.Before.Arrival,
                      b2a_1hr_prev = X4.6.B2.Agonists.Before.Arrival,
                      b2a_admin = X4.7.B2.Agonists.After.Arrival,
                      b2a_admin_date = X4.7a.B2.Agonists.Date,
                      b2a_admin_time = X4.7b.B2.Agonists.Time,
                      life_status = X5.1.Discharge.Life.Status,  # feel weird about having 'died'
                      discharge_date = X5.2a.Discharge.Date,
                      discharge_time = X5.2b.Discharge.Time,
                      discharge_bundle = X5.3.Discharge.Bundle,
                      # X5.4.Discharge.Elements # completely useless: drop.
                      DB_inhaler = X5.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X5.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X5.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X5.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X5.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X5.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_comm_FU_2_days = X5.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
                      DB_spec_review_4_weeks = X5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks,
                      DB_none = X5.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X6.1.Discharge.Inhaled.Steroids,
                      oral_steroids_dis = X6.2.Discharge.Oral.Steroids,
                      oral_steroids_rescue_history = X6.3.Rescue.Steroids.Last.12.Months,
                      overseas = X..Overseas.or.Non.NHS)
# dataset = X..Dataset # useless - drop
# referred_for_FU = X6.4.Referred.for.Followup) # no longer in dataset
# Dataset.Version   # useless - drop

# dat <- dat %>% rename(study_ID = ROWID, # first we use rename, then when we're happy we use 'select'
dat <- dat %>% select(study_ID = ROWID,
                      patient_ID = PATIENTID,
                      LSOA = lsoa11,
                      hosp_code = Org,
                      hosp_name = OrgName,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      region = Region,
                      integrated_care_system = ICS,
                      country = Country,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                    #  hospital_transfer = X1.1c.Hospital.Transfer,
                      first_department = X1.2.First.Department,
                      age = X..Age.At.Arrival,
                      gender = X2.3.Gender,
                    ethnicity = X2.5.Ethnicity,
                    impairments_none = X..2.6.Impairments...No.None,
                    impairments_anxiety = X..2.6.Impairments...Anxiety,
                    impairments_depression = X..2.6.Impairments...Depression,
                    impairments_severe_mental_illness = X..2.6.Impairments...Severe.mental.illness,
                    impairments_dementia_mci = X..2.6.Impairments...Dementia.mild.cognitive.impairment,
                    impairments_other = X..2.6.Impairments...Other,
                    impairments_NR = X..2.6.Impairments...Not.recorded, 
                    #smoke_status = X2.5.Smoking.Status,
                    smoke_tobacco = X2.7a.Smoking.Tobacco, # new
                    smoke_shisha = X2.7b.Smoking.Shisha, # new
                    smoke_cannabis = X2.7c.Smoking.Cannabis, # new
                    smoke_other = X2.7d.Smoking.Other, # new
                    vaping_status = X2.8.Vaping.status, # new
                      heart_rate = X3.1.Heart.Rate,
                      resp_rate = X3.2.Respiratory.Rate,
                      oxygen_sat_value = X3.3.Oxygen.Saturation.,
                      oxygen_sat_recorded = X3.3.x.Oxygen.saturation.not.recorded,
                      oxygen_sat_measurement_type = X3.3a.Oxygen.Measurement,
                      PEF_init_value = X3.4a.First.Recorded.Peak.Flow,
                      PEF_init_recorded = X3.4.Peak.Flow.Measurement,
                      PEF_init_date = X3.4b.Peak.Flow.Date,
                      PEF_init_date_recorded = X3.4b1.Peak.Flow.Date.not.recorded,
                      PEF_init_time = X3.4c.Peak.Flow.Time,
                      PEF_init_time_recorded = X3.4c1.Peak.Flow.Time.not.recorded,
                      PEF_prev_value = X3.5.Previous.Best.Peak.Flow,
                      PEF_prev_recorded = X3.5.x.Previous.Best.Peak.Flow.Not.Recorded,
                      PEF_predict_value = X3.5a.Predicted.Peak.Flow,
                      PEF_predict_recorded = X3.5a.x.Predicted.Peak.Flow.Not.Recorded,
                    symptoms_low_PaO2 = X..3.6.Symptoms...PaO2,                                                                  
                    symptoms_normal_PaCO2 = X..3.6.Symptoms...Normal.PaCO2,                                                 
                    symptoms_raised_PaCO2 = X..3.6.Symptoms...Raised.PaCO2,                                                 
                    symptoms_breathless = X..3.6.Symptoms...Breathlessness,                                               
                    symptoms_silent_chest = X..3.6.Symptoms...Silent.chest,                                                 
                    symptoms_cyanosis = X..3.6.Symptoms...Cyanosis,                                                     
                    symptoms_poor_respiratory_effort = X..3.6.Symptoms...Poor.respiratory.effort,                                      
                    symptoms_hypotension = X..3.6.Symptoms...Hypotension,                                                  
                    symptoms_exhaustion = X..3.6.Symptoms...Exhaustion,                                                   
                    symptoms_altered_conscious = X..3.6.Symptoms...Altered.conscious.level,                                      
                    symptoms_none = X..3.6.Symptoms...None,  
                      RSR = X4.1.Specialist.Respiratory.Review,
                      RSR_date = X4.1a.Review.Date,
                      RSR_time = X4.1b.Review.Time,
                      oxygen_prescribed = X4.2.Oxygen.Prescribed.Target.Range,
                      oxygen_date = X4.2a.Date.of.oxygen.prescription, # don't actually need this variable
                      oxygen_time = X4.2b.Time.of.oxygen.prescription, # don't actually need this variable
                      # X4.2.Oxygen.Prescribed...No # drop
                      # X4.2.Oxygen.Prescribed...Prescribed # drop
                      # X4.2.Oxygen.Prescribed...Administered # drop
                      oxygen_admin = X4.3.Oxygen.Administered,
                      steroids_admin = X4.4.Systemic.steroids.administered,
                      steroids_admin_date = X4.4a.Steroids.Date,
                      steroids_admin_time = X4.4b.Steroids.Time,
                      steroids_24hr_prev = X4.5.Steroids.Administered.24.Hours.Before.Arrival,
                      b2a_1hr_prev = X4.6.B2.Agonists.Before.Arrival,
                      b2a_admin = X4.7.B2.Agonists.After.Arrival,
                      b2a_admin_date = X4.7a.B2.Agonists.Date,
                      b2a_admin_time = X4.7b.B2.Agonists.Time,
                      life_status = X5.1.Discharge.Life.Status,  # feel weird about having 'died'
                      discharge_date = X5.2a.Discharge.Date,
                      discharge_time = X5.2b.Discharge.Time,
                      discharge_bundle = X5.3.Discharge.Bundle,
                      # X5.4.Discharge.Elements # completely useless: drop.
                      DB_inhaler = X..5.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X..5.4.Discharge.Elements...Maintenance.medication.reviewed,
                      DB_adherence = X..5.4.Discharge.Elements...Adherence.discussed,
                      DB_PAAP = X..5.4.Discharge.Elements...PAAP.issued.reviewed,
                      DB_triggers = X..5.4.Discharge.Elements...Triggers.discussed,
                      DB_smoke = X..5.4.Discharge.Elements...Tobacco.dependency.addressed,
                      DB_comm_FU_2_days = X..5.4.Discharge.Elements...Community.follow.up.requested.within.2.working.days,
                      DB_spec_review_4_weeks = X..5.4.Discharge.Elements...Specialist.review.requested.within.4.weeks,
                      DB_none = X..5.4.Discharge.Elements...None,
                      inhaled_steroids_dis = X6.1.Discharge.Inhaled.Steroids,
                      oral_steroids_dis = X6.2.Discharge.Oral.Steroids,
                      oral_steroids_rescue_history = X6.3.Rescue.Steroids.Last.12.Months)
                      # overseas = X..Overseas.or.Non.NHS)
                      # dataset = X..Dataset # useless - drop
                      # referred_for_FU = X6.4.Referred.for.Followup) # no longer in dataset
                      # Dataset.Version   # useless - drop


# - smoking status -> shisha/tobacco/cannabis/other, current/ex/never/NR
# ethnicity
# impairments
# asthma additional symptoms - doesn't need to be directly reported  but can be used to properly
# classify asthma severity
                      
# removed: hospital transfer

# need to add in the hospital details


# No. records in original dataset:
nrow(dat)

# No. records that are drafts or test hospitals:

dat %>% filter(hosp_code == "YYY" | trust_code == "YYYT") %>% nrow() 


# Need to add in the empty gender factors and smoking status factors, and while we're doing it we might as well
# put it in the correct order


summary(dat$gender)
dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Other", "Not recorded/Preferred not to say"))
summary(dat$gender)


# sort out smoking status variables, which have changed now.

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

dat$tobacco_vape_combo <- NA
dat$tobacco_vape_combo[dat$smoke_tobacco == "Never" & dat$vaping_status == "Current"] <- "Never smoker and current vaper"
dat$tobacco_vape_combo[dat$smoke_tobacco == "Never" & dat$vaping_status %in% c("Ex", "Never")] <- "Never smoker and non-vaper"
dat$tobacco_vape_combo[dat$smoke_tobacco == "Ex" & dat$vaping_status == "Current"] <- "Ex smoker and current vaper"
dat$tobacco_vape_combo[dat$smoke_tobacco == "Ex" & dat$vaping_status %in% c("Ex", "Never")] <- "Ex smoker and non-vaper"
dat$tobacco_vape_combo[dat$smoke_tobacco == "Current" & dat$vaping_status == "Current"] <- "Current smoker and current vaper"
dat$tobacco_vape_combo[dat$smoke_tobacco == "Current" & dat$vaping_status %in% c("Ex", "Never")] <- "Current smoker and non-vaper"
dat$tobacco_vape_combo <- factor(dat$tobacco_vape_combo, levels = c("Current smoker and current vaper",
                                                                    "Current smoker and non-vaper",
                                                                    "Ex smoker and current vaper",
                                                                    "Ex smoker and non-vaper",
                                                                    "Never smoker and current vaper",
                                                                    "Never smoker and non-vaper"))

table(dat$tobacco_vape_combo, useNA = "ifany")
table(dat$tobacco_vape_combo, dat$vaping_status, useNA = "ifany")
table(dat$tobacco_vape_combo, dat$smoke_tobacco, useNA = "ifany")

# Now we do more building and cleaning:

dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))

# Everything seems to be alright at this point.

# Now would probably be a good point to link to the LSOAs.

# Read in the IMD quintiles
# let's use the joint quintiles seeing as we no longer have scotland

IMDeng_wal <- read.csv("C:/Alex Harley/Audit_2023_onwards/General UK data/IMD/2019_England_and_Wales_Income_Employment_IMD_clean.csv",
                       header = TRUE, stringsAsFactors = FALSE)

summary(IMDeng_wal$new_IMD_quintile)

IMDeng_wal <- IMDeng_wal %>% select(LSOA = LSOA_code_2011, IMD_quintile = new_IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng_wal, by = "LSOA")
summary(dat$IMD_quintile)

dat$IMD_quintile <- as.character(dat$IMD_quintile)
dat$IMD_quintile[is.na(dat$IMD_quintile)] <- "Missing IMD quintile"
dat$IMD_quintile <- factor(dat$IMD_quintile, levels = c("1", "2", "3", "4", "5", "Missing IMD quintile"))


dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)

levels(dat$country)
summary(dat$country)
summary(dat$IMD_quintile)


dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)


# Let's convert all our dates to dates and times to times (as they're currently factors)

dat$arrival_date <- as.Date(dat$arrival_date, "%d/%m/%Y")
dat$steroids_admin_date <- as.Date(dat$steroids_admin_date, "%d/%m/%Y")
dat$b2a_admin_date <- as.Date(dat$b2a_admin_date, "%d/%m/%Y")
dat$discharge_date <- as.Date(dat$discharge_date, "%d/%m/%Y")
dat$PEF_init_date <- as.Date(dat$PEF_init_date, "%d/%m/%Y")
dat$RSR_date <- as.Date(dat$RSR_date, "%d/%m/%Y")
dat$oxygen_date <- as.Date(dat$oxygen_date, "%d/%m/%Y")

summary(dat$PEF_init_date)

dat$arrival_time <- as.character(dat$arrival_time)
dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
dat$discharge_time <- as.character(dat$discharge_time)
dat$PEF_init_time <- as.character(dat$PEF_init_time)
dat$RSR_time <- as.character(dat$RSR_time)
dat$oxygen_time <- as.character(dat$oxygen_time)


summary(dat$arrival_date)
summary(dat$arrival_time)
summary(dat$steroids_admin_date)
summary(dat$steroids_admin_time)
summary(dat$b2a_admin_date)
summary(dat$b2a_admin_time)
summary(dat$discharge_date)
summary(dat$discharge_time)

# dat$arrival_time <- as.character(dat$arrival_time)
# dat$steroids_admin_time <- as.character(dat$steroids_admin_time)
# dat$b2a_admin_time <- as.character(dat$b2a_admin_time)
# dat$discharge_time <- as.character(dat$discharge_time)

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
head(dat$steroids_admin_time)
head(dat$b2a_admin_time)

# different format this time, don't need this bit:

# dat <- dat %>% mutate(arrival_time = ifelse(arrival_time == "", "", 
#                                      paste0(arrival_time, ":00")),
#                       steroids_admin_time = ifelse(steroids_admin_time == "", "", 
#                                             paste0(steroids_admin_time, ":00")),
#                       b2a_admin_time = ifelse(b2a_admin_time == "", "", 
#                                             paste0(b2a_admin_time, ":00")),
#                       discharge_time = ifelse(discharge_time == "", "", 
#                                             paste0(discharge_time, ":00")),
#                       PEF_init_time = ifelse(PEF_init_time == "", "", 
#                                               paste0(PEF_init_time, ":00")),
#                       RSR_time = ifelse(RSR_time == "", "", 
#                                               paste0(RSR_time, ":00")))


head(dat$steroids_admin_time)
head(dat$arrival_time)
head(dat$arrival_date)
head(dat$discharge_time)
head(dat$discharge_date)
head(dat$RSR_time)

# We can try converting times to 'difftime' when we try and create the table of when people are given
# particular things.


# if the times have :00 on the end use this:

# dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M:%S"),
#                       steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time), 
#                                                      format="%Y-%m-%d %H:%M:%S"),
#                       b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M:%S"),
#                       discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M:%S"),
#                       PEF_init_DT = as.POSIXct(paste(PEF_init_date, PEF_init_time), format="%Y-%m-%d %H:%M:%S"),
#                       RSR_DT = as.POSIXct(paste(RSR_date, RSR_time), format="%Y-%m-%d %H:%M:%S"),
#                       oxygen_DT = as.POSIXct(paste(oxygen_date, oxygen_time), format="%Y-%m-%d %H:%M:%S"))


# otherwise, use this:

dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M"),
                      steroids_admin_DT = as.POSIXct(paste(steroids_admin_date, steroids_admin_time),
                                                     format="%Y-%m-%d %H:%M"),
                      b2a_admin_DT = as.POSIXct(paste(b2a_admin_date, b2a_admin_time), format="%Y-%m-%d %H:%M"),
                      discharge_DT = as.POSIXct(paste(discharge_date, discharge_time), format="%Y-%m-%d %H:%M"),
                      PEF_init_DT = as.POSIXct(paste(PEF_init_date, PEF_init_time), format="%Y-%m-%d %H:%M"),
                      RSR_DT = as.POSIXct(paste(RSR_date, RSR_time), format="%Y-%m-%d %H:%M"),
                      oxygen_DT = as.POSIXct(paste(oxygen_date, oxygen_time), format="%Y-%m-%d %H:%M"))


summary(dat$arrival_DT)
head(dat$arrival_DT)
head(dat$steroids_admin_DT)
summary(dat$steroids_admin_DT)
head(dat$RSR_DT)

summary(dat$oxygen_DT)
summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)

# dat %>% filter(arrival_date == "2019-09-27") %>% select(arrival_DT) %>% arrange(arrival_DT) # crosses a time zone. Let's hope it doesn't mess 
# anything up


# This is how many people were discharged before the discharge date of 2023-04-01 and need to be removed
dat %>% filter(discharge_date < "2023-04-01") %>% nrow()

# This is how many people were discharged after the date of 2024-03-31 and have to be removed:
dat %>% filter(discharge_date > "2024-03-31") %>% nrow() 



# Life status
summary(dat$life_status)

# Already coded like this
# dat <- dat %>% mutate(life_status = recode_factor(life_status, Yes = "Alive"))
# dat$life_status <- factor(dat$life_status, levels = c(levels(dat$life_status), "Died as inpatient"))


# Time to b2a minutes
table(dat$b2a_1hr_prev)
dat <- dat %>% mutate(arrival_to_b2a_minutes = difftime(b2a_admin_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_b2a_minutes)
dat$arrival_to_b2a_minutes <- as.integer(dat$arrival_to_b2a_minutes)
dat$arrival_to_b2a_minutes[dat$b2a_1hr_prev == "Yes - up to 1 hour prior to arrival"] <- NA



# This many people have a time from arrival to first beta agonists <0 so need to be removed:")
dat %>% filter(arrival_to_b2a_minutes < 0) %>% nrow()
dat %>% filter(arrival_to_b2a_minutes < 0)  %>% select(arrival_DT, b2a_admin_DT, b2a_1hr_prev)

dat <- dat %>% filter(arrival_to_b2a_minutes > -1 | is.na(arrival_to_b2a_minutes))

# Our new N is:
nrow(dat)

# Time to steroids in hours

dat <- dat %>% mutate(arrival_to_steroids_hours = difftime(steroids_admin_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_steroids_hours)
dat$arrival_to_steroids_hours <- as.numeric(dat$arrival_to_steroids_hours)
dat$arrival_to_steroids_hours[dat$steroids_24hr_prev == "Yes"] <- NA

summary(dat$arrival_to_steroids_hours)
summary(dat$steroids_24hr_prev)

# This many people received steroids before they arrived so need to be removed:
dat %>% filter(arrival_to_steroids_hours < 0) %>% nrow()


# Time to peak flow

dat <- dat %>% mutate(arrival_to_PEF_init_hours = difftime(PEF_init_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_PEF_init_hours)
dat$arrival_to_PEF_init_hours <- as.numeric(dat$arrival_to_PEF_init_hours)

summary(dat$arrival_to_PEF_init_hours)

dat %>% filter(arrival_to_PEF_init_hours < 0) %>% nrow() # 49 people had peak flow measured before arrival


# earliest time accepted used to be up to 1 hour before. This needs to change to the limit being the arrival time.
# Until then, those with a PEF time before arrival are recoded as not receiving it.

table(dat$PEF_init_recorded)
dat %>% select(contains("PEF")) %>% colnames()
dat %>% select(contains("PEF")) %>% summary()
dat$PEF_init_recorded[dat$arrival_to_PEF_init_hours < 0] <- "Not recorded"
dat$PEF_init_value[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$PEF_init_date[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$PEF_init_date_recorded[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$PEF_init_time[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$PEF_init_time_recorded[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$PEF_init_DT[dat$arrival_to_PEF_init_hours < 0] <- NA
dat$arrival_to_PEF_init_hours[dat$arrival_to_PEF_init_hours < 0] <- NA
dat %>% select(contains("PEF")) %>% summary()



# Time to RSR

dat <- dat %>% mutate(arrival_to_RSR_hours = difftime(RSR_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_RSR_hours)
dat$arrival_to_RSR_hours <- as.numeric(dat$arrival_to_RSR_hours)

summary(dat$arrival_to_RSR_hours)

# This many people received an RSR before they arrived so need to be removed:
dat %>% filter(arrival_to_RSR_hours < 0) %>% nrow()

# summary(dat$RSR_DT)
# summary(dat$b2a_admin_DT)
# summary(dat$steroids_admin_DT)
# length of stay hours

dat <- dat %>% mutate(LOS_hours = difftime(discharge_DT, arrival_DT, units = "hours"))
head(dat$LOS_hours)
dat$LOS_hours <- as.numeric(dat$LOS_hours)
summary(dat$LOS_hours)

# length of stay hours just for those who died
table(dat$life_status)

dat <- dat %>% mutate(LOS_hours_died = ifelse(life_status == "Died as inpatient", LOS_hours, NA))


# length of stay days

dat <- dat %>% mutate(LOS_days = difftime(discharge_date, arrival_date, units = "days"))

dat$LOS_days <- as.numeric(dat$LOS_days)

# length of stay hours just for those who are alive
table(dat$life_status)

dat <- dat %>% mutate(LOS_days_alive = ifelse(life_status == "Alive", LOS_days, NA))

dat %>% select(LOS_days_alive) %>% summary()


# Length of stay categorised

dat <- dat %>% mutate(LOS_3day = NA)
dat$LOS_3day[dat$LOS_days_alive < 4] <- "<=3 days"
dat$LOS_3day[dat$LOS_days_alive > 3] <- ">3 days"
dat$LOS_3day <- factor(dat$LOS_3day, levels = c("<=3 days", ">3 days"))
summary(dat$LOS_3day)

# This many people were discharged before they arrived and need to be removed:

dat %>% filter(LOS_hours < 0) %>% nrow()

# Need to remove those who were transferred from this (only used in child asthma)
# dat <- dat %>% mutate(LOS_hours = ifelse(discharge_bundle == "Patient transferred to another hospital", 
#                                          NA, LOS_hours))


# day of arrival and day of discharge


dat <- dat %>% mutate(arrival_day_of_week = weekdays(arrival_date, abbreviate = FALSE))

dat$arrival_day_of_week <- ordered(dat$arrival_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

table(dat$arrival_day_of_week, useNA = "ifany")

# new format for discharge day of week



dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))

summary(dat$discharge_day_of_week)

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday")] <- "Weekday"

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Saturday", "Sunday")] <- "Weekend"


dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Weekday", "Weekend"))

table(dat$discharge_day_of_week, useNA = "ifany")


# Remove the 'day of discharge' of anyone who died.

dat$discharge_day_of_week[dat$life_status != "Alive"] <- NA
summary(dat$discharge_day_of_week)


# dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))
# 
# dat$discharge_day_of_week <- ordered(dat$discharge_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
#                                                                      "Friday", "Saturday", "Sunday"))
# 
# table(dat$discharge_day_of_week, useNA = "ifany")


# Those who transferred need to be removed from the discharge day of week

# Using ifelse here changes them to factor levels so I'm doing it the old school method

# Only for child asthma:
# dat$discharge_day_of_week[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA

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

#          
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




# Peak flow things

# first off, PEF_init...

dat %>% select(contains("PEF")) %>% summary()

# is everyone who is missing a value down as not being recorded etc? 
dat %>% filter(is.na(PEF_init_value)) %>% select(PEF_init_recorded) %>% table(useNA = "ifany") # yes

# does everyone down as recorded have a value?
dat %>%  filter(PEF_init_recorded == "Yes") %>% select(PEF_init_value) %>% summary() # yes

# 43 people managed to add that the time wasn't recorded even though they shouldn't have been able to
# get through to that question if they answered that PEF wasn't recorded - we recode

dat %>% filter(PEF_init_recorded != "Yes") %>% select(contains("PEF")) %>% summary()
dat$PEF_init_time_recorded[dat$PEF_init_recorded != "Yes"] <- NA
dat$PEF_init_date_recorded[dat$PEF_init_recorded != "Yes"] <- NA

# 


# This many people are marked as being both too ill for PEF or having it not recorded, but also had a value, so need to be removed
# as inconsistent:

summary(dat$PEF_init_recorded)

dat %>% filter(PEF_init_recorded != "Yes") %>%  filter(!is.na(PEF_init_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print()

nrow(dat)
# dat <- dat %>% filter(is.na(PEF_init_value) | is.na(PEF_init_recorded))

# New total:
nrow(dat) 

dat %>% filter(!is.na(PEF_init_value) & !is.na(PEF_init_recorded)) %>% select(PEF_init_value) %>% summary() # table(useNA = "ifany")

dat %>% select(contains("PEF")) %>% summary()

# Therefore, we recode all the missing from that variable as 'Recorded'.
dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
dat$PEF_init_recorded[!is.na(dat$PEF_init_value)] <- "Recorded" 
dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded", "Not recorded", "No - patient unable to do PEF", "No - not done"))
table(dat$PEF_init_recorded, useNA = "ifany")

# Do the same for the previous and predicted values

dat %>% filter(is.na(PEF_init_time)) %>% nrow() # 787 people missing a time for PEF
dat %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_date) %>% summary() # 3 people missing a time do have a date though
# but this is ok.

dat %>% filter(!is.na(PEF_prev_recorded)) %>%  filter(!is.na(PEF_prev_value)) %>%
  select(contains("PEF"))

# This many people are marked as not having previous PEF recorded but also had a value, so need to be removed
# as inconsistent:


dat %>% filter(!is.na(PEF_prev_recorded)) %>%  filter(!is.na(PEF_prev_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print()

dat <- dat %>% filter(is.na(PEF_prev_value) | is.na(PEF_prev_recorded))

# New total:
nrow(dat) 

dat %>% filter(is.na(PEF_prev_value)) %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(PEF_prev_recorded)) %>% select(PEF_prev_value) %>% summary() # table(useNA = "ifany")
dat %>% filter(!is.na(PEF_prev_recorded)) %>% select(PEF_prev_value) %>% summary() # table(useNA = "ifany")


summary(dat$PEF_prev_recorded)

dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
dat$PEF_prev_recorded[!is.na(dat$PEF_prev_value)] <- "Recorded" 
dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded, levels = c("Recorded", "Not recorded"))
table(dat$PEF_prev_recorded, useNA = "ifany")


summary(dat$PEF_prev_value)
summary(dat$PEF_prev_recorded)


# This many people are marked as not having predicted PEF recorded but also had a value, so need to be removed
# as inconsistent:

dat %>% filter(!is.na(PEF_predict_recorded)) %>%  filter(!is.na(PEF_predict_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print()


# This many people had a predicted PEF in addition to the previous, when it should only be possible to put in one or the other:


dat %>% filter(!is.na(PEF_prev_value)) %>%  filter(!is.na(PEF_predict_value)) %>%
  select(contains("PEF")) %>% nrow() %>% print()

nrow(dat)

# Inconsistent so removed, leaving our new N as:

dat <- dat %>% filter((!is.na(PEF_prev_value) & is.na(PEF_predict_value)) |
                        (is.na(PEF_prev_value) & !is.na(PEF_predict_value)) |
                        (is.na(PEF_prev_value) & is.na(PEF_predict_value)))

nrow(dat) 

dat %>% select(contains("PEF")) %>% summary()
dat %>% filter(PEF_init_recorded != "Recorded") %>% select(contains("PEF")) %>% summary()
dat %>% filter(PEF_init_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()

dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(PEF_prev_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()

# If people managed to get through to the PEF_predict_recorded position when they shouldn't have been able to get there, 
# we recode.

dat$PEF_predict_recorded[dat$PEF_prev_recorded == "Recorded" &  dat$PEF_predict_recorded == "Not recorded"] <- NA

dat %>% filter(PEF_init_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()
dat %>% filter(!is.na(PEF_predict_value)) %>% select(contains("PEF")) %>% summary() # everyone with a recorded PEF_predict value
                                                                                    # is marked as they should be
dat %>% filter(PEF_predict_recorded == "Not recorded") %>% select(contains("PEF")) %>% summary()



dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)
dat$PEF_predict_recorded[!is.na(dat$PEF_predict_value)] <- "Recorded" 
dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded, levels = c("Recorded", "Not recorded"))
table(dat$PEF_predict_recorded, useNA = "ifany")

dat %>% filter(PEF_predict_recorded == "Recorded") %>% select(contains("PEF")) %>% summary()




dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_recorded) %>% table(useNA = "ifany")

summary(dat$PEF_prev_value)
dat %>% filter(is.na(PEF_prev_value)) %>% select(PEF_predict_value) %>% summary()


# All now consistent!

# Invalid PEF values? No.
dat %>% filter(PEF_init_value < 60) %>% nrow()
dat %>% filter(PEF_init_value > 800) %>% nrow()

dat %>% filter(PEF_prev_value < 60) %>% nrow()
dat %>% filter(PEF_prev_value > 800) %>% nrow()

dat %>% filter(PEF_predict_value < 60) %>% nrow()
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
# Did this above in a much more convoluted way

# dat$PEF_init_recorded <- as.character(dat$PEF_init_recorded)
# dat$PEF_prev_recorded <- as.character(dat$PEF_prev_recorded)
# dat$PEF_predict_recorded <- as.character(dat$PEF_predict_recorded)
# 
# dat$PEF_init_recorded[(!is.na(dat$PEF_init_value))] <- "Recorded"
# dat$PEF_prev_recorded[(!is.na(dat$PEF_prev_value))] <- "Recorded"
# dat$PEF_predict_recorded[(!is.na(dat$PEF_predict_value))] <- "Recorded"
# 
# dat$PEF_init_recorded <- factor(dat$PEF_init_recorded)
# dat$PEF_prev_recorded <- factor(dat$PEF_prev_recorded)
# dat$PEF_predict_recorded <- factor(dat$PEF_predict_recorded)

summary(dat$PEF_init_recorded)
summary(dat$PEF_prev_recorded)
summary(dat$PEF_predict_recorded)

# This many people are missing an initial value for PEF but still have a date, so must be removed:

dat %>% filter(!is.na(PEF_init_date) & is.na(PEF_init_value))  %>% nrow()

# Removing the date issues also sorts out the time issues
# nlc("This many people are missing an initial value for PEF but still have a time, so must be removed:")
# 
# dat %>% filter(!is.na(PEF_init_time) & is.na(PEF_init_value))  %>% nrow() %>% nlc()


# This is our new N for the dataset:
dat <- dat %>% filter(!(!is.na(PEF_init_date) & is.na(PEF_init_value)))
# dat <- dat %>% filter(!(!is.na(PEF_init_time) & is.na(PEF_init_value)))
nrow(dat)


dat %>% filter(!is.na(PEF_init_time) & is.na(PEF_init_time))  %>% nrow() 
dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(!is.na(PEF_init_time) & !is.na(PEF_init_time_recorded)) %>% nrow()
# 0 people have a time down in addition to saying 'time not recorded' (so this is good)

dat %>% filter(PEF_init_recorded == "Recorded") %>% select(PEF_init_time_recorded) %>% table(useNA = "ifany")
dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(is.na(PEF_init_time) & is.na(PEF_init_time_recorded)) %>% nrow()
#  select(contains("PEF")) # nrow()


dat %>% filter(PEF_init_recorded == "Recorded") %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_time_recorded) %>% 
  table(useNA = "ifany") # 


# No date checks this time

# dat %>% filter(PEF_init_recorded == "Recorded" & is.na(PEF_init_date_recorded)) %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_time_recorded) %>% 
#   table(useNA = "ifany") # 320 people are missing a time but not a date

dat %>% filter(PEF_init_recorded == "Yes") %>% select(PEF_init_DT) %>% summary()
dat %>% filter(PEF_init_recorded == "Yes") %>% filter(PEF_init_date_recorded == "Not recorded") %>% nrow()
dat %>% filter(PEF_init_recorded == "Yes") %>% 
  filter(PEF_init_date_recorded == "Not recorded" | PEF_init_time_recorded == "Not recorded") %>% nrow()

dat %>% filter(PEF_init_recorded == "Yes") %>% filter(is.na(PEF_init_time)) %>% select(PEF_init_DT) %>% summary()

summary(dat$PEF_init_DT)
summary(dat$PEF_init_recorded)

# Let's add in some things to make all this time/date not recorded stuff easier:

# Have to change this as PEF_init_date_recorded not available - have to create it instead

# dat$PEF_init_date_recorded <- as.character(dat$PEF_init_date_recorded)
# dat$PEF_init_date_recorded[!is.na(dat$PEF_init_date)] <- "Recorded"
# dat$PEF_init_date_recorded <- factor(dat$PEF_init_date_recorded, levels = c("Recorded", "Not recorded"))

summary(dat$PEF_init_date_recorded)
summary(dat$PEF_init_time_recorded)

dat %>% filter(!is.na(PEF_init_value)) %>% filter(is.na(PEF_init_date)) %>% nrow()



dat$PEF_init_date_recorded <- NA
dat$PEF_init_date_recorded[!is.na(dat$PEF_init_value)] <- "Not recorded"
dat$PEF_init_date_recorded[!is.na(dat$PEF_init_date)] <- "Recorded"
dat$PEF_init_date_recorded <- factor(dat$PEF_init_date_recorded, levels = c("Recorded", "Not recorded"))

dat %>% filter(!is.na(PEF_init_value)) %>% filter(is.na(PEF_init_time)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% filter(is.na(PEF_init_time)) %>% nrow()


dat$PEF_init_time_recorded <- as.character(dat$PEF_init_time_recorded)
dat$PEF_init_time_recorded[!is.na(dat$PEF_init_time)] <- "Recorded"
dat$PEF_init_time_recorded <- factor(dat$PEF_init_time_recorded, levels = c("Recorded", "Not recorded"))

summary(dat$PEF_init_date_recorded)
summary(dat$PEF_init_time_recorded)


# Now to make the % predicted

dat <- dat %>% mutate(PEF_percent_pred_value = round(ifelse(!is.na(PEF_prev_value), PEF_init_value/PEF_prev_value,
                                                PEF_init_value/PEF_predict_value)*100, 0))

dat <- dat %>% mutate(PEF_percent_pred_calculated = factor(ifelse(is.na(PEF_percent_pred_value), "Not calculated", "Calculated"))) 

summary(dat$PEF_percent_pred_calculated)

dat %>% filter(!is.na(PEF_init_value)) %>% filter(!is.na(PEF_prev_value) | !is.na(PEF_predict_value)) %>% nrow()

summary(dat$PEF_percent_pred_value)
summary(dat$PEF_percent_pred_calculated)


dat %>% filter(PEF_init_recorded == "Recorded" & 
                 (PEF_prev_recorded == "Recorded" | PEF_predict_recorded == "Recorded")) %>% nrow()

dat <- dat %>% mutate(PEF_prev_or_predict_recorded = factor(ifelse(PEF_prev_recorded == "Recorded" | 
                                                              PEF_predict_recorded == "Recorded", 
                                                            "Recorded", "Not recorded"), 
                                                            levels = c("Recorded", "Not recorded")))

dat %>% select(PEF_prev_recorded, PEF_predict_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_recorded, PEF_percent_pred_calculated) %>% table(useNA = "ifany")



dat %>% select(PEF_percent_pred_calculated, PEF_init_recorded) %>% table(useNA = "ifany")
dat %>% select(PEF_prev_or_predict_recorded, PEF_init_recorded) %>% table(useNA = "ifany")

dat %>% select(PEF_prev_or_predict_recorded, PEF_percent_pred_calculated) %>% table(useNA = "ifany")

# Create a variable for only those where the initial PEF value was recorded

dat <- dat %>% mutate(PEF_prev_or_predict_recorded_only_PEF_init = PEF_prev_or_predict_recorded)
dat$PEF_prev_or_predict_recorded_only_PEF_init[dat$PEF_init_recorded != "Recorded"] <- NA             

dat %>% select(PEF_prev_or_predict_recorded_only_PEF_init, PEF_init_recorded) %>% table(useNA = "ifany")

# Split into <75% and => 75%

dat <- dat %>% mutate(PEF_percpred_75 = factor(ifelse(PEF_percent_pred_value < 75, "<75%",
                                       ifelse(PEF_percent_pred_value >= 75, ">= 75%", NA))))

# Seems to work
summary(dat$PEF_percpred_75)

# Matches the expected nummber

# Cleaning


dat %>% filter(is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_time)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% nrow()

dat %>% filter(is.na(steroids_admin_DT)) %>% filter(!is.na(steroids_admin_date)) %>% nrow()
dat %>% filter(is.na(steroids_admin_DT)) %>% filter(!is.na(steroids_admin_time)) %>% nrow()


dat %>% select(steroids_admin) %>% table()
dat %>% filter(steroids_admin == "Yes") %>% filter(is.na(steroids_admin_DT)) %>% nrow()
dat %>% filter(!is.na(steroids_admin_date)) %>% select(steroids_admin) %>% table(useNA = "ifany")

dat %>% filter(!is.na(steroids_admin_DT)) %>% select(steroids_admin) %>% table(useNA = "ifany")
dat %>% filter(!is.na(steroids_admin_DT)) %>% filter(steroids_admin != "Yes") %>% nrow()
dat %>% filter(!is.na(steroids_admin_date)) %>% filter(steroids_admin != "Yes")  %>% nrow()
dat %>% filter(!is.na(steroids_admin_time)) %>% filter(steroids_admin != "Yes") %>% nrow()

dat %>% filter(is.na(steroids_admin_DT)) %>% filter(steroids_admin == "Yes") %>% nrow()


# This many people have an steroids administration date but shouldn't have one according to their steroid status:
dat %>% filter(!is.na(steroids_admin_date)) %>% filter(steroids_admin != "Yes") %>% nrow()

# This many people have time/date descrepancies for steroids so need to be removed:
dat %>% filter((is.na(steroids_admin_DT) & steroids_admin == "Yes"))  %>% nrow()


# Need to be removed, to leave this many people:
dat <- dat %>% filter(!(is.na(steroids_admin_DT) & steroids_admin == "Yes"))


nrow(dat)




# and then do the same for beta agonists

dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% filter(!is.na(b2a_admin_date)) %>% select(b2a_admin) %>% table(useNA = "ifany") 
dat %>% filter(!is.na(b2a_admin_DT)) %>% select(b2a_admin) %>% table(useNA = "ifany")
dat %>% filter(!is.na(b2a_admin_DT)) %>% filter(b2a_admin != "Yes")  %>% nrow()
dat %>% filter(!is.na(b2a_admin_date)) %>% filter(b2a_admin != "Yes") %>% nrow() 
dat %>% filter(!is.na(b2a_admin_time)) %>% filter(b2a_admin != "Yes") %>% nrow()


# This many people have an b2a administration date but shouldn't have one according to their beta agonist status:
dat %>% filter(!is.na(b2a_admin_date)) %>% filter(b2a_admin != "Yes") %>% nrow()

# Need to be removed, to leave this many people:
dat <- dat %>% filter(!(!is.na(b2a_admin_date) & b2a_admin != "Yes"))
nrow(dat)


dat %>% select(b2a_admin) %>% table()
dat %>% filter(!is.na(b2a_admin_date)) %>% select(b2a_admin) %>% table()


dat %>% filter(is.na(b2a_admin_date)) %>% nrow()
dat %>% filter(is.na(b2a_admin_time)) %>% nrow()
dat %>% filter(is.na(b2a_admin_DT)) %>% nrow()
dat %>% select(b2a_admin) %>% table()
dat %>% filter(b2a_admin == "Yes") %>% filter(is.na(b2a_admin_DT)) %>% nrow()




# and then do the same for RSR

dat %>% filter(is.na(RSR_date)) %>% nrow()
dat %>% filter(is.na(RSR_time)) %>% nrow()
dat %>% filter(is.na(RSR_DT)) %>% nrow()
dat %>% select(RSR) %>% table()
dat %>% filter(RSR == "Yes") %>% filter(is.na(RSR_DT)) %>% nrow()
dat %>% filter(!is.na(RSR_date)) %>% select(RSR) %>% table(useNA = "ifany") 
dat %>% filter(!is.na(RSR_DT)) %>% select(RSR) %>% table(useNA = "ifany")
dat %>% filter(!is.na(RSR_DT)) %>% filter(RSR != "Yes") %>% nrow()
dat %>% filter(!is.na(RSR_date)) %>% filter(RSR != "Yes") %>% nrow()
dat %>% filter(!is.na(RSR_time)) %>% filter(RSR != "Yes") %>% nrow()


# This many people have an RSR date but shouldn't have one according to their RSR status:
dat %>% filter(!is.na(RSR_date)) %>% filter(RSR != "Yes") %>% nrow()

# Need to be removed, to leave this many people:
dat <- dat %>% filter(!(!is.na(RSR_date) & RSR != "Yes"))
nrow(dat)




# Discharge bundle consistency check - How many patients marked as having no discharge bundle elements,
# but actually did have at least 1?

dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), any_vars(. == 1)) %>% 
  nrow()

# For reference, this many people had 'none' ticked and truly had none
dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), all_vars(. == 0)) %>% 
  nrow()

dat %>% select(discharge_bundle, life_status) %>% table(useNA = "ifany")





# I'm just going to filter these using the study IDs because I'm really stuck trying to invert what I've done using 
# filter and filter_at...

IDOI <- dat %>% filter(DB_none == 1) %>% filter_at(vars(DB_inhaler:DB_spec_review_4_weeks), any_vars(. == 1)) %>%
  select(study_ID)

dat <- dat %>% filter(!(study_ID %in% IDOI$study_ID))

# After filtering them out, our new N is:
nrow(dat)

# Filter out invalid dates (N = 0)
table(dat$life_status)



# Was anyone receive anything after being discharged? 

dat %>% filter(discharge_DT - arrival_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()
dat %>% filter(discharge_DT - steroids_admin_DT < 0) %>% nrow()


# This many people received beta agonists after discharge so need to be removed:
dat %>% filter(discharge_DT - b2a_admin_DT < 0) %>% nrow()

dat <- dat %>% filter((discharge_DT - b2a_admin_DT >= 0 ) | is.na(b2a_admin_DT))

# This leaves this many people:
dat %>% nrow()


# This many people received their PEF after discharge so need to be removed:
dat %>% filter(discharge_DT - PEF_init_DT < 0) %>% nrow()

dat <- dat %>% filter((discharge_DT - PEF_init_DT >= 0 ) | is.na(PEF_init_DT))

# This leaves this many people:
dat %>% nrow()

# Filter out invalid heart rates (0-250) or resp rates (0-80) or oxygen sats (60-100). None present.

dat %>% filter(heart_rate < 0) %>% nrow()
dat %>% filter(heart_rate > 250) %>% nrow()

dat %>% filter(resp_rate < 0) %>% nrow()
dat %>% filter(resp_rate > 80) %>% nrow()

dat %>% filter(oxygen_sat_value < 60) %>% nrow()
dat %>% filter(oxygen_sat_value > 100) %>% nrow()


dat %>% filter(is.na(oxygen_sat_value)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value)) %>% nrow()
dat %>% select(oxygen_sat_recorded) %>% table(useNA = "ifany")
dat %>% filter(is.na(oxygen_sat_value) & is.na(oxygen_sat_recorded)) %>% nrow()
dat %>% filter(!is.na(oxygen_sat_value) & !is.na(oxygen_sat_recorded)) %>% nrow()

summary(dat$oxygen_sat_recorded)

str(dat$oxygen_sat_recorded)

dat <- dat %>% mutate(oxygen_sat_recorded = fct_na_value_to_level(oxygen_sat_recorded, "Recorded"))



# Do we have any duplicates?

# assess whether there are duplicate records. Done based on:
# country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date

# We have this many duplicated records:

dat %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% duplicated() %>% 
  sum()

nrow(dat)

dat %>% select(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% duplicated() %>% sum()
#  mutate(patient_ID = as.character(patient_ID)) %>% duplicated()  %>% arrange(patient_ID)

# dat[duplicated(select(dat, country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)), ] %>% 
#   mutate(patient_ID = as.character(patient_ID)) %>% arrange(patient_ID)

dat %>% group_by(country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date) %>% filter(n()>1) %>% 
  ungroup() %>% as.data.frame() %>% nrow()


dat <- dat %>% arrange(patient_ID, arrival_DT)



dat <- dat[!duplicated(select(dat, country, trust_code, patient_ID, hosp_code, LSOA, age, gender, arrival_date, discharge_date)), ]


# These are removed to leave this many records:
nrow(dat)

summary(dat$life_status)

# dat %>% filter(life_status == "Died as inpatient") %>% select(country, trust_code, hosp_code, LSOA, age, gender, arrival_date, discharge_date)

# Now, we create the variables



# More on steroids

# This is what we have to break it down into:

summary(dat$steroids_admin)
summary(dat$steroids_24hr_prev)

dat$steroids_admin_or_24hr_prev <- "No"
dat$steroids_admin_or_24hr_prev[dat$steroids_admin == "Yes" | dat$steroids_24hr_prev == "Yes"] <- "Yes"
dat$steroids_admin_or_24hr_prev <- factor(dat$steroids_admin_or_24hr_prev, levels = c("Yes", "No"))

dat <- dat %>% mutate(steroids_1hour = factor(ifelse(steroids_admin == "Not administered", "Not administered",
                                                     ifelse(arrival_to_steroids_hours >= 1, ">=1 hour", "<1 hour")),
                                              levels = c("<1 hour", ">=1 hour", "Not administered")))
dat$steroids_1hour[dat$steroids_24hr_prev == "Yes"] <- NA

summary(dat$steroids_1hour)


dat <- dat %>% mutate(PEF_init_1hour = as.character(PEF_init_recorded))
dat$PEF_init_1hour[dat$arrival_to_PEF_init_hours >= 1] <- ">=1 hour"
dat$PEF_init_1hour[dat$arrival_to_PEF_init_hours < 1] <- "<1 hour"
dat$PEF_init_1hour[dat$PEF_init_1hour == "No - patient unable to do PEF"] <- NA
dat$PEF_init_1hour[dat$PEF_init_1hour %in% c("No - not done", "Not recorded")] <- "Not done or not recorded"
dat$PEF_init_1hour[dat$PEF_init_recorded == "Recorded" & is.na(dat$PEF_init_DT)] <- "PEF done but time or date not recorded"
dat$PEF_init_1hour <- factor(dat$PEF_init_1hour, levels = c("<1 hour", ">=1 hour", "PEF done but time or date not recorded", "Not done or not recorded"))

summary(dat$PEF_init_1hour)
summary(dat$PEF_init_recorded)

dat$b2a_admin_or_1hr_prev <- "No"
dat$b2a_admin_or_1hr_prev[dat$b2a_admin == "Yes" | dat$b2a_1hr_prev == "Yes - up to 1 hour prior to arrival"] <- "Yes"
dat$b2a_admin_or_1hr_prev <- factor(dat$b2a_admin_or_1hr_prev, levels = c("Yes", "No"))


dat <- dat %>% mutate(b2a_1hour = factor(ifelse(b2a_admin == "Not administered", "Not administered",
                                                ifelse(arrival_to_b2a_minutes/60 >= 1, ">=1 hour", "<1 hour")),
                                         levels = c("<1 hour", ">=1 hour", "Not administered")))
dat$b2a_1hour[dat$b2a_1hr_prev == "Yes - up to 1 hour prior to arrival"] <- NA

summary(dat$b2a_1hour)
table(dat$b2a_1hr_prev)


table(dat$RSR)

dat <- dat %>% mutate(arrival_to_RSR_24hour = factor(ifelse(RSR == "No", "Not received", 
                                                 ifelse(arrival_to_RSR_hours >= 24, ">=24 hours", "<24 hours")),
                                          levels = c("<24 hours", ">=24 hours", "Not received")))
summary(dat$arrival_to_RSR_24hour)


# The RSR needs to be further split up into weekdays and weekends
# easiest way to do this is to create a new variable for weekday arrival


# # # # NOTE THIS HAS CHANGED NOW THAT DAYTIME/NIGHT TIME AUTOMATICALLY INCLUDED.

# also here, we split into daytime and night time

# discharge weekday/weekend daytime/night time.




# Looks good. Now we break down the RSR variable further
# 

dat$arrival_to_RSR_24hour_weekday <- dat$arrival_to_RSR_24hour
dat$arrival_to_RSR_24hour_weekday[dat$weekday_weekend_arrival == "Weekend arrival"] <- NA

dat$arrival_to_RSR_24hour_weekend <- dat$arrival_to_RSR_24hour
dat$arrival_to_RSR_24hour_weekend[dat$weekday_weekend_arrival == "Weekday arrival"] <- NA

dat %>% select(arrival_to_RSR_24hour, arrival_to_RSR_24hour_weekday) %>% table(useNA = "ifany")
dat %>% select(arrival_to_RSR_24hour, arrival_to_RSR_24hour_weekend) %>% table(useNA = "ifany")
dat %>% select(arrival_to_RSR_24hour_weekday, arrival_to_RSR_24hour_weekend) %>% table(useNA = "ifany")


# dat$steroids_1hour[dat$steroids_pre_arrival == "Yes"] <- NA
summary(dat$steroids_1hour)

# Sorted. For some reason, NA aren't counted as not being <1.

# summary(dat$steroids_pre_arrival)
# summary(dat$arrival_to_steroids_hours)
# summary(dat$arrival_to_steroids_hours_inc_pre_arrival_steroids)
# summary(dat$steroids_admin)
# 
# dat %>% select(steroids_pre_arrival, steroids_1hour, steroids_admin) %>% table(useNA = "ifany")

# And now we sort out the discharge bundle variables, with inappropriate tobacco smoking ones recoded to missing.
# We also have to remove those who transferred to another hospital.


# Make those who don't smoke missing for 'DB_smoke'.

summary(dat$smoke_status)
table(dat$DB_smoke)

dat %>% filter(DB_smoke == 1) %>% select(starts_with("smoke")) %>% summary()

dat %>% filter(DB_smoke == 1) %>% filter(smoke_tobacco != "Current" &
                                          smoke_shisha != "Current" &
                                          smoke_cannabis != "Current" &
                                          smoke_other != "Current" &
                                           vaping_status != "Current") %>% nrow()
table(dat$smoke_shisha, dat$DB_smoke)
table(dat$smoke_cannabis, dat$DB_smoke)
table(dat$smoke_other, dat$DB_smoke)
table(dat$vaping_status, dat$DB_smoke)


# Going to save a copy of this original variable (including everyone

dat <- dat %>% mutate(DB_smoke_everyone = DB_smoke)



dat$DB_smoke[dat$smoke_tobacco != "Current"] <- NA


# Follow-up variable:

dat$DB_FU_any <- dat$DB_comm_FU_2_days
dat$DB_FU_any[dat$DB_spec_review_4_weeks == 1] <- 1

table(dat$DB_FU_any, useNA = "ifany")


dat %>% filter(discharge_bundle == "Patient transferred to another hospital") %>% select(starts_with("DB")) %>% summary()

dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")),
                         .funs = ~ifelse(life_status == "Died as inpatient" | 
                                         discharge_bundle == "Patient transferred to another hospital", NA, .))





# And now we make all those who aren't current smokers missing for 'DB_smoke':

# dat$DB_smoke[dat$smoke_status != "Current smoker" | is.na(dat$smoke_status)] <- NA
# dat %>% select(smoke_status, DB_smoke) %>% table(useNA = "ifany")
# 
# # And we do the same for parents' smoking status
# 
# dat$DB_parent_smoke[dat$SH_smoke != "Yes"] <- NA
# dat %>% select(SH_smoke, DB_parent_smoke) %>% table(useNA = "ifany")
# 

# # Going to create a new variable for transferred and not transferred, and then recode transferred as missing
# # for the discharge bundle variable. Doing it right at the end because I know I use that variable previously.
# 
# # No missing so doing this is fine. But maybe need to watch out if I re-use this if someone dies.
# 
# dat <- dat %>% mutate(transferred = factor(ifelse(
#                                            discharge_bundle == "Patient transferred to another hospital", "Yes",
#                                     ifelse(discharge_bundle != "Patient transferred to another hospital", "No",
#                                                   NA))))
# 
# # And now we change discharge bundle so that the transferreds are missing
# 
# dat$discharge_bundle[dat$discharge_bundle == "Patient transferred to another hospital"] <- NA
# dat$discharge_bundle <- factor(dat$discharge_bundle)
# 
# str(dat$transferred)
# 
# dat %>% select(transferred, discharge_bundle) %>% table(useNA = "ifany")
# 
# # I also need to create a variable for discharge bundle yes/no for the subanalysis section

summary(dat$discharge_bundle)



dat <- dat %>% mutate(discharge_bundle_yes_no = discharge_bundle)
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Self discharge"] <- "No"
dat$discharge_bundle_yes_no[dat$discharge_bundle_yes_no == "Patient transferred to another hospital"] <- NA
dat$discharge_bundle_yes_no <- factor(dat$discharge_bundle_yes_no, levels = c("No", "Yes"))

summary(dat$discharge_bundle_yes_no)


dat %>% select(starts_with("DB")) %>% summary()






summary(dat$oxygen_sat_recorded)
dat$oxygen_sat_recorded <- factor(dat$oxygen_sat_recorded, levels = c("Recorded", "Not recorded"))
summary(dat$oxygen_sat_recorded)

summary(dat$oxygen_sat_measurement_type)
dat$oxygen_sat_measurement_type <- factor(dat$oxygen_sat_measurement_type, levels = c("Yes", "No - room air",
                                                                                      "Not recorded"))
summary(dat$oxygen_sat_measurement_type)



summary(dat$PEF_init_recorded)
dat$PEF_init_recorded <- factor(dat$PEF_init_recorded, levels = c("Recorded",
                                                                  "No - patient unable to do PEF", "No - not done", "Not recorded"))
summary(dat$PEF_init_recorded)




# dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered", "Not recorded"))
summary(dat$steroids_admin)
dat$steroids_admin <- factor(dat$steroids_admin, levels = c("Yes", "Not administered")) 
summary(dat$steroids_admin)


# dat$b2a_pre_arrival <- factor(dat$b2a_pre_arrival, levels = c("Yes", "Not recorded", "No"))
summary(dat$b2a_admin)
dat$b2a_admin <- factor(dat$b2a_admin, levels = c("Yes", "Not administered"))
summary(dat$b2a_admin)


# dat$crit_care_total <- factor(dat$crit_care_total, levels = c("Yes - ICU", "Yes - HDU", 
#                                                               "Yes - HDU,Yes - ICU", "No"))
# dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Parental/carer/self-discharge"))

summary(dat$discharge_bundle)
dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Self discharge", "Patient transferred to another hospital"))
summary(dat$discharge_bundle)

# dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis, 
#                                    levels = c("Yes", "No - not medically indicated", "No - reason not given",
#                                               "Offered but patient/parent/carer declined"))

summary(dat$inhaled_steroids_dis)
dat$inhaled_steroids_dis <- factor(dat$inhaled_steroids_dis,
                                   levels = c("Yes", "No", "Not prescribed for medical reasons"))
summary(dat$inhaled_steroids_dis)


table(dat$inhaled_steroids_dis)
table(dat$oral_steroids_dis, useNA = "ifany")

dat %>% filter(life_status == "Alive") %>% filter(is.na(DB_inhaler)) %>% nrow()

dat %>% filter(is.na(patient_ID)) %>% nrow()
# dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No - not medically indicated", 
#                                 "No - reason not given"))

summary(dat$oral_steroids_dis)
dat$oral_steroids_dis <- factor(dat$oral_steroids_dis, levels = c("Yes", "No"))
summary(dat$oral_steroids_dis)



dat %>% filter(life_status == "Died as inpatient") %>% select(oral_steroids_rescue_history) %>% table()
# dat %>% filter(is.na(oral_steroids_rescue_history)) %>% summary()


# oral steroids rescue history should be missing if patient died or transferred

dat$oral_steroids_rescue_history[dat$life_status == "Died as inpatient" | 
                                   dat$discharge_bundle == "Patient transferred to another hospital"] <- NA

dat$oral_steroids_rescue_history <- factor(dat$oral_steroids_rescue_history, levels = c("Yes", "No", 
                                                                                        "Not recorded"))

summary(dat$oral_steroids_rescue_history)

# dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No - not medically indicated", 
#                                                               "Not recorded", 
#                               "Patient/parent/carer declined", "Already being seen in secondary care clinic"))

# no longer variable in dataset

# dat$referred_for_FU <- factor(dat$referred_for_FU, levels = c("Yes", "No", "Not recorded", 
#                                                               "Patient declined", "Already being seen in secondary care clinic"))




summary(dat$oxygen_prescribed)
dat$oxygen_prescribed <- factor(dat$oxygen_prescribed, levels = c("Yes", "Yes - but date/time not recorded", "No"))
summary(dat$oxygen_prescribed)

summary(dat$oxygen_admin)
dat$oxygen_admin <- factor(dat$oxygen_admin, levels = c("Yes", "No"))
summary(dat$oxygen_admin)


# Format of this changed

# dat$oxygen_prescribed <- fct_recode(dat$oxygen_prescribed, `Prescribed and administered` = "Prescribed,Administered",
#                                     `Prescribed and administered` = "Administered,Prescribed",
#                                     `Prescribed only` = "Prescribed", `Administered only` = "Administered")
# 
# dat$oxygen_prescribed <- factor(dat$oxygen_prescribed, levels = c("Prescribed only", "Administered only", 
#                                                                   "Prescribed and administered", "No"))
# 
# summary(dat$oxygen_prescribed)






# - - - - - - - - - - - - - -#
#  Defining asthma severity  #
# - - - - - - - - - - - - - -#

summary(dat$PEF_percent_pred_value)
summary(dat$PEF_predict_value)
summary(dat$PEF_prev_value)
summary(dat$PEF_init_value)
summary(dat$PEF_percent_pred_calculated)

dat %>% filter(!is.na(PEF_init_value)) %>% nrow()
dat %>% filter(!is.na(PEF_init_value)) %>% filter(!is.na(PEF_prev_value) | !is.na(PEF_predict_value)) %>% nrow()
dat %>% filter(!is.na(PEF_percent_pred_value)) %>% nrow()

# this is correct.

# Create a variable for peak flow severity:
# Bear in mind that 396 people were too unwell for peak flow to be measured - let's see how they
# compare with the other stats used to define severity. Only becomes something to worry about if
# their other stats are all normal.


# Don't need to worry about floating point rounding errors for this 
# But can't use cut because of the definitions the left and the right are both < and >. 

dat <- dat %>% mutate(PEF_percent_pred_sev = NA)


table(dat$PEF_init_recorded)

dat$PEF_percent_pred_sev[dat$PEF_percent_pred_value > 50] <- "Moderate"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred_value <= 50] <- "Severe"
dat$PEF_percent_pred_sev[dat$PEF_percent_pred_value < 33] <- "Life-threatening"
dat$PEF_percent_pred_sev[is.na(dat$PEF_percent_pred_value)] <- "Unavailable"

dat$PEF_percent_pred_sev[dat$PEF_init_recorded == "No - patient unable to do PEF"] <- "Severe"
table(dat$PEF_percent_pred_sev)
dat$PEF_percent_pred_sev <- factor(dat$PEF_percent_pred_sev, 
                                   levels = c("Moderate", "Severe", "Life-threatening", "Unavailable"))
table(dat$PEF_percent_pred_sev)

summary(dat$PEF_percent_pred_sev)
summary(dat$PEF_init_recorded)

dat$resp_rate_sev <- cut(dat$resp_rate, breaks = c(-1, 24, Inf),
                       labels = c("Normal", "High"))



table(dat$resp_rate_sev)
dat %>% filter(is.na(resp_rate_sev)) %>% nrow()
dat %>% filter(is.na(resp_rate)) %>% nrow()



dat$heart_rate_sev <- cut(dat$heart_rate, breaks = c(-1, 109, Inf),
                        labels = c("Normal", "High"))

table(dat$heart_rate_sev, dat$resp_rate_sev, useNA = "ifany")
dat %>% filter(is.na(resp_rate_sev)) %>% nrow()
dat %>% filter(is.na(resp_rate)) %>% nrow()




dat$oxygen_sat_sev <- as.character(cut(dat$oxygen_sat_value, breaks = c(-1, 92, 101), right = FALSE,
                    labels = c("Low", "Normal")))
dat$oxygen_sat_sev[is.na(dat$oxygen_sat_value)] <- "Unavailable"
dat$oxygen_sat_sev <- factor(dat$oxygen_sat_sev)
table(dat$oxygen_sat_sev, useNA = "ifany")


dat %>% filter(oxygen_sat_value == 92) %>% select(oxygen_sat_sev) %>% head()
dat %>% filter(oxygen_sat_value < 92) %>% select(oxygen_sat_sev) %>% head()




table(dat$oxygen_sat_sev, dat$resp_rate_sev, useNA = "ifany")
table(dat$oxygen_sat_sev, dat$heart_rate_sev, useNA = "ifany")
table(dat$resp_rate_sev, dat$heart_rate_sev, useNA = "ifany")
table(dat$PEF_percent_pred_sev, useNA = "ifany")


# We can now add in the last variables we needed

dat <- dat %>% mutate(symptom_sev = ifelse(symptoms_low_PaO2 == 1 |
                                           symptoms_normal_PaCO2 == 1 |           
                                           symptoms_silent_chest == 1 |
                                           symptoms_cyanosis == 1 |
                                           symptoms_poor_respiratory_effort == 1 |
                                           symptoms_hypotension == 1 |
                                           symptoms_exhaustion == 1 |
                                           symptoms_altered_conscious == 1, 
                                           "Life-threatening", "Moderate"))
  
dat$symptom_sev[dat$symptoms_breathless == 1 & dat$symptom_sev != "Life-threatening"] <- "Severe"
dat$symptom_sev[dat$symptoms_raised_PaCO2 == 1] <- "Near-fatal"

dat$symptom_sev <- factor(dat$symptom_sev, levels = c("Moderate", "Severe", "Life-threatening", "Near-fatal"))

dat %>% select(starts_with("symptom")) %>% summary()
dat %>% filter(symptoms_none == 1) %>% select(starts_with("symptom")) %>% summary()

dat %>% filter(PEF_init_recorded == "No - patient unable to do PEF") %>% filter(symptom_sev == "Moderate") %>% 
  select(ends_with("sev")) %>% summary()

# I'm just going to define asthma severity as one thing now, which doesn't account for whether a measurement it missing
# unless it is 'too ill for PEF'.
# This one only uses high resp/heart rates for severity.

summary(dat$oxygen_sat_sev)

dat <- dat %>% mutate(asthma_sev = factor(ifelse(symptom_sev == "Near-fatal", "Near-fatal",    # near-fatal
                                                 ifelse(symptom_sev == "Life-threatening" |    # life-threatening
                                                          PEF_percent_pred_sev == "Life-threatening" |
                                                          oxygen_sat_sev == "Low", "Life-threatening",
                                                        ifelse(symptom_sev == "Severe" | 
                                                                 PEF_percent_pred_sev == "Severe" |   # severe
                                                                 resp_rate_sev == "High" | 
                                                                 heart_rate_sev == "High", "Severe",
                                                               "Moderate"))),  # moderate
                                          levels = c("Moderate", "Severe", "Life-threatening", "Near-fatal")))


summary(dat$asthma_sev)

dat %>% filter(asthma_sev == "Moderate") %>% select(ends_with("sev")) %>% summary()
dat %>% filter(asthma_sev == "Severe") %>% select(ends_with("sev")) %>% summary()
dat %>% filter(asthma_sev == "Life-threatening") %>% select(ends_with("sev")) %>% summary()
dat %>% filter(asthma_sev == "Near-fatal") %>% select(ends_with("sev")) %>% summary()

# All looks good


########################################
# BPT: 
########################################


dat$RSR_24hour_BPT <- NA
dat$RSR_24hour_BPT[dat$arrival_to_RSR_24hour == "<24 hours"] <- 1
dat$RSR_24hour_BPT[dat$arrival_to_RSR_24hour %in% c(">=24 hours", "Not received")] <- 0

# dat$RSR_BPT[dat$life_status == "Died as inpatient"] <- NA  # those who died still included now

dat <- dat %>% mutate(DB_BPT = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
                                        DB_adherence == 1 & DB_PAAP == 1 &
                                        DB_FU_any == 1 &
                                        discharge_bundle == "Yes" & (DB_smoke == 1 | is.na(DB_smoke)),
                                      1, 0))
dat$DB_BPT[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA

# dat <- dat %>% mutate(DB_BPT1 = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
#                                         DB_adherence == 1 & DB_PAAP == 1 & 
#                                         DB_FU_any == 1 & 
#                                         (DB_smoke == 1 | is.na(DB_smoke)),
#                                       1, 0))
# table(dat$DB_BPT)
# 
# dat <- dat %>% mutate(DB_BPT2 = ifelse(DB_inhaler == 1 & DB_maintenance == 1 & 
#                                         DB_adherence == 1 & DB_PAAP == 1 &
#                                         DB_FU_any == 1 & 
#                                         discharge_bundle == "Yes" & (DB_smoke == 1 | is.na(DB_smoke)),
#                                       1, 0))
# 
# table(dat$DB_BPT1, dat$DB_BPT2)



dat <- dat %>% mutate(BPT = ifelse(RSR_24hour_BPT == 1 & DB_BPT == 1,
                                   1, 0))
dat$BPT[dat$life_status != "Alive"] <- NA

table(dat$BPT, useNA = "ifany")
table(dat$RSR_24hour_BPT, useNA = "ifany")
table(dat$DB_BPT, useNA = "ifany")


dat <- dat %>% mutate(arrival_to_RSR_hours_died = arrival_to_RSR_hours)
dat$arrival_to_RSR_hours_died[dat$life_status == "Alive"] <- NA







# Extra variables:

summary(dat$anyIMD)


# length of time until oxygen administration


dat <- dat %>% mutate(arrival_to_oxygen_minutes = difftime(oxygen_DT, arrival_DT, units = "mins"))
head(dat$arrival_to_oxygen_minutes)
dat$arrival_to_oxygen_minutes <- as.integer(dat$arrival_to_oxygen_minutes)

dat$arrival_to_oxygen_minutes[dat$oxygen_admin != "Yes"] <- NA


summary(dat$arrival_to_oxygen_minutes)
summary(dat$oxygen_DT)
summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)

dat %>% select(arrival_to_oxygen_minutes) %>% arrange(arrival_to_oxygen_minutes) %>% head(20)

dat %>% select(oxygen_admin, oxygen_prescribed) %>% table(useNA = "ifany")


# New variable one: oxygen prescribed in those who were administered

dat$oxygen_prescribed_if_administered <- as.character(dat$oxygen_prescribed)
dat$oxygen_prescribed_if_administered[dat$oxygen_prescribed_if_administered == "Yes - but date/time not recorded"] <- "Yes"
dat$oxygen_prescribed_if_administered[dat$oxygen_admin == "No"] <- NA
dat$oxygen_prescribed_if_administered <- factor(dat$oxygen_prescribed_if_administered, levels = c("Yes", "No"))
summary(dat$oxygen_prescribed_if_administered)

# New variable two: oxygen prescribed within 1 hour, in those administered oxygen





# And, we create James' extra variable

summary(dat$arrival_to_oxygen_minutes)
summary(dat$oxygen_prescribed)

summary(dat$oxygen_admin)

dat %>% filter(!is.na(arrival_to_oxygen_minutes)) %>% select(oxygen_prescribed) %>% summary()
# dat %>% filter(!is.na(arrival_to_oxygen_minutes)) %>% select(oxygen_admin) %>% summary()


dat$oxygen_presc_1hr <- as.character(dat$oxygen_prescribed)
dat$oxygen_presc_1hr[dat$arrival_to_oxygen_minutes < 60] <- "<1 hour"
dat$oxygen_presc_1hr[dat$arrival_to_oxygen_minutes >= 60] <- ">=1 hour"
dat$oxygen_presc_1hr[dat$oxygen_presc_1hr == "Yes - but date/time not recorded"] <- "Prescribed but date or time not recorded"
dat$oxygen_presc_1hr[dat$oxygen_admin == "No"] <- NA
table(dat$oxygen_prescribed)

dat$oxygen_presc_1hr <- factor(dat$oxygen_presc_1hr, 
                               levels = c("<1 hour", ">=1 hour", 
                                          "Prescribed but date or time not recorded", "No"))
summary(dat$oxygen_presc_1hr)
table(dat$oxygen_admin, dat$oxygen_presc_1hr, useNA = "ifany")
# add this bit in - only interested if they were administered oxygen
dat$oxygen_presc_1hr[dat$oxygen_admin != "Yes"] <- NA



saveRDS(dat, "C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Data/tidyData/AA_SCC_2023-24_clean_data.RDS")
