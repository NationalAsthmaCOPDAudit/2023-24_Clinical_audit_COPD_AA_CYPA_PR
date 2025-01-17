#                                                           #
# COPD SCC 2024                                             #
#                                                           #
# Author: Alex Adamson                                      #
# Date created:  31st May 2024                              #
#                                                           #




library(tidyverse)
library(psych)

medTableforadmiss <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- sum(eng, na.rm = TRUE)
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- sum(wal, na.rm = TRUE)
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- sum(scot, na.rm = TRUE)
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- sum(all, na.rm = TRUE)
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
  return(ret)
}


meanSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, mean, sd)
  varcol[ ,3:4] <- format(round(varcol[ ,3:4], roundno), nsmall = roundno)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
  
}

mediSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, median, lo.quart, hi.quart)
  # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno), nsmall = roundno) # otherwise use 'roundno'
  
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  
  if(nrow(gen) == 0) {return(var_N)}
  
  else {
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
    # gen.E2 <- select(gen.E2, Var1, England)
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      var_N <- cbind(var_N, gen3[ ,2:3])
    }
    return(var_N)
    
  }
}



medTable <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, scot, wal, all), nrow = 1, ncol = 4)
  
  colnames(ret) <- c(paste("England (N=", EN, ")", sep = ""),
                     paste("Scotland (N=", SN, ")", sep = ""),
                     paste("Wales (N=", WN, ")", sep = ""),
                     paste("All (N=", AN, ")", sep = ""))
  
  
  return(ret)
}

# And another one that will work for calculatng frequencies:


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



# comparing with dat_old shows it is basically the same, with some minor changes

dat_old <- read.csv("C:/Alex Harley/Audit_2023_onwards/2022-2023/COPD/Data/rawData/NACAP-COPDSC-2204-2303-v101+LSOA-NDO-Imperial.csv",
                header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

dat <- read.csv("C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Data/rawData/COPD-2405-Imperial-A.csv",
                    header = TRUE, stringsAsFactors = TRUE, na.strings = c("NA", ""))

checkSame(dat, dat_old)



# i need to add in the hospital and trust names, areas etc this time.

dat_old <- dat_old %>% select(study_ID = STUDYID,
                      patient_ID = PATIENTID,
                      overseas = X..Overseas.or.Non.NHS,
                      LSOA = lsoa11,
                      hosp_code = Org,
                  #   imd = imd, # doesn't add anything
                  #    hosp_name = OrgName,
                      trust_code = TrustCode,
                  #    trust_name = Trust.Now,
                  #    region = Region,
                      country = Country,
                      arrival_date = X1.2a.Arrival.Date,
                      arrival_time = X1.2b.Arrival.Time,
                      admission_date = X4.1a.Admission.Date,
                      admission_time = X4.1b.Admission.Time,
                      # age = X.Age.At.Arrival, # Just keeps things the same as they were by using admission age
                      age = X.Age.At.Admission,
                      gender = X2.3.Gender,
                      smoke_status = X2.5.Smoking.Status,
                      RSR = X5.1.Respiratory.Review,
                      RSR_date = X5.1a.Date.of.respiratory.review,
                      RSR_time = X5.1b.Time.of.respiratory.review,
                      oxygen_prescribed = X6.1.Oxygen.Prescribed.During.Admission,
                      oxygen_target_range = X6.1a.Oxygen.Stipulated.Target.Range,
                      oxygen_target_range_other = X6.1a.Other.Oxygen.Stipulated.Target.Range,
                      oxygen_admin = X6.2.Oxygen.Administered.During.Admission,
                      NIV = X7.1.Acute.treatment.with.NIV,
                      NIV_date = X7.1a.Date.NIV.First.Commenced,
                      NIV_date_NR = X7.1a.NIV.Date.Not.Recorded,
                      NIV_time = X7.1b.Time.NIV.first.commenced,
                      NIV_time_NR = X7.1b.NIV.Time.Not.Recorded,
                      FEV1_perc_pred_value = X8.1.Most.recently.recorded.FEV1...predicted,
                      FEV1_perc_pred_NR = X8.1.FEV1...Predicted.Not.Recorded,
                      FEV1_perc_pred_date = X8.1a.Date.of.last.recorded.FEV1...predicted,
                      FEV1_perc_pred_date_NR = X8.1a.FEV1...Predicted.Date.Not.Recorded,
                      FEV1FVC_value = X8.2.Most.recently.recorded.FEV1.FVC.ratio,
                      FEV1FVC_NR = X8.2.Recent.FEV1.FVC.Ratio.Not.Recorded,
                      FEV1FVC_date = X8.2a.Date.of.last.recorded.FEV1.FVC.ratio,
                     FEV1FVC_date_NR = X8.2a.Last.FEV1.FVC.Ratio.Date.Not.Recorded,
                      life_status = X10.1.Discharge.Life.Status,
                      discharge_date = X10.2.Discharge.Date,
                      #                     discharge_time = X6.2b.Discharge.Time, - don't have this for some reason
                      discharge_bundle = X10.3.Discharge.Bundle.Completed,
                      discharge_elements_all = X10.4.Discharge.Elements, # completely useless: drop.
                      DB_inhaler = X10.4.Discharge.Elements...Inhaler.technique.checked,
                      DB_maintenance = X10.4.Discharge.Elements...Medication.issued.classes.reviewed,
                      DB_plan = X10.4.Discharge.Elements...Self.management.plan.provided.or.referred.to.community.team.for.plan,
                      DB_pack = X10.4.Discharge.Elements...Emergency.drug.pack.provided.or.referred.to.community.team.for.pack,
                      DB_unsuitable_for_pack = X10.4.Discharge.Elements...Emergency.drug.pack.not.provided.as.assessed.as.unsuit,
                      DB_oxygen_alert = X10.4.Discharge.Elements...Oxygen.alert.card.provided,
                      DB_smoke = X10.4.Discharge.Elements...Smoking.cessation.drugs.referred.for.behavioral.change.intervention,
                      DB_PR = X10.4.Discharge.Elements...Assessed.for.suitability.for.pulmonary.rehabilitation,
                      DB_FU_72hour = X10.4.Discharge.Elements...Follow.up.requests.at.home.within.72.hours.by.person.or.by.phone,
                      DB_MDT = X10.4.Discharge.Elements...Patient.discussed.at.an.MDT.with.a.community.and.or.primary.care.team,
                      DB_BLF_passport = X10.4.Discharge.Elements...BLF.passport.offered.to.the.patient,
                      DB_none = X10.4.Discharge.Elements...None)
# X..Dataset.Version   # useless - drop

# dat <- dat %>% rename(study_ID = ROWID,
                      
dat <- dat %>% select(study_ID = ROWID,
                      patient_ID = PATIENTID,
                      #   overseas = X..Overseas.or.Non.NHS,
                      LSOA = lsoa11,
                      # let's keep this as this is the one that links to IMD
                      hosp_code = Org,
                      #   imd = imd, # doesn't add anything
                      hosp_name = OrgName,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      integrated_care_system = ICS,
                      region = Region,
                      country = Country,
                      arrival_date = X1.1a.Arrival.Date,
                      arrival_time = X1.1b.Arrival.Time,
                      admission_date = X4.1a.Admission.Date,
                      admission_time = X4.1b.Admission.Time,
                      # age = X.Age.At.Arrival, # Just keeps things the same as they were by using admission age
                      age = X..Age.at.Admission,
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
                      vaping_status = X2.8.Vaping.Status, # new
                      NEWS2_recorded_score_only = X3.1.NEWS2.First.Recorded.Score,
                      NEWS2_resp_rate = X3.1a.NEWS2.Respiratory.Rate,
                      NEWS2_oxygen_scale = X3.1b.NEWS2.Oxygen.Scale,
                      NEWS2_oxygen_saturation = X3.1c.NEWS2.Oxygen.Saturation,
                      NEWS2_supplemental_air_oxygen = X3.1d.NEWS2.Supplemental.Air.Oxygen,
                      NEWS2_BP = X3.1e.NEWS2.Systolic.Blood.Pressure,
                      NEWS2_pulse = X3.1f.NEWS2.Pulse,
                      NEWS2_consciousness = X3.1g.NEWS2.Consciousness,
                      NEWS2_temperature = X3.1h.NEWS2.Temperature,
                      NEWS2_score = X3.2.NEWS2.Score.Total..XI.,
                      RSR = X5.1.Respiratory.Review,
                      RSR_date = X5.1a.Respiratory.Review.Date,
                      RSR_time = X5.1b.Respiratory.Review.Time,
                      oxygen_prescribed = X6.1.Oxygen.Prescribed.During.Admission,
                      oxygen_target_range = X6.1a.Oxygen.Target.Range,
                      oxygen_target_range_other = X6.1b.Oxygen.Target.Range.Other,
                      oxygen_admin = X6.2.Oxygen.Administered,
                      AHVF_ever = X7.1.Diagnosis.of.AHVF.at.any.point,
                      AHVF_cont = X7.2.Continued.AHVF.diagnosis.after.1.hour,
                      AHVF_cont_date = X7.2a.Blood.Gas.Date,
                      AHVF_cont_time = X7.2b.Blood.Gas.Time,
                      NIV = X7.3.NIV.Treatment,
                      NIV_date = X7.3a.NIV.Date,
                      NIV_date_NR = X7.3a.x.NIV.Date.Not.Recorded,
                      NIV_time = X7.3b.NIV.Time,
                      NIV_time_NR = X7.3b.x.NIV.Time.Not.Recorded,
                      NIV_location = X7.4.NIV.Location,
                      FEV1_perc_pred_value = X8.1.FEV1..Predicted,
                      FEV1_perc_pred_NR = X8.1.x.FEV1..Predicted.Not.Recorded,
                      FEV1_perc_pred_date = X8.1a.Date.FEV1..Predicted,
                      FEV1_perc_pred_date_NR = X8.1a.x.Date.FEV1..Predicted.Not.Recorded,
                      FEV1FVC_value = X8.2.FEV1.FVC.Ratio,
                      FEV1FVC_NR = X8.2.x.FEV1.FVC.Ratio.Not.Recorded,
                      FEV1FVC_date = X8.2a.FEV1.FVC.Ratio.Date,
                      FEV1FVC_date_NR = X8.2a.x.FEV1.FVC.Ratio.Date.Not.Recorded,
                      life_status = X9.1.Discharge.Life.Status,
                      discharge_date = X9.2.Discharge.Date,
                      #                     discharge_time = X6.2b.Discharge.Time, - don't have this for some reason
                      discharge_bundle = X9.3.Discharge.Bundle,
                    #  discharge_elements_all = X10.4.Discharge.Elements,
                      # completely useless: drop.
                      DB_inhaler = X.9.4.Discharge.Elements...Inhaler.technique,
                      DB_maintenance = X.9.4.Discharge.Elements...Medication.assessment,
                      DB_plan = X.9.4.Discharge.Elements...Self.management.plan,
                      DB_pack = X.9.4.Discharge.Elements...Emergency.drug.pack,
                   #   DB_unsuitable_for_pack = X10.4.Discharge.Elements...Emergency.drug.pack.not.provided.as.assessed.as.unsuit,
                      DB_unsuitable_for_pack = X.9.4.Discharge.Elements...Emergency.drug.pack.not.provided,
                      DB_oxygen_alert = X.9.4.Discharge.Elements...Oxygen.alert,
                      DB_smoke = X.9.4.Discharge.Elements...Smoking.cessation,
                      DB_PR = X.9.4.Discharge.Elements...Pulmonary.rehabilitation,
                 #     DB_PR = X10.4.Discharge.Elements...Assessed.for.suitability.for.pulmonary.rehabilitation,
                      DB_FU_72hour = X.9.4.Discharge.Elements...Follow.up.requests,
                      DB_MDT = X.9.4.Discharge.Elements...Multidisciplinary.team.meeting,
                      DB_BLF_passport = X.9.4.Discharge.Elements...BLF.patient.passport,
                      DB_none = X.9.4.Discharge.Elements...None)

# X..Dataset.Version   # useless - drop

# need to add in the hospital details


# CHANGES:

# - smoking status -> shisha/tobacco/cannabis/other, current/ex/never/NR
# ethnicity
# impairments
# NEWS2 - recorded/calculated/not available. median NEWS2 score?
# AHVF - NIV. NIV by AHVF.

colnames(dat)

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






# Need to add in the empty gender factors and smoking status factors, and while we're doing it we might as well
# put it in the correct order
summary(dat$gender)

dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Transgender", "Not recorded/Preferred not to say", "Other"))
summary(dat$gender)


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



dat$arrival_date <- as.Date(dat$arrival_date, "%d/%m/%Y")
dat$admission_date <- as.Date(dat$admission_date, "%d/%m/%Y")
dat$RSR_date <- as.Date(dat$RSR_date, "%d/%m/%Y")
dat$NIV_date <- as.Date(dat$NIV_date, "%d/%m/%Y")
dat$AHVF_cont_date <- as.Date(dat$AHVF_cont_date, "%d/%m/%Y")
dat$FEV1_perc_pred_date <- as.Date(dat$FEV1_perc_pred_date, "%d/%m/%Y")
dat$FEV1FVC_date <- as.Date(dat$FEV1FVC_date, "%d/%m/%Y")
dat$discharge_date <- as.Date(dat$discharge_date, "%d/%m/%Y")

head(dat$AHVF_cont_date, 50)

dat$arrival_time <- as.character(dat$arrival_time)
dat$admission_time <- as.character(dat$admission_time)
dat$RSR_time <- as.character(dat$RSR_time)
dat$NIV_time <- as.character(dat$NIV_time)
dat$AHVF_cont_time <- as.character(dat$AHVF_cont_time)


summary(dat$arrival_date)
summary(dat$arrival_time)
summary(dat$NIV_date)
summary(dat$FEV1_perc_pred_date)
summary(dat$discharge_date)



# sort out the NIV date/time not recorded variables

dat$NIV_time_NR[dat$NIV == "No"] <- NA
dat$NIV_date_NR[dat$NIV == "No"] <- NA

dat %>% select(NIV_date_NR, NIV) %>% table(useNA = "ifany")
dat %>% select(NIV_time_NR, NIV) %>% table(useNA = "ifany")

# dat$arrival_time[dat$arrival_time != ""] <- paste0(dat$arrival_time[dat$arrival_time != ""]
#                                                    
#                                                    
#                                                    
# dat$NIV_time[dat$NIV_time)
# dat$FEV1_perc_pred_time[dat$FEV1_perc_pred_time)
# dat$discharge_time[dat$discharge_time)
# 
# 
head(dat$arrival_time)



# dat <- dat %>% mutate(arrival_time = ifelse(arrival_time == "", "", 
#                                      paste0(arrival_time, ":00")),
#                       NIV_time = ifelse(NIV_time == "", "", 
#                                             paste0(NIV_time, ":00")),
#                       FEV1_perc_pred_time = ifelse(FEV1_perc_pred_time == "", "", 
#                                             paste0(FEV1_perc_pred_time, ":00")),
#                       discharge_time = ifelse(discharge_time == "", "", 
#                                             paste0(discharge_time, ":00")))

head(dat$NIV_time, 100)
summary(dat$NIV_date)
head(dat$NIV_time)
head(dat$arrival_time)
head(dat$admission_time)
head(dat$RSR_time)
head(dat$arrival_date)
head(dat$discharge_date)

# We can try converting times to 'difftime' when we try and create the table of when people are given
# particular things.

# watch out for this: previously one blooming person was admitted in the grey zone of changing time zones! 
# Just give them an extra 15 minutes...

# dat$admission_time[dat$admission_date == "2022-03-27" & dat$admission_time == "01:50:00"] <- "02:05:00"


dat <- dat %>% mutate(arrival_DT = as.POSIXct(paste(arrival_date, arrival_time), format="%Y-%m-%d %H:%M"), # no ':%S' required this time
                      admission_DT = as.POSIXct(paste(admission_date, admission_time), format="%Y-%m-%d %H:%M"),
                      RSR_DT = as.POSIXct(paste(RSR_date, RSR_time), format="%Y-%m-%d %H:%M"),
                      NIV_DT = as.POSIXct(paste(NIV_date, NIV_time), 
                                                     format="%Y-%m-%d %H:%M"),
                      AHVF_cont_DT = as.POSIXct(paste(AHVF_cont_date, AHVF_cont_time), 
                                          format="%Y-%m-%d %H:%M"))

summary(dat$AHVF_cont_DT)


head(dat$arrival_DT)
head(dat$NIV_DT)
summary(dat$NIV_DT)
summary(dat$RSR_DT)
summary(dat$admission_DT)

# Life status
summary(dat$life_status)


# Time to admission in hours

dat <- dat %>% mutate(arrival_to_admission_hours = difftime(admission_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_admission_hours)
dat$arrival_to_admission_hours <- as.numeric(dat$arrival_to_admission_hours)

summary(dat$arrival_to_admission_hours)


# Time to NIV in hours

dat <- dat %>% mutate(arrival_to_NIV_hours = difftime(NIV_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_NIV_hours)
dat$arrival_to_NIV_hours <- as.numeric(dat$arrival_to_NIV_hours)

# For the ridiculous ones, it's because the years were not applied properly for AHVF
# take the year that they were admitted 


dat <- dat %>% mutate(check_AHVF = as.numeric(round(difftime(AHVF_cont_DT, arrival_DT, units = "hours"), 2)))

dat %>% filter(check_AHVF < -24) %>% select(arrival_DT, arrival_date, AHVF_cont_DT, AHVF_cont_date)

# Pretty much all of these rows are due to errors in the AHVF date, either the month or the year,
# or rarely the day.
# therefore, change them.

summary(dat$AHVF_cont_DT)
summary(dat$check_AHVF)

dat <- dat %>% mutate(AHVF_cont_date = as.Date(ifelse(check_AHVF >= -24 | is.na(check_AHVF), 
                                              AHVF_cont_date,
                                              arrival_date)),
                      AHVF_cont_DT = as.POSIXct(paste(AHVF_cont_date, AHVF_cont_time), 
                                                format="%Y-%m-%d %H:%M"),
                      check_AHVF = NULL)


# then, calculate the AHVF to NIV.
# if AHVF was before arrival (but not an error), take the time of arrival instead as hospitals
# can't be expected to be judged on quality of care before the patient even arrives

dat <- dat %>% mutate(AHVF_cont_to_NIV_hours = ifelse(AHVF_cont_DT < arrival_DT, 
                                                      as.numeric(difftime(NIV_DT, arrival_DT, units = "hours")),
                                                      as.numeric(difftime(NIV_DT, AHVF_cont_DT, units = "hours"))))



dat %>% filter(AHVF_cont_DT < arrival_DT) %>% nrow()
dat %>% filter(AHVF_cont_DT >= arrival_DT) %>% nrow()
dat %>% filter(AHVF_cont_DT < arrival_DT) %>% select(NIV) %>% table()



dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_DT)) %>% nrow()
dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_DT)) %>% nrow()
dat %>% filter(NIV == "No") %>% filter(!is.na(NIV_DT)) %>% nrow()

dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_date) | is.na(NIV_time)) %>% nrow()

dat %>% filter(NIV == "Yes") %>% filter(NIV_date_NR == "Not recorded" | NIV_time_NR == "Not recorded") %>% nrow()

# No discrepancies there.



# create a variable for <2 hours, 2-24 hours, +24 hours, and NR

dat %>% filter(NIV_DT < AHVF_cont_DT) %>% nrow()
dat %>% filter(NIV_DT >= AHVF_cont_DT) %>% nrow()
dat %>% filter(NIV_DT >= AHVF_cont_DT) %>% nrow()

dat %>% select(AHVF_ever, AHVF_cont, NIV) %>% summary()
dat %>% select(AHVF_cont, NIV) %>% table(useNA = "ifany")

dat %>% filter(is.na(NIV))

# we don't need arrival to NIV any more - use AHVF

summary(dat$AHVF_cont_to_NIV_hours)

dat$AHVF_cont_to_NIV_cat <- NA
dat$AHVF_cont_to_NIV_cat[dat$AHVF_cont_to_NIV_hours <2] <- "<2 hours"
# dat$AHVF_cont_to_NIV_cat[dat$AHVF_cont_to_NIV_hours <0] <- "Given before continued AHVF recording"
dat$AHVF_cont_to_NIV_cat[dat$AHVF_cont_to_NIV_hours >=2 & dat$AHVF_cont_to_NIV_hours < 6] <- "2-6 hours"
dat$AHVF_cont_to_NIV_cat[dat$AHVF_cont_to_NIV_hours >= 6] <- "6+ hours"
dat$AHVF_cont_to_NIV_cat[(dat$NIV_date_NR == "Not recorded" | dat$NIV_time_NR == "Not recorded") & 
                           dat$AHVF_cont == "Yes"] <- "NIV given but date or time not recorded"
dat$AHVF_cont_to_NIV_cat[dat$AHVF_cont == "Yes" & dat$NIV == "No"] <- "Not given NIV"
dat$AHVF_cont_to_NIV_cat <- factor(dat$AHVF_cont_to_NIV_cat, levels = c("<2 hours", "2-6 hours", "6+ hours", 
                                                                    "NIV given but date or time not recorded",
                                                                    "Not given NIV"))

dat$NIV_in_AHVF_cont <- dat$NIV
dat$NIV_in_AHVF_cont[dat$AHVF_cont == "No" | dat$AHVF_ever == "No"] <- NA

summary(dat$AHVF_cont)
summary(dat$AHVF_ever)

summary(dat$AHVF_cont_to_NIV_cat)
table(dat$AHVF_cont_to_NIV_cat, dat$NIV_in_AHVF_cont, useNA = "ifany")

table(dat$AHVF_cont_to_NIV_cat, dat$life_status)
prop.table(table(dat$AHVF_cont_to_NIV_cat, dat$life_status), 2)

dat$NIV_timings <- "Not given NIV"
dat$NIV_timings[dat$AHVF_cont_to_NIV_cat == "NIV given but date or time not recorded"] <- "NIV given but date or time not recorded"
dat$NIV_timings[dat$NIV_DT < dat$AHVF_cont_DT] <- "NIV given before cont AHVF"
dat$NIV_timings[dat$NIV_DT >= dat$AHVF_cont_DT] <- "NIV given after cont AHVF"
dat$NIV_timings <- factor(dat$NIV_timings)

table(dat$NIV_timings, dat$AHVF_ever, useNA = "ifany")
table(dat$NIV_timings, dat$AHVF_cont, useNA = "ifany")

dat %>% filter(NIV_timings == "NIV given after cont AHVF" & AHVF_ever == "No")

summary(dat$AHVF_cont_to_NIV_cat)
summary(dat$AHVF_cont_to_NIV_hours)
summary(dat$AHVF_cont_to_NIV_hours)

dat$arrival_time_mins_from_midnight

dat %>% select(AHVF_cont_to_NIV_cat, NIV) %>% table(useNA = "ifany")


dat %>% select(NIV_date_NR, NIV) %>% table(useNA = "ifany")
dat %>% select(NIV_time_NR, NIV) %>% table(useNA = "ifany")


# Time to NIV in hours

dat <- dat %>% mutate(arrival_to_NIV_hours = difftime(NIV_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_NIV_hours)
dat$arrival_to_NIV_hours <- as.numeric(dat$arrival_to_NIV_hours)

summary(dat$arrival_to_NIV_hours)

dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_DT)) %>% nrow()
dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_DT)) %>% nrow()
dat %>% filter(NIV == "No") %>% filter(!is.na(NIV_DT)) %>% nrow()

dat %>% filter(NIV == "Yes") %>% filter(is.na(NIV_date) | is.na(NIV_time)) %>% nrow()

dat %>% filter(NIV == "Yes") %>% filter(NIV_date_NR == "Not recorded" | NIV_time_NR == "Not recorded") %>% nrow()

# No descrepancies there.

# create a variable for <2 hours, 2-24 hours, +24 hours, and NR

dat$arrival_to_NIV_cat <- NA
dat$arrival_to_NIV_cat[dat$arrival_to_NIV_hours <2] <- "<2 hours"
dat$arrival_to_NIV_cat[dat$arrival_to_NIV_hours >=2 & dat$arrival_to_NIV_hours < 24] <- "2-24 hours"
dat$arrival_to_NIV_cat[dat$arrival_to_NIV_hours >= 24] <- "24+ hours"
dat$arrival_to_NIV_cat[dat$NIV_date_NR == "Not recorded" | dat$NIV_time_NR == "Not recorded"] <- "NIV given but date or time not recorded"
dat$arrival_to_NIV_cat <- factor(dat$arrival_to_NIV_cat, levels = c("<2 hours", "2-24 hours", "24+ hours", 
                                                                    "NIV given but date or time not recorded"))
summary(dat$arrival_to_NIV_cat)
summary(dat$arrival_to_NIV_hours)
summary(dat$arrival_to_NIV_hours)

dat %>% select(arrival_to_NIV_cat, NIV) %>% table(useNA = "ifany")

dat %>% select(NIV_date_NR, NIV) %>% table(useNA = "ifany")
dat %>% select(NIV_time_NR, NIV) %>% table(useNA = "ifany")


# FEV1 sort out

dat$FEV1_perc_pred <- "No"
dat$FEV1_perc_pred[!is.na(dat$FEV1_perc_pred_value)] <- "Yes"
dat$FEV1_perc_pred <- factor(dat$FEV1_perc_pred, levels = c("No", "Yes"))

summary(dat$FEV1_perc_pred)
dat %>% select(FEV1_perc_pred, FEV1_perc_pred_date_NR) %>% table(useNA = "ifany")
dat$FEV1_perc_pred_date_NR[dat$FEV1_perc_pred == "No"] <- NA
dat %>% select(FEV1_perc_pred, FEV1_perc_pred_date_NR) %>% table(useNA = "ifany")
dat %>% filter(!is.na(FEV1_perc_pred_date)) %>% filter(FEV1_perc_pred_date_NR == "Not recorded") %>% nrow()



# FEV1FVC sort out

dat$FEV1FVC <- "No"
dat$FEV1FVC[!is.na(dat$FEV1FVC_value)] <- "Yes"
dat$FEV1FVC <- factor(dat$FEV1FVC, levels = c("No", "Yes"))

summary(dat$FEV1FVC)
dat %>% select(FEV1FVC, FEV1FVC_date_NR) %>% table(useNA = "ifany")
dat$FEV1FVC_date_NR[dat$FEV1FVC == "No"] <- NA
dat %>% select(FEV1FVC, FEV1FVC_date_NR) %>% table(useNA = "ifany")
dat %>% filter(!is.na(FEV1FVC_date)) %>% filter(FEV1FVC_date_NR == "Not recorded") %>% nrow()



# any spirometry available


dat$spirometry <- "No"
dat$spirometry[!is.na(dat$FEV1_perc_pred_value) | !is.na(dat$FEV1FVC_value)] <- "Yes"
dat$spirometry <- factor(dat$spirometry, levels = c("No", "Yes"))
summary(dat$spirometry)

dat$obstruction <- NA
dat$obstruction[dat$FEV1FVC_value < 0.7] <- "Yes (<0.7)"
dat$obstruction[dat$FEV1FVC_value >= 0.7] <- "No (>=0.7)"
dat$obstruction <- factor(dat$obstruction, levels = c("No (>=0.7)", "Yes (<0.7)"))




summary(dat$obstruction)


dat %>% filter(FEV1_perc_pred_date > arrival_date) %>% nrow()
dat %>% filter(FEV1_perc_pred_date < arrival_date) %>% nrow()
dat %>% filter(FEV1FVC_date > arrival_date) %>% nrow()
dat %>% filter(FEV1FVC_date < arrival_date) %>% nrow()

# length of stay days

dat <- dat %>% mutate(LOS_days = discharge_date - arrival_date)

head(sort(dat$LOS_days))


# admission to RSR in hours

dat <- dat %>% mutate(admission_to_RSR_hours = difftime(RSR_DT, admission_DT, units = "hours"))
head(dat$admission_to_RSR_hours)
dat$admission_to_RSR_hours <- as.numeric(dat$admission_to_RSR_hours)


# arrival to RSR in hours

dat <- dat %>% mutate(arrival_to_RSR_hours = difftime(RSR_DT, arrival_DT, units = "hours"))
head(dat$arrival_to_RSR_hours)
dat$arrival_to_RSR_hours <- as.numeric(dat$arrival_to_RSR_hours)

dat %>% select(arrival_to_RSR_hours, admission_to_RSR_hours) %>% head(20)

summary(dat$arrival_to_admission_hours)

dat <- dat %>% mutate(admission_to_RSR_24hour = factor(ifelse(RSR == "No", "Not received", 
                                                 ifelse(admission_to_RSR_hours >= 24, ">=24 hours", "<24 hours")),
                                          levels = c("<24 hours", ">=24 hours", "Not received")))
summary(dat$admission_to_RSR_24hour)


# Need to remove those who died from this

dat <- dat %>% mutate(LOS_days = ifelse(life_status == "Died as inpatient", 
                                         NA, LOS_days))

summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)

dat %>% select(oxygen_admin, oxygen_prescribed) %>% table()

table(dat$oxygen_target_range, dat$oxygen_prescribed, useNA = "ifany")

# For oxygen administered, we want the denominator to be oxygen prescribed.

summary(dat$oxygen_admin)
summary(dat$oxygen_prescribed)

table(dat$oxygen_admin, dat$oxygen_prescribed)

table(dat$oxygen_target_range, useNA = "ifany")
table(dat$oxygen_target_range, dat$oxygen_prescribed, useNA = "ifany")

# it shouldn't be possible to mark 'no' for oxygen prescribed and to provide a target range - 
# remove

dat %>% filter(oxygen_prescribed == "No" & !is.na(oxygen_target_range)) %>% nrow()

dat <- dat %>% filter(!(oxygen_prescribed == "No" & !is.na(oxygen_target_range))) 


# New variable: oxygen prescribed in those who were administered

dat$oxygen_prescribed_if_administered <- as.character(dat$oxygen_prescribed)
dat$oxygen_prescribed_if_administered[dat$oxygen_admin == "No"] <- NA
dat$oxygen_prescribed_if_administered <- factor(dat$oxygen_prescribed_if_administered, levels = c("Yes", "No"))
summary(dat$oxygen_prescribed_if_administered)
summary(dat$oxygen_prescribed)
summary(dat$oxygen_admin)

# # # # 

# # # # NOTE THIS HAS CHANGED NOW THAT DAYTIME/NIGHT TIME AUTOMATICALLY INCLUDED.

# also here, we split into daytime and night time

# discharge weekday/weekend daytime/night time.




# # # # # #

summary(dat$NIV)
summary(dat$NIV_date)
summary(dat$NIV_date_NR)
summary(dat$NIV_time)
summary(dat$NIV_time_NR)

summary(dat$NIV_DT)



# day of arrival and day of discharge


dat <- dat %>% mutate(arrival_day_of_week = weekdays(arrival_date, abbreviate = FALSE))

dat$arrival_day_of_week <- ordered(dat$arrival_day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

table(dat$arrival_day_of_week)


dat <- dat %>% mutate(discharge_day_of_week = weekdays(discharge_date, abbreviate = FALSE))

str(dat$discharge_day_of_week)

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday")] <- "Weekday"

dat$discharge_day_of_week[dat$discharge_day_of_week %in% c("Saturday", "Sunday")] <- "Weekend" 

dat$discharge_day_of_week <- factor(dat$discharge_day_of_week, levels=c("Weekday", "Weekend"))




# Those who transferred need to be removed from the discharge day of week

# Using ifelse here changes them to factor levels so I'm doing it the old school method

# dat <- dat %>% mutate(discharge_day_of_week = ifelse(discharge_bundle == "Patient transferred to another hospital", 
#                                          NA, discharge_day_of_week))



dat$discharge_day_of_week[dat$life_status == "Died as inpatient"] <- NA

summary(dat$life_status)
summary(dat$discharge_bundle)
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



# Discharge bundle consistency check

dat %>% select(discharge_bundle, DB_none) %>% table(useNA = "ifany") 
table(dat$DB_none)

# 144 people received a discharge bundle but were marked as 'none' for each element.



dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")),
                         .funs = ~ifelse(life_status == "Died as inpatient", NA, .))

# Going to save a copy of this original variable (including those who are not recorded) and will split into smokers and vapers

dat <- dat %>% mutate(DB_smoke_NR_included = DB_smoke)



dat$DB_smoke[dat$smoke_tobacco != "Current"] <- NA




summary(dat$discharge_bundle)

dat$discharge_bundle <- factor(dat$discharge_bundle, levels = c("Yes", "No", "Self discharge"))

dat$discharge_day_of_week[dat$life_status != "Alive"] <- NA
summary(dat$discharge_day_of_week)



# Create a variable indicating whether NEWS2 had been recorded, calculated, or is not available.

dat <- dat %>% mutate(NEWS2 = ifelse(NEWS2_recorded_score_only == "Score not available", 
                      "Score not recorded or able to be calculated", ifelse(NEWS2_recorded_score_only == "Calculate score",
                                                "Score calculated post-hoc using physiological variables",
                                                "Score recorded in hospital notes")))

dat$NEWS2 <- factor(dat$NEWS2, levels = c("Score recorded in hospital notes", 
                                          "Score calculated post-hoc using physiological variables",
                                          "Score not recorded or able to be calculated"))
                                          

# Extra variables:


# CHECK HERE THAT THE NAME OF THIS VALUE HAS NOT CHANGED
# Luckily this will eventually throw up an error anyway if not recoded to missing

dat$NEWS2_score[dat$NEWS2_score == "Score not available"] <- NA
dat$NEWS2_score <- as.integer(dat$NEWS2_score)


dat$NEWS2_score_sev[dat$NEWS2 == "Score not recorded or able to be calculated"] <- 
  "Score not recorded or able to be calculated"
dat$NEWS2_score_sev[dat$NEWS2_score < 5] <- "Low/low-medium"
dat$NEWS2_score_sev[dat$NEWS2_score %in% c(5, 6)] <- "Medium"
dat$NEWS2_score_sev[dat$NEWS2_score > 6] <- "High"

dat$NEWS2_score_sev <- factor(dat$NEWS2_score_sev, 
                              levels = c("Low/low-medium", "Medium", "High", 
                                         "Score not recorded or able to be calculated"))
table(dat$NEWS2_score_sev, dat$life_status)


# Filter out invalid dates (N = 0)

dat %>% filter(discharge_date < "2023-04-01") %>% nrow()
dat %>% filter(discharge_date > "2024-03-31") %>% nrow()


summary(dat$discharge_date)
summary(dat$arrival_date)

dat %>% filter(arrival_date < "2023-04-01") %>%
  select(arrival_date, discharge_date) %>% arrange(arrival_date) %>% head(20)

summary(dat$admission_date)
head(sort(dat$arrival_date), 20)
head(sort(dat$admission_date), 20)

# Was anyone discharged before they arrived, or received medication before they arrived, or received medication
# after they left? No.


# This many people have an admission date before they arrive:
dat %>% filter(admission_date - arrival_date < 0) %>% nrow()

# dat <- dat %>% filter(!(admission_date - arrival_date < 0))

# This many people have an NIV date before they arrived:
dat %>% filter(NIV_DT - arrival_DT < 0) %>% nrow()

# This many people have an RSR date before they arrived:
dat %>% filter(RSR_date - arrival_date < 0) %>% nrow()

# This many people have a discharge date before they arrived: 
dat %>% filter(discharge_date - arrival_date < 0) %>% nrow()

# dat <- dat %>% filter(!(discharge_date - arrival_date < 0))

# people missing essential variables:

# this many people are missing an essential variable:
dat %>% filter(is.na(NIV)) %>% nrow()

dat <- dat %>% filter(!is.na(NIV))


# who has not received AHVF but still has a date?
dat %>% filter(AHVF_ever == "No" & !is.na(AHVF_cont_date)) %>% nrow()

# inconsistent so remove
dat <- dat %>% filter(!(AHVF_ever == "No" & !is.na(AHVF_cont_date)))


dat %>% select(ends_with("DT")) %>% summary()

nrow(dat)


# inconsistent timings

dat %>% filter(arrival_date > discharge_date) %>% nrow()

  
dat %>% filter(admission_DT < arrival_DT) %>% nrow()
dat %>% filter(RSR_DT < arrival_DT) %>% nrow()
dat %>% filter(NIV_DT < arrival_DT) %>% nrow()
dat %>% filter(AHVF_cont_DT < arrival_DT) %>% nrow() # this is okay though

dat %>% filter(admission_date > discharge_date) %>% nrow()
dat %>% filter(RSR_date > discharge_date) %>% nrow()
dat %>% filter(NIV_date > discharge_date) %>% nrow()
dat %>% filter(AHVF_cont_date > discharge_date) %>% nrow() 



# remove those who are too young to be included:

dat %>% filter(age < 35) %>% nrow()

# remove duplicates

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


# # and do the arrival times
# 
# dat$arrival8hourtimes <- cut(dat$arrival_time_mins_from_midnight, breaks = seq(-0.5, 1439.5, 480),
#                              labels = paste0("lessthan", seq(8, 24, 8)))
# 
# dat$weekday_8hours <- interaction(dat$arrival_day_of_week, dat$arrival8hourtimes, sep = "_")
# 
# # and the weekday arrivals
# 
# summary(dat$weekday_weekend_admission)



########################################
# BPT: 
########################################


dat$RSR_24hour_BPT <- NA
dat$RSR_24hour_BPT[dat$admission_to_RSR_24hour == "<24 hours"] <- 1
dat$RSR_24hour_BPT[dat$admission_to_RSR_24hour %in% c(">=24 hours", "Not received")] <- 0

table(dat$RSR_24hour_BPT, dat$admission_to_RSR_24hour)
dat %>% select(starts_with("DB")) %>% colnames()

# create a BPT variable


# create the combo DB variables

dat$DB_plan_or_pack_or_unsuitable <- 0
dat$DB_plan_or_pack_or_unsuitable[dat$DB_plan == 1 | dat$DB_pack == 1 | dat$DB_unsuitable_for_pack == 1] <- 1
dat$DB_plan_or_pack_or_unsuitable[is.na(dat$DB_plan)] <- NA


dat %>% select(starts_with("DB_")) %>% summary()

dat <- dat %>% mutate(DB_BPT = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
                                               DB_plan == 1 & (DB_pack == 1 | DB_unsuitable_for_pack == 1) &
                                               (DB_smoke == 1 | is.na(DB_smoke)) & DB_PR == 1 &
                                               DB_FU_72hour == 1, 1, 0))
dat$DB_BPT[dat$life_status == "Died"] <- NA


dat <- dat %>% mutate(old_KPI_DB = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
                                                (DB_pack == 1 | DB_unsuitable_for_pack == 1 | DB_plan == 1) &
                                                DB_PR == 1 &
                                               DB_FU_72hour == 1, 1, 0))
dat$old_KPI_DB[dat$life_status == "Died"] <- NA

dat %>% select(DB_BPT, old_KPI_DB) %>% table(useNA = "ifany")


dat <- dat %>% mutate(DB_elements_and_RSR_24hour_BPT = ifelse(RSR_24hour_BPT == 1 & DB_BPT == 1,
                                   1, 0))
dat$DB_elements_and_RSR_24hour_BPT[dat$life_status != "Alive"] <- NA

dat <- dat %>% mutate(DB_ticked_and_RSR_24hour_BPT = ifelse(RSR_24hour_BPT == 1 & 
                                                              discharge_bundle == "Yes",
                                                              1, 0))
dat$DB_ticked_and_RSR_24hour_BPT[dat$life_status != "Alive"] <- NA


dat %>% select(DB_elements_and_RSR_24hour_BPT, DB_ticked_and_RSR_24hour_BPT) %>% table(useNA = "ifany")

table(dat$discharge_bundle, useNA = "ifany")

# dat %>% select(starts_with("DB_")) %>% summary()
# 
# dat <- dat %>% mutate(BPT_DB1 = factor(ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
#                                                DB_plan == 1 & (DB_pack == 1 | DB_unsuitable_for_pack == 1) &
#                                                (DB_smoke == 1 | is.na(DB_smoke)) & DB_PR == 1 &
#                                                DB_FU_72hour == 1 & discharge_bundle == "Yes", "Achieved", "Not achieved")))
# dat$BPT_DB1[dat$life_status == "Died"] <- NA
# 
# dat <- dat %>% mutate(BPT_DB2 = factor(ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
#                                                DB_plan == 1 & (DB_pack == 1 | DB_unsuitable_for_pack == 1) &
#                                                (DB_smoke == 1 | is.na(DB_smoke)) & DB_PR == 1 &
#                                                DB_FU_72hour == 1, "Achieved", "Not achieved")))
# dat$BPT_DB2[dat$life_status == "Died"] <- NA
# 
# table(dat$BPT_DB1, dat$BPT_DB2, dat$discharge_bundle)


dat %>% select(starts_with("DB")) %>% summary()
dat %>% select(ends_with("BPT")) %>% summary()


dat$arrival8hourtimes <- cut(dat$arrival_time_mins_from_midnight, breaks = seq(-0.5, 1439.5, 480),
                             labels = paste0("lessthan", seq(8, 24, 8)))

dat$weekday_8hours <- interaction(dat$arrival_day_of_week, dat$arrival8hourtimes, sep = "_")


table(dat$arrival_to_NIV_cat)

dat <- dat %>% mutate(old_KPI_arrival_to_NIV = ifelse(arrival_to_NIV_cat == "<2 hours", 1, 0))

dat %>% select(old_KPI_arrival_to_NIV, arrival_to_NIV_cat) %>% table(useNA = "ifany")


saveRDS(dat, "C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Data/tidyData/COPD_SCC_2023-2024_clean_data.RDS")

