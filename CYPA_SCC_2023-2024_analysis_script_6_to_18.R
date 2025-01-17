#                                                             #
# CYPA SCC 2023-24 analysisript                               #
#                                                             #
# Author: Alex Adamson                                        #
# Date created:  31st May 2024                                #
#                                                             #




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
  
  
  # scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  # SN <- sum(scot, na.rm = TRUE)
  # scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  # scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- sum(all, na.rm = TRUE)
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 0)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  # ret <- matrix(c(varname, all, eng, scot, wal), nrow = 1, ncol = 5)
  ret <- matrix(c(varname, all, eng, wal), nrow = 1, ncol = 4)
  
  
  # colnames(ret) <- c("Variable", 
  #                          paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                          paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  colnames(ret) <- c("Variable", 
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  # 
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
  # 
  ret <- as.data.frame(ret)
  
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

mediSumRound <- function(x, variable, roundno = 0) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable) %>% 
    dplyr::select(vars, N, median, lo.quart, hi.quart)
  # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno)) # otherwise use 'roundno'
  
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  
  #   if(nrow(gen) == 0) {return(var_N)}
  
  #  else {
  
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
  
  # }
}




makeFlatNPercInf <- function(subana.N, varname = NULL) {
  
  
  colnames(subana.N) <- gsub(" ", "_", colnames(subana.N))
  rownames(subana.N) <- gsub(" ", "_", rownames(subana.N))
  subana.perc <- round(prop.table(subana.N, 1)*100, 1)  # create the prop table
  # totals <- matrix(margin.table(subana.N, 2), nrow = 1, ncol = 2)
  # colnames(totals) <- paste0(colnames(subana.N), "_N")
  
  
  subana.N_flat <- matrix(subana.N, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.N)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.N)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_n")
  
  colnames(subana.N_flat) <- cols
  subana.N_flat <- as.data.frame(subana.N_flat)
  
  subana.perc_flat <- matrix(subana.perc, nrow = 1, ncol = ncol(subana.N)*nrow(subana.N), byrow = FALSE)
  cols <- paste(rep(colnames(subana.perc)[1:ncol(subana.N)], each = nrow(subana.N)),
                rownames(subana.perc)[1:nrow(subana.N)], sep = "_with_")
  cols <- paste0(cols, "_perc")
  
  colnames(subana.perc_flat) <- cols
  subana.perc_flat <- as.data.frame(subana.perc_flat)
  
  # subana.flat <- cbind(totals, subana.N_flat, subana.perc_flat)
  subana.flat <- cbind(subana.N_flat, subana.perc_flat)
  
  if (!is.null(varname)) {
    
    colnames(subana.flat) <- paste(varname, colnames(subana.flat), sep = "_")
  }
  
  return(subana.flat)
}





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                       # 
#  NEED TO RUN THREE SCRIPTS FOR DIFFERENT AGE BRACKETS #
#                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #








# Okay let's go!


dat <- readRDS("G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Data/tidyData/CYPA_SCC_2023-24_clean_data.RDS")

dat <- dat %>% filter(age > 5)


# Need the median number of hospital admissions for this one, which requires a seperate dataframe:

admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])




dat$arrival8hourtimes <- cut(dat$arrival_time_mins_from_midnight, breaks = seq(-0.5, 1439.5, 480),
                             labels = paste0("lessthan", seq(8, 24, 8)))

summary(dat$arrival8hourtimes)
# Day of the week N
admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
sum(admisstimedow.N)

admisstimedow.N.all <- admisstimedow.N

# Day of the week %
admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)

summary(admisstimedow.perc)






# START OF CSV WRITING


# First up, need to make sure all those binary variables that are currently
# coded as numeric are classed as factors. I think this is just for the IV and DB variables.
# Now also impairments


dat <- dat %>% mutate_at(.vars = vars(starts_with("DB")), 
                         .funs = ~factor(., levels = c("0", "1"))) #%>% str()

dat <- dat %>% mutate_at(.vars = vars(starts_with("IV")), 
                         .funs = ~factor(., levels = c("0", "1"))) #%>% str()

dat <- dat %>% mutate_at(.vars = vars(starts_with("impairments")), 
                         .funs = ~factor(., levels = c("0", "1"))) #%>% str()


# Now we should be fine to get on with what we're doing.

# Second, create our 'psychic' data frame for the medians

psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)

# We need to create a new row in psychic for the admissions IQR.

admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                   mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                   sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                   se = NA,
                                   lo.quart = round(quantile(admissmeds$admisscount, 
                                                    probs = 0.25, na.rm = TRUE), 0),
                                   median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                   hi.quart = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.75, na.rm = TRUE), 0))

row.names(admissmedsforpsychic) <- "admissions"

psychic <- rbind(psychic, admissmedsforpsychic)


# makeFlatNPercInf(testtable)



flat <- data.frame(country = "All")

flat <- cbind(flat,
              
             mediSumRound(dat, "age", 0),
              FreqSum(dat, "gender"),
             FreqSum(dat, "ethnicity"),
             FreqSum(dat, "IMD_quintile"),
              mediSumRound(dat, "admissions", 0))


# Now create the 2 hour table and bind it in

admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                rownames(admisstimedow.N)[1:3], "arrival_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
# bind this

flat <- cbind(flat, admisstime_flat)


admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                    rownames(admisstimedow.perc)[1:3], "arrival_perc", sep = "_")

colnames(admisstime_flat_perc) <- colsssperc
admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)


flat <- cbind(flat, admisstime_flat_perc)

# admisstimedow.N.all <- admisstimedow.N


# bind these below
# flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
# flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
# flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
# flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
# flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
# flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
# flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]

# Then carry on as normal:
       
              
         
flat <- cbind(flat,
            #  FreqSum(dat, "ambulance"), # removed from 2023-24 audit
            #  FreqSum(dat, "smoke_status"), # removed from 2023-24 audit. Replaced with 
            # various smoking variables below
            FreqSum(dat, "weekday_weekend_arrival"),
            FreqSum(dat, "impairments_none"), # 2023-24 impairments added
            FreqSum(dat, "impairments_anxiety"),
            FreqSum(dat, "impairments_depression"),
            FreqSum(dat, "impairments_severe_mental_illness"),
            FreqSum(dat, "impairments_self_harm"),
            FreqSum(dat, "impairments_eating_disorder"),
            FreqSum(dat, "impairments_MHS"),
            FreqSum(dat, "impairments_other"),
            FreqSum(dat, "impairments_NR"),
            FreqSum(dat, "smoke_tobacco"),
            FreqSum(dat, "smoke_shisha"),
            FreqSum(dat, "smoke_cannabis"),
            FreqSum(dat, "smoke_other"),
            FreqSum(dat, "vaping_status"),
              FreqSum(dat, "SH_smoke"),
              mediSumRound(dat, "LOS_hours", 0),
              FreqSum(dat, "life_status"),
            #  mediSumRound(dat, "heart_rate", 0),
            #  mediSumRound(dat, "resp_rate", 0),
              FreqSum(dat, "oxygen_sat_recorded"),
            #  mediSumRound(dat, "oxygen_sat_value", 0),
            #  FreqSum(dat, "oxygen_sat92"),
              FreqSum(dat, "oxygen_sat_measurement_type"),
         #     FreqSum(dat, "oxygen_supp_hypoxic_only"),
            #  mediSumRound(dat, "PEF_init_value", 0),
              FreqSum(dat, "PEF_init_recorded"),
            #  mediSumRound(dat, "PEF_prev_value", 0),
              FreqSum(dat, "PEF_prev_recorded"),
            #  mediSumRound(dat, "PEF_predict_value", 0),
              FreqSum(dat, "PEF_predict_recorded"),
            #  mediSumRound(dat, "PEF_percent_pred", 0),
            #  FreqSum(dat, "PEF_percpred_75"),
              FreqSum(dat, "asthma_sev"),
              FreqSum(dat, "RSR"),
         FreqSum(dat, "RSR_if_severe_or_LT"),
         FreqSum(dat, "steroids_admin_or_24hr_prev"),
                       FreqSum(dat, "steroids_24hr_prev"),
              mediSumRound(dat, "arrival_to_steroids_hours", 0),
              FreqSum(dat, "steroids_1hour"))
        #      FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
        #      FreqSum(dat, "steroids_1hour_alt_6_plus_years"))

flat <- cbind(flat,
              FreqSum(dat, "b2a_1hr_prev"),
              FreqSum(dat, "b2a_admin_or_1hr_prev"),
              mediSumRound(dat, "arrival_to_b2a_minutes", 0),
              FreqSum(dat, "b2a_1hour"),
              FreqSum(dat, "IV_med_aminophylline"),
              FreqSum(dat, "IV_med_ketamine"),
              FreqSum(dat, "IV_med_mag_sulphate"),
              FreqSum(dat, "IV_med_b2a"),
              FreqSum(dat, "IV_med_none"),
              
              FreqSum(dat, "crit_care_total"),
              
              
              FreqSum(dat, "discharge_bundle"),
            #  FreqSum(dat, "transferred"),
            FreqSum(dat, "weekday_weekend_discharge"),
            makeFlatNPercInf(table(dat$weekday_weekend_discharge, dat$discharge_bundle), varname = "discharge_bundle"),
            
            #   makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
              
              FreqSum(dat, "DB_inhaler"),
              FreqSum(dat, "DB_maintenance"),
              FreqSum(dat, "DB_adherence"),
              FreqSum(dat, "DB_PAAP"),
              FreqSum(dat, "DB_triggers"),
              
              # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
              
              FreqSum(dat, "DB_smoke"),
              FreqSum(dat, "DB_parent_smoke"),
              FreqSum(dat, "DB_comm_FU_2_days"),
              FreqSum(dat, "DB_asthma_clinic_4_weeks"),
            FreqSum(dat, "DB_FU_any"),
            FreqSum(dat, "DB_comm_FU_2_days_if_severe_or_LT"),
            
           #   FreqSum(dat, "DB_vape_cess"),
              FreqSum(dat,"DB_none"),
           
              FreqSum(dat, "inhaled_steroids_dis"),
#               FreqSum(dat, "oral_steroids_dis"),
              FreqSum(dat, "oral_steroids_rescue_history"),
FreqSum(dat, "referred_for_FU"), 
FreqSum(dat, "DB_selected_elements"))

flat.all <- flat
dat.save <- dat



for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)

  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  
  # makeFlatNPercInf(testtable)
  
  
  
  flat <- data.frame(country = i)
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", 0),
                FreqSum(dat, "gender"),
                FreqSum(dat, "ethnicity"),
                FreqSum(dat, "IMD_quintile"),
                mediSumRound(dat, "admissions", 0))
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                  rownames(admisstimedow.N)[1:3], "arrival_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                      rownames(admisstimedow.perc)[1:3], "arrival_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # Then carry on as normal:
  
  
  
  flat <- cbind(flat,
                #  FreqSum(dat, "ambulance"), # removed from 2023-24 audit
                #  FreqSum(dat, "smoke_status"), # removed from 2023-24 audit. Replaced with 
                # various smoking variables below
                FreqSum(dat, "weekday_weekend_arrival"),
                FreqSum(dat, "impairments_none"), # 2023-24 impairments added
                FreqSum(dat, "impairments_anxiety"),
                FreqSum(dat, "impairments_depression"),
                FreqSum(dat, "impairments_severe_mental_illness"),
                FreqSum(dat, "impairments_self_harm"),
                FreqSum(dat, "impairments_eating_disorder"),
                FreqSum(dat, "impairments_MHS"),
                FreqSum(dat, "impairments_other"),
                FreqSum(dat, "impairments_NR"),
                FreqSum(dat, "smoke_tobacco"),
                FreqSum(dat, "smoke_shisha"),
                FreqSum(dat, "smoke_cannabis"),
                FreqSum(dat, "smoke_other"),
                FreqSum(dat, "vaping_status"),
                FreqSum(dat, "SH_smoke"),
                mediSumRound(dat, "LOS_hours", 0),
                FreqSum(dat, "life_status"),
                #  mediSumRound(dat, "heart_rate", 0),
                #  mediSumRound(dat, "resp_rate", 0),
                FreqSum(dat, "oxygen_sat_recorded"),
                #  mediSumRound(dat, "oxygen_sat_value", 0),
                #  FreqSum(dat, "oxygen_sat92"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                #     FreqSum(dat, "oxygen_supp_hypoxic_only"),
                #  mediSumRound(dat, "PEF_init_value", 0),
                FreqSum(dat, "PEF_init_recorded"),
                #  mediSumRound(dat, "PEF_prev_value", 0),
                FreqSum(dat, "PEF_prev_recorded"),
                #  mediSumRound(dat, "PEF_predict_value", 0),
                FreqSum(dat, "PEF_predict_recorded"),
                #  mediSumRound(dat, "PEF_percent_pred", 0),
                #  FreqSum(dat, "PEF_percpred_75"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "RSR"),
                FreqSum(dat, "RSR_if_severe_or_LT"),
                FreqSum(dat, "steroids_admin_or_24hr_prev"),
                FreqSum(dat, "steroids_24hr_prev"),
                mediSumRound(dat, "arrival_to_steroids_hours", 0),
                FreqSum(dat, "steroids_1hour"))
  #      FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
  #      FreqSum(dat, "steroids_1hour_alt_6_plus_years"))
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_admin_or_1hr_prev"),
                mediSumRound(dat, "arrival_to_b2a_minutes", 0),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "IV_med_aminophylline"),
                FreqSum(dat, "IV_med_ketamine"),
                FreqSum(dat, "IV_med_mag_sulphate"),
                FreqSum(dat, "IV_med_b2a"),
                FreqSum(dat, "IV_med_none"),
                
                FreqSum(dat, "crit_care_total"),
                
                
                FreqSum(dat, "discharge_bundle"),
                #  FreqSum(dat, "transferred"),
                FreqSum(dat, "weekday_weekend_discharge"),
                makeFlatNPercInf(table(dat$weekday_weekend_discharge, dat$discharge_bundle), varname = "discharge_bundle"),
                
                #   makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                
                # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
                
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_parent_smoke"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_asthma_clinic_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_comm_FU_2_days_if_severe_or_LT"),
                
                #   FreqSum(dat, "DB_vape_cess"),
                FreqSum(dat,"DB_none"),
                
                FreqSum(dat, "inhaled_steroids_dis"),
                #               FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                FreqSum(dat, "referred_for_FU"), 
                FreqSum(dat, "DB_selected_elements"))
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save


# # # # # # # # And now we do the site-level csv # # # # # # # # # #

dat$hosp_code

for (i in unique(dat.save$hosp_code)) {
  
  dat <- filter(dat.save, hosp_code == i)

  # From the adult asthma script, let's hope it works
  
  # (From here...)
  
  flat <- data.frame(hosp_code = i)
  flat$hosp_name <- as.character(dat$hosp_name[1])
  flat$trust_code <- as.character(dat$trust_code[1])
  flat$trust_name <- as.character(dat$trust_name[1])
  flat$integrated_care_system <- as.character(dat$integrated_care_system[1])
  flat$region <- as.character(dat$region[1])
  flat$country <- as.character(dat$country[1])
  flat$record_N <- nrow(dat)
  
  # (... to here)
  
    
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR.
  
  admissmeds <- dat %>% group_by(hosp_code) %>% summarise(admisscount = n(), country = head(country)[1])
  admissmedsforpsychic <- data.frame(vars = "admissions", N = nrow(dat), 
                                     mean = mean(admissmeds$admisscount, na.rm = TRUE),
                                     sd = sd(admissmeds$admisscount, na.rm = TRUE),
                                     se = NA,
                                     lo.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.25, na.rm = TRUE), 0),
                                     median = round(quantile(admissmeds$admisscount, 
                                                             probs = 0.5, na.rm = TRUE), 0),
                                     hi.quart = round(quantile(admissmeds$admisscount, 
                                                               probs = 0.75, na.rm = TRUE), 0))
  
  row.names(admissmedsforpsychic) <- "admissions"
  
  psychic <- rbind(psychic, admissmedsforpsychic)
  
  
  # makeFlatNPercInf(testtable)
  
  
  
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", 0),
                FreqSum(dat, "gender"),
                FreqSum(dat, "ethnicity"),
                FreqSum(dat, "IMD_quintile"),
                mediSumRound(dat, "admissions", 0))
  
  
  # Now create the 2 hour table and bind it in
  
  admisstimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(admisstimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 3),
                  rownames(admisstimedow.N)[1:3], "arrival_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  # bind this
  
  flat <- cbind(flat, admisstime_flat)
  
  
  admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)
  rownames(admisstimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  admisstime_flat_perc <- matrix(admisstimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(admisstimedow.perc)[1:7], each = 3),
                      rownames(admisstimedow.perc)[1:3], "arrival_perc", sep = "_")
  
  colnames(admisstime_flat_perc) <- colsssperc
  admisstime_flat_perc <- as.data.frame(admisstime_flat_perc)
  
  
  flat <- cbind(flat, admisstime_flat_perc)
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # Then carry on as normal:
  
  
  
  flat <- cbind(flat,
                #  FreqSum(dat, "ambulance"), # removed from 2023-24 audit
                #  FreqSum(dat, "smoke_status"), # removed from 2023-24 audit. Replaced with 
                # various smoking variables below
                FreqSum(dat, "weekday_weekend_arrival"),
                FreqSum(dat, "impairments_none"), # 2023-24 impairments added
                FreqSum(dat, "impairments_anxiety"),
                FreqSum(dat, "impairments_depression"),
                FreqSum(dat, "impairments_severe_mental_illness"),
                FreqSum(dat, "impairments_self_harm"),
                FreqSum(dat, "impairments_eating_disorder"),
                FreqSum(dat, "impairments_MHS"),
                FreqSum(dat, "impairments_other"),
                FreqSum(dat, "impairments_NR"),
                FreqSum(dat, "smoke_tobacco"),
                FreqSum(dat, "smoke_shisha"),
                FreqSum(dat, "smoke_cannabis"),
                FreqSum(dat, "smoke_other"),
                FreqSum(dat, "vaping_status"),
                FreqSum(dat, "SH_smoke"),
                mediSumRound(dat, "LOS_hours", 0),
                FreqSum(dat, "life_status"),
                #  mediSumRound(dat, "heart_rate", 0),
                #  mediSumRound(dat, "resp_rate", 0),
                FreqSum(dat, "oxygen_sat_recorded"),
                #  mediSumRound(dat, "oxygen_sat_value", 0),
                #  FreqSum(dat, "oxygen_sat92"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                #     FreqSum(dat, "oxygen_supp_hypoxic_only"),
                #  mediSumRound(dat, "PEF_init_value", 0),
                FreqSum(dat, "PEF_init_recorded"),
                #  mediSumRound(dat, "PEF_prev_value", 0),
                FreqSum(dat, "PEF_prev_recorded"),
                #  mediSumRound(dat, "PEF_predict_value", 0),
                FreqSum(dat, "PEF_predict_recorded"),
                #  mediSumRound(dat, "PEF_percent_pred", 0),
                #  FreqSum(dat, "PEF_percpred_75"),
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "RSR"),
                FreqSum(dat, "RSR_if_severe_or_LT"),
                FreqSum(dat, "steroids_admin_or_24hr_prev"),
                FreqSum(dat, "steroids_24hr_prev"),
                mediSumRound(dat, "arrival_to_steroids_hours", 0),
                FreqSum(dat, "steroids_1hour"))
  #      FreqSum(dat, "steroids_1hour_alt_1_to_5_years"),
  #      FreqSum(dat, "steroids_1hour_alt_6_plus_years"))
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_admin_or_1hr_prev"),
                mediSumRound(dat, "arrival_to_b2a_minutes", 0),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "IV_med_aminophylline"),
                FreqSum(dat, "IV_med_ketamine"),
                FreqSum(dat, "IV_med_mag_sulphate"),
                FreqSum(dat, "IV_med_b2a"),
                FreqSum(dat, "IV_med_none"),
                
                FreqSum(dat, "crit_care_total"),
                
                
                FreqSum(dat, "discharge_bundle"),
                #  FreqSum(dat, "transferred"),
                FreqSum(dat, "weekday_weekend_discharge"),
                makeFlatNPercInf(table(dat$weekday_weekend_discharge, dat$discharge_bundle), varname = "discharge_bundle"),
                
                #   makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle)),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                
                # When a country's missing an entire factor, need to make sure it's coded as a factor rather than numeric.
                
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_parent_smoke"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_asthma_clinic_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_comm_FU_2_days_if_severe_or_LT"),
                
                #   FreqSum(dat, "DB_vape_cess"),
                FreqSum(dat,"DB_none"),
                
                FreqSum(dat, "inhaled_steroids_dis"),
                #               FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                FreqSum(dat, "referred_for_FU"), 
                FreqSum(dat, "DB_selected_elements"))
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save

nrow(flat.all)
unique(flat.all$hosp_code)
head(flat.all$country)



# colnames(flat.all)
# str(flat.all)

colnames(flat.all)
str(flat.all)

# # change to appropriate order and remove unnecessary 'median admissions' columns and heat map columns,
# flat.all <- flat.all %>% select(hosp_code:record_N, 
#                                 DB_selected_elements_N:DB_selected_elements_1_perc) 
# 
# write.csv(flat.all,
#           "G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Analysis/Output/DB_selected_elements_6_to_18.csv",
#           row.names = FALSE)


# change to appropriate order and remove unnecessary 'median admissions' columns and heat map columns,
flat.all <- flat.all %>% select(hosp_code:record_N, 
                                country:referred_for_FU_Already_being_seen_in_secondary_care_clinic_perc) 
  # select(-admissions_N, -admissions_median, -admissions_lo.quart, -admissions_hi.quart) %>%
  # select(-c(Monday_0.00to3.59_admiss_with_1hour_steroids_n:Sunday_20.00to23.59_admiss_with_1hour_steroids_perc))
# remove the 'all', 'england', 'wales', and 'scotland' rows

summary(flat.all$record_N)

# they are the ones without a 'record_N' variable.

# flat.all <- flat.all %>% filter(!is.na(record_N))

nrow(flat.all)
length(unique(flat.all$hosp_code))
length(unique(dat.save$hosp_code))

flat.all$record_N <- flat.all$age_N
table(dat$arrival8hourtimes)


# and now we need to see how well the column names match up
library(openxlsx2)

data_desc <- read_xlsx("G:/Alex Harley/Audit_2023_onwards/2023-2024/Combined/2022-23 Complete column descriptions/CYPA_analysis_plan_SOTN_2024.xlsx",
                       check.names = TRUE)

data_desc_AA <- read.csv("G:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Analysis/Output/AA_SCC_2023-2024_national_country_hospital_level_data.csv",
                         stringsAsFactors = FALSE)

data_desc_AA <- data_desc_AA[1, ]

colnames(data_desc) <- data_desc[2, ]
data_desc <- data_desc[1, ]


# convert all columns to character and bind them together
flat.all <- flat.all %>% mutate_all(~as.character(.))
data_desc <- data_desc %>% mutate_all(~as.character(.))
clean <- bind_rows(flat.all, data_desc)


# Then, make it so the column description is the second row
clean <- clean[c(nrow(clean), 1:(nrow(clean)-1)), ]

colnames(clean)
colnames(flat.all)
colnames(data_desc)

colnames(clean)
clean[1, 78:119] <- clean[1, 475:516] # arrival times
clean[1, c(282:288, 302:308)] <- clean[1, c(571:577, 594:600)] # pre-arrival steroids and beta agonists
clean[1, c(293:301)] <- clean[1, c(585:593)] # steroids within 1 hour

data_desc[1, 190:240]

# Then, get rid of the old columns that are no longer used:
clean <- clean %>% select(-ICS:-DB_vape_cess_1_perc)
table(dat$gender, useNA = "ifany")
ncol(clean)
clean[1:2, 420:447]

# And get rid of the columns that we don't want:
# clean <- clean %>% select(-ends_with("0_n"), -ends_with("0_perc"),
#                           -one_of(c("oxygen_prescribed_No_n", "oxygen_prescribed_No_perc",
#                                     "oxygen_admin_No_n", "oxygen_admin_No_perc",
#                                     "steroids_24hr_prev_No_n", "steroids_24hr_prev_No_perc",
#                                     "b2a_admin_Not_administered_n", "b2a_admin_Not_administered_perc",
#                                     "b2a_1hr_prev_No_n", "b2a_1hr_prev_No_perc", 
#                                     "discharge_bundle_Self_discharge_with_Weekday_perc",
#                                     "discharge_bundle_Self_discharge_with_Weekend_perc",
#                                     "inhaled_steroids_dis_No_n", "inhaled_steroids_dis_No_perc",
#                                     "oral_steroids_dis_No_n", "oral_steroids_dis_No_perc",
#                                     "oral_steroids_rescue_history_No_n", "oral_steroids_rescue_history_No_perc",
#                                     "BPT_Not_achieved_n", "BPT_Not_achieved_perc")))

# new_look <- data_desc %>% select(ends_with("1_n"), ends_with("1_perc"),
#                                  one_of(c("oxygen_prescribed_Yes_n", "oxygen_prescribed_Yes_perc",
#                                           "oxygen_admin_Yes_n", "oxygen_admin_Yes_perc",
#                                           "steroids_24hr_prev_Yes_n", "steroids_24hr_prev_Yes_perc",
#                                           "b2a_admin_Yes_n", "b2a_admin_Yes_perc",
#                                           "b2a_1hr_prev_Yes_-_up_to_1_hour_prior_to_arrival_n", "b2a_1hr_prev_Yes_-_up_to_1_hour_prior_to_arrival_perc",
#                                           "discharge_bundle_Self_discharge_with_Weekday_n",
#                                           "discharge_bundle_Self_discharge_with_Weekend_n",
#                                           "inhaled_steroids_dis_Yes_n", "inhaled_steroids_dis_Yes_perc",
#                                           "oral_steroids_dis_Yes_n", "oral_steroids_dis_Yes_perc",
#                                           "oral_steroids_rescue_history_Yes_n", "oral_steroids_rescue_history_Yes_perc",
#                                           "BPT_Achieved_n", "BPT_Achieved_perc", "PEF_b2a_steroids_oxygen_1hr_combo_Yes_n")))
# 
# new_look
# 
# # And get rid of the columns that we don't want:
# clean <- clean %>% select(-one_of(c("discharge_bundle_Self_discharge_with_Weekday_perc",
#                                     "discharge_bundle_Self_discharge_with_Weekend_perc")))

# where are we missing column descriptions now?

columns_to_update <- clean %>% slice(1) %>% select_if(is.na(.))

ncol(columns_to_update)
columns_to_update

columns_to_update[1, ] <- c("Hospital code", "Trust code", 
                            "Integrated care system", 
                            "Number of patients with gender or not recorded or who preferred not to say", 
                            "Percentage of patients with gender or not recorded or who preferred not to say",
                            rep(NA, 37),
                            "Number of arrivals", "Number of patients arriving on a weekday",
                            "Percentage of patients arriving on a weekday",
                            "Number of patients arriving on a weekend",
                            "Percentage of patients arriving on a weekend",
                            "Number of admissions",
                            "Number of patients admitted with at least one impairment",
                            "Percentage of patients admitted with at least one impairment",
                            "Number of patients admitted with no impairments",
                            "Percentage of patients admitted with no impairments",
                            "Number of admissions",
                            "Number of patients admitted without anxiety",
                            "Percentage of patients admitted without anxiety",
                            "Number of patients admitted with anxiety",
                            "Percentage of patients admitted with anxiety",
                            "Number of admissions",
                            "Number of patients admitted without depression",
                            "Percentage of patients admitted without depression",
                            "Number of patients admitted with depression",
                            "Percentage of patients admitted with depression",
                            "Number of admissions",
                            "Number of patients admitted without severe mental illness",
                            "Percentage of patients admitted without severe mental illness",
                            "Number of patients admitted with severe mental illness",
                            "Percentage of patients admitted with severe mental illness",
                            "Number of admissions",
                            "Number of patients admitted who do not have a history of self-harm",
                            "Percentage of patients admitted who do not have a history of self-harm",
                            "Number of patients admitted who have a history of self-harm",
                            "Percentage of patients admitted who have a history of self-harm",
                            "Number of admissions",
                            "Number of patients admitted without an eating disorder",
                            "Percentage of patients admitted without an eating disorder",
                            "Number of patients admitted with an eating disorder",
                            "Percentage of patients admitted with an eating disorder",
                            "Number of admissions",
                            "Number of patients not known to mental health services",
                            "Percentage of patients not known to mental health services",
                            "Number of patients known to mental health services",
                            "Percentage of patients known to mental health services",
                            "Number of admissions",
                            "Number of patients admitted without any other mental health illness or cognitive impairment",
                            "Percentage of patients admitted without any other mental health illness or cognitive impairment",
                            "Number of patients admitted with any other mental health illness or cognitive impairment",
                            "Percentage of patients admitted with any other mental health illness or cognitive impairment",
                            "Number of admissions",
                            "Number of patients admitted with a recording of presence of impairments",
                            "Percentage of patients admitted a recording of presence of impairments",
                            "Number of patients admitted without any recording of presence of impairments",
                            "Percentage of patients admitted without any recording of presence of impairments",
                            "Number of admissions",
                            "Number of patients admitted who have never smoked tobacco",
                            "Percentage of patients admitted who have never smoked tobacco",
                            "Number of patients admitted who are ex-smokers of tobacco",
                            "Percentage of patients admitted who are ex-smokers of tobacco",
                            "Number of patients admitted who are current smokers of tobacco",
                            "Percentage of patients admitted who are current smokers of tobacco",
                            "Number of patients admitted who do not have tobacco smoking status recorded",
                            "Percentage of patients admitted who do not have tobacco smoking status recorded",
                            "Number of admissions",
                            "Number of patients admitted who have never smoked shisha",
                            "Percentage of patients admitted who have never smoked shisha",
                            "Number of patients admitted who are ex-smokers of shisha",
                            "Percentage of patients admitted who are ex-smokers of shisha",
                            "Number of patients admitted who are current smokers of shisha",
                            "Percentage of patients admitted who are current smokers of shisha",
                            "Number of patients admitted who do not have shisha smoking status recorded",
                            "Percentage of patients admitted who do not have shisha smoking status recorded",
                            "Number of admissions",
                            "Number of patients admitted who have never smoked cannabis",
                            "Percentage of patients admitted who have never smoked cannabis",
                            "Number of patients admitted who are ex-smokers of cannabis",
                            "Percentage of patients admitted who are ex-smokers of cannabis",
                            "Number of patients admitted who are current smokers of cannabis",
                            "Percentage of patients admitted who are current smokers of cannabis",
                            "Number of patients admitted who do not have cannabis smoking status recorded",
                            "Percentage of patients admitted who do not have cannabis smoking status recorded",
                            "Number of admissions",
                            "Number of patients admitted who have never smoked other substances",
                            "Percentage of patients admitted who have never smoked other substances",
                            "Number of patients admitted who are ex-smokers of other substances",
                            "Percentage of patients admitted who are ex-smokers of other substances",
                            "Number of patients admitted who are current smokers of other substances",
                            "Percentage of patients admitted who are current smokers of other substances",
                            "Number of patients admitted who do not have other substances smoking status recorded",
                            "Percentage of patients admitted who do not have other substances smoking status recorded",
                            "Number of admissions",
                            "Number of patients admitted who have never vaped",
                            "Percentage of patients admitted who have never vaped",
                            "Number of patients admitted who are ex-vapers",
                            "Percentage of patients admitted who are ex-vapers",
                            "Number of patients admitted who are current vapers",
                            "Percentage of patients admitted who are current vapers",
                            "Number of patients admitted who do not have vaping status recorded",
                            "Percentage of patients admitted who do not have vaping status recorded",
                            "Number of patients whose asthma severity was: severe", 
                            "Percentage  of patients whose asthma severity was: severe",
                            "Number of patients whose asthma severity was: life-threatening", 
                            "Percentage of patients whose asthma severity was: life-threatening",
                            "Number of patients whose asthma was severe or life-threatening",  # RSR with severe of life threatening
                            "Number of patients whose asthma was severe or life-threatening who did not receive a respiratory specialist review", # RSR with severe of life threatening
                            "Percentage of patients whose asthma was severe or life-threatening who did not receive a respiratory specialist review", # RSR with severe of life threatening
                            "Number of patients whose asthma was severe or life-threatening who received a respiratory specialist review", # RSR with severe of life threatening
                            "Percentage of patients whose asthma was severe or life-threatening who received a respiratory specialist review", # RSR with severe of life threatening
                            "Number of admissions", 
                            "Number of patients who received steroids in hospital or in the 24 hours prior to arrival",
                            "Percentage of patients who received steroids in hospital or in the 24 hours prior to arrival",
                            "Number of patients who did not receive steroids in hospital or were not recorded as receiving steroids in hospital, and did not receive them in the 24 hours prior to arrival",
                            "Percentage of patients who did not receive steroids in hospital or were not recorded as receiving steroids in hospital, and did not receive them in the 24 hours prior to arrival",
                            "Number of admissions", 
                            "Number of patients who received beta agonists in hospital or in the hour prior to arrival",
                            "Percentage of patients who received beta agonists in hospital or in the hour prior to arrival",
                            "Number of patients who did not receive beta agonists in hospital and did not receive them in the hour prior to arrival",
                            "Percentage of patients who did not receive beta agonists in hospital and did not receive them in the hour prior to arrival",
                            "Total number of patients who did not receive beta agonists in the hour prior to their arrival at hospial",
                            "Number of patients who received beta agonists in the hour after arrival at hospital",
                            "Percentage of patients who received beta agonists in the hour after arrival at hospital",
                            "Number of patients who received beta agonists more than one hour after arrival at hospital",
                            "Percentage of patients who received beta agonists more than one hour after arrival at hospital",
                            "Number of patients who did not receive beta agonists in hospital",
                            "Percentage of patients who did not receive beta agonists in hospital",
                            "Number of patients who did not have a recording of beta agonist administration in hospital",
                            "Percentage of patients who did not have a recording of beta agonist administration in hospital",
                            "Number of patients who did not die or who were transferred",
                            "Number of patients discharged on a weekday",
                            "Percentage of patients discharged on a weekday",
                            "Number of patients discharged on a weekend",
                            "Percentage of patients discharged on a weekend",
                            "Number of patients who received a discharge bundle and were discharged on a weekday",
                            "Number of people who received a discharge bundle were discharged on the weekend",
                            "Number of people who did not receive a discharge bundle and were discharged on a weekday",
                            "Number of people who did not receive a discharge bundle and were discharged on a weekend",
                            "number of people who parent/carer/self-discharged on a weekday",
                            "number of people who parent/carer/self-discharged on a weekend",
                            "Percentage of patients who received a discharge bundle and were discharged on a weekday",
                            "Percentage of people who received a discharge bundle were discharged on the weekend",
                            "Percentage of people who did not receive a discharge bundle and were discharged on a weekday",
                            "Percentage of people who did not receive a discharge bundle and were discharged on a weekend",
                            "Percentage of people who parent/carer/self-discharged on a weekday",
                            "Percentage of people who parent/carer/self-discharged on a weekend",
                            "Number of current tobacco smokers without tobacco dependency addressed", # DB_smoke_0_n, 
                            "Percentage of current tobacco smokers without tobacco dependency addressed",
                            "Total number of patients discharged alive who were not transferred",
                            "Number of people discharged without two-day community follow-up or 4 week hospital follow-up ticked in the discharge bundle",
                            "Percentage of people discharged without two-day community follow-up or 4 week hospital follow-up ticked in the discharge bundle",
                            "Number of people discharged with two-day community follow-up or 4 week hospital follow-up ticked in the discharge bundle",
                            "Percentage of people discharged with two-day community follow-up or 4 week hospital follow-up ticked in the discharge bundle",
                            "Total number of patients discharged alive who were not transferred and who were classed as having severe or life-threatening asthma in hospital",
                            "Number of patients without community follow-up requested within 2 working days",
                            "Percentage of patients without community follow-up requested within 2 working days",
                            "Number of patients with community follow-up requested within 2 working days",
                            "Percentage of patients with community follow-up requested within 2 working days")

data_desc_AA %>% select(starts_with("discharge_bundle"))

clean$patch <- NA
clean$patch[1] <- 1
columns_to_update$patch <- 1
clean <- rows_patch(clean, columns_to_update, by = "patch")
clean <- clean %>% select(-patch)

colnames(clean)

write.csv(clean,
          "G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Analysis/Output/CYPA_SCC_2023-2024_national_country_hospital_level_data_6_to_18.csv",
          row.names = FALSE)


