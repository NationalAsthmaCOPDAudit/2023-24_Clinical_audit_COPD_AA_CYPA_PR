#                                   #
# AA SCC analysis script            #
#                                   #
# Author: Alex Adamson              #
# Date created:  17th July 2024     #
#                                   #



library(tidyverse)
library(psych)
'%!in%' <- function(x,y)!('%in%'(x,y))



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
  cols <- paste0(cols, "_N")
  
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






# Okay let's go!

dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Data/tidyData/AA_SCC_2023-24_clean_data.RDS")

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


dat <- dat %>% mutate_at(.vars = vars(starts_with("DB"), starts_with("impairments")), 
                         .funs = ~factor(.)) #%>% str()



# Now we should be fine to get on with what we're doing.

# Second, create our 'psychic' data frame for the medians

psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)

# We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.

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

# And again 

BPT_table <- dat %>% group_by(hosp_code) %>%
  summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
            BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
            BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))


FreqSum(BPT_table, "BPT_pass")





flat <- data.frame(country = "All")

flat <- cbind(flat,
              
             mediSumRound(dat, "age", roundno = 0),

             # FreqSum(dat, "hospital_transfer"), # Removed for 2023-24
             FreqSum(dat, "first_department"),
             
              FreqSum(dat, "gender"),
             FreqSum(dat, "IMD_quintile"),
             mediSumRound(dat, "admissions", roundno = 0))
             
              

# Now create the 8 hour arrivals table and bind it in

arrivaltimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
rownames(arrivaltimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

arrivaltime_flat <- matrix(arrivaltimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
colsss <- paste(rep(colnames(arrivaltimedow.N)[1:7], each = 3),
                rownames(arrivaltimedow.N)[1:3], "arrival_n", sep = "_")

colnames(arrivaltime_flat) <- colsss
arrivaltime_flat <- as.data.frame(arrivaltime_flat)
# bind this

flat <- cbind(flat, arrivaltime_flat)


arrivaltimedow.perc <- round(prop.table(arrivaltimedow.N, 2)*100, 1)
rownames(arrivaltimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")

arrivaltime_flat_perc <- matrix(arrivaltimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
colsssperc <- paste(rep(colnames(arrivaltimedow.perc)[1:7], each = 3),
                    rownames(arrivaltimedow.perc)[1:3], "arrival_perc", sep = "_")

colnames(arrivaltime_flat_perc) <- colsssperc
arrivaltime_flat_perc <- as.data.frame(arrivaltime_flat_perc)

# bind this


flat <- cbind(flat, arrivaltime_flat_perc)


# add in the weekday/weekend arrival:
flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))
table(dat$weekday_weekend_arrival)



       
              
              



flat <- cbind(flat,
              mediSumRound(dat, "LOS_days_alive"),
      #        FreqSum(dat, "smoke_status"), # removed in the 2023-24 audit. Replaced with 
      # various smoking variables below
      FreqSum(dat, "impairments_none"), # 2023-24 impairments added
      FreqSum(dat, "impairments_anxiety"),
      FreqSum(dat, "impairments_depression"),
      FreqSum(dat, "impairments_severe_mental_illness"),
      FreqSum(dat, "impairments_dementia_mci"),
      FreqSum(dat, "impairments_other"),
      FreqSum(dat, "impairments_NR"),
      FreqSum(dat, "smoke_tobacco"),
      FreqSum(dat, "smoke_shisha"),
      FreqSum(dat, "smoke_cannabis"),
      FreqSum(dat, "smoke_other"),
      FreqSum(dat, "vaping_status"),
      FreqSum(dat, "tobacco_vape_combo"),
              FreqSum(dat, "oxygen_sat_recorded"),
              FreqSum(dat, "oxygen_sat_measurement_type"),
              
              FreqSum(dat, "PEF_init_recorded"),
              mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
              FreqSum(dat, "PEF_init_1hour"))
              


# 


flat <- cbind(flat, 
FreqSum(dat, "PEF_prev_recorded"),
FreqSum(dat, "PEF_predict_recorded"),
FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
FreqSum(dat, "PEF_percpred_75"),

FreqSum(dat, "RSR"),
mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
FreqSum(dat, "arrival_to_RSR_24hour_weekday"),
FreqSum(dat, "arrival_to_RSR_24hour_weekend"),
FreqSum(dat, "oxygen_prescribed"),
FreqSum(dat, "oxygen_admin"),
FreqSum(dat, "oxygen_prescribed_if_administered"),
mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
FreqSum(dat, "oxygen_presc_1hr"),

FreqSum(dat, "steroids_admin_or_24hr_prev"),
mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
FreqSum(dat, "steroids_24hr_prev"),
FreqSum(dat, "steroids_1hour"))

dat %>% select(starts_with("DB")) %>% summary()

flat <- cbind(flat,
FreqSum(dat, "b2a_admin_or_1hr_prev"),
mediSumRound(dat, "arrival_to_b2a_minutes"),
FreqSum(dat, "b2a_1hr_prev"),
FreqSum(dat, "b2a_1hour"),
FreqSum(dat, "discharge_day_of_week"),
FreqSum(dat, "discharge_bundle"),

makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),

FreqSum(dat, "DB_inhaler"),
FreqSum(dat, "DB_maintenance"),
FreqSum(dat, "DB_adherence"),
FreqSum(dat, "DB_PAAP"),
FreqSum(dat, "DB_triggers"),
FreqSum(dat, "DB_comm_FU_2_days"),
FreqSum(dat, "DB_spec_review_4_weeks"),
FreqSum(dat, "DB_FU_any"),
FreqSum(dat, "DB_smoke"),
FreqSum(dat,"DB_none"),
# FreqSum(dat, "GPC"),
FreqSum(dat, "inhaled_steroids_dis"),
FreqSum(dat, "oral_steroids_dis"),
FreqSum(dat, "oral_steroids_rescue_history"),
# FreqSum(dat, "referred_for_FU"),
# FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
FreqSum(dat, "RSR_24hour_BPT"),
FreqSum(dat, "DB_BPT"),
FreqSum(dat, "BPT"),

# FreqSum(dat, "BPT_mandatory"),
# FreqSum(dat, "BPT_all"),
FreqSum(BPT_table, "BPT_pass"),

# Then we do first hour of care

FreqSum(dat, "asthma_sev"),
FreqSum(dat, "life_status"),
FreqSum(dat, "ethnicity"))





flat.all <- flat
dat.save <- dat

# # # # now for countries

for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)

  flat <- data.frame(country = i)
  
  
  # Now we should be fine to get on with what we're doing.
  
  # Second, create our 'psychic' data frame for the medians
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.
  
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
  
  # And again 
  
  # BPT_table <- dat %>% filter(life_status == "Alive") %>%
  #   mutate(BPT_mandatory = fct_recode(BPT_mandatory, `0` = "Not achieved", `1` = "Achieved")) %>%
  #   mutate(BPT_mandatory = as.numeric(as.character(BPT_mandatory))) %>% group_by(hosp_code) %>%
  #   summarise(BPT_nume = sum(BPT), country = first(country),
  #             BPT_denom = n(), BPT_perc = BPT_nume/BPT_denom,
  #             BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))

  BPT_table <- dat %>% group_by(hosp_code) %>%
    summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
              BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
              BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  
  
  
  FreqSum(BPT_table, "BPT_pass")

  
  # makeFlatNPercInf(testtable)
  
  
  
  
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                # FreqSum(dat, "hospital_transfer"), # Removed for 2023-24
                FreqSum(dat, "first_department"),
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                mediSumRound(dat, "admissions", roundno = 0))
  
  
  
  # Now create the 8 hour arrivals table and bind it in
  
  arrivaltimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(arrivaltimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  arrivaltime_flat <- matrix(arrivaltimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(arrivaltimedow.N)[1:7], each = 3),
                  rownames(arrivaltimedow.N)[1:3], "arrival_n", sep = "_")
  
  colnames(arrivaltime_flat) <- colsss
  arrivaltime_flat <- as.data.frame(arrivaltime_flat)
  # bind this
  
  flat <- cbind(flat, arrivaltime_flat)
  
  
  arrivaltimedow.perc <- round(prop.table(arrivaltimedow.N, 2)*100, 1)
  rownames(arrivaltimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  arrivaltime_flat_perc <- matrix(arrivaltimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(arrivaltimedow.perc)[1:7], each = 3),
                      rownames(arrivaltimedow.perc)[1:3], "arrival_perc", sep = "_")
  
  colnames(arrivaltime_flat_perc) <- colsssperc
  arrivaltime_flat_perc <- as.data.frame(arrivaltime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, arrivaltime_flat_perc)
  
  
  # add in the weekday/weekend arrival:
  flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))
  table(dat$weekday_weekend_arrival)
  

  
  
  
  
  
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days_alive"),
                #        FreqSum(dat, "smoke_status"), # removed in the 2023-24 audit. Replaced with 
                # various smoking variables below
                FreqSum(dat, "impairments_none"), # 2023-24 impairments added
                FreqSum(dat, "impairments_anxiety"),
                FreqSum(dat, "impairments_depression"),
                FreqSum(dat, "impairments_severe_mental_illness"),
                FreqSum(dat, "impairments_dementia_mci"),
                FreqSum(dat, "impairments_other"),
                FreqSum(dat, "impairments_NR"),
                FreqSum(dat, "smoke_tobacco"),
                FreqSum(dat, "smoke_shisha"),
                FreqSum(dat, "smoke_cannabis"),
                FreqSum(dat, "smoke_other"),
                FreqSum(dat, "vaping_status"),
                FreqSum(dat, "tobacco_vape_combo"),
                FreqSum(dat, "oxygen_sat_recorded"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
                FreqSum(dat, "PEF_init_1hour"))
  
  
  
  # 
  
  
  flat <- cbind(flat, 
                FreqSum(dat, "PEF_prev_recorded"),
                FreqSum(dat, "PEF_predict_recorded"),
                FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
                FreqSum(dat, "PEF_percpred_75"),
                
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                FreqSum(dat, "arrival_to_RSR_24hour_weekday"),
                FreqSum(dat, "arrival_to_RSR_24hour_weekend"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_admin"),
                FreqSum(dat, "oxygen_prescribed_if_administered"),
                mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
                FreqSum(dat, "oxygen_presc_1hr"),
                
                FreqSum(dat, "steroids_admin_or_24hr_prev"),
                mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
                FreqSum(dat, "steroids_24hr_prev"),
                FreqSum(dat, "steroids_1hour"))
  
  dat %>% select(starts_with("DB")) %>% summary()
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_admin_or_1hr_prev"),
                mediSumRound(dat, "arrival_to_b2a_minutes"),
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_spec_review_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat,"DB_none"),
                # FreqSum(dat, "GPC"),
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                # FreqSum(dat, "referred_for_FU"),
                # FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
                FreqSum(dat, "RSR_24hour_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "BPT"),
                
                # FreqSum(dat, "BPT_mandatory"),
                # FreqSum(dat, "BPT_all"),
                FreqSum(BPT_table, "BPT_pass"),
                
                # Then we do first hour of care
                
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "life_status"),
                FreqSum(dat, "ethnicity"))
  
  
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save
str(flat.all)



# # # # now for hospitals

unique(dat$hosp_code)


for (i in unique(dat.save$hosp_code)) {
  
  dat <- filter(dat.save, hosp_code == i)
  
  
  flat <- data.frame(hosp_code = i)
  flat$hosp_name <- as.character(dat$hosp_name[1])
  flat$trust_code <- as.character(dat$trust_code[1])
  flat$trust_name <- as.character(dat$trust_name[1])
  flat$integrated_care_system <- as.character(dat$integrated_care_system[1])
  flat$region <- as.character(dat$region[1])
  flat$country <- as.character(dat$country[1])
 
  
  # Now we should be fine to get on with what we're doing.
  
  # Second, create our 'psychic' data frame for the medians
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.
  
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
  
  # And again 
  
  # BPT_table <- dat %>% filter(life_status == "Alive") %>% 
  #   mutate(BPT_mandatory = fct_recode(BPT_mandatory, `0` = "Not achieved", `1` = "Achieved")) %>% 
  #   mutate(BPT_mandatory = as.numeric(as.character(BPT_mandatory))) %>% group_by(hosp_code) %>% 
  #   summarise(BPT_nume = sum(BPT_mandatory), country = first(country), 
  #             BPT_denom = n(), BPT_perc = BPT_nume/BPT_denom,
  #             BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  # 
  # FreqSum(BPT_table, "BPT_pass")
  # 
  
  BPT_table <- dat %>% group_by(hosp_code) %>%
    summarise(BPT_nume = sum(BPT, na.rm = TRUE), country = first(country),
              BPT_denom = sum(!is.na(BPT), na.rm = TRUE), BPT_perc = BPT_nume/BPT_denom,
              BPT_pass = factor(ifelse(BPT_perc >= 0.5, "Pass", "Fail"), levels = c("Pass", "Fail")))
  
  
  
  
  
 # flat <- data.frame(country = i)
  
  

  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                # FreqSum(dat, "hospital_transfer"), # Removed for 2023-24
                FreqSum(dat, "first_department"),
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                mediSumRound(dat, "admissions", roundno = 0))
  
  
  
  # Now create the 8 hour arrivals table and bind it in
  
  arrivaltimedow.N <- table(dat$arrival8hourtimes, dat$arrival_day_of_week)
  rownames(arrivaltimedow.N) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  arrivaltime_flat <- matrix(arrivaltimedow.N, nrow = 1, ncol = 21, byrow = FALSE)
  colsss <- paste(rep(colnames(arrivaltimedow.N)[1:7], each = 3),
                  rownames(arrivaltimedow.N)[1:3], "arrival_n", sep = "_")
  
  colnames(arrivaltime_flat) <- colsss
  arrivaltime_flat <- as.data.frame(arrivaltime_flat)
  # bind this
  
  flat <- cbind(flat, arrivaltime_flat)
  
  
  arrivaltimedow.perc <- round(prop.table(arrivaltimedow.N, 2)*100, 1)
  rownames(arrivaltimedow.perc) <- paste0(seq(0, 16, 8), ".00to", seq(7, 23, 8), ".59")
  
  arrivaltime_flat_perc <- matrix(arrivaltimedow.perc, nrow = 1, ncol = 21, byrow = FALSE)
  colsssperc <- paste(rep(colnames(arrivaltimedow.perc)[1:7], each = 3),
                      rownames(arrivaltimedow.perc)[1:3], "arrival_perc", sep = "_")
  
  colnames(arrivaltime_flat_perc) <- colsssperc
  arrivaltime_flat_perc <- as.data.frame(arrivaltime_flat_perc)
  
  # bind this
  
  
  flat <- cbind(flat, arrivaltime_flat_perc)
  
  
  # add in the weekday/weekend arrival:
  flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))
  table(dat$weekday_weekend_arrival)
  
  

  
  
  
  
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days_alive"),
                #        FreqSum(dat, "smoke_status"), # removed in the 2023-24 audit. Replaced with 
                # various smoking variables below
                FreqSum(dat, "impairments_none"), # 2023-24 impairments added
                FreqSum(dat, "impairments_anxiety"),
                FreqSum(dat, "impairments_depression"),
                FreqSum(dat, "impairments_severe_mental_illness"),
                FreqSum(dat, "impairments_dementia_mci"),
                FreqSum(dat, "impairments_other"),
                FreqSum(dat, "impairments_NR"),
                FreqSum(dat, "smoke_tobacco"),
                FreqSum(dat, "smoke_shisha"),
                FreqSum(dat, "smoke_cannabis"),
                FreqSum(dat, "smoke_other"),
                FreqSum(dat, "vaping_status"),
                FreqSum(dat, "tobacco_vape_combo"),
                FreqSum(dat, "oxygen_sat_recorded"),
                FreqSum(dat, "oxygen_sat_measurement_type"),
                
                FreqSum(dat, "PEF_init_recorded"),
                mediSumRound(dat, "arrival_to_PEF_init_hours", roundno = 1),
                FreqSum(dat, "PEF_init_1hour"))
  
  
  
  # 
  
  
  flat <- cbind(flat, 
                FreqSum(dat, "PEF_prev_recorded"),
                FreqSum(dat, "PEF_predict_recorded"),
                FreqSum(dat, "PEF_prev_or_predict_recorded_only_PEF_init"),
                FreqSum(dat, "PEF_percpred_75"),
                
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                FreqSum(dat, "arrival_to_RSR_24hour_weekday"),
                FreqSum(dat, "arrival_to_RSR_24hour_weekend"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_admin"),
                FreqSum(dat, "oxygen_prescribed_if_administered"),
                mediSumRound(dat, "arrival_to_oxygen_minutes", roundno = 0),
                FreqSum(dat, "oxygen_presc_1hr"),
                
                FreqSum(dat, "steroids_admin_or_24hr_prev"),
                mediSumRound(dat, "arrival_to_steroids_hours", roundno = 1),
                FreqSum(dat, "steroids_24hr_prev"),
                FreqSum(dat, "steroids_1hour"))
  
  dat %>% select(starts_with("DB")) %>% summary()
  
  flat <- cbind(flat,
                FreqSum(dat, "b2a_admin_or_1hr_prev"),
                mediSumRound(dat, "arrival_to_b2a_minutes"),
                FreqSum(dat, "b2a_1hr_prev"),
                FreqSum(dat, "b2a_1hour"),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                
                makeFlatNPercInf(table(dat$discharge_day_of_week, dat$discharge_bundle), varname = "discharge_bundle"),
                
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_adherence"),
                FreqSum(dat, "DB_PAAP"),
                FreqSum(dat, "DB_triggers"),
                FreqSum(dat, "DB_comm_FU_2_days"),
                FreqSum(dat, "DB_spec_review_4_weeks"),
                FreqSum(dat, "DB_FU_any"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat,"DB_none"),
                # FreqSum(dat, "GPC"),
                FreqSum(dat, "inhaled_steroids_dis"),
                FreqSum(dat, "oral_steroids_dis"),
                FreqSum(dat, "oral_steroids_rescue_history"),
                # FreqSum(dat, "referred_for_FU"),
                # FreqSum(dat, "referred_for_FU_with_2_oral_hist"),
                FreqSum(dat, "RSR_24hour_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "BPT"),
                
                # FreqSum(dat, "BPT_mandatory"),
                # FreqSum(dat, "BPT_all"),
                FreqSum(BPT_table, "BPT_pass"),
                
                # Then we do first hour of care
                
                FreqSum(dat, "asthma_sev"),
                FreqSum(dat, "life_status"),
                FreqSum(dat, "ethnicity"))
  
  
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save

colnames(flat.all)


flat.all$record_N <- flat.all$age_N

flat.all <- flat.all %>% relocate(hosp_code:record_N, .before = country) %>% 
  select(-admissions_N, -admissions_median, -admissions_lo.quart, -admissions_hi.quart)


# All seems legit! Let's write the table



# and now we need to see how well the column names match up
library(openxlsx2)

data_desc <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2023-2024/Combined/2022-23 Complete column descriptions/AA_analysis_plan_SOTN_2024.xlsx",
                       check.names = TRUE)

colnames(data_desc) <- data_desc[2, ]
data_desc <- data_desc[1, ]


# convert all columns to character and bind them together
flat.all <- flat.all %>% mutate_all(~as.character(.))
data_desc <- data_desc %>% mutate_all(~as.character(.))
clean <- bind_rows(flat.all, data_desc)


# Then, make it so the column description is the second row
clean <- clean[c(nrow(clean), 1:(nrow(clean)-1)), ]

# and add in the arrival descriptions now the column name has changed

colnames(clean)
clean[1, 48:89] <- clean[1, 529:570]


clean <- clean %>% select(-Monday_0.00to7.59_admiss_n:-Sunday_16.00to23.59_admiss_perc)

# Then, get rid of the old columns that are no longer used:
clean <- clean %>% select(-ICS:-`BPT_optional_Not_achieved_perc`)


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
#                           one_of(c("oxygen_prescribed_Yes_n", "oxygen_prescribed_Yes_perc",
#                                     "oxygen_admin_Yes_n", "oxygen_admin_Yes_perc",
#                                     "steroids_24hr_prev_Yes_n", "steroids_24hr_prev_Yes_perc",
#                                     "b2a_admin_Yes_n", "b2a_admin_Yes_perc",
#                                     "b2a_1hr_prev_Yes_-_up_to_1_hour_prior_to_arrival_n", "b2a_1hr_prev_Yes_-_up_to_1_hour_prior_to_arrival_perc",
#                                     "discharge_bundle_Self_discharge_with_Weekday_n",
#                                     "discharge_bundle_Self_discharge_with_Weekend_n",
#                                     "inhaled_steroids_dis_Yes_n", "inhaled_steroids_dis_Yes_perc",
#                                     "oral_steroids_dis_Yes_n", "oral_steroids_dis_Yes_perc",
#                                     "oral_steroids_rescue_history_Yes_n", "oral_steroids_rescue_history_Yes_perc",
#                                     "BPT_Achieved_n", "BPT_Achieved_perc", "PEF_b2a_steroids_oxygen_1hr_combo_Yes_n")))
# 
# new_look

# where are we missing column descriptions now?

columns_to_update <- clean %>% slice(1) %>% select_if(is.na(.))

columns_to_update[1, ] <- c("Hospital code", "Hospital name", "Trust code", "Trust name", 
                            "Integrated care system", "Region", "Number of admissions", 
                            "Country", "Number of arrivals", "Number of patients arriving on a weekday",
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
                            "Number of patients admitted without dementia or mild cognitive impairment",
                            "Percentage of patients admitted without dementia or mild cognitive impairment",
                            "Number of patients admitted with dementia or mild cognitive impairment",
                            "Percentage of patients admitted with dementia or mild cognitive impairment",
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
                            "Number of patients with a tobacco status and vaping status recorded",
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, # Ellie can fill these out
                            NA, NA, NA, NA, # Ellie can fill these out too for PEF
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, # And these (RSR 24 hour update)
                            "Total number of patients not prescribed oxygen", # oxygen_prescribed_No_n, 
                            "Percentage of patients not prescribed oxygen", # oxygen_prescribed_No_perc, 
                            "Total number of patients not administered oxygen", # oxygen_admin_No_n, 
                            "Percentage of patients not administered oxygen", # oxygen_admin_No_perc, 
                            "Number of patients administered oxygen",
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, # Oxygen columns
                            "Number of admissions", 
                            "Number of patients who received steroids in hospital or in the 24 hours prior to arrival",
                            "Percentage of patients who received steroids in hospital or in the 24 hours prior to arrival",
                            "Number of patients who did not receive steroids in hospital and did not receive them in the 24 hours prior to arrival",
                            "Percentage of patients who did not receive steroids in hospital and did not receive them in the 24 hours prior to arrival",
                            "Number of patients who did not receive steroids within 24 hours of arrival to hospital", # steroids_24hr_prev_No_n, 
                            "Percentage of patients who did not receive steroids within 24 hours of arrival to hospital", # steroids_24hr_prev_No_perc, 
                            "Number of patients not administered steroids in hospital",
                            "Percentage of patients not administered steroids in hospital",
                            "Number of admissions",
                            "Number of patients who received beta agonists in hospital or in the hour prior to arrival",
                            "Percentage of patients who received beta agonists in hospital or in the hour prior to arrival",
                            "Number of patients who did not receive beta agonists in hospital and did not receive them in the hour prior to arrival",
                            "Percentage of patients who did not receive beta agonists in hospital and did not receive them in the hour prior to arrival",
                            "Total number of patients who did not receive beta agonists up to an hour before arrival", # b2a_1hr_prev_No_n, 
                            "Percentage of patients who did not receive beta agonists up to an hour before arrival", # b2a_1hr_prev_No_perc, 
                            "Number of patients not administered B2 agonists", # b2a_admin_Not_administered_n, 
                            "Percentage of patients not administered B2 agonists", # b2a_admin_Not_administered_perc, 
                            "Percentage of patients who self-discharged on a weekday",
                            "Percentage of patients who self-discharged on a weekend",
                            "Number of patients not provided with an inhaler technique check", # DB_inhaler_0_n, 
                            "Percentage of patients not provided with an inhaler technique check", # DB_inhaler_0_perc, 
                            "Number of patients without maintenance medication reviewed", # DB_maintenance_0_n, 
                            "Percentage of patients without maintenance medication reviewed", # DB_maintenance_0_perc, 
                            "Number of patients without adherence discussed", # DB_adherence_0_n, 
                            "Percentage of patients without adherence discussed", # DB_adherence_0_perc, 
                            "Number of patients without a PAAP issued or reviewed", # DB_PAAP_0_n, 
                            "Percentage of patients without a PAAP issued or reviewed", # DB_PAAP_0_perc, 
                            "Number of patients without triggers discussed", # DB_triggers_0_n, 
                            "Percentage of patients without triggers discussed", # DB_triggers_0_perc, 
                            "Number of patients without community follow-up requested within 2 working days", # DB_comm_FU_2_days_0_n, 
                            "Percentage of patients without community follow-up requested within 2 working days", # DB_comm_FU_2_days_0_perc, 
                            "Number of patients without a specialist review requested within 4 weeks", # DB_spec_review_4_weeks_0_n, 
                            "Percentage of patients without a specialist review requested within 4 weeks", # DB_spec_review_4_weeks_0_perc, 
                            "Number of current tobacco smokers without tobacco dependency addressed", # DB_smoke_0_n, 
                            "Percentage of current tobacco smokers without tobacco dependency addressed", # DB_smoke_0_perc, 
                            "Number of patients in receipt of at least one element of good practice care", # DB_none_0_n, 
                            "Percentage of patients in receipt of at least one element of good practice care", # DB_none_0_perc, 
                            "Number of patients not prescribed inhaled steroids at discharge", # inhaled_steroids_dis_No_n, 
                            "Percentage of patients in receipt of at least one element of good practice care", # inhaled_steroids_dis_No_perc, 
                            "Number of patients not prescribed at least 5 days of oral steroids", # oral_steroids_dis_No_n, 
                            "Percentage of patients not prescribed at least 5 days of oral steroids", # oral_steroids_dis_No_perc, 
                            "Number of patients not prescribed more than two courses of oral steroids in the past 12 months", # oral_steroids_rescue_history_No_n, 
                            "Percentage of patients not prescribed more than two courses of oral steroids in the past 12 months", # oral_steroids_rescue_history_No_perc, 
                            "Total number of admissions",
                            "Number of patients not given respiratory specialist review within 24 hours of arrival",
                            "Percentage of patients not given respiratory specialist review within 24 hours of arrival",
                            "Number of patients given respiratory specialist review within 24 hours of arrival",
                            "Percentage of patients given respiratory specialist review within 24 hours of arrival",
                            "Number of patients that did not meet the best practice tariff criteria for the discharge bundle element (Inhaler technique checked, maintenance medication checked, adherence checked, PAAP checked, discharge bundle provided, smokers with tobacco dependency addressed, referred for follow-up within 4 weeks)", # DB_BPT_0_n, 
                            "Percentage of patients that did not meet the best practice tariff criteria for the discharge bundle element (Inhaler technique checked, maintenance medication checked, adherence checked, PAAP checked, discharge bundle provided, smokers with tobacco dependency addressed,  referred for follow-up within 4 weeks)", # DB_BPT_0_perc, 
                            "Total number of patients that did not achieve the BPT (discharge bundle elements (in those who were not transferred or died) and RSR within 24 hours element)", # BPT_Not_achieved_n,  
                            "Percentage of patients that did not achieve the BPT (discharge bundle elements (in those who were not transferred or died) and RSR within 24 hours element)", # BPT_Not_achieved_perc, 
                            "Total number of patients that achieved the BPT (discharge bundle elements (in those who were not transferred or died) and RSR within 24 hours element)", # BPT_Not_achieved_n,  
                            "Percentage of patients that achieved the BPT (discharge bundle elements (in those who were not transferred or died) and RSR within 24 hours element)", # BPT_Not_achieved_perc, 
                            "Number of patients whose asthma severity was: severe", 
                            "Percentage  of patients whose asthma severity was: severe",
                            "Number of patients whose asthma severity was: life-threatening", 
                            "Percentage of patients whose asthma severity was: life-threatening",
                            "Number of patients whose asthma severity was: near-fatal", 
                            "Percentage of patients whose asthma severity was: near-fatal",
                            rep(NA, 49)) # ethnicity

clean$patch <- NA
clean$patch[1] <- 1
columns_to_update$patch <- 1
clean <- rows_patch(clean, columns_to_update, by = "patch")
clean <- clean %>% select(-patch)

# Now write the csv
write.csv(clean,
          "C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Analysis/Output/AA_SCC_2023-2024_national_country_hospital_level_data.csv",
          row.names = FALSE)
