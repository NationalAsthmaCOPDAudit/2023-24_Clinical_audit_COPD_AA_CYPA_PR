#                                               #
# COPD SCC analysis script 2024                 #
#                                               #
# Author: Alex Adamson                          #
# Date created:  05/07/2024                     #
#                                               #


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



medTable <- function(x, varname, roundno = 0) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  
  # NOTE!!! Medians rounded to 0dp by default
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  # scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  # SN <- length(scot[!is.na(scot)])
  # scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  # scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  #  ret <- matrix(c(varname, eng, scot, wal, all), nrow = 1, ncol = 5)
  ret <- matrix(c(varname, eng, wal, all), nrow = 1, ncol = 4)
  
  # colnames(ret) <- c("Variable", 
  #                    paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
  #                    paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))

  colnames(ret) <- c("Variable", 
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
 
  ret <- as.data.frame(ret)
  
  return(ret)
}





makeFlatNPercInf <- function(subana.N, varname = NULL) {
  
  
  colnames(subana.N) <- gsub("", "_", colnames(subana.N))
  rownames(subana.N) <- gsub("", "_", rownames(subana.N))
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





# Now let's put this into a function to make it easier

WTmed <- function(x, variable, roundno = 0) {
  print(medTable(x, variable, roundno))
  write.table(medTable(x, variable, roundno), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}

WTfreq <- function(x, variable) {
  print(myFreqTable(x, variable))
  write.table(myFreqTable(x, variable), 
              file = reporttabs, sep = "\t", append = TRUE, 
              quote = FALSE,
              col.names = TRUE, row.names = FALSE)
  cat("\n", file=reporttabs, append=TRUE)
}







dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Data/tidyData/COPD_SCC_2023-2024_clean_data.RDS")





dat <- dat %>% mutate_at(.vars = vars(starts_with("DB"), 
                                      starts_with("impairments"),
                                      starts_with("old_KPI")), 
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



flat <- data.frame(country = "All")

flat <- cbind(flat,
              
             mediSumRound(dat, "age", roundno = 0),


              
              FreqSum(dat, "gender"),
             FreqSum(dat, "IMD_quintile"),
              # FreqSum(dat, "IMD_quintile_Eng"),
              # FreqSum(dat, "IMD_quintile_Scot"),
              # FreqSum(dat, "IMD_quintile_Wal"),
             FreqSum(dat, "ethnicity"),
             mediSumRound(dat, "admissions", roundno = 0),
             mediSumRound(dat, "arrival_to_admission_hours", roundno = 0))
             
              

# Now create the 2 hour table and bind it in



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

# admisstimedow.N.all <- admisstimedow.N


# # bind these below
# flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
# flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
# flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
# flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
# flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
# flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
# flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]

# # bind these below
# flat$Weekday_admit_N <- sum(margin.table(admisstimedow.N, 2)[1:5])
# flat$Weekend_admit_N <- sum(margin.table(admisstimedow.N, 2)[6:7])

# Then carry on as normal:
       
              
flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))          
         
flat <- cbind(flat,
              mediSumRound(dat, "LOS_days"),
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
              FreqSum(dat, "life_status"),
FreqSum(dat, "RSR"),
mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
mediSumRound(dat, "admission_to_RSR_hours", roundno = 1),
FreqSum(dat, "admission_to_RSR_24hour"),
FreqSum(dat, "oxygen_prescribed"),
FreqSum(dat, "oxygen_target_range"),
FreqSum(dat, "oxygen_admin"),
FreqSum(dat, "oxygen_prescribed_if_administered"),
FreqSum(dat, "NEWS2"),
FreqSum(dat, "NEWS2_score_sev"),
FreqSum(dat, "NIV"),
FreqSum(dat, "NIV_location"),
FreqSum(dat, "AHVF_ever"),
FreqSum(dat, "AHVF_cont"),
FreqSum(dat, "NIV_in_AHVF_cont"),
FreqSum(dat, "AHVF_cont_to_NIV_cat"),
FreqSum(dat, "spirometry"),
FreqSum(dat, "obstruction"),
mediSumRound(dat, "FEV1_perc_pred_value", 1),
FreqSum(dat, "discharge_day_of_week"),
FreqSum(dat, "discharge_bundle"),
FreqSum(dat, "DB_inhaler"),
FreqSum(dat, "DB_maintenance"),
FreqSum(dat, "DB_plan"),
FreqSum(dat, "DB_pack"),
FreqSum(dat, "DB_unsuitable_for_pack"),
FreqSum(dat, "DB_oxygen_alert"),
FreqSum(dat, "DB_smoke"),
FreqSum(dat, "DB_PR"),
FreqSum(dat, "DB_FU_72hour"),
FreqSum(dat, "DB_MDT"),
FreqSum(dat, "DB_BLF_passport"),
FreqSum(dat, "DB_none"),
FreqSum(dat, "DB_plan_or_pack_or_unsuitable"),
FreqSum(dat, "RSR_24hour_BPT"),
FreqSum(dat, "DB_BPT"),
FreqSum(dat, "DB_elements_and_RSR_24hour_BPT"),
FreqSum(dat, "DB_ticked_and_RSR_24hour_BPT"),
FreqSum(dat, "old_KPI_arrival_to_NIV"),
FreqSum(dat, "old_KPI_DB"))








flat.all <- flat
dat.save <- dat

# # # # now for countries

for (i in unique(dat.save$country)) {
  
  dat <- filter(dat.save, country == i)

  
  
  
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
  
  
  
  flat <- data.frame(country = i)
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                FreqSum(dat, "ethnicity"),
                mediSumRound(dat, "admissions", roundno = 0),
                mediSumRound(dat, "arrival_to_admission_hours", roundno = 0))
  
  
  
  # Now create the 2 hour table and bind it in
  
  
  
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
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # # bind these below
  # flat$Weekday_admit_N <- sum(margin.table(admisstimedow.N, 2)[1:5])
  # flat$Weekend_admit_N <- sum(margin.table(admisstimedow.N, 2)[6:7])
  
  # Then carry on as normal:
  
  
  flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))          
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days"),
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
                FreqSum(dat, "life_status"),
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                mediSumRound(dat, "admission_to_RSR_hours", roundno = 1),
                FreqSum(dat, "admission_to_RSR_24hour"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_target_range"),
                FreqSum(dat, "oxygen_admin"),
                FreqSum(dat, "oxygen_prescribed_if_administered"),
                FreqSum(dat, "NEWS2"),
                FreqSum(dat, "NEWS2_score_sev"),
                FreqSum(dat, "NIV"),
                FreqSum(dat, "NIV_location"),
                FreqSum(dat, "AHVF_ever"),
                FreqSum(dat, "AHVF_cont"),
                FreqSum(dat, "NIV_in_AHVF_cont"),
                FreqSum(dat, "AHVF_cont_to_NIV_cat"),
                FreqSum(dat, "spirometry"),
                FreqSum(dat, "obstruction"),
                mediSumRound(dat, "FEV1_perc_pred_value", 1),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_plan"),
                FreqSum(dat, "DB_pack"),
                FreqSum(dat, "DB_unsuitable_for_pack"),
                FreqSum(dat, "DB_oxygen_alert"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_PR"),
                FreqSum(dat, "DB_FU_72hour"),
                FreqSum(dat, "DB_MDT"),
                FreqSum(dat, "DB_BLF_passport"),
                FreqSum(dat, "DB_none"),
                FreqSum(dat, "DB_plan_or_pack_or_unsuitable"),
                FreqSum(dat, "RSR_24hour_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "DB_elements_and_RSR_24hour_BPT"),
                FreqSum(dat, "DB_ticked_and_RSR_24hour_BPT"),
                FreqSum(dat, "old_KPI_arrival_to_NIV"),
                FreqSum(dat, "old_KPI_DB"))
  
  
  
  
  
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
  flat$record_N <- nrow(dat)
  
  # Now we should be fine to get on with what we're doing.
  
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
  
  
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                FreqSum(dat, "ethnicity"),
                mediSumRound(dat, "admissions", roundno = 0),
                mediSumRound(dat, "arrival_to_admission_hours", roundno = 0))
  
  
  
  # Now create the 2 hour table and bind it in
  
  
  
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
  
  # admisstimedow.N.all <- admisstimedow.N
  
  
  # # bind these below
  # flat$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  # flat$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  # flat$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  # flat$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  # flat$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  # flat$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  # flat$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  # # bind these below
  # flat$Weekday_admit_N <- sum(margin.table(admisstimedow.N, 2)[1:5])
  # flat$Weekend_admit_N <- sum(margin.table(admisstimedow.N, 2)[6:7])
  
  # Then carry on as normal:
  
  
  flat <- cbind(flat, FreqSum(dat, "weekday_weekend_arrival"))          
  
  flat <- cbind(flat,
                mediSumRound(dat, "LOS_days"),
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
                FreqSum(dat, "life_status"),
                FreqSum(dat, "RSR"),
                mediSumRound(dat, "arrival_to_RSR_hours", roundno = 1),
                mediSumRound(dat, "admission_to_RSR_hours", roundno = 1),
                FreqSum(dat, "admission_to_RSR_24hour"),
                FreqSum(dat, "oxygen_prescribed"),
                FreqSum(dat, "oxygen_target_range"),
                FreqSum(dat, "oxygen_admin"),
                FreqSum(dat, "oxygen_prescribed_if_administered"),
                FreqSum(dat, "NEWS2"),
                FreqSum(dat, "NEWS2_score_sev"),
                FreqSum(dat, "NIV"),
                FreqSum(dat, "NIV_location"),
                FreqSum(dat, "AHVF_ever"),
                FreqSum(dat, "AHVF_cont"),
                FreqSum(dat, "NIV_in_AHVF_cont"),
                FreqSum(dat, "AHVF_cont_to_NIV_cat"),
                FreqSum(dat, "spirometry"),
                FreqSum(dat, "obstruction"),
                mediSumRound(dat, "FEV1_perc_pred_value", 1),
                FreqSum(dat, "discharge_day_of_week"),
                FreqSum(dat, "discharge_bundle"),
                FreqSum(dat, "DB_inhaler"),
                FreqSum(dat, "DB_maintenance"),
                FreqSum(dat, "DB_plan"),
                FreqSum(dat, "DB_pack"),
                FreqSum(dat, "DB_unsuitable_for_pack"),
                FreqSum(dat, "DB_oxygen_alert"),
                FreqSum(dat, "DB_smoke"),
                FreqSum(dat, "DB_PR"),
                FreqSum(dat, "DB_FU_72hour"),
                FreqSum(dat, "DB_MDT"),
                FreqSum(dat, "DB_BLF_passport"),
                FreqSum(dat, "DB_none"),
                FreqSum(dat, "DB_plan_or_pack_or_unsuitable"),
                FreqSum(dat, "RSR_24hour_BPT"),
                FreqSum(dat, "DB_BPT"),
                FreqSum(dat, "DB_elements_and_RSR_24hour_BPT"),
                FreqSum(dat, "DB_ticked_and_RSR_24hour_BPT"),
                FreqSum(dat, "old_KPI_arrival_to_NIV"),
                FreqSum(dat, "old_KPI_DB"))
  
  
  
  
  
  flat.all <- bind_rows(flat.all, flat)
  
}

dat <- dat.save

colnames(flat.all)

# change to appropriate order and remove unnecessary 'median admissions' columns and heat map columns,
flat.all <- flat.all %>% relocate(hosp_code:record_N, .before = country) %>% 
  select(-admissions_N, -admissions_median, -admissions_lo.quart, -admissions_hi.quart)


colnames(flat.all)


# and now we need to see how well the column names match up
library(openxlsx2)

data_desc <- read_xlsx("C:/Alex Harley/Audit_2023_onwards/2023-2024/Combined/2022-23 Complete column descriptions/COPD_analysis_plan_SOTN_2024.xlsx",
                       check.names = TRUE)

colnames(data_desc) <- data_desc[2, ]
data_desc <- data_desc[1, ]

# # # # # 
# NEW ADDITIONS 2024-07-12:
# FEV1_PRED CHANGED TO FEV1_PERC_PRED
# HAVE TO ADD IN ETHNICITY


# convert all columns to character and bind them together
flat.all <- flat.all %>% mutate_all(~as.character(.))
data_desc <- data_desc %>% mutate_all(~as.character(.))
clean <- bind_rows(flat.all, data_desc)


# Then, make it so the column description is the second row
clean <- clean[c(nrow(clean), 1:(nrow(clean)-1)), ]

# and add in the arrival descriptions now the column name has changed

colnames(clean)

# UPDATE THIS WHEN CODE IS ALL UPDATED
clean[1, 78:119] <- clean[1, 457:498]


clean <- clean %>% select(-Monday_0.00to7.59_admiss_n:-Sunday_16.00to23.59_admiss_perc)



ncol(clean)

colnames(clean)
# Then, get rid of the old columns that are no longer used:
clean <- clean %>% select(-ICS:-`BPT_all_Achieved_perc`)


# where are we missing column descriptions now?

columns_to_update <- clean %>% slice(1) %>% select_if(is.na(.))

# NOte - column name change from 'pred' to 'perc_pred'

columns_to_update %>% select(starts_with("ethnicity")) %>% ncol()

columns_to_update[1, ] <- c("Hospital code", "Trust code", 
                            "Integrated care system",
                            "Number of admissions", rep(NA, 36), # Ethnicity
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
                            "Number of patients who did not receive a respiratory specialist review",
                            "Percentage of patients who did not receive a respiratory specialist review",
                            rep(NA, 11), # RSR variables - Ellie can fill out
                            "Total number of patients not prescribed oxygen", # oxygen_prescribed_No_n, 
                            "Percentage of patients not prescribed oxygen", # oxygen_prescribed_No_perc, 
                            "Total number of patients not administered oxygen", # oxygen_admin_No_n, 
                            "Percentage of patients not administered oxygen", # oxygen_admin_No_perc,
                            "Number of patients administered oxygen",
                            NA, NA, NA, NA, # Oxygen columns
                            "Number of admissions", # "NEWS2_N",
                            "Number of patients with a NEWS2 score recorded", # "NEWS2_Score_recorded_in_hospital_notes_n",
                            "Percentage of patients with a NEWS2 score recorded", # "NEWS2_Score_recorded_in_hospital_notes_perc",
                            "Number of patients whose NEWS2 score was calculated post-hoc using physiological variables", # "NEWS2_Score_calculated_post-hoc_using_physiological_variables_n",
                            "Percentage of patients whose NEWS2 score was calculated post-hoc using physiological variables", # "NEWS2_Score_calculated_post-hoc_using_physiological_variables_perc",
                            "Number of patients whose NEWS2 score was not recorded and not able to be calculated", # "NEWS2_Score_not_recorded_or_able_to_be_calculated_n",
                            "Percentage of patients whose NEWS2 score was not recorded and not able to be calculated", # "NEWS2_Score_not_recorded_or_able_to_be_calculated_perc",
                            "Number of admissions", # "NEWS2_score_sev_N",
                            "Number of patients with a low or low/medium NEWS2 score", # "NEWS2_score_sev_Low/low-medium_n",
                            "Percentage of patients with a low or low/medium NEWS2 score", # "NEWS2_score_sev_Low/low-medium_perc",
                            "Number of patients with a medium NEWS2 score", # "NEWS2_score_sev_Medium_n",
                            "Percentage of patients with a medium NEWS2 score", # "NEWS2_score_sev_Medium_perc",
                            "Number of patients with a high NEWS2 score", # "NEWS2_score_sev_High_n",
                            "Percentage of patients with a high NEWS2 score", # "NEWS2_score_sev_High_perc",
                            "Number of patients whose NEWS2 score was not recorded and not able to be calculated", # "NEWS2_score_sev_Score_not_recorded_or_able_to_be_calculated_n",
                            "Percentage of patients whose NEWS2 score was not recorded and not able to be calculated", # "NEWS2_score_sev_Score_not_recorded_or_able_to_be_calculated_perc",
                            "Number of patients who did not receive non-invasive ventilation", # "NIV_No_n",
                            "Percentage of patients who did not receive non-invasive ventilation", # "NIV_No_perc",
                            "Number of patients who received non-invasive ventilation (NIV)", # "NIV_location_N",
                            "Number of patients who received NIV in the emergency department", # "NIV_location_Emergency_department_n",
                            "Percentage of patients who received NIV in the emergency department", # "NIV_location_Emergency_department_perc",
                            "Number of patients who received NIV in the general ward", # "NIV_location_General_ward_n",
                            "Percentage of patients who received NIV in the general ward", # "NIV_location_General_ward_perc",
                            "Number of patients who received NIV in the high dependency unit", # "NIV_location_High_dependency_unit_n",
                            "Percentage of patients who received NIV in the high dependency unit", # "NIV_location_High_dependency_unit_perc",
                            "Number of patients who received NIV in the ICU", # "NIV_location_ICU_n",
                            "Percentage of patients who received NIV in the ICU", # "NIV_location_ICU_perc",
                            "Number of patients who received NIV in the medical admissions unit", # "NIV_location_Medical_admissions_unit_n",
                            "Percentage of patients who received NIV in the medical admissions unit", # "NIV_location_Medical_admissions_unit_perc",
                            "Number of patients who received NIV in another location", # "NIV_location_Other_n",
                            "Percentage of patients who received NIV in another location", # "NIV_location_Other_perc",
                            "Number of patients who received NIV in the respiratory support unit", # "NIV_location_Respiratory_support_unit_n",
                            "Percentage of patients who received NIV in the respiratory support unit", # "NIV_location_Respiratory_support_unit_perc",
                            "Number of patients who received NIV in the respiratory ward", # "NIV_location_Respiratory_ward_n",
                            "Percentage of patients who received NIV in the respiratory ward", # "NIV_location_Respiratory_ward_perc",
                            "Number of admissions", # "AHVF_ever_N",
                            "Number of patients who did not receive a diagnosis of acidotic hypercapnic ventilatory failure at any point during admission", # "AHVF_ever_No_n",
                            "Percentage of patients who did not receive a diagnosis of acidotic hypercapnic ventilatory failure at any point during admission", # "AHVF_ever_No_n",
                            "Number of patients who received a diagnosis of acidotic hypercapnic ventilatory failure at any point during admission", # "AHVF_ever_No_n",
                            "Percentage of patients who received a diagnosis of acidotic hypercapnic ventilatory failure at any point during admission", # "AHVF_ever_No_n",
                            "Number of patients who received a diagnosis of acidotic hypercapnic ventilatory failure at any point during admission", # "AHVF_cont_N",
                            "Number of patients who no longer had a diagnosis of acidotic hypercapnic ventilatory failure after an hour of optimal treatment", # "AHVF_cont_Yes_n",
                            "Percentage of patients who no longer had a diagnosis of acidotic hypercapnic ventilatory failure after an hour of optimal treatment", # "AHVF_cont_Yes_n",
                            "Number of patients who received a continued diagnosis of acidotic hypercapnic ventilatory failure after an hour of optimal treatment", # "AHVF_cont_Yes_n",
                            "Percentage of patients who received a continued diagnosis of acidotic hypercapnic ventilatory failure after an hour of optimal treatment", # "AHVF_cont_Yes_n",
                            "Total number of patients who received a continued diagnosis of acidotic hypercapnic ventilatory failure after an hour of optimal treatment", # "NIV_in_AHVF_cont",
                            "Number of patients who did not receive non-invasive ventilation", # "NIV_in_AHVF_cont_No_n",
                            "Percentage of patients who did not receive non-invasive ventilation", # "NIV_in_AHVF_cont_No_perc",
                            "Number of patients who received non-invasive ventilation", # "NIV_in_AHVF_cont_Yes_n",
                            "Percentage of patients who received non-invasive ventilation", # "NIV_in_AHVF_cont_yes_perc",
                            "Number of patients who had a continued diagnosis of acidotic hypercapnic ventilatory failure (AHVF)", # "AHVF_cont_to_NIV_cat_N",
                            "Number of patients who received NIV within 2 hours of a continued diagnosis of AHVF (or within 2 hours of arrival if the continued diagnosis occurred before arrival to hospital), or received NIV before their continued diagnosis", # "AHVF_cont_to_NIV_cat_<2_hours_n",
                            "Percentage of patients who received NIV within 2 hours of a continued diagnosis of AHVF (or within 2 hours of arrival if the continued diagnosis occurred before arrival to hospital), or received NIV before their continued diagnosis", # "AHVF_cont_to_NIV_cat_<2_hours_perc",
                            "Number of patients who received NIV within 2-6 hours of continued diagnosis of AHVF (or arrival if continued diagnosis of AHVF occurs before arrival at hospital)", # "AHVF_cont_to_NIV_cat_2-24_hours_n",
                            "Percentage of patients who received NIV within 2-6 hours of continued diagnosis of AHVF (or arrival if continued diagnosis of AHVF occurs before arrival at hospital)", # "AHVF_cont_to_NIV_cat_2-24_hours_perc",
                            "Number of patients who received NIV over 6 hours after continued diagnosis of AHVF (or arrival if continued diagnosis of AHVF occurs before arrival at hospital)", # "AHVF_cont_to_NIV_cat_2-24_hours_n",
                            "Percentage of patients who received NIV over 6 hours after continued diagnosis of AHVF (or arrival if continued diagnosis of AHVF occurs before arrival at hospital)", # "AHVF_cont_to_NIV_cat_2-24_hours_perc",
                            "Number of patients who received a continued diagnosis of AHVF and NIV but did not have a date or time for NIV recorded", # "AHVF_cont_to_NIV_cat_NIV_given_but_date_or_time_not_recorded_n",
                            "Percentage of patients who received a continued diagnosis of AHVF and NIV but did not have a date or time for NIV recorded", # "AHVF_cont_to_NIV_cat_NIV_given_but_date_or_time_not_recorded_perc",
                            "Number of patients who received a continued diagnosis of AHVF but did not receive NIV", # "AHVF_cont_to_NIV_cat_NIV_given_but_date_or_time_not_recorded_n",
                            "Percentage of patients who received a continued diagnosis of AHVF but did not receive NIV", # "AHVF_cont_to_NIV_cat_NIV_given_but_date_or_time_not_recorded_perc",
                            "Number of patients with no spirometry measurements available", # "spirometry_No_n",
                            "Percentage of patients with no spirometry measurements available", # "spirometry_No_perc",
                            "Number of patients without obstruction (FEV1/FVC >= 0.7)", # "obstruction_No_(>=0.7)_n",
                            "Percentage of patients without obstruction (FEV1/FVC >= 0.7)", # "obstruction_No_(>=0.7)_perc",
                            "Number of patients with a value for %-predicted FEV1", NA, NA, NA,
                            "Number of patients provided with an inhaler technique check",
                            "Percentage of patients provided with an inhaler technique check",
                            "Number of patients with maintenance medication reviewed",
                            "Percentage of patients with maintenance medication reviewed",
                            "Total number of patients provided a self management plan or referred to community team for plan", 
                            "Percentage of patients provided a self management plan or referred to community team for plan", 
                            "Total number of patients provided an emergency drug pack or referred to community for plan", 
                            "Percentage of patients provided an emergency drug pack or referred to community for plan", 
                            "Total number of patients assessed as unsuitable for emergency drug pack", 
                            "Percentage of patients assessed as unsuitable for emergency drug pack",
                            "Total number of patients provided an oxygen alert card", 
                            "Percentage of patients provided an oxygen alert card",
                            "Total number of patients with tobacco dependency addressed",
                            "Percentage of patients with tobacco dependency addressed",
                            "Total number of patients assessed for suitability for pulmonary rehabilitation", 
                            "Percentage of patients assessed for suitability for pulmonary rehabilitation", 
                            "Total number of patients requested a follow up within 72 hours",
                            "Percentage of patients requested a follow up within 72 hours", 
                            "Total number of patients discussed at an MDT with a community or primary care team", 
                            "Percentage of patients discussed at an MDT with a community or primary care team",
                            "Total number of patients offered BLF passport", 
                            "Percentage of patients offered BLF passport",
                            "Total number of patients in receipt of no elements of good practice care", 
                            "Percentage of patients in receipt of no elements of good practice care",
                            "Total number of patients provided with a rescue plan or pack or deemed unsuitable for one",
                            "Percentage patients provided with a rescue plan or pack or deemed unsuitable for one",
                            "Total number of patients admitted", NA, NA, NA, NA, # RSR 24hour BPT
                            "Total number of patients discharged alive", NA, NA, NA, NA, # DB BPT
                            "Total number of patients discharged alive", NA, NA, NA, NA, # DB elements and 24 hours
                            "Total number of patients discharged alive", NA, NA, NA, NA, # DB ticked and 24 hours
                            "Total number of patients who received NIV", NA, NA, NA, NA, # old_KPI_arrival_to_NIV
                            "Total number of patients discharged alive", NA, NA, NA, NA)
                          #  rep("Where", 5)) # old_KPI_DB



clean$patch <- NA
clean$patch[1] <- 1
columns_to_update$patch <- 1
clean <- rows_patch(clean, columns_to_update, by = "patch")
clean <- clean %>% select(-patch)



write.csv(clean,
          "C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Analysis/Output/COPD_SCC_2023-2024_national_country_hospital_level_data.csv",
          row.names = FALSE)

