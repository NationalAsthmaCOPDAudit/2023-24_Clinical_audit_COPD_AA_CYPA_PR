# Benchmarking COPD SCC 2023-2024

library(tidyverse)
library(psych)
library(readxl)


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





dat <- readRDS("G:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Data/tidyData/COPD_SCC_2023-2024_clean_data.RDS")


# sort out regions
regions <- readxl::read_excel("G:/Alex Harley/Audit_2023_onwards/General UK data/Regional data 2024/COPD corrected regions and ICS.xlsm")
colnames(regions)

regions <- regions %>% select(hosp_code = `Hospital code`, region = Region)

colnames(regions)

dat <- dat %>% select(-region)
dat <- left_join(dat, regions, by = "hosp_code")
dat$region <- factor(dat$region)

table(dat$region)



# Need to make all the variables binary for this
# Actually it's not too bad, only steroids 1 hour is still not binary.
unique(dat$region)

summary(dat$arrival_to_NIV_cat)
dat <- dat %>% mutate(BM_NIV_2hour = NA)
dat$BM_NIV_2hour[dat$AHVF_cont_to_NIV_cat == "<2 hours"] <- 1
dat$BM_NIV_2hour[dat$AHVF_cont_to_NIV_cat != "<2 hours"] <- 0 
summary(dat$BM_NIV_2hour)
table(dat$BM_NIV_2hour)

table(dat$oxygen_target_range, dat$oxygen_prescribed, useNA = "ifany")
table(dat$oxygen_target_range, dat$oxygen_admin, useNA = "ifany")

dat <- dat %>% mutate(BM_oxygen_sat = NA)
dat$BM_oxygen_sat[dat$oxygen_target_range != "Target range not stipulated"] <- 1
dat$BM_oxygen_sat[dat$oxygen_target_range == "Target range not stipulated"] <- 0 
summary(dat$BM_oxygen_sat)
table(dat$BM_oxygen_sat, useNA = "ifany")

dat <- dat %>% mutate(BM_spirometry = 0)
dat$BM_spirometry[dat$spirometry == "Yes"] <- 1
table(dat$spirometry, dat$BM_spirometry, useNA = "ifany")

dat <- dat %>% mutate(BM_smoke = NA)
dat$BM_smoke[dat$DB_smoke == 1] <- 1
dat$BM_smoke[dat$DB_smoke == 0] <- 0
summary(dat$BM_smoke)


table(dat$RSR_24hour, dat$RSR, useNA = "ifany") # RSR_24hour is only out of those with RSR

table(dat$RSR_24hour_BPT, useNA = "ifany")

dat <- dat %>% mutate(BM_RSR_24hour = RSR_24hour_BPT)

# # and the KPI discharge bundle one 
# 
# dat <- dat %>% mutate(BM_DB = NA)
# dat$BM_DB[dat$BPT_DB == "Achieved"] <- 1
# dat$BM_DB[dat$BPT_DB == "Not achieved"] <- 0
# summary(dat$BM_DB)

# Smoking is taken out because it is included previously, and discharge bundle not required if elements hit.

# this made in the build script, and is the same
# dat <- dat %>% mutate(KPI_DB = ifelse(DB_inhaler == 1 & DB_maintenance == 1 &
#                                                DB_plan == 1 & (DB_pack == 1 | DB_unsuitable_for_pack == 1) &
#                                                (DB_smoke == 1 | is.na(DB_smoke)) & 
#                                                 DB_PR == 1 &
#                                                 DB_FU_72hour == 1, # & discharge_bundle == "Yes",
#                                                 1, 0))
# dat$KPI_DB[dat$life_status == "Died"] <- NA
# dat %>% select(KPI_DB, DB_BPT) %>% table(useNA = "ifany")
# it's the same

dat <- dat %>% mutate(BM_DB = as.numeric(DB_BPT))

table(dat$DB_BPT)
# columns we need:

# NIV_2hours
# oxygen_sat
# spirometry
# smoke
# RSR
# DB

# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(region) %>%
  summarise(cases.audited = n(),
            
            BM_NIV_2hour_denom = sum(!is.na(BM_NIV_2hour)),
            BM_NIV_2hour_nume = sum(BM_NIV_2hour, na.rm = TRUE),
            BM_NIV_2hour_perc = (BM_NIV_2hour_nume/BM_NIV_2hour_denom)*100,
            
            BM_oxygen_sat_denom = sum(!is.na(BM_oxygen_sat)),
            BM_oxygen_sat_nume = sum(BM_oxygen_sat, na.rm = TRUE),
            BM_oxygen_sat_perc = (BM_oxygen_sat_nume/BM_oxygen_sat_denom)*100,
            
            BM_spirometry_denom = sum(!is.na(BM_spirometry)),
            BM_spirometry_nume = sum(BM_spirometry, na.rm = TRUE),
            BM_spirometry_perc = (BM_spirometry_nume/BM_spirometry_denom)*100,
            
            BM_smoke_denom = sum(!is.na(BM_smoke)),
            BM_smoke_nume = sum(BM_smoke, na.rm = TRUE),
            BM_smoke_perc = (BM_smoke_nume/BM_smoke_denom)*100,
            
            BM_RSR_24hour_denom = sum(!is.na(BM_RSR_24hour)),
            BM_RSR_24hour_nume = sum(BM_RSR_24hour, na.rm = TRUE),
            BM_RSR_24hour_perc = (BM_RSR_24hour_nume/BM_RSR_24hour_denom)*100,
            
            BM_DB_denom = sum(!is.na(BM_DB)),
            BM_DB_nume = sum(BM_DB, na.rm = TRUE),
            BM_DB_perc = (BM_DB_nume/BM_DB_denom)*100) %>%
  mutate_at(.vars = vars(ends_with("_perc")), .funs = ~round(., 1))
            
         



bmk


dat <- dat %>% mutate_at(.vars = vars(starts_with("DB"), 
                                      starts_with("impairments"),
                                      starts_with("old_KPI")), 
                         .funs = ~factor(.)) #%>% str()



dat.save <- dat

flatlist <- vector(mode = "list", length = 0)

for (i in unique(dat.save$region)) {
  
  dat <- filter(dat.save, region == i)
  
  
  
  
  # Now we should be fine to get on with what we're doing.
  
  # Second, create our 'psychic' data frame for the medians
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  # We need to create a new row in psychic for the admissions IQR and the BPT hospital level analysis.
  

  
  
  
  flat <- data.frame(region = i)
  
  flat <- cbind(flat,
                
                mediSumRound(dat, "age", roundno = 0),
                
                
                
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                FreqSum(dat, "ethnicity"),
         
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
                FreqSum(dat, "NEWS2"),
                FreqSum(dat, "NEWS2_score_sev"),
                FreqSum(dat, "NIV"))
  
  
  
  
  
  flatlist[[i]] <- flat
  
}

casemix <- bind_rows(flatlist) %>% arrange(region)

bmk <- left_join(bmk, casemix, by = "region")
bmk



# hospital level ones

write.csv(bmk, file =
              "G:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Analysis/Output/REGIONAL_COPD_SCC_2023-2024_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!

