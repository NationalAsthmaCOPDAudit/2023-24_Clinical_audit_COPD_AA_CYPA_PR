
#--------------------------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b e n c h m a r k i n g    s c r i p t   #
#                                                                                            #
# Author: Alex                                                                               #
# Date created: 2024-09-17                                                                   #
#--------------------------------------------------------------------------------------------#


source("G:/Alex Harley/Audit_2023_onwards/My R functions/MySummary.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/lintestOR.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/tidyoutput.R")
source("G:/Alex Harley/Audit_2023_onwards/My R functions/checkSame.R")

library(tidyverse)
library(psych)
library(lme4)
library(finalfit)
library(readxl)



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
  
  # NOTE!!! Medians all rounded to 0dp
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- sprintf(paste0("%.", roundno, "f"), 
                     round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- sprintf(paste0("%.", roundno, "f"), 
                    round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno), nsmall = roundno)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, eng, scot, wal, all), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
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

# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  # print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  #  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  #  print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  #  print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  #  print(gen.A2)
  
  gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
    inner_join(gen.A2, by = "Var1")
  colnames(gen.table) <- c(varname, paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}




histnorm <- function(g) {
  
  h <- hist(g, breaks = 10, density = 10,
            col = "lightgray", xlab = "Accuracy", main = "Overall") 
  xfit <- seq(min(g, na.rm = TRUE), max(g, na.rm = TRUE), length = 40) 
  yfit <- dnorm(xfit, mean = mean(g, na.rm = TRUE), sd = sd(g, na.rm = TRUE)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  
  plot(h, ylim = c(0, max(yfit)))
  lines(xfit, yfit, col = "black", lwd = 2)
}

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





dat <- readRDS("G:/Alex Harley/Audit_2023_onwards/2023-2024/PR/Data/tidyData/PR_clinical_2023-24_cleaned.RDS")

# sort out regions
regions <- readxl::read_excel("G:/Alex Harley/Audit_2023_onwards/General UK data/Regional data 2024/PR regions and ICS.xlsm")
colnames(regions)

regions <- regions %>% select(org_code, region)

colnames(regions)

dat <- dat %>% select(-region)
dat <- left_join(dat, regions, by = "org_code")
dat$region <- factor(dat$region)

table(dat$region)



# just keep COPD records

dat <- dat %>% filter(primary_COPD == "COPD")

unique(dat$region)


# just so that everything works fine, we need to recode some binary variables as factors 

dat <- dat %>% mutate_at(.vars = vars(starts_with("impairments"),
                                      starts_with("secondary_conditions"),
                                      centre_based,
                                      starts_with("home_based")), 
                         .funs = ~factor(.))


dat <- dat %>% mutate_at(.vars = vars(EQ5D_mobility_init,
                                      EQ5D_self_care_init,
                                      EQ5D_activities_init,
                                      EQ5D_pain_init,
                                      EQ5D_depression_init,
                                      EQ5D_mobility_dis,
                                      EQ5D_self_care_dis,
                                      EQ5D_activities_dis,
                                      EQ5D_pain_dis,
                                      EQ5D_depression_dis),
                         .funs = ~factor(., levels = c("1", "2", "3", "4", "5")))

dat <- dat %>% mutate_at(.vars = vars(EQ5D_mobility_diff,
                                      EQ5D_self_care_diff,
                                      EQ5D_activities_diff,
                                      EQ5D_pain_diff,
                                      EQ5D_depression_diff),
                         .funs = ~factor(., levels = c("-4", "-3", "-2", "-1", "0", 
                                                       "1", "2", "3", "4")))



# glimpse(flat.all)
# 
# write.xlsx(dat, file, sheetName = "Sheet1", 
#            col.names = TRUE, row.names = FALSE)


# sprintf("%.1f", round((BM_prac_test_nume/BM_prac_test_denom)*100,1)),


# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(region) %>%
  summarise(cases.audited = n(),
            start_90_denom = sum(!is.na(BM_start_90)),
            start_90_nume = sum(BM_start_90, na.rm = TRUE),
            start_90_perc = (start_90_nume/start_90_denom)*100,
            
            BM_prac_test_denom = sum(!is.na(BM_prac_test)),
            BM_prac_test_nume = sum(BM_prac_test, na.rm = TRUE),
            BM_prac_test_perc = (BM_prac_test_nume/BM_prac_test_denom)*100,
            
            BM_discharge_assess_denom = sum(!is.na(BM_discharge_assess)),
            BM_discharge_assess_nume = sum(BM_discharge_assess, na.rm = TRUE),
            BM_discharge_assess_perc = (BM_discharge_assess_nume/BM_discharge_assess_denom)*100,
            
            BM_exercise_plan_denom = sum(!is.na(BM_exercise_plan)),
            BM_exercise_plan_nume = sum(BM_exercise_plan, na.rm = TRUE),
            BM_exercise_plan_perc = (BM_exercise_plan_nume/BM_exercise_plan_denom)*100,
            
            BM_MCID_exercise_denom = sum(!is.na(BM_MCID_exercise)),
            BM_MCID_exercise_nume = sum(BM_MCID_exercise, na.rm = TRUE),
            BM_MCID_exercise_perc = (BM_MCID_exercise_nume/BM_MCID_exercise_denom)*100,
            
            BM_MCID_CAT_CRQ_denom = sum(!is.na(BM_MCID_CAT_CRQ)),
            BM_MCID_CAT_CRQ_nume = sum(BM_MCID_CAT_CRQ, na.rm = TRUE),
            BM_MCID_CAT_CRQ_perc = (BM_MCID_CAT_CRQ_nume/BM_MCID_CAT_CRQ_denom)*100) %>%
  mutate_at(.vars = vars(ends_with("_perc")), .funs = ~round(., 1))

bmk



dat.save <- dat

#### For region...


flatlist <- vector(mode = "list", length = 0)

for (i in unique(dat.save$region)) {
  
  dat <- filter(dat.save, region == i)
  
  psychic <- psych::describe(dat, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(N = n, median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75)
  
  
  

  # I don't know what we're doing for case ascertainment so let's just leave that for now.
  
  # From here, we start off with all, and then re-run it for the individual countries.
  
  flat <- data.frame(region = i)

  #  flat <- data.frame(country = i, record_N = nrow(dat))
  
  
  flat <- cbind(flat, mediSumRound(dat, "age", 0), # Normal-ish
                FreqSum(dat, "gender"),
                FreqSum(dat, "IMD_quintile"),
                # FreqSum(dat, "IMD_quintile_Eng"),
                # FreqSum(dat, "IMD_quintile_Wal"),
                # FreqSum(dat, "IMD_quintile_Scot"),
                # FreqSum(dat, "anyIMD"),
                FreqSum(dat, "ethnicity"),
                FreqSum(dat, "impairments_none"), # 2023-24 impairments added
                FreqSum(dat, "impairments_anxiety"),
                FreqSum(dat, "impairments_depression"),
                FreqSum(dat, "impairments_severe_mental_illness"),
                FreqSum(dat, "impairments_dementia_mci"),
                FreqSum(dat, "impairments_other"),
                FreqSum(dat, "impairments_NR"),
                FreqSum(dat, "secondary_conditions_asthma"),
                FreqSum(dat, "secondary_conditions_bronchiectasis"),
                FreqSum(dat, "secondary_conditions_COPD"),
                FreqSum(dat, "secondary_conditions_CHF"),
                FreqSum(dat, "secondary_conditions_ILD"),
                FreqSum(dat, "secondary_conditions_covid"),
                FreqSum(dat, "secondary_conditions_other_CRD"),
                FreqSum(dat, "secondary_conditions_hypertension"),
                FreqSum(dat, "secondary_conditions_thoracic_surgery"),
                FreqSum(dat, "secondary_conditions_none"),
                FreqSum(dat, "secondary_conditions_NR"))
    
  flatlist[[i]] <- flat    

}

str(flatlist)

casemix <- bind_rows(flatlist) %>% arrange(region)

bmk <- left_join(bmk, casemix, by = "region")
bmk



write.csv(bmk, file =
 "G:/Alex Harley/Audit_2023_onwards/2023-2024/PR/Analysis/Output/REGIONAL_PR_clinical_2023-24_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!



