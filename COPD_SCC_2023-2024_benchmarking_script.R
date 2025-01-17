# Benchmarking COPD SCC 2023-2024

library(tidyverse)
dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Data/tidyData/COPD_SCC_2023-2024_clean_data.RDS")

# Need to make all the variables binary for this
# Actually it's not too bad, only steroids 1 hour is still not binary.


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
bmk <- dat %>% dplyr::group_by(hosp_code) %>%
  summarise(hosp_name = first(hosp_name),
            trust_code = first(trust_code),
            trust_name = first(trust_name),
            region = first(region),
            country = first(country),
            cases.audited = n(),
            
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
            BM_DB_perc = (BM_DB_nume/BM_DB_denom)*100)
            
         



bmk
# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one



quartz1 <- matrix(data = NA, nrow = 3, ncol = 7)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$BM_NIV_2hour_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$BM_oxygen_sat_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$BM_spirometry_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$BM_smoke_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$BM_RSR_24hour_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 7] <- quantile(bmk$BM_DB_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "BM_NIV_2hour_perc", "BM_oxygen_sat_perc", "BM_spirometry_perc", "BM_smoke_perc", 
                       "BM_RSR_24hour_perc", "BM_DB_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 7)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$BM_NIV_2hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$BM_oxygen_sat_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$BM_spirometry_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$BM_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$BM_RSR_24hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 7] <- sprintf("%.0f", round(quantile(bmk$BM_DB_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "BM_NIV_2hour_perc", "BM_oxygen_sat_perc", "BM_spirometry_perc", "BM_smoke_perc", 
                          "BM_RSR_24hour_perc", "BM_DB_perc") 

quartz_fmt <- as.data.frame(quartz_fmt)



# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(BM_NIV_2hour_colour_end = ifelse(BM_NIV_2hour_denom < 5 | is.na(BM_NIV_2hour_denom) == TRUE, "Grey",
                                                       ifelse(BM_NIV_2hour_perc < quartz1$BM_NIV_2hour_perc[1], "Red",
                                                              ifelse(BM_NIV_2hour_perc >= quartz1$BM_NIV_2hour_perc[3], "Green", 
                                                                     "Yellow"))),
                      BM_oxygen_sat_colour_end = ifelse(BM_oxygen_sat_denom < 5 | is.na(BM_oxygen_sat_denom) == TRUE, "Grey",
                                                            ifelse(BM_oxygen_sat_perc < quartz1$BM_oxygen_sat_perc[1], "Red",
                                                                   ifelse(BM_oxygen_sat_perc >= quartz1$BM_oxygen_sat_perc[3], "Green", 
                                                                          "Yellow"))),
                      BM_spirometry_colour_end = ifelse(BM_spirometry_denom < 5 | is.na(BM_spirometry_denom) == TRUE, "Grey",
                                                            ifelse(BM_spirometry_perc < quartz1$BM_spirometry_perc[1], "Red",
                                                                   ifelse(BM_spirometry_perc >= quartz1$BM_spirometry_perc[3], "Green", 
                                                                          "Yellow"))),
                      BM_smoke_colour_end = ifelse(BM_smoke_denom < 5 | is.na(BM_smoke_denom) == TRUE, "Grey",
                                                   ifelse(BM_smoke_perc < quartz1$BM_smoke_perc[1], "Red",
                                                          ifelse(BM_smoke_perc >= quartz1$BM_smoke_perc[3], "Green", 
                                                                 "Yellow"))),
                      BM_RSR_24hour_colour_end = ifelse(BM_RSR_24hour_denom < 5 | is.na(BM_RSR_24hour_denom) == TRUE, "Grey",
                                                        ifelse(BM_RSR_24hour_perc < quartz1$BM_RSR_24hour_perc[1], "Red",
                                                               ifelse(BM_RSR_24hour_perc >= quartz1$BM_RSR_24hour_perc[3], "Green", 
                                                                      "Yellow"))),
                      BM_DB_colour_end = ifelse(BM_DB_denom < 5 | is.na(BM_DB_denom) == TRUE, "Grey",
                                                            ifelse(BM_DB_perc < quartz1$BM_DB_perc[1], "Red",
                                                                   ifelse(BM_DB_perc >= quartz1$BM_DB_perc[3], "Green", 
                                                                          "Yellow"))))
                    







bmk <- bmk %>% add_column(BM_NIV_2hour_colour = bmk$BM_NIV_2hour_colour_end, .after = "BM_NIV_2hour_perc") %>%
  add_column(BM_oxygen_sat_colour = bmk$BM_oxygen_sat_colour_end, .after = "BM_oxygen_sat_perc") %>% 
  add_column(BM_spirometry_colour = bmk$BM_spirometry_colour_end, .after = "BM_spirometry_perc") %>% 
  add_column(BM_smoke_colour = bmk$BM_smoke_colour_end, .after = "BM_smoke_perc") %>% 
  add_column(BM_RSR_24hour_colour = bmk$BM_RSR_24hour_colour_end, .after = "BM_RSR_24hour_perc") %>% 
  add_column(BM_DB_colour = bmk$BM_DB_colour_end, .after = "BM_DB_perc") %>%
  select(-BM_NIV_2hour_colour_end, -BM_RSR_24hour_colour_end, -BM_oxygen_sat_colour_end, -BM_DB_colour_end, -BM_smoke_colour_end, 
         -BM_spirometry_colour_end)





bmk_all <- dat %>%
  summarise(hosp_name = "National",
            region = "National",
            country = "All",
            trust_name = "National", 
            cases.audited = n(),
            
            BM_NIV_2hour_denom = sum(!is.na(BM_NIV_2hour)),
            BM_NIV_2hour_nume = sum(BM_NIV_2hour, na.rm = TRUE),
            BM_NIV_2hour_perc = round((BM_NIV_2hour_nume/BM_NIV_2hour_denom)*100, 0),
            
            BM_spirometry_denom = sum(!is.na(BM_spirometry)),
            BM_spirometry_nume = sum(BM_spirometry, na.rm = TRUE),
            BM_spirometry_perc = round((BM_spirometry_nume/BM_spirometry_denom)*100, 0),
            
            BM_oxygen_sat_denom = sum(!is.na(BM_oxygen_sat)),
            BM_oxygen_sat_nume = sum(BM_oxygen_sat, na.rm = TRUE),
            BM_oxygen_sat_perc = round((BM_oxygen_sat_nume/BM_oxygen_sat_denom)*100, 0),
            
            BM_smoke_denom = sum(!is.na(BM_smoke)),
            BM_smoke_nume = sum(BM_smoke, na.rm = TRUE),
            BM_smoke_perc = round((BM_smoke_nume/BM_smoke_denom)*100, 0),
            
            BM_RSR_24hour_denom = sum(!is.na(BM_RSR_24hour)),
            BM_RSR_24hour_nume = sum(BM_RSR_24hour, na.rm = TRUE),
            BM_RSR_24hour_perc = round((BM_RSR_24hour_nume/BM_RSR_24hour_denom)*100, 0),
            
            BM_DB_denom = sum(!is.na(BM_DB)),
            BM_DB_nume = sum(BM_DB, na.rm = TRUE),
            BM_DB_perc = round((BM_DB_nume/BM_DB_denom)*100, 0))

            
# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

# overall ones

write.csv(quartz_fmt, file =
    "C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Analysis/Output/COPD_SCC_2023-2024_benchmarking_quartiles.csv",
          row.names = FALSE)


# hospital level ones

write.csv(bmk, file =
              "C:/Alex Harley/Audit_2023_onwards/2023-2024/COPD/Analysis/Output/COPD_SCC_2023-2024_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!

