# Benchmarking AA SCC 2023-24



library(tidyverse)

dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Data/tidyData/AA_SCC_2023-24_clean_data.RDS")

# peak flow within 1 hour
table(dat$PEF_init_1hour, useNA = "ifany")
table(dat$PEF_init_recorded, useNA = "ifany")

dat <- dat %>% mutate(BM_PEF_1hour = NA)
dat$BM_PEF_1hour[dat$PEF_init_1hour == "<1 hour"] <- 1
dat$BM_PEF_1hour[dat$PEF_init_1hour != "<1 hour"] <- 0 
table(dat$BM_PEF_1hour, useNA = "ifany")

# resp review within 24 hours
table(dat$RSR_24hour_BPT)
dat <- dat %>% mutate(BM_RSR_24hour = RSR_24hour_BPT)

# We didn't have the steroids in previous 24 hours variable before so now we exclude those from this
# benchmarking variable because steroids are only recommended in those who were not given steroids in the 
# 24 hours previously

summary(dat$steroids_1hour)

# steroids within 1 hour
dat <- dat %>% mutate(BM_steroids_1hour = NA)
dat$BM_steroids_1hour[dat$steroids_1hour == "<1 hour"] <- 1
dat$BM_steroids_1hour[dat$steroids_1hour != "<1 hour"] <- 0
summary(dat$BM_steroids_1hour)

table(dat$steroids_1hour, dat$BM_steroids_1hour)
table(dat$steroids_1hour, dat$steroids_24hr_prev, useNA = "ifany")

# discharge bundle elements
dat <- dat %>% mutate(BM_DB = DB_BPT)
summary(dat$BM_DB)


# old discharge bundle elements
dat <- dat %>% mutate(BM_DB_old = ifelse(DB_inhaler == 1 & DB_maintenance == 1 & DB_PAAP == 1 &
                                        discharge_bundle == "Yes" & (DB_smoke == 1 | is.na(DB_smoke)),
                                      1, 0))
dat$BM_DB_old[dat$discharge_bundle == "Patient transferred to another hospital" | dat$life_status != "Alive"] <- NA


# smoking cessation
dat <- dat %>% mutate(BM_smoke = DB_smoke)


# inhaled steroids at discharge
dat$BM_inhaled_steroids_dis <- NA
dat$BM_inhaled_steroids_dis[dat$inhaled_steroids_dis == "Yes"] <- 1
dat$BM_inhaled_steroids_dis[dat$inhaled_steroids_dis == "No"] <- 0


# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(hosp_code) %>%
  summarise(hosp_name = first(hosp_name),
            trust_name = first(trust_name),
            cases.audited = n(),
            
            BM_PEF_1hour_denom = sum(!is.na(BM_PEF_1hour)),
            BM_PEF_1hour_nume = sum(BM_PEF_1hour, na.rm = TRUE),
            BM_PEF_1hour_perc = (BM_PEF_1hour_nume/BM_PEF_1hour_denom)*100,
            
            BM_RSR_24hour_denom = sum(!is.na(BM_RSR_24hour)),
            BM_RSR_24hour_nume = sum(BM_RSR_24hour, na.rm = TRUE),
            BM_RSR_24hour_perc = (BM_RSR_24hour_nume/BM_RSR_24hour_denom)*100,
            
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = (BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100,
            
            BM_DB_denom = sum(!is.na(BM_DB)),
            BM_DB_nume = sum(BM_DB, na.rm = TRUE),
            BM_DB_perc = (BM_DB_nume/BM_DB_denom)*100,
            
            BM_DB_old_denom = sum(!is.na(BM_DB_old)),
            BM_DB_old_nume = sum(BM_DB_old, na.rm = TRUE),
            BM_DB_old_perc = (BM_DB_old_nume/BM_DB_old_denom)*100,
            
            BM_smoke_denom = sum(!is.na(BM_smoke)),
            BM_smoke_nume = sum(BM_smoke, na.rm = TRUE),
            BM_smoke_perc = (BM_smoke_nume/BM_smoke_denom)*100,
            
            BM_inhaled_steroids_dis_denom = sum(!is.na(BM_inhaled_steroids_dis)),
            BM_inhaled_steroids_dis_nume = sum(BM_inhaled_steroids_dis, na.rm = TRUE),
            BM_inhaled_steroids_dis_perc = (BM_inhaled_steroids_dis_nume/BM_inhaled_steroids_dis_denom)*100)



bmk
# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one

colnames(bmk)

quartz1 <- matrix(data = NA, nrow = 3, ncol = 8)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$BM_PEF_1hour_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$BM_RSR_24hour_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$BM_steroids_1hour_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$BM_DB_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$BM_DB_old_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 7] <- quantile(bmk$BM_smoke_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 8] <- quantile(bmk$BM_inhaled_steroids_dis_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "BM_PEF_1hour_perc", "BM_RSR_24hour_perc", "BM_steroids_1hour_perc", 
                       "BM_DB_perc", "BM_DB_old_perc", "BM_smoke_perc", "BM_inhaled_steroids_dis_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 8)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$BM_PEF_1hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$BM_RSR_24hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$BM_steroids_1hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$BM_DB_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$BM_DB_old_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 7] <- sprintf("%.0f", round(quantile(bmk$BM_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 8] <- sprintf("%.0f", round(quantile(bmk$BM_inhaled_steroids_dis_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "BM_PEF_1hour_perc", "BM_RSR_24hour_perc", "BM_steroids_1hour_perc", "BM_DB_perc", 
                          "BM_DB_old_perc", "BM_smoke_perc", "BM_inhaled_steroids_dis")

quartz_fmt <- as.data.frame(quartz_fmt)

write.csv(quartz_fmt, file =
            "C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Analysis/Output/AA_SCC_2023-2024_benchmarking_quartiles.csv",
          row.names = FALSE)


# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(BM_PEF_1hour_colour_end = ifelse(BM_PEF_1hour_denom < 5 | is.na(BM_PEF_1hour_denom) == TRUE, "Grey",
                                                       ifelse(BM_PEF_1hour_perc < quartz1$BM_PEF_1hour_perc[1], "Red",
                                                              ifelse(BM_PEF_1hour_perc >= quartz1$BM_PEF_1hour_perc[3], "Green", 
                                                                     "Yellow"))),
                      BM_RSR_24hour_colour_end = ifelse(BM_RSR_24hour_denom < 5 | is.na(BM_RSR_24hour_denom) == TRUE, "Grey",
                                                              ifelse(BM_RSR_24hour_perc < quartz1$BM_RSR_24hour_perc[1], "Red",
                                                                     ifelse(BM_RSR_24hour_perc >= quartz1$BM_RSR_24hour_perc[3], "Green", 
                                                                            "Yellow"))),
                      BM_steroids_1hour_colour_end = ifelse(BM_steroids_1hour_denom < 5 | is.na(BM_steroids_1hour_denom) == TRUE, "Grey",
                                                            ifelse(BM_steroids_1hour_perc < quartz1$BM_steroids_1hour_perc[1], "Red",
                                                                   ifelse(BM_steroids_1hour_perc >= quartz1$BM_steroids_1hour_perc[3], "Green", 
                                                                          "Yellow"))),
                      BM_DB_colour_end = ifelse(BM_DB_denom < 5 | is.na(BM_DB_denom) == TRUE, "Grey",
                                                            ifelse(BM_DB_perc < quartz1$BM_DB_perc[1], "Red",
                                                                   ifelse(BM_DB_perc >= quartz1$BM_DB_perc[3], "Green", 
                                                                          "Yellow"))),
                      BM_DB_old_colour_end = ifelse(BM_DB_old_denom < 5 | is.na(BM_DB_old_denom) == TRUE, "Grey",
                                                ifelse(BM_DB_old_perc < quartz1$BM_DB_old_perc[1], "Red",
                                                       ifelse(BM_DB_old_perc >= quartz1$BM_DB_old_perc[3], "Green", 
                                                              "Yellow"))),
                      BM_smoke_colour_end = ifelse(BM_smoke_denom < 5 | is.na(BM_smoke_denom) == TRUE, "Grey",
                                                            ifelse(BM_smoke_perc < quartz1$BM_smoke_perc[1], "Red",
                                                                   ifelse(BM_smoke_perc >= quartz1$BM_smoke_perc[3], "Green", 
                                                                          "Yellow"))),
                      BM_inhaled_steroids_dis_colour_end = ifelse(BM_inhaled_steroids_dis_denom < 5 | is.na(BM_inhaled_steroids_dis_denom) == TRUE, "Grey",
                                                ifelse(BM_inhaled_steroids_dis_perc < quartz1$BM_inhaled_steroids_dis_perc[1], "Red",
                                                       ifelse(BM_inhaled_steroids_dis_perc >= quartz1$BM_inhaled_steroids_dis_perc[3], "Green", 
                                                              "Yellow"))))







bmk <- bmk %>% add_column(BM_PEF_1hour_colour = bmk$BM_PEF_1hour_colour_end, .after = "BM_PEF_1hour_perc") %>%
  add_column(BM_RSR_24hour_colour = bmk$BM_RSR_24hour_colour_end, .after = "BM_RSR_24hour_perc") %>% 
  add_column(BM_steroids_1hour_colour = bmk$BM_steroids_1hour_colour_end, .after = "BM_steroids_1hour_perc") %>% 
  add_column(BM_DB_colour = bmk$BM_DB_colour_end, .after = "BM_DB_perc") %>% 
  add_column(BM_DB_old_colour = bmk$BM_DB_old_colour_end, .after = "BM_DB_old_perc") %>% 
  add_column(BM_smoke_colour = bmk$BM_smoke_colour_end, .after = "BM_smoke_perc") %>% 
  add_column(BM_inhaled_steroids_dis_colour = bmk$BM_inhaled_steroids_dis_colour_end, .after = "BM_inhaled_steroids_dis_perc") %>% 
  select(-BM_PEF_1hour_colour_end, -BM_RSR_24hour_colour_end, -BM_steroids_1hour_colour_end, -BM_DB_colour_end, -BM_DB_old_colour_end, 
         -BM_smoke_colour_end, -BM_inhaled_steroids_dis_colour_end)





bmk_all <- dat %>%
  summarise(hosp_name = "National",
            trust_name = "National", 
            cases.audited = n(),
            
            BM_PEF_1hour_denom = sum(!is.na(BM_PEF_1hour)),
            BM_PEF_1hour_nume = sum(BM_PEF_1hour, na.rm = TRUE),
            BM_PEF_1hour_perc = round((BM_PEF_1hour_nume/BM_PEF_1hour_denom)*100, 0),
            
            BM_RSR_24hour_denom = sum(!is.na(BM_RSR_24hour)),
            BM_RSR_24hour_nume = sum(BM_RSR_24hour, na.rm = TRUE),
            BM_RSR_24hour_perc = round((BM_RSR_24hour_nume/BM_RSR_24hour_denom)*100, 0),
            
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = round((BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100, 0),
            
            BM_DB_denom = sum(!is.na(BM_DB)),
            BM_DB_nume = sum(BM_DB, na.rm = TRUE),
            BM_DB_perc = round((BM_DB_nume/BM_DB_denom)*100, 0),

            BM_DB_old_denom = sum(!is.na(BM_DB_old)),
            BM_DB_old_nume = sum(BM_DB_old, na.rm = TRUE),
            BM_DB_old_perc = round((BM_DB_old_nume/BM_DB_old_denom)*100, 0),
            
            BM_smoke_denom = sum(!is.na(BM_smoke)),
            BM_smoke_nume = sum(BM_smoke, na.rm = TRUE),
            BM_smoke_perc = round((BM_smoke_nume/BM_smoke_denom)*100, 0),
            
            BM_inhaled_steroids_dis_denom = sum(!is.na(BM_inhaled_steroids_dis)),
            BM_inhaled_steroids_dis_nume = sum(BM_inhaled_steroids_dis, na.rm = TRUE),
            BM_inhaled_steroids_dis_perc = round((BM_inhaled_steroids_dis_nume/BM_inhaled_steroids_dis_denom)*100, 0))

            
# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

write.csv(bmk, file =
   "C:/Alex Harley/Audit_2023_onwards/2023-2024/AA/Analysis/Output/AA_SCC_2023-2024_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!

