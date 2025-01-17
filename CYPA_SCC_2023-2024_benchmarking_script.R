# Benchmarking CA SCC 2019


library(tidyverse)


dat <- readRDS("C:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Data/tidyData/CYPA_SCC_2023-24_clean_data.RDS")


# Need to make all the variables binary for this
# Actually it's not too bad, only steroids 1 hour is still not binary.
summary(dat$steroids_24hr_prev)
summary(dat$steroids_1hour)

dat <- dat %>% mutate(BM_steroids_1hour = NA)
dat$BM_steroids_1hour[dat$steroids_1hour == "<1 hour"] <- 1
dat$BM_steroids_1hour[dat$steroids_1hour != "<1 hour"] <- 0
dat$BM_steroids_1hour[dat$steroids_24hr_prev == "Yes"] <- NA
dat$BM_steroids_1hour[dat$age < 6] <- NA

table(dat$BM_steroids_1hour, dat$age)
table(dat$BM_steroids_1hour, dat$steroids_24hr_prev)
colnames(dat)

# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(hosp_code) %>%
  summarise(hosp_name = first(hosp_name),
            trust_name = first(trust_name),
            cases.audited = n(),
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = (BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100,
            
            DB_smoke_denom = sum(!is.na(DB_smoke)),
            DB_smoke_nume = sum(DB_smoke, na.rm = TRUE),
            DB_smoke_perc = (DB_smoke_nume/DB_smoke_denom)*100,
            
            DB_parent_smoke_denom = sum(!is.na(DB_parent_smoke)),
            DB_parent_smoke_nume = sum(DB_parent_smoke, na.rm = TRUE),
            DB_parent_smoke_perc = (DB_parent_smoke_nume/DB_parent_smoke_denom)*100,
            
            DB_inhaler_denom = sum(!is.na(DB_inhaler)),
            DB_inhaler_nume = sum(DB_inhaler, na.rm = TRUE),
            DB_inhaler_perc = (DB_inhaler_nume/DB_inhaler_denom)*100,
            
            DB_PAAP_denom = sum(!is.na(DB_PAAP)),
            DB_PAAP_nume = sum(DB_PAAP, na.rm = TRUE),
            DB_PAAP_perc = (DB_PAAP_nume/DB_PAAP_denom)*100)
 

bmk
# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one



quartz1 <- matrix(data = NA, nrow = 3, ncol = 6)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$BM_steroids_1hour_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$DB_smoke_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$DB_parent_smoke_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$DB_inhaler_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$DB_PAAP_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "BM_steroids_1hour_perc", "DB_smoke_perc", "DB_parent_smoke_perc", 
                       "DB_inhaler_perc", "DB_PAAP_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 6)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$BM_steroids_1hour_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$DB_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$DB_parent_smoke_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$DB_inhaler_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$DB_PAAP_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "BM_steroids_1hour_perc", "DB_smoke_perc", "DB_parent_smoke_perc", 
                          "DB_inhaler_perc", "DB_PAAP_perc") 

quartz_fmt <- as.data.frame(quartz_fmt)

write.csv(quartz_fmt, file =
            "C:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Analysis/Output/CYPA_SCC_2023-2024_benchmarking_quartiles.csv",
          row.names = FALSE)


# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(BM_steroids_1hour_colour_end = ifelse(BM_steroids_1hour_denom < 5 | is.na(BM_steroids_1hour_denom) == TRUE, "Grey",
                                                   ifelse(BM_steroids_1hour_perc < quartz1$BM_steroids_1hour_perc[1], "Red",
                                                          ifelse(BM_steroids_1hour_perc >= quartz1$BM_steroids_1hour_perc[3], "Green", 
                                                                 "Yellow"))),
                      DB_smoke_colour_end = ifelse(DB_smoke_denom < 5 | is.na(DB_smoke_denom) == TRUE, "Grey",
                                                       ifelse(DB_smoke_perc < quartz1$DB_smoke_perc[1], "Red",
                                                              ifelse(DB_smoke_perc >= quartz1$DB_smoke_perc[3], "Green", 
                                                                     "Yellow"))),
                      DB_parent_smoke_colour_end = ifelse(DB_parent_smoke_denom < 5 | is.na(DB_parent_smoke_denom) == TRUE, "Grey",
                                                              ifelse(DB_parent_smoke_perc < quartz1$DB_parent_smoke_perc[1], "Red",
                                                                     ifelse(DB_parent_smoke_perc >= quartz1$DB_parent_smoke_perc[3], "Green", 
                                                                            "Yellow"))),
                      DB_inhaler_colour_end = ifelse(DB_inhaler_denom < 5 | is.na(DB_inhaler_denom) == TRUE, "Grey",
                                                           ifelse(DB_inhaler_perc < quartz1$DB_inhaler_perc[1], "Red",
                                                                  ifelse(DB_inhaler_perc >= quartz1$DB_inhaler_perc[3], "Green", 
                                                                         "Yellow"))),
                      DB_PAAP_colour_end = ifelse(DB_PAAP_denom < 5 | is.na(DB_PAAP_denom) == TRUE, "Grey",
                                                           ifelse(DB_PAAP_perc < quartz1$DB_PAAP_perc[1], "Red",
                                                                  ifelse(DB_PAAP_perc >= quartz1$DB_PAAP_perc[3], "Green", 
                                                                         "Yellow"))))





bmk <- bmk %>% add_column(BM_steroids_1hour_colour = bmk$BM_steroids_1hour_colour_end, .after = "BM_steroids_1hour_perc") %>% 
  add_column(DB_smoke_colour = bmk$DB_smoke_colour_end, .after = "DB_smoke_perc") %>%
  add_column(DB_parent_smoke_colour = bmk$DB_parent_smoke_colour_end, .after = "DB_parent_smoke_perc") %>% 
  add_column(DB_inhaler_colour = bmk$DB_inhaler_colour_end, .after = "DB_inhaler_perc") %>% 
  add_column(DB_PAAP_colour = bmk$DB_PAAP_colour_end, .after = "DB_PAAP_perc") %>%
  select(-BM_steroids_1hour_colour_end, -DB_smoke_colour_end, -DB_parent_smoke_colour_end, -DB_inhaler_colour_end,
         -DB_PAAP_colour_end)





bmk_all <- dat %>%
  summarise(hosp_name = "National",
            trust_name = "National", 
            cases.audited = n(),
            BM_steroids_1hour_denom = sum(!is.na(BM_steroids_1hour)),
            BM_steroids_1hour_nume = sum(BM_steroids_1hour, na.rm = TRUE),
            BM_steroids_1hour_perc = round((BM_steroids_1hour_nume/BM_steroids_1hour_denom)*100, 0),
            
            DB_smoke_denom = sum(!is.na(DB_smoke)),
            DB_smoke_nume = sum(DB_smoke, na.rm = TRUE),
            DB_smoke_perc = round((DB_smoke_nume/DB_smoke_denom)*100, 0),
            
            DB_parent_smoke_denom = sum(!is.na(DB_parent_smoke)),
            DB_parent_smoke_nume = sum(DB_parent_smoke, na.rm = TRUE),
            DB_parent_smoke_perc = round((DB_parent_smoke_nume/DB_parent_smoke_denom)*100, 0),
            
            DB_inhaler_denom = sum(!is.na(DB_inhaler)),
            DB_inhaler_nume = sum(DB_inhaler, na.rm = TRUE),
            DB_inhaler_perc = round((DB_inhaler_nume/DB_inhaler_denom)*100, 0),
            
            DB_PAAP_denom = sum(!is.na(DB_PAAP)),
            DB_PAAP_nume = sum(DB_PAAP, na.rm = TRUE),
            DB_PAAP_perc = round((DB_PAAP_nume/DB_PAAP_denom)*100, 0))

# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

write.csv(bmk, file =
"C:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Analysis/Output/CYPA_SCC_2023-2024_benchmarking.csv",
  row.names = FALSE)


# That's it for everything apart from the analyses!


# what if we change the CYPA benchmarking for smoking so that everyone has to be given smoking cessation advice if they smoke.

head(bmk)

colnames(bmk)

str(bmk$DB_smoke_perc)

is.nan(bmk$DB_smoke_perc)

bmk$DB_smoke_colour[bmk$DB_smoke_perc == "100"] <- "Green"
bmk$DB_smoke_colour[bmk$DB_smoke_perc != "100" & bmk$DB_smoke_denom != 0 & bmk$hosp_name != "National"] <- "Red"

table(bmk$DB_smoke_colour)
table(bmk$DB_smoke_denom)
colnames(bmk)

write.csv(bmk, file =
            "C:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Analysis/Output/CYPA_SCC_2023-2024_benchmarking_alt_for_smoking.csv",
          row.names = FALSE)
