# CYPA deep dive

library(tidyverse)
library(finalfit)

dat <- readRDS("G:/Alex Harley/Audit_2023_onwards/2023-2024/CYPA/Data/tidyData/CYPA_SCC_2023-24_clean_data.RDS")



# how many received at least one IV med?
dat %>% filter(IV_med_none == 0) %>% nrow()

# within IV med patients, who received steroids within one hour or pre-arrival?


dat$steroids_1hour_or_prev <- "Not received"
dat$steroids_1hour_or_prev[dat$steroids_1hour == "<1 hour"] <- "Received within 1 hour or before arrival"
dat$steroids_1hour_or_prev[dat$steroids_24hr_prev == "Yes"] <- "Received within 1 hour or before arrival"
dat$steroids_1hour_or_prev <- factor(dat$steroids_1hour_or_prev)


# who received any follow-up (discharge bundle elements)
dat$follow_up_2_day_or_4_week <- "No"
dat$follow_up_2_day_or_4_week[dat$DB_FU_any == 1] <- "Yes"
dat$follow_up_2_day_or_4_week[is.na(dat$DB_FU_any)] <- NA
dat$follow_up_2_day_or_4_week <- factor(dat$follow_up_2_day_or_4_week)


# NOTE: THOSE WHO TRANSFERRED ARE NOT INCLUDED IN FOLLOW-UP ELEMENTS 
dat %>% filter(IV_med_none == 0) %>%
  finalfit::summary_factorlist(explanatory = c("steroids_1hour_or_prev", "crit_care_total", 
                                              "follow_up_2_day_or_4_week", "referred_for_FU"),
                               add_col_totals = TRUE) %>%
  rename(IV_medication_received = all)



# how many were seen in HDU or ICU?

dat %>% filter(crit_care_total != "No") %>% nrow()

# NOTE: THOSE WHO TRANSFERRED ARE NOT INCLUDED IN FOLLOW-UP OR DISCHARGE BUNDLE ELEMENTS 

dat %>% mutate(across(all_of(c("DB_PAAP", "DB_inhaler", "DB_smoke")),
                      ~fct_recode(factor(., levels = c("1", "0")), `No` = "0", `Yes` = "1"))) %>% 
  filter(crit_care_total != "No") %>% mutate(crit_care_total = as.character(crit_care_total)) %>%
  summary_factorlist(explanatory = c("steroids_1hour_or_prev", "crit_care_total", 
                                     "follow_up_2_day_or_4_week", "referred_for_FU",
                                     "DB_PAAP", "DB_inhaler", "DB_smoke"), add_col_totals = TRUE) %>%
  rename(seen_in_HDU_or_ICU = all)


