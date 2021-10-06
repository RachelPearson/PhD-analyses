#-------------------------------------------------------------------------------#
# Chapter 7 script - predictors for returning to court with a subsequent infant #
#-------------------------------------------------------------------------------#

# packages ----
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(janitor)
library(purrr)
library(broom)
library(ggplot2)
library(naniar)
library(readxl)
library(readr)
library(tableone)
library(rms)



#-----------------------------------------------------------------------------------------------------------------------#
# 1. Get Cafcass Person IDs for women with index proceedings 2008/9 to 2017/18 in SLaM catchment (from Cafcass) ----
#-----------------------------------------------------------------------------------------------------------------------#

source("R scripts/creating cohorts.R")

  
# index case in SLaM catchment AND index case 2008/8 - 2017/18 (i.e. at least 1yr look-back and 1 yr follow-up):
study_cohort_ids <- full_cohort %>%
  dplyr::filter(any_case_slam4 == 1) %>%
  group_by(new_person_id) %>%
  mutate(
    cohort_fl = ifelse(case_seq == 1 & case_fyear >= 2008 & case_fyear <= 2017 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark"), 1, 0),
    cohort = max(cohort_fl)
  ) %>%
  ungroup() %>%
  dplyr::filter(cohort == 1) %>%
  dplyr::select(person_id, dup_person_id, new_person_id) %>%
  gather(key = "id_type", value = "id", person_id:new_person_id) %>%
  dplyr::filter(!is.na(id)) %>%
  distinct(id) %>%
  unlist(use.names = FALSE)

  

#-----------------------------------------------------------------------------------------------------------------------#
# 2. Get Cafcass Person IDs for women with index proceedings 2008/9 to 2017/18 in SLaM catchment (from CRIS-Cafcass) ----
#-----------------------------------------------------------------------------------------------------------------------#
# list of CRIS Cafcass person IDs:
cris_person_ids <- purrr::map(c("1_cafcass_cris_dataset_main.xlsx", 
                                "2_cafcass_cris_dataset_diagnoses.xlsx", 
                                "3_cafcass_cris_dataset_services.xlsx"),
                              ~read_xlsx(paste0("Data/Cafcass cohort + CRIS data/", .x),
                                         na= c("", "NA", "NULL"))) %>%
  purrr::reduce(left_join, by = "person_id") %>%
  dplyr::filter(person_id %in% study_cohort_ids) %>%
  distinct(person_id) %>%
  unlist(use.names = FALSE) 


# Cafcass Person ID spine to link cris and cafcass data together ----
id_link <- full_cohort %>%
  dplyr::filter(new_person_id %in% study_cohort_ids) %>%
  dplyr::select(person_id_1 = new_person_id, person_id_2 = person_id) %>%
  mutate(new_person_id = person_id_1) %>%
  pivot_longer(cols = starts_with("person_id_"), names_to = "id_type", values_to = "id") %>%
  distinct(new_person_id, id)


#--------------------------------#
# 3. CAFCASS DATA ON RETURNS ----
#--------------------------------#

# Tackling consolidated cases: 
# if case has unborn child <= -37 weeks old (i.e. term) at case start date then split the case into two with next start at birth of unborn child.

# identify consolidated cases:
consolidated_cases <- full_cohort %>%
  dplyr::select(new_person_id, match_status, first_case_date, case_id, old_case_yngst_age = new_case_yngst_age, old_case_start_date = case_start_date) %>%
  dplyr::filter(new_person_id %in% study_cohort_ids) %>%
  mutate_if(is.POSIXct, as.Date) %>%
  dplyr::filter(old_case_yngst_age <= (-37*7)/365.25) %>%
  mutate(yngst_age = 0,
         case_start_date = old_case_start_date - old_case_yngst_age*365.25) # set case youngest to 0 and case date to their date of birth


# Get new 'yngst age' for these consolidated cases (i.e. youngest child  in case >= -37 weeks old):
new_child_ages <- read_xlsx("Data/Cafcass cohort + match status/child_ages_alt.xlsx",
                        na = c("", 'NULL')) %>% 
  distinct() %>%
  left_join(id_link, by = c("mother_id" = "id")) %>%
  right_join(distinct(consolidated_cases, new_person_id, case_id, old_case_start_date), by = c("case_id", "mother_id" = "new_person_id")) %>%
  mutate(
    dob_imp = case_when(dob_m == 2 ~ paste(dob_y, dob_m, 28, sep = "-"),
                        dob_m %in% c(4, 6, 9, 11) ~ paste(dob_y, dob_m, 30, sep = "-"),
                        dob_m %in% c(1, 3, 5, 7, 8, 10, 12) ~ paste(dob_y, dob_m, 31, sep = "-")),
    dob_imp = as.Date(dob_imp, format = "%Y-%m-%d"),
    child_age = difftime(old_case_start_date, dob_imp, units = "days")/365.25,
    consolidated_case_fl = 1
    ) %>%
  arrange(case_id, mother_id, child_age) %>%
  group_by(case_id, mother_id) %>%
  slice(2) %>%
  ungroup() %>% # some are still < 0 because of guessing day of birth as end of month
  dplyr::select(case_id, new_child_age = child_age, consolidated_case_fl)



# because only month/year child date of birth available, make day last day of month (i.e. assume child is younger in some cases): 
new_return_yngst <- read_xlsx("Data/Cafcass cohort + match status/child_ages_alt.xlsx",
                              na = c("", 'NULL')) %>% 
  filter(!is.na(mother_id) & mother_id %in% id_link$id) %>%
  distinct() %>%
  mutate(
    dob_imp = case_when(dob_m == 2 ~ paste(dob_y, dob_m, 28, sep = "-"),
                        dob_m %in% c(4, 6, 9, 11) ~ paste(dob_y, dob_m, 30, sep = "-"),
                        dob_m %in% c(1, 3, 5, 7, 8, 10, 12) ~ paste(dob_y, dob_m, 31, sep = "-")),
    dob_imp = as.Date(dob_imp, format = "%Y-%m-%d")
    ) %>%
  left_join(id_link, by = c("mother_id" = "id")) %>%
  arrange(case_id, mother_id, desc(dob_imp)) %>%
  group_by(case_id, new_person_id) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(new_person_id, case_id, return_yngst_dob_imp = dob_imp)
  
  
  
# identify all women with a return, taking into account the unconsolidated cases:
any_return_dat <- full_cohort %>%
  dplyr::select(new_person_id, match_status, case_num, first_case_date, case_seq, case_id, case_start_date, new_case_yngst_age) %>%
  dplyr::filter(new_person_id %in% study_cohort_ids) %>%
  mutate_if(is.POSIXct, as.Date) %>%
  
  # add in new case youngest ages:
  left_join(new_return_yngst, by = c("case_id", "new_person_id")) %>%
  mutate(case_yngst_age = as.numeric(difftime(case_start_date, return_yngst_dob_imp, units = "days"))/365.25) %>%
  
  
  # merge & harmonize unconsolidated cases:
  left_join(new_child_ages, by = "case_id") %>%
  mutate(new_case_yngst_age = ifelse(!is.na(consolidated_case_fl), new_child_age, case_yngst_age)) %>%
  bind_rows(dplyr::select(consolidated_cases, new_person_id, match_status, first_case_date, case_id, new_case_yngst_age = yngst_age, case_start_date)) %>%
  dplyr::select(-consolidated_case_fl, -new_child_age) %>%
  
  # reorder & recalculate case_seq and case_num variables:
  arrange(new_person_id, case_start_date) %>%
  group_by(new_person_id) %>%
  mutate(case_num = n(),
         case_seq = row_number()) %>%
  ungroup() %>%
  
  dplyr::filter(case_num > 1 & case_seq >= 2) %>% # select only women with at least 1 return and remove index case record
  group_by(new_person_id) %>%
  mutate(
    
    # time from index case to return:
    time_to_next        = difftime(case_start_date, first_case_date, units = "days"),
    time_to_next_yr     = time_to_next/365.25,
    
    # time from prev. case (if more than one return)
    time_from_last      = ifelse(case_seq > 2, difftime(case_start_date, lag(case_start_date), units = "days"), time_to_next),
    time_from_last_yr   = time_from_last/365.25,
    
    # outcomes:
    outcome_main        = ifelse(case_seq >= 2 & (new_case_yngst_age) <= time_to_next_yr, # new child
                                1,
                                0),
    outcome_main_infant = ifelse(case_seq >= 2 & (new_case_yngst_age) <= time_to_next_yr & new_case_yngst_age < 1, # new child (infants only)
                                 1,
                                 0),
    outcome_secondary   = ifelse(case_seq >= 2 & (new_case_yngst_age) > time_to_next_yr, # existing child
                                 1,
                                 0)
  ) %>%
  ungroup() %>%
  dplyr::filter(outcome_main == 1 | outcome_main_infant == 1 | outcome_secondary == 1) %>%
  group_by(new_person_id) %>%
  mutate(
    sum_outcome = sum(outcome_main == 1, na.rm = TRUE),
    sum_outcome_infant = sum(outcome_main_infant == 1, na.rm = TRUE),
    sum_outcome_secondary = sum(outcome_secondary == 1, na.rm = TRUE),
    any_return = case_when(sum_outcome >= 1 & sum_outcome_secondary == 0 ~ "new child",
                           sum_outcome == 0 & sum_outcome_secondary >= 1 ~ "prev child",
                           sum_outcome >= 1 & sum_outcome_secondary >= 1 ~ "both")
  ) %>% 
  ungroup() %>%
  dplyr::select(new_person_id, 
                match_status,
                case_seq, 
                first_case_date, 
                case_start_date, 
                return_yngst = new_case_yngst_age,
                starts_with("time_"),
                contains("outcome"),
                any_return) %>%
  arrange(new_person_id, case_seq, case_start_date)

## how many women return and what types of returns:
with(distinct(any_return_dat, new_person_id, .keep_all = TRUE),
     table(any_return)
)


# how many new infant returns occur within 37wks of index proceedings?
sum(filter(any_return_dat, outcome_main_infant == 1)$time_to_next <= 7*37)
View(filter(any_return_dat, outcome_main_infant == 1 & time_to_next <= 7*37))

# take just the first new infant return per woman ----
first_new_infant_return <- any_return_dat %>%
  dplyr::filter(outcome_main_infant == 1 & time_to_next >= 7*37) %>%
  arrange(new_person_id, case_seq) %>%
  group_by(new_person_id) %>%
  dplyr::filter(row_number() == 1) %>%
  ungroup()


# sensitivity outcome ----
first_new_infant_return_sens <- any_return_dat %>%
  dplyr::filter(outcome_main == 1 & time_to_next >= 7*37) %>%
  arrange(new_person_id, case_seq) %>%
  group_by(new_person_id) %>%
  dplyr::filter(row_number() == 1) %>%
  ungroup()


n_outcome_events <- distinct(any_return_dat, new_person_id, .keep_all = TRUE) %>%
  filter(any_return != "prev child") %>%
  nrow()

n_outcome_events_infant <- any_return_dat %>%
  dplyr::filter(outcome_main_infant == 1) %>%
  distinct(new_person_id) %>%
  nrow()

n_outcome_events_infant_excl <- n_outcome_events_infant - nrow(first_new_infant_return)

write_csv(data.frame(
  row_name = c("number outcome events",
               "number outcome events infant",
               "number outcome events infants - (return < 37 wks)"),
  n        = c(n_outcome_events,
               n_outcome_events_infant,
               n_outcome_events_infant_excl)),
  "Outputs/Core4/returns to court/number of outcome events.csv"
)


#----------------------------------#
# 4. EXPLORATORY (cafcass only) -----
#----------------------------------#

# dist of age of returning to court with a new child:
with(distinct(any_return_dat, new_person_id, .keep_all = TRUE),
     table(any_return != "prev child", sum_outcome_infant>= 1))


# Distribution of child age across all returns ----
outcome_total <- table(any_return_dat$outcome_main)

any_return_dat %>%
  filter(match_status == 1) %>%
  mutate(outcome = ifelse(outcome_main == 0, 
                          paste0("Previous child (n = ", outcome_total[[1]], ")"), 
                          paste0("New child (n = ", outcome_total[[2]], ")")
                          ),
         age = floor(return_yngst)) %>%
  group_by(outcome, outcome_main, age) %>%
  summarise(count =  n()) %>%
  ungroup() %>%
  mutate(count_cens = ifelse(count < 10, 10, count),
         percent = ifelse(outcome_main == 0, count_cens/outcome_total[[1]], count_cens/outcome_total[[2]]),
         label = ifelse(count < 10, "<10", as.character(count))) %>%
ggplot(aes(x = age, y = percent)) +
  facet_wrap(~outcome, nrow = 2) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  scale_y_continuous("", limits = c(0,1), labels = scales::percent) +
  scale_x_continuous("\nAge (years) of youngest child involved\nin a subsequent set of proceedings",
                     breaks = c(-1, seq(0,17,1)),
                     expand = c(0.01,0.01)) +
  theme(plot.title = element_text(size = 14),
        axis.text  = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))

ggsave("Outputs/Core4/returns to court/distribution of youngest child age by type of return.png",
       units = "mm", height = 150, width = 200)


# how many women have a new infant return ----
with(distinct(any_return_dat, new_person_id, .keep_all = TRUE) %>% filter(any_return != "prev child"),
     prop.table(table(sum_outcome_infant >= 1)), margin = 1)

# age dist of infants ----
dplyr::filter(any_return_dat, outcome_main_infant == 1) %>%
  mutate(age_months = floor(return_yngst*12)) %>%
  group_by(age_months) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(count_cens = ifelse(count < 10, 10, count),
         percent = count_cens/sum(count),
         label = ifelse(count < 10, "<10", as.character(count))) %>%
ggplot(aes(x = age_months, y = percent)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  scale_y_continuous("", limits = c(0,0.8), labels = scales::percent) +
  scale_x_continuous("\nAge (months) of youngest child involved\nin a subsequent set of proceedings",
                     breaks = seq(-8, 11, 1),
                     labels = seq(-8, 11, 1)) +
  theme(plot.title = element_text(size = 14),
        axis.text  = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))

ggsave("Outputs/Core4/returns to court/distribution of youngest child age (infants).png",
       units = "mm", height = 100, width = 200)



# Returns to court stratified by match status ----
n_match <- 
  with(distinct(full_cohort, new_person_id, match_status) %>% 
          filter(new_person_id %in% study_cohort_ids),
       table(match_status))

n_outcome_nolink   <- table(first_new_infant_return$match_status)[[1]]
pct_outcome_nolink <- table(first_new_infant_return$match_status)[[1]]*100/n_match[[1]]

n_outcome_link   <- table(first_new_infant_return$match_status)[[2]]
pct_outcome_link <- table(first_new_infant_return$match_status)[[2]]*100/n_match[[2]]

write_csv(data.frame(match_status = c("Non link", "Link"),
                     number       = c(n_outcome_nolink, n_outcome_link),
                     percent      = c(pct_outcome_nolink, pct_outcome_link)),
          "Outputs/Core4/returns to court/outcome_by_linkage.csv")





#----------------------------------#
# 5. BUILD STUDY DATA SET ----
#----------------------------------#

# get youngest child legal statuses at index case:
child_legal_status <- read_xlsx("Data/Cafcass cohort + match status/child_legal_status.xlsx",
                                na = c("", 'NULL')) %>% 
  distinct() %>%
  filter(!is.na(mother_id)) %>%
  left_join(dplyr::select(new_child_ages, case_id, consolidated_case_fl), by = "case_id") %>%
  arrange(case_id, mother_id, desc(dob_y), desc(dob_m)) %>%
  group_by(case_id, mother_id) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter((is.na(consolidated_case_fl) & row_num == 1) | (!is.na(consolidated_case_fl) & row_num == 2)) %>%
  left_join(id_link, by = c("mother_id" = "id"))


# source CRIS data sets:
source("R scripts/return to court - CRIS data function.r")

# combine all data:
study_dat <- id_link %>%
  
  # Join index case data:
  left_join(full_cohort %>% dplyr::filter(case_seq == 1) %>% dplyr::select(new_person_id, 
                                                                    la_name,
                                                                    match_status,
                                                                    first_case_date, 
                                                                    case_id, 
                                                                    cafcass_ethnicity = new_eth_cat, 
                                                                    imd = address_imd_quintile, 
                                                                    cafcass_dob = dob_trunc,
                                                                    has_father_party,
                                                                    child_count,
                                                                    yngst_age = new_case_yngst_age,
                                                                    has_epo,
                                                                    has_final_order,
                                                                    starts_with("any_final_order")),
            by = "new_person_id") %>%
  
  # get youngest at index final legal order:
  left_join(dplyr::select(child_legal_status, case_id, new_person_id, starts_with("final")), by = c("case_id", "new_person_id")) %>%

  # address consolidated cases:
  left_join(new_child_ages, by = "case_id") %>%
  mutate(yngst_age = ifelse(!is.na(consolidated_case_fl), new_child_age, yngst_age)) %>%
  
  # Join on CRIS data:
  left_join(cris_dat, by = c("id" = "person_id")) %>%
  left_join(inpt_dat, by = c("id" = "person_id")) %>%
  left_join(diagnoses_dat_sens, by = c("id" = "person_id")) %>%
  left_join(ndtms_sens %>% rename_all(~sub("sens_", "", .x)), by = c("id" = "person_id")) %>%
  
  # Join on returns data:
  left_join(
    dplyr::select(
      first_new_infant_return, 
      new_person_id,
      return_case_date = case_start_date,
      time_to_next,
      return_yngst,
      outcome_main_infant
    ), by = "new_person_id"
  ) %>%
  
  # harmonize data between Cafcass & CRIS:
  dplyr::filter(match_status == 1 & !is.na(any_prior)) %>%
  mutate(
    yngst_age         = ifelse(!is.na(consolidated_case_fl), new_child_age, yngst_age),
    ethnicity_combo   = ifelse(match_status == 0, tools::toTitleCase(as.character(cafcass_ethnicity)), as.character(ethnicity)),
    ethnicity_combo   = ifelse(ethnicity_combo %in% c("Unknown", "Missing"), NA, ethnicity_combo),
    ethnicity_combo   = ifelse(ethnicity_combo == "Asian or Asian British", "Other", ethnicity_combo),
    dob_combo         = if_else(match_status == 0, as.Date(cafcass_dob), as.Date(study_dob)),
    index_year        = as.numeric(ifelse(month(first_case_date) < 4, year(first_case_date) - 1, year(first_case_date))) - 2008,
    age_at_index      = as.numeric(difftime(first_case_date, dob_combo, units = "days"))/365.25,
    study_end_date    = case_when(is.na(outcome_main_infant) & !is.na(dod_trunc) ~ as.Date(dod_trunc),
                                  is.na(outcome_main_infant) &  is.na(dod_trunc) ~ as.Date("2019-03-31")),
    time_to_next      = ifelse(is.na(time_to_next), as.numeric(difftime(study_end_date, first_case_date, units = "days")), time_to_next),
    imd               = as.factor(ifelse(imd %in% c(4,5), "4 or 5", imd)),
    ethnicity_combo   = as.factor(ethnicity_combo),
    time_to_next_cens = case_when((is.na(dod_trunc) | (!is.na(dod_trunc) & dod_trunc > as.Date("2019-03-31"))) & is.na(return_yngst) ~ as.numeric(difftime(as.Date("2019-03-31"), first_case_date, units = "days")),
                                  (!is.na(dod_trunc) & dod_trunc <= as.Date("2019-03-31")) & is.na(return_yngst) ~  as.numeric(difftime(dod_trunc, first_case_date, units = "days")),
                                  TRUE ~ time_to_next),
    final_order_grp   = ifelse(final_order_grp %in% c("epo", "co/sao"), "epo/co/sao", final_order_grp),
    died              = ifelse(!is.na(dod_trunc), 1, 0)
  ) %>%

  # Fill missing with 0 for binary variables:
  mutate_at( 
    vars(starts_with("sum_"),
         ends_with("_dx"),
         ends_with("_su"),
         "outcome_main_infant"),
    list(~ifelse(is.na(.), 0, .))
  ) %>%
  
  # new derived CRIS variables:
  mutate(
    fup_time = case_when( 
      !is.na(dod_trunc) & dod_trunc <  as.Date("2019-03-31") ~ as.numeric(difftime(dod_trunc, first_case_start, units = "days"))/365.25,
      (!is.na(dod_trunc) & dod_trunc >= as.Date("2019-03-31")) | is.na(dod_trunc) ~ as.numeric(difftime(as.Date("2019-03-31"), first_case_start, units = "days"))/365.25
      ),
    child_count_cat = ifelse(child_count >= 4, "4+", as.character(child_count)),
    index_year_cat  = case_when(index_year %in% c(0,1) ~ "2008/9-2009/10",
                                index_year %in% c(2,3) ~ "2010/11-2011/12",
                                index_year %in% c(4,5) ~ "2012/13-2013/14",
                                index_year %in% c(6,7) ~ "2014/15-2015/16",
                                index_year %in% c(8,9) ~ "2016/17-2017/18"),
    any_inpt = ifelse(sum_inpt_sens > 0, 1, 0),
    sum_dx = rowSums(select(., ends_with("_dx"))),
    sum_F1_dx = rowSums(select(., ends_with("_dx"), -F1_dx)),
    sum_su = rowSums(select(., ends_with("_su"))),
    sum_su_dx = rowSums(select(., ends_with("_dx"), ends_with("_su"))),
    sum_dx_cat = case_when(sum_dx == 1 ~ "1 diag",
                           sum_dx %in% c(2,3) ~ "2-3 diags",
                           sum_dx >= 4 ~ "4+ diags",
                           sum_dx == 0 ~ "0 diags"),
    sum_su_cat = case_when(sum_su == 1 ~ "1 substance",
                           sum_su >= 2 ~ "2+ substances",
                           sum_su == 0 ~ "0 substances"),
    any_dx = ifelse(sum_dx >= 1, 1, 0),
    any_su = ifelse(sum_su >= 1 | F1_dx == 1, 1, 0),
    any_su_dx = ifelse(sum_su_dx >= 1, 1, 0),
    F8_9_dx = ifelse(F9_dx == 1 | F8_dx == 1, 1, 0),
    dual_dx = ifelse(any_su == 1 & sum_F1_dx >= 1, 1 , 0),
    other_su = ifelse(other_su == 1 | ecstasy_su == 1 | amphetamines_su == 1 | hallucinogens_su == 1 | benzodiazepines_su == 1, 1, 0),
    any_prior_sens = ifelse(as.numeric(difftime(as.Date(first_case_date), as.Date(earliest_slam, origin = "1970-01-01"), units = "days")) >= -91, 1, 0),
    any_prior_sens = ifelse(is.na(any_prior_sens), 0, any_prior_sens)
  )

range(study_dat$time_to_next_cens)
range(study_dat[study_dat$outcome_main_infant == 1,]$time_to_next_cens)

# how many experiencing outcome:

n_primary_outcome <- filter(first_new_infant_return, new_person_id %in% filter(study_dat, any_prior_sens == 1)$new_person_id) %>% distinct(new_person_id) %>% nrow()
n_secondary_1     <- filter(first_new_infant_return_sens1, new_person_id %in% filter(study_dat, any_prior_sens == 1)$new_person_id) %>% distinct(new_person_id) %>% nrow()
n_secondary_2     <- filter(first_new_infant_return_sens2, new_person_id %in% filter(study_dat, any_prior_sens == 1)$new_person_id) %>% distinct(new_person_id) %>% nrow() 

write_csv(data.frame(outcome = c("primary", "sensitivity 1 - infants anytime", "sensitivity 2 - any new child >= 37 wks"),
                     count   = c(n_primary_outcome, n_secondary_1, n_secondary_2)),
          "Outputs/Core4/returns to court/outcome counts.csv")

n_outcome_excl <- filter(study_dat, time_to_next_cens < 37*7) %>% nrow()

#--------------------------------#
# 5b. COHORT SELECTION ----
#--------------------------------#

# Women with any case in SLaM (2008-2018) 
n_1 <- dplyr::filter(full_cohort, any_case_slam4 == 1 & case_seq == 1) %>% dplyr::filter(tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark")) %>% distinct(new_person_id) %>% nrow

# women with index case in SLaM (2008-2018) 
n_2 <- dplyr::filter(full_cohort, any_case_slam4 == 1 & case_seq == 1) %>% dplyr::filter(case_fyear > 2007 & case_fyear < 2018 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark")) %>% distinct(new_person_id) %>% nrow 

# women with index case in SLaM (2008-2018) who match 
n_3 <- dplyr::filter(full_cohort, any_case_slam4 == 1 & case_seq == 1 & match_status == 1) %>% dplyr::filter(case_fyear > 2007 & case_fyear < 2018 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark")) %>% distinct(new_person_id) %>% nrow 

# women with index case in SLaM (2008-2018)  who match & have prior_sens == 1 
n_4 <- dplyr::filter(study_dat, match_status == 1 & any_prior_sens == 1) %>%  nrow 

# women in above criteria with missing values:
n_5 <- dplyr::filter(study_dat, match_status == 1 & any_prior_sens == 1) %>% dplyr::select(imd, age_at_index, ethnicity_combo, yngst_age, final_order_grp) %>% n_case_miss()



data.frame(step = c("1. women with any case in SLaM catchement (2008/9-2017/8)",
                    "2. women with index case in SLaM catchement (2008/9-2017/8)",
                    "3. women with index case in SLaM catchement (2008/9-2017/8), who match",
                    "4. women with index case in SLaM catchement (2008/9-2017/8), who match and have SLaM contact before index proceeding (+ 91 days)",
                    "5. number in (4.) with at least one missing value in prognostic variables"),
           n = c(n_1, n_2, n_3, n_4, n_5)) %>%
  write_csv("Outputs/Core4/returns to court/cohort_selection.csv")



# remove data sets no longer needed:
rm(full_cohort, diagnoses_dat, diagnoses_dat_sens, 
   id_link, ndtms_before, ndtms_dat, ndtms_sens, new_child_ages, child_legal_status,
   inpt_dat, cris_dat)

# restrict study_dat to study cohort only:
final_study_dat <- filter(study_dat, any_prior_sens == 1)

range(final_study_dat$fup_time)

range(final_study_dat$time_to_next_cens)

#----------------------------------#
# 6. ASSESS MISSINGNESS ----
#----------------------------------#

# missingness by match status:
gg_miss_var(dplyr::select(final_study_dat, 
                          match_status,
                          ethnicity_combo,
                          dob_combo,
                          child_count,
                          yngst_age,
                          has_father_party,
                          imd),
            facet = match_status,
            show_pct = TRUE)  # more missing among unmatched


# missingness by index year:
gg_miss_var(dplyr::select(final_study_dat, 
                          index_year,
                          ethnicity_combo,
                          dob_combo,
                          child_count,
                          yngst_age,
                          has_father_party,
                          imd),
            facet = index_year,
            show_pct = TRUE)


# % with complete data:
pct_complete_case(dplyr::select(final_study_dat, 
                                ethnicity_combo,
                                dob_combo,
                                child_count,
                                yngst_age,
                                has_father_party,
                                imd))



# restrict to 2010 onwards:
pct_complete_case(dplyr::select(dplyr::filter(final_study_dat, index_year > 1), 
                                ethnicity_combo,
                                dob_combo,
                                child_count,
                                yngst_age,
                                has_father_party,
                                imd))


#----------------------------------#
# 7. MULTIPLE IMPUTATION ----
#----------------------------------#
study_vars <- c("new_person_id", "match_status", "la_name", "age_at_index", "yngst_age", "child_count", "ethnicity_combo", "index_year",
                "has_father_party", "outcome_main_infant", "time_to_next", "imd", "sum_inpt")

# la_name, age at index, age_yngst_child, number of children, father party, index year, match status, ethnicity
mi_dat <- mice(data = dplyr::select(study_dat, new_person_id, match_status, la_name, age_at_index, yngst_age, imd, ethnicity_combo,
                                    has_father_party, index_year),
               m = 1, 
               blocks = c("age_at_index", "yngst_age", "imd", "ethnicity_combo", "outcome_main_infant", "time_to_next"),
               meth = c("pmm", "pmm", "polr", "polyreg"))

complete_dat <- complete(mi_dat)

xyplot(mi_dat, age_at_index ~ yngst_age)

densityplot(mi_dat)

# compare associations:
mod1 <- glm(data = dplyr::select(study_dat, new_person_id, match_status, la_name, age_at_index, yngst_age, imd, ethnicity_combo,
                                 has_father_party, index_year),
            match_status ~ ethnicity_combo,
            family = binomial(link= "logit"))

mod2 <- glm(data = complete_dat,
            match_status ~ ethnicity_combo,
            family = binomial(link= "logit"))

purrr::map_df(list(mod1, mod2), ~broom::tidy(., exponentiate = TRUE, conf.int = TRUE)) #precision improvement


# check ethnicity distribution:
with(dplyr::select(study_dat, new_person_id, match_status, la_name, age_at_index, yngst_age, imd, ethnicity_combo,
                   has_father_party, index_year),
     prop.table(table(ethnicity_combo)))

with(complete_dat,
     prop.table(table(ethnicity_combo)))

# check IMD distribution:
with(dplyr::select(study_dat, new_person_id, match_status, la_name, age_at_index, yngst_age, imd, ethnicity_combo,
                   has_father_party, index_year),
     prop.table(table(imd)))

with(complete_dat,
     prop.table(table(imd)))

# check all cases complete?
pct_complete_case(complete_dat)


#-----------------------------------------------#
# 8. TABLE 1  ----
#-----------------------------------------------#

final_study_dat$anx_depr <- with(final_study_dat, ifelse(anxiety_dx == 1 | mild_depress_dx == 1, 1, 0))
final_study_dat$smi <- with(final_study_dat, ifelse(F2_dx == 1 | severe_mood_dx == 1, 1, 0))
final_study_dat$other_dx <- with(final_study_dat, ifelse(F8_dx == 1 | F9_dx == 1 | other_dx == 1, 1, 0))
final_study_dat$imd <- with(final_study_dat, ifelse(imd %in% c("3", "4 or 5"), "3,4 or 5", imd))
final_study_dat$final_order_grp <- with(final_study_dat, ifelse(final_order_grp %in% c("dismissed/ono", "fao/so"), "dismissed/ono/fao/so", final_order_grp))
final_study_dat$custody_loss <- with(final_study_dat, case_when(!is.na(final_order_grp) & final_order_grp %in% c("po", "ro/sgo/cao", "epo/co/sao") ~ 1, 
                                                                is.na(final_order_grp) ~ NA_real_,
                                                                TRUE ~ 0)) 

diags <- c("any_su_dx", "anxiety_dx", "mild_depress_dx",  "F2_dx", "severe_mood_dx", "pd_dx", "F1_dx", "other_dx", "sum_dx_cat")
drugs <- c("alcohol_su", "cannabis_su", "cocaine_su", "opioids_su", "other_su", "dual_dx")

my_vars <- c("fup_time",
             "la_name", 
             "died",
             "age_at_index", 
             "yngst_age", 
             "child_count_cat", 
             "ethnicity_combo", 
             "index_year_cat",
             "has_father_party",  
             "imd", 
             "any_inpt",
             "sum_inpt_sens",
             "sum_bed_days_sens",
             diags,
             "any_su",
             "sum_su_cat",
             drugs,
             "final_order_grp",
             "custody_loss")

my_fac_vars <- c("la_name", "died", "child_count_cat", "ethnicity_combo", "index_year_cat",
                 "has_father_party", "imd", "any_inpt", diags, "any_su", "sum_su_cat",
                 drugs, "final_order_grp", "custody_loss")

table_1 <- final_study_dat %>%
  mutate(sum_inpt_sens = ifelse(any_inpt == 0, NA, sum_inpt_sens),
         sum_bed_days_sens = ifelse(any_inpt == 0, NA, sum_bed_days_sens)) %>%
  mutate(child_count_cat = ifelse(child_count > 3, "4+", child_count)) %>%
  CreateTableOne(vars = my_vars,
                 factorVars = my_fac_vars,
                 strata = "outcome_main_infant",
                 includeNA = TRUE,
                 addOverall = TRUE) %>%
  print(printToggle = FALSE, test = FALSE, nonnormal = my_vars[!my_vars %in% my_fac_vars]) %>%
  as.data.frame() %>%
  rownames_to_column()

write_csv(table_1, "Outputs/Core4/returns to court/table_1_final.csv")


miss_tab <- final_study_dat %>%
  mutate(child_count_cat = ifelse(child_count > 3, "4+", child_count)) %>%
  dplyr::select(!!my_vars, outcome_main_infant) %>%
  mutate(miss_fl = rowSums(is.na(.)),
         miss_fl = ifelse(miss_fl >= 1, 1, 0)) %>%
  mutate(sum_inpt_sens = ifelse(any_inpt == 0, NA, sum_inpt_sens),
         sum_bed_days_sens = ifelse(any_inpt == 0, NA, sum_bed_days_sens)) %>%
  CreateTableOne(vars = c(my_vars, "outcome_main_infant"),
                 factorVars = c(my_fac_vars, "outcome_main_infant"),
                 strata = "miss_fl",
                 includeNA = TRUE,
                 addOverall = TRUE) %>%
  print(printToggle = FALSE, test = FALSE, nonnormal = my_vars[!my_vars %in% my_fac_vars]) %>%
  as.data.frame() %>%
  rownames_to_column()

write_csv(miss_tab, "Outputs/Core4/returns to court/miss_table_1_final.csv")


# check overall col is same for both tables in terms of counts:
sum(as.character(miss_tab$Overall) != as.character(table_1$Overall))
check_tab <- inner_join(dplyr::select(miss_tab, rowname, Overall_1 = Overall), dplyr::select(table_1, rowname, Overall_2 = Overall), by = "rowname")



#-----------------------------------------------#
# 9. SURVIVAL ANALYSIS ----
#-----------------------------------------------#
final_study_dat$time_to_next_cens_37 <- with(final_study_dat, time_to_next_cens - 37*7)


study_dat_res <- final_study_dat %>%
  mutate(miss_fl = rowSums(is.na(dplyr::select(., !!my_vars))),
         miss_fl = ifelse(miss_fl >= 1, 1, 0)) %>%
  filter(miss_fl == 0)

#rm(study_dat)

# 9a. KM plots ----

# KM plots: ---
fit <- survfit(Surv(time_to_next_cens, outcome_main_infant) ~ 1, data = final_study_dat)

fit_plot <- ggsurvplot(fit, 
                       conf.int = TRUE,
                       fun = "event",
                       data = final_study_dat,
                       break.time.by = 2*365.25,
                       xlim = c(0, 365.25*8),
                       #ylim = c(0, 0.4),
                       xlab = "Time (years) from onset of index proceedings",
                       ylab = "Cumulative Incidence (%)\n ",
                       xscale = 365.25,
                       conf.int.fill = "grey90",
                       conf.int.alpha = 0.55,
                       conf.int.style = "ribbon",
                       colour = "#009E73",
                       censor = FALSE,
                       axes.offset = T,
                       ggtheme = theme_bw(),
                       font.x = 14,
                       font.y = 14,
                       font.tickslab = 12,
                       font.main = 14,
                       fontsize = 3.5,
                       surv.scale = c("percent"),
                       tables.theme = theme_cleantable(),
                       risk.table = "nrisk_cumevents")
fit_plot

ggsave(plot = print(fit_plot, newpage = FALSE), 
       device = "png",
       "Outputs/Core4/returns to court/KM_whole_cohort_final.png",
       units = "mm", height = 150, width = 190)



# shift start time by 37 weeks:
fit_adj <- survfit(Surv(time_to_next_cens_37, outcome_main_infant) ~ 1, data = filter(final_study_dat, time_to_next_cens_37 >= 0))

fit_adj_plot <- ggsurvplot(fit_adj, 
                           conf.int = TRUE,
                           fun = "event",
                           data = filter(final_study_dat, time_to_next_cens_37 >= 0),
                           break.time.by = 2*365.25,
                           xlim = c(0, 365.25*8),
                           #ylim = c(0, 0.4),
                           xlab = "Time (years) from start of at-risk period",
                           ylab = "Cumulative Incidence (%)\n ",
                           xscale = 365.25,
                           conf.int.fill = "grey90",
                           conf.int.alpha = 0.55,
                           conf.int.style = "ribbon",
                           colour = "#009E73",
                           censor = FALSE,
                           axes.offset = T,
                           ggtheme = theme_bw(),
                           font.x = 14,
                           font.y = 14,
                           font.tickslab = 12,
                           font.main = 14,
                           fontsize = 3.5,
                           surv.scale = c("percent"),
                           tables.theme = theme_cleantable(),
                           risk.table = "nrisk_cumevents")
fit_adj_plot

ggsave(plot = print(fit_adj_plot, newpage = FALSE), 
       device = "png",
       "Outputs/Core4/returns to court/KM_without_immortal_time_final.png",
       units = "mm", height = 150, width = 190)



#-------------------------------#
# 9d. Cox PH model ----
#-------------------------------#

# primary outcome model:
full_mod <- coxph(
  formula = Surv(time_to_next_cens_37, outcome_main_infant) ~ age_at_index + yngst_age + has_father_party + child_count_cat + final_order_grp + sum_bed_days_sens + F2_dx + severe_mood_dx + anxiety_dx + mild_depress_dx + pd_dx + other_dx + any_su + la_name,
  data = filter(study_dat_res, time_to_next_cens_37 >= 0)
  )

full_mod_tidy <- tidy(full_mod, exponentiate = TRUE)

AIC(full_mod)
BIC(full_mod)

# PH assumption
cox.zph(full_mod)


# sensitivity analysis model:
full_mod_sens <- coxph(
  formula = Surv(time_to_next_cens_37, outcome) ~ age_at_index + yngst_age + has_father_party + child_count_cat + final_order_grp + sum_bed_days_sens + F2_dx + severe_mood_dx + anxiety_dx + mild_depress_dx + pd_dx + other_dx + any_su + la_name,
  data = filter(study_dat_res, time_to_next_cens_37 >= 0) %>% mutate(outcome = ifelse(new_person_id %in% first_new_infant_return_sens$new_person_id, 1, 0))
)

cox.zph(full_mod_sens)
full_mod_sens_tidy <- tidy(full_mod_sens, exponentiate = TRUE, conf.int = T)

#-------------------------------#
# Non-linearity ----
#-------------------------------#

ggcoxfunctional(Surv(time_to_next_cens_37, outcome_main_infant) ~ age_at_index, data = filter(study_dat_res, time_to_next_cens_37 >= 0))
ggcoxfunctional(Surv(time_to_next_cens_37, outcome_main_infant) ~ yngst_age, data = filter(study_dat_res, time_to_next_cens_37 >= 0))
ggcoxfunctional(Surv(time_to_next_cens_37, outcome_main_infant) ~ child_count, data = filter(study_dat_res, time_to_next_cens_37 >= 0))

# create function from stu's code

# maternal age non-linearity ----
age_model <- coxph(
  formula = Surv(time_to_next_cens, outcome_main_infant) ~ age_at_index,
  data = study_dat
)

tidy(age_model, exponentiate = TRUE)

age_nonl_model <- coxph(
  formula = Surv(time_to_next_cens, outcome_main_infant) ~ pspline(age_at_index, df = 3),
  data = study_dat
)

summary(age_nonl_model)

AIC(age_model)
AIC(age_nonl_model)


# youngest child age non-linearity ----
child_age_model <- coxph(
  formula = Surv(time_to_next_cens, outcome_main_infant) ~ yngst_age,
  data = study_dat
)

tidy(child_age_model, exponentiate = TRUE)

child_age_nonl_model <- coxph(
  formula = Surv(time_to_next_cens, outcome_main_infant) ~ pspline(yngst_age, df = 3),
  data = study_dat
)

summary(child_age_nonl_model)

AIC(child_age_model)
AIC(child_age_nonl_model)


#-----------------------------------------#
# 9d. Plot Schoenfeld residual plot ----
#-----------------------------------------#

test_ph <- cox.zph(full_mod)

test_ph$y %>%
  as_tibble(rownames = "x") %>%
  mutate(x = as.numeric(x)) %>%
  pivot_longer( cols = -x, names_to = "var") %>%
  ggplot(aes(x = x, y = value)) +
  facet_wrap(~var, scales = "free") +
  geom_point() +
  geom_smooth() +
  labs(x = "KM time", y = "Scaled Schoenfeld residuals")

ggsave("Outputs/Core4/returns to court/final_model_schoenfeld_plots_sept.png")


#-------------------------------#
# 9e. model diagnostics ----
#-------------------------------#

#----------------#
# CALIBRATION
#----------------#

# calibration slope
# calibrate function in rms package
set.seed(12345)
full_mod_cph <- cph(
  formula = Surv(time_to_next_cens_37, outcome_main_infant) ~ age_at_index + yngst_age + has_father_party + child_count_cat + final_order_grp + sum_bed_days_sens + F2_dx + severe_mood_dx + anxiety_dx + mild_depress_dx + pd_dx + other_dx + any_su + la_name,
  data = filter(study_dat_res, time_to_next_cens_37 >= 0),
  x = TRUE,
  y = TRUE,
  surv = TRUE,
  time.inc = 5*365.25 # years
)

cal <- calibrate(full_mod_cph, method = "boot", u = 5*365.25, B = 200)
plot(cal, 
     #ylim = c(0.4, 1), xlim = c(0.4, 1), 
     xlab = "Predicted 5-year survival", ylab = "Proportion surviving 5 years")
cal

calkm <- calibrate(full_mod_cph, u = 5*365.25, m = 50, cmethod = "KM", B = 200)
plot(calkm, add = TRUE)


#----------------#
# DISCRIMINATION
#----------------#

# Harrel's C
concordance(full_mod_cph, timewt = "n")

# Concordance over time plot:
c <- concordance(full_mod_cph, ranks = TRUE)
c_wt <- concordance(full_mod_cph, ranks = TRUE, timewt = "S/G") # Schemper et al 

d_to_c <- function(x) (x+1)/2

plot(d_to_c(rank) ~ time, data = c$ranks, log = 'x', ylab = "rank")
lfit <- with(c$ranks, lowess(log(time), d_to_c(rank)))
lines(exp(lfit$x), lfit$y, col = 2, lwd = 2)
abline(0.5, 0, lty = 2)


# get confidence intervals for Validate statistics:
set.seed(12345)

B <- 200; reps <- 500; 
dxy <- numeric(reps); r2 <- numeric(reps); slope <- numeric(reps)

n <- nrow(filter(study_dat_res, time_to_next_cens_37 >= 0))

val <- validate(full_mod_cph, 
                method = "boot",
                B = B) # slope indicates mild overfitting 

for (i in 1 : reps) {
  g <- update(full_mod_cph, B = B)
  v <- validate(g, B=B)
  dxy[i] <- v['Dxy', 'index.corrected']
  r2[i] <- v['R2', 'index.corrected']
  slope[i] <- v['Slope', 'index.corrected']
}

quantile(dxy, c(.025, .975))
quantile(r2, c(.025, .975))
quantile(slope, c(.025, .975))

val


# Roysteon's D - a discrimination measure quantifying observed separation between subjects with low and high predicted risk
royston(full_mod, adjust = TRUE) # log hazard ratio comparing two prognostic groups of equal size (dichotomized linear predictor at median)



#-------------------------------#
# 9e. model coef plot ----
#-------------------------------#
term_labels <- c("Age at index",
                 "Age of youngest child at index",
                 "Father party status at index",
                 "Child count at index: 2",
                 "Child count at index: 3",
                 "Child count at index: 4 or more",
                 "Final order of youngest child at index: EPO/CO/SAO",
                 "Final order of youngest child at index: PO",
                 "Final order of youngest child at index: RO/SGO/CAO",
                 "SLaM inpatient bed day count over before/during index",
                 "Schizophrenia, schizotypal, delusional disorder diagnosis before/during index",
                 "Bipolar, severe/moderate depressive disorder diagnosis before/during index",
                 "Anxiety disorder diagnosis before/during index",
                 "Other depressive disorder diagnosis before/during index",
                 "Personality disorder diagnosis before/during index",
                 "Other mental health disorder diagnosis before/during index",
                 "Any substance misuse record before/during index",
                 "LA at index: Lambeth",
                 "LA at index: Lewisham",
                 "LA at index: Southwark")

model_coef <- tidy(full_mod, exponentiate = TRUE) %>%
  bind_cols(data.frame(labels = term_labels)) %>%
  mutate(labels = factor(labels, levels = labels)) 

write_csv(model_coef, "Outputs/Core4/returns to court/final_model_results_sept.csv")

write_csv(full_mod_sens_tidy, "Outputs/Core4/returns to court/final_model_results_sept-sens2.csv")



model_coef_updated <- read_csv("Outputs/Core4/returns to court/final_model_results_sept_updated.csv") %>%
  mutate(labels = factor(labels, levels = labels))

ggplot(data = model_coef_updated,
       aes(x = estimate, y = fct_rev(labels))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) +
  geom_point(size = 2.5, shape = 21, fill = "white") +
  scale_x_log10(breaks = c(0.5,  1, 2, 4, 6)) +
  labs(x = expression(Hazard~Ratio~(log[10]~scale)),
       y = "") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave( "Outputs/Core4/returns to court/final_model_results_sept.png", units = "in",
        width = 7.5, height = 6)

#-----------------------------------------------------#
# 10. model predicted rates among 'high risk' groups ----
#-----------------------------------------------------#

# calculated estimate 5-year and 10-year rates of return by diff vars ----
# estimated at 25%, median and 75% quantile of study cohort ages:
test_dat <- data.frame(
  age_at_index = c(20,25,32,38,44),
  yngst_age = 2,
  has_father_party = 1,
  child_count_cat = "1",
  final_order_grp = "ro/sgo/cao",
  sum_bed_days_sens = 0,
  F2_dx = 0,
  anxiety_dx = 0,
  mild_depress_dx = 0,
  severe_mood_dx = 0, 
  other_dx = 0,
  pd_dx = 0,
  any_su = 0,
  la_name = "southwark"
)

surv_tm_dat <- survfit(full_mod_cph, newdata = test_dat, type = "aalen",
                       se.fit = TRUE, conf.type = "log-log", censor = TRUE)

expected <- summary(surv_tm_dat, time = c((365.25*3 - 259), 365.25*3, (365.25*5 - 259), 365.25*5))

expected_dat <- list(surv =  as.data.frame(expected$surv) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv", -row),
                     surv_ll = as.data.frame(expected$lower) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv_ll", -row),
                     surv_ul = as.data.frame(expected$upper) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("age", "row")) %>%
  mutate_at(vars(starts_with("surv")), 
            list(cuminc = ~round((1 - .)*100, 3))) %>%
  mutate(cat = case_when(age == 1 ~ "20",
                         age == 2 ~ "25",
                         age == 3 ~ "32",
                         age == 4 ~ "38",
                         age == 5 ~ "44"),
         time = case_when(row == 1 ~ "3yrs - 37wks",
                          row == 2 ~ "3yrs",
                          row == 3 ~ "5yrs - 37wks",
                          row == 4 ~ "5yrs")) %>%
  dplyr::select(time, cat, starts_with("surv"), ends_with("cuminc"))

write_csv(expected_dat, "Outputs/Core4/returns to court/exp_rates_by_age_sept.csv")


# plot expected curves:

expected_plot_dat <- summary(surv_tm_dat, time = seq(0, (365.25*5 - 37*7) , 1))

expected_plot_dat <- list(surv = as.data.frame(expected_plot_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv", -row),
                          surv_ll = as.data.frame(expected_plot_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv_ll", -row),
                          surv_ul = as.data.frame(expected_plot_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "age", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "age")) %>%
  mutate_at(vars(starts_with("surv")), list(~round((1 - .)*100, 4))) %>%
  mutate(time = as.numeric(row) - 1)


ggplot(data = expected_plot_dat, 
       aes(x = time, y = surv, 
           group    = fct_rev(age), 
           colour   = fct_rev(age),
           fill     = fct_rev(age),
           linetype = fct_rev(age))) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = surv_ul, ymax = surv_ll), alpha = 0.2, colour = "white") +#, linetype = "dashed") +
  theme_classic() +
  scale_colour_manual("",
                      values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7"),
                      labels = paste0(rev(c(20, 25,32,38, 44)), "yrs")) +
  scale_linetype_manual("", 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash"), 
                          labels = paste0(rev(c(20, 25, 32,38, 44)), "yrs")) +
  scale_fill_manual("",
                    values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7"),
                    labels = paste0(rev(c(20, 25, 32,38, 44)), "yrs")) +
  scale_y_continuous("Estimated\ncumulative\nincidence") +
  #breaks = seq(0,5,1), labels = paste0(seq(0,5,1), "%")) +
  #scale_x_continuous("\nTime since onset of index case (years)", breaks = seq(0, 365.25*5, 365.25),
  #                   labels = seq(0,5,1)) +
  scale_x_continuous("\nTime since onset of index case (years)", breaks = c(-259, 106.25, seq(106.25 + 365.25 , 106.25 + (365.25*4), 365.25)),
                     labels = seq(0,5,1),
                     limits = c(-259, 106.25 + 365.25*4)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.text = element_text(size = 12),
        legend.position = c(0.2, 0.85),
        legend.key.width = unit(1.7, "cm"),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("Outputs/Core4/returns to court/plot_exp_return_rates_age_sept.png",
       units = "in", width = 7, height = 5, dpi = "print")

#--------------------#
# final order type ----
#--------------------#
test_dat <- data.frame(
  age_at_index = 32,
  yngst_age = 2,
  has_father_party = 1,
  child_count_cat = "1",
  final_order_grp =  c("dismissed/ono/fao/so", "epo/co/sao", "po", "ro/sgo/cao"),
  sum_bed_days_sens = 0,
  F2_dx = 0,
  anxiety_dx = 0,
  mild_depress_dx = 0,
  severe_mood_dx = 0, 
  other_dx = 0,
  pd_dx = 0,
  any_su = 0,
  la_name = "southwark"
)


surv_tm_dat <- survfit(full_mod_cph, newdata = test_dat, type = "aalen",
                       se.fit = TRUE, conf.type = "log-log", censor = TRUE)

order_rates_dat <- summary(surv_tm_dat, time = c((365.25*3 - 259), 365.25*3, (365.25*5 - 259), 365.25*5))

order_rates_dat <- list(surv = as.data.frame(order_rates_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv", -row),
                          surv_ll = as.data.frame(order_rates_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv_ll", -row),
                          surv_ul = as.data.frame(order_rates_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "order")) %>%
  mutate_at(vars(starts_with("surv")), 
            list(cuminc = ~round((1 - .)*100, 3))) %>%
  mutate(cat = case_when(order == 1 ~ "dismissed/ono/fao/so", 
                         order == 2 ~ "epo/co/sao",
                         order == 3 ~ "po", 
                         order == 4 ~ "ro/sgo/cao"),
         time = case_when(row == 1 ~ "3yrs - 37wks",
                          row == 2 ~ "3yrs",
                          row == 3 ~ "5yrs - 37wks",
                          row == 4 ~ "5yrs")) %>%
  dplyr::select(time, cat, starts_with("surv"), ends_with("cuminc"))

write_csv(order_rates_dat, "Outputs/Core4/returns to court/exp_rates_by_order_sept.csv")

expected_plot_dat <- summary(surv_tm_dat, time = seq(0, (365.25*5 - 37*7) , 1))

expected_plot_dat <- list(surv = as.data.frame(expected_plot_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv", -row),
                          surv_ll = as.data.frame(expected_plot_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv_ll", -row),
                          surv_ul = as.data.frame(expected_plot_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "order", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "order")) %>%
  mutate_at(vars(starts_with("surv")), list(~round((1 - .)*100, 4))) %>%
  mutate(time = as.numeric(row) - 1)


ggplot(data = expected_plot_dat, 
       aes(x = time, y = surv, 
           group    = fct_rev(order), 
           colour   = fct_rev(order),
           fill     = fct_rev(order),
           linetype = fct_rev(order))) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = surv_ul, ymax = surv_ll), alpha = 0.2, colour = "white") +#, linetype = "dashed") +
  #geom_line(aes(y = surv_ul), linetype = "dashed") +
  theme_classic() +
  scale_colour_manual("",
                      values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2"),
                      labels = rev(c("remained or returned home", "placed in out-of-home care", "placed for adoption", "placed with extended family"))) +
  scale_linetype_manual("", 
                        values = c("solid", "dashed", "dotdash", "dotted"),
                        labels = rev(c("remained or returned home", "placed in out-of-home care", "placed for adoption", "placed with extended family"))) +
  scale_fill_manual("",
                    values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2"),
                    labels = rev(c("remained or returned home", "placed in out-of-home care", "placed for adoption", "placed with extended family"))) +
  scale_y_continuous("Estimated\ncumulative\nincidence") +
  #breaks = seq(0,5,1), labels = paste0(seq(0,5,1), "%")) +
  #scale_x_continuous("\nTime since onset of index case (years)", breaks = seq(0, 365.25*5, 365.25),
  #                   labels = seq(0,5,1)) +
  scale_x_continuous("\nTime since onset of index case (years)", breaks = c(-259, 106.25, seq(106.25 + 365.25 , 106.25 + (365.25*4), 365.25)),
                     labels = seq(0,5,1),
                     limits = c(-259, 106.25 + 365.25*4)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.text = element_text(size = 12),
        legend.position = c(0.3, 0.85),
        legend.key.width = unit(1.7, "cm"),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("Outputs/Core4/returns to court/plot_exp_return_rates_order_sept.png",
       units = "in", width = 7, height = 5, dpi = "print")


#--------------------#
# age of youngest child ----
#--------------------#

test_dat <- data.frame(
  age_at_index = 32,
  yngst_age = c(0.04, 0.115, 2, 7, 12), # 2wks, 6wks, 2yrs, 7yrs, 12yrs
  has_father_party = 1,
  child_count_cat = "1",
  final_order_grp =  c("ro/sgo/cao"),
  sum_bed_days_sens = 0,
  F2_dx = 0,
  anxiety_dx = 0,
  mild_depress_dx = 0,
  severe_mood_dx = 0, 
  other_dx = 0,
  pd_dx = 0,
  any_su = 0,
  la_name = "southwark"
)


surv_tm_dat <- survfit(full_mod_cph, newdata = test_dat, type = "aalen",
                       se.fit = TRUE, conf.type = "log-log", censor = TRUE)

yng_rates_dat <- summary(surv_tm_dat, time = c((365.25*3 - 259), 365.25*3, (365.25*5 - 259), 365.25*5))

yng_rates_dat <- list(surv = as.data.frame(yng_rates_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv", -row),
                        surv_ll = as.data.frame(yng_rates_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv_ll", -row),
                        surv_ul = as.data.frame(yng_rates_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "yng_age")) %>%
  mutate_at(vars(starts_with("surv")), 
            list(cuminc = ~round((1 - .)*100, 3))) %>%
  mutate(cat = case_when(yng_age == 1 ~ "2wks", 
                         yng_age == 2 ~ "6wks",
                         yng_age == 3 ~ "2yrs", 
                         yng_age == 4 ~ "7yrs",
                         yng_age == 5 ~ "12yrs"),
         time = case_when(row == 1 ~ "3yrs - 37wks",
                          row == 2 ~ "3yrs",
                          row == 3 ~ "5yrs - 37wks",
                          row == 4 ~ "5yrs")) %>%
  dplyr::select(time, cat, starts_with("surv"), ends_with("cuminc"))

write_csv(yng_rates_dat, "Outputs/Core4/returns to court/exp_rates_by_yng_sept.csv")

expected_plot_dat <- summary(surv_tm_dat, time = seq(0, (365.25*5 - 37*7) , 1))

expected_plot_dat <- list(surv = as.data.frame(expected_plot_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv", -row),
                          surv_ll = as.data.frame(expected_plot_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv_ll", -row),
                          surv_ul = as.data.frame(expected_plot_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "yng_age", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "yng_age")) %>%
  mutate_at(vars(starts_with("surv")), list(~round((1 - .)*100, 4))) %>%
  mutate(time = as.numeric(row) - 1)


ggplot(data = expected_plot_dat, 
       aes(x = time, y = surv, 
           group    = fct_rev(yng_age), 
           colour   = fct_rev(yng_age),
           fill     = fct_rev(yng_age),
           linetype = fct_rev(yng_age))) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = surv_ul, ymax = surv_ll), alpha = 0.2, colour = "white") +#, linetype = "dashed") +
  #geom_line(aes(y = surv_ul), linetype = "dashed") +
  theme_classic() +
  scale_colour_manual("",
                      values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7"), 
                      labels = rev(c("2wks", "6wks", "2yrs", "7yrs", "12yrs"))) +
  scale_linetype_manual("", 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash"), 
                          labels = rev(c("2wks", "6wks", "2yrs", "7yrs", "12yrs"))) +
  scale_fill_manual("",
                    values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7"),
                    labels = rev(c("2wks", "6wks", "2yrs", "7yrs", "12yrs"))) +
  scale_y_continuous("Estimated\ncumulative\nincidence") +
  #breaks = seq(0,5,1), labels = paste0(seq(0,5,1), "%")) +
  #scale_x_continuous("\nTime since onset of index case (years)", breaks = seq(0, 365.25*5, 365.25),
  #                   labels = seq(0,5,1)) +
  scale_x_continuous("\nTime since onset of index case (years)", breaks = c(-259, 106.25, seq(106.25 + 365.25 , 106.25 + (365.25*4), 365.25)),
                     labels = seq(0,5,1),
                     limits = c(-259, 106.25 + 365.25*4)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.text = element_text(size = 12),
        legend.position = c(0.2, 0.85),
        legend.key.width = unit(1.7, "cm"),
        panel.grid.major.y = element_line(colour = "grey90"))

ggsave("Outputs/Core4/returns to court/plot_exp_return_rates_yng_sept.png",
       units = "in", width = 7, height = 5, dpi = "print")


#-------------------------------------#
# risk of young mum scenarios ----
#-------------------------------------#


test_dat <- data.frame(
  age_at_index = c(25, 25, 25, 32, 32, 32, 38 , 38, 38),
  yngst_age = c(0.11, 2, 7, 0.11, 2, 7, 0.11, 2, 7), # , 6wks, 2yrs, 7yrs
  has_father_party = 1,
  child_count_cat = "1",
  final_order_grp = c("ro/sgo/cao"),
  sum_bed_days_sens = 0,
  F2_dx = 0,
  anxiety_dx = 0,
  mild_depress_dx = 0,
  severe_mood_dx = 0, 
  other_dx = 0,
  pd_dx = 0,
  any_su = 0,
  la_name = "southwark"
)

surv_tm_dat <- survfit(full_mod_cph, newdata = test_dat, type = "aalen",
                       se.fit = TRUE, conf.type = "log-log", censor = TRUE)


expected_plot_dat <- summary(surv_tm_dat, time = seq(0, (365.25*5 - 37*7) , 1))

expected_plot_dat <- list(surv = as.data.frame(expected_plot_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv", -row),
                          surv_ll = as.data.frame(expected_plot_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv_ll", -row),
                          surv_ul = as.data.frame(expected_plot_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "var")) %>%
  mutate_at(vars(starts_with("surv")), list(~round((1 - .)*100, 4))) %>%
  left_join(dplyr::select(mutate(test_dat, var = as.character(row_number())), var, age_at_index, yngst_age), by = "var") %>%
  mutate(time = as.numeric(row) - 1,
         age_at_index = paste("Maternal age:", age_at_index, "years old"),
         age_at_index = factor(age_at_index, levels = c("Maternal age: 25 years old", "Maternal age: 32 years old", "Maternal age: 38 years old")),
         yngst_age = ifelse(yngst_age == 0.11, "Youngest child: 6 weeks old", paste("Youngest child:", yngst_age, "years old")),
         yngst_age = factor(yngst_age, levels = c("Youngest child: 6 weeks old", "Youngest child: 2 years old", "Youngest child: 7 years old")))



ggplot(data = expected_plot_dat, 
       aes(x = time, y = surv, 
           group    = fct_rev(var), 
           colour   = fct_rev(var),
           fill     = fct_rev(var))) +
  facet_wrap(~age_at_index+yngst_age) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = surv_ul, ymax = surv_ll), alpha = 0.2, colour = "white") +#, linetype = "dashed") +
  #geom_line(aes(y = surv_ul), linetype = "dashed") +
  theme_classic() +
  scale_colour_manual("",
                      values = c("#E69F00",  "#009E73",  "#CC79A7", "#E69F00",  "#009E73",  "#CC79A7", "#E69F00",  "#009E73",  "#CC79A7"), 
                      labels = rev(c("2wks", "2yrs",  "12yrs"))) +
  #scale_linetype_discrete("", labels = rev(c("2wks",  "2yrs",  "12yrs"))) +
  scale_fill_manual("",
                    values = c("#E69F00",  "#009E73",  "#CC79A7", "#E69F00",  "#009E73",  "#CC79A7", "#E69F00",  "#009E73",  "#CC79A7"),
                    labels = rev(c("2wks", "2yrs", "12yrs"))) +
  scale_y_continuous("Estimated\ncumulative\nincidence") +
  #breaks = seq(0,5,1), labels = paste0(seq(0,5,1), "%")) +
  #scale_x_continuous("\nTime since onset of index case (years)", breaks = seq(0, 365.25*5, 365.25),
  #                   labels = seq(0,5,1)) +
  scale_x_continuous("\nTime since onset of index case (years)", breaks = c(-259, 106.25, seq(106.25 + 365.25 , 106.25 + (365.25*4), 365.25)),
                     labels = seq(0,5,1),
                     limits = c(-259, 106.25 + 365.25*4)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.7, "cm"),
        panel.grid.major.y = element_line(colour = "grey90"),
        strip.background = element_blank()) +
  guides(colour = "none", fill = "none", linetype = "none")

ggsave("Outputs/Core4/returns to court/plot_exp_return_rates_age_and_yng_sept.png",
       units = "in", width = 7, height = 7, dpi = 300)



test_dat <- data.frame(
  age_at_index = 25,
  yngst_age = c(0.11,0.11, 0.11, 0.11, 2, 2, 2, 2, 7, 7, 7, 7), # , 6wks
  has_father_party = 1,
  child_count_cat = "1",
  final_order_grp = c("dismissed/ono/fao/so", "ro/sgo/cao", "epo/co/sao", "po", "dismissed/ono/fao/so", "ro/sgo/cao", "epo/co/sao", "po", "dismissed/ono/fao/so", "ro/sgo/cao", "epo/co/sao", "po"),
  sum_bed_days_sens = 0,
  F2_dx = 0,
  anxiety_dx = 0,
  mild_depress_dx = 0,
  severe_mood_dx = 0, 
  other_dx = 0,
  pd_dx = 0,
  any_su = 0,
  la_name = "southwark"
)

surv_tm_dat <- survfit(full_mod_cph, newdata = test_dat, type = "aalen",
                       se.fit = TRUE, conf.type = "log-log", censor = TRUE)


expected_plot_dat <- summary(surv_tm_dat, time = seq(0, (365.25*5 - 37*7) , 1))

expected_plot_dat <- list(surv = as.data.frame(expected_plot_dat$surv) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv", -row),
                          surv_ll = as.data.frame(expected_plot_dat$lower) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv_ll", -row),
                          surv_ul = as.data.frame(expected_plot_dat$upper) %>% rownames_to_column(var = "row") %>% gather(key = "var", value = "surv_ul", -row)) %>%
  reduce(left_join, by = c("row", "var")) %>%
  mutate_at(vars(starts_with("surv")), list(~round((1 - .)*100, 4))) %>%
  left_join(dplyr::select(mutate(test_dat, var = as.character(row_number())), var, age_at_index, yngst_age, final_order_grp), by = "var") %>%
  mutate(time = as.numeric(row) - 1,
         #age_at_index = paste("Maternal age:", age_at_index, "years old"),
         yngst_age = ifelse(yngst_age == 0.11, "Youngest child: 6 weeks old", paste("Youngest child:", yngst_age, "years old")),
         final_order_grp = case_when(final_order_grp == "dismissed/ono/fao/so" ~ "remained or returned home",
                                     final_order_grp == "ro/sgo/cao" ~ "placed with extended family",
                                     final_order_grp == "epo/co/sao" ~ "placed in OOHC",
                                     final_order_grp == "po" ~ "placed for adoption"),

         yngst_age = factor(yngst_age, levels = c("Youngest child: 6 weeks old", "Youngest child: 2 years old", "Youngest child: 7 years old")),
         final_order_grp = factor(final_order_grp, levels = c("remained or returned home",
                                                              "placed with extended family",
                                                              "placed in OOHC",
                                                              "placed for adoption"))) %>%
  arrange(yngst_age, final_order_grp) %>%
  mutate(var = factor(var, levels = seq(1,12,1)))



ggplot(data = expected_plot_dat, 
       aes(x = time, y = surv, 
           group    = fct_rev(var), 
           colour   = fct_rev(var),
           fill     = fct_rev(var))) +
  facet_wrap(~yngst_age+final_order_grp, nrow = 3) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = surv_ul, ymax = surv_ll), alpha = 0.2, colour = "white") +#, linetype = "dashed") +
  #geom_line(aes(y = surv_ul), linetype = "dashed") +
  theme_classic() +
  scale_colour_manual(values = c("#E69F00",  "#009E73",  "#CC79A7", "#0072B2", 
                                 "#E69F00",  "#009E73",  "#CC79A7","#0072B2",  
                                 "#E69F00",  "#009E73",  "#CC79A7", "#0072B2")) +
  #scale_linetype_discrete("", labels = rev(c("2wks",  "2yrs",  "12yrs"))) +
  scale_fill_manual("",
                    values = c("#E69F00",  "#009E73",  "#CC79A7", "#0072B2", 
                               "#E69F00",  "#009E73",  "#CC79A7", "#0072B2", 
                               "#E69F00",  "#009E73",  "#CC79A7", "#0072B2")) +
  scale_y_continuous("Estimated\ncumulative\nincidence") +
  #breaks = seq(0,5,1), labels = paste0(seq(0,5,1), "%")) +
  #scale_x_continuous("\nTime since onset of index case (years)", breaks = seq(0, 365.25*5, 365.25),
  #                   labels = seq(0,5,1)) +
  scale_x_continuous("\nTime since onset of index case (years)", breaks = c(-259, 106.25, seq(106.25 + 365.25 , 106.25 + (365.25*4), 365.25)),
                     labels = seq(0,5,1),
                     limits = c(-259, 106.25 + 365.25*4)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.7, "cm"),
        panel.grid.major.y = element_line(colour = "grey90"),
        strip.background = element_blank()) +
  guides(colour = "none", fill = "none", linetype = "none")

ggsave("Outputs/Core4/returns to court/plot_exp_return_rates_group_and_yng_sept.png",
       units = "in", width = 8.5, height = 7, dpi = "print")





