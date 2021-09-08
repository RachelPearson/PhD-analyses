#--------------------------------------------#
# local authority variation - final data set #
#--------------------------------------------#

# packages ----
library(janitor)
library(tidyr)
library(purrr)
library(dplyr)
library(readxl)
library(readr)


#-------------#
# get data ----
#-------------#
setwd("N:/Documents/Data & Programming/Public data")

la_lkup <- read_xls("gov uk/nlac-2011.xls") %>% 
  clean_names()

gdp <- read_xlsx("LA spending/gdp_deflator_1718.xlsx")


#------------#
# LA data ----
#------------#
col_name <- c("csc_centre_spend", "csc_youth_spend")
csc_spend <- map(c("sure_start_spend.xlsx",
                   "young_people_spend.xlsx"), ~read_xlsx(paste0("LAIT/",.x), sheet = "data"))  %>% 
  map(., ~gather(., key = "year", value = "spend", -la_code, -la_name)) %>% 
  map(., ~mutate(., year = as.numeric(gsub("year_", "", year)))) %>% 
  map2(., col_name, ~rename(.x, !!.y := spend, "old_la_code" = la_code, "old_la_name" = la_name)) %>% 
  reduce(left_join, by = c("old_la_code", "old_la_name", "year")) %>% 
  left_join(la_lkup, by = "old_la_code") %>% 
  right_join(gdp, by = "year") %>% 
  mutate(la_code = case_when(new_la_code == "E08000037" ~ "E08000020",
                             new_la_code == "E06000057" ~ "E06000048",
                             new_la_code == "E06000050" ~ "E06000049",
                             TRUE ~ as.character(new_la_code))) %>% 
  dplyr::select(la_code, year, csc_centre_spend, csc_youth_spend, gdp_deflator) %>% 
  group_by(la_code, year, gdp_deflator) %>% 
  summarise_all(funs(sum(.))) %>% 
  mutate_at(vars(contains("spend")), funs(.*(gdp_deflator/100))) %>% 
  ungroup()

#-------------#
# IMD data ----
#-------------#
  
# 2010:
old_lad_lkup <- read_csv("N:/Documents/Data & Programming/Public data/ons/old_LAD_to_LA_code_lookup.csv") %>% 
  dplyr::select(lad_old_code, la_code)

IMD_2010_dat <- read.csv("N:/Documents/Data & Programming/Public data/gov uk/imd2010.csv") %>%
  clean_names() %>%
  dplyr::select(lsoa_code, lad_old_code = post_2009_la_code, imd_score, contains("geog")) %>%
  left_join(old_lad_lkup, by = "lad_old_code") %>% 
  arrange(desc(imd_score)) %>%
  mutate(rank = row_number(),
         lsoa10_pct = ifelse(rank < n()/10,1,0)) %>%
  group_by(la_code) %>%
  summarise(prop10_imd = sum(lsoa10_pct)/n()) %>% 
  mutate(la_code = case_when(la_code == "E08000037" ~ "E08000020",
                             la_code == "E06000057" ~ "E06000048",
                             la_code == "E06000050" ~ "E06000049",
                             TRUE ~ as.character(la_code))) %>% 
  group_by(la_code) %>% 
  summarise_all(list( ~round(median(as.numeric(.)), 2))) %>% 
  ungroup()  


#----------------------------------------#
# mid-year population estimates (ONS) ----
#----------------------------------------#
pop_est_dat <- readRDS("ONS/pop_est_dat.rds") %>% 
  dplyr::select(la_code, year, pop_U1, total, pop_child_tot, pop_adult_tot, pop_women) %>% 
  mutate(la_code = ifelse(la_code == "E06000050", "E06000049", as.character(la_code))) %>% 
  group_by(la_code, year) %>% 
  summarise_all(funs(sum(.))) %>% 
  ungroup()

#-------------------------------#
# Rural/Urban classification ----
#-------------------------------#
ru11 <- readRDS("gov uk/ruralurban_class_la.rds") %>% 
  filter(la_code != "E06000050") %>%  # cheshire east and west have the same classification
  dplyr::select(la_code, starts_with("ru"))


#--------------------------#
# ethnicity data (ONS) ----
#--------------------------#
ethnicity <- read.csv("ONS/ethnicity_notdetailed.csv") %>% 
  clean_names() %>% 
  rename(la_code = geography_code) %>% 
  dplyr::select(la_code, contains("white_measures"), contains("all_categories")) %>% 
  mutate(la_code = case_when(la_code == "E08000037" ~ "E08000020",
                             la_code == "E06000057" ~ "E06000048",
                             la_code == "E06000050" ~ "E06000049",
                             TRUE ~ as.character(la_code))) %>% 
  group_by(la_code) %>% 
  summarise_all(funs(sum(.))) %>% 
  mutate(white_pct = round(ethnic_group_white_measures_value*100/ethnic_group_all_categories_ethnic_group_measures_value, 2)) %>% 
  dplyr::select(-contains("ethnic_group")) %>% 
  ungroup()

#--------------#
# CLA data ----
#--------------#
ADM_dat <- readRDS("CLA/CLA rds/ADM_tidy.rds") %>% 
  filter(geog_l == "LA") %>% 
  dplyr::select(year, la_code = New_geog_code, SCLA_U1) %>% 
  mutate(la_code = case_when(la_code == "E08000037" ~ "E08000020",
                             la_code == "E06000057" ~ "E06000048",
                             la_code == "E06000050" ~ "E06000049",
                             TRUE ~ as.character(la_code))) %>% 
  group_by(year, la_code) %>% 
  summarise_all(funs(sum(.))) %>% 
  ungroup()

#--------------#
# PHE data ----
#--------------#
look_up_code <- read.csv("gov uk/Ward_to_LAD_to_County_to_County_Electoral_Division_May_2017_Lookup_for_England.csv", header = TRUE) %>% 
  dplyr::select(lad2014_code = 3, la_code = 5) %>% 
  filter(tolower(substr(lad2014_code,1,1)) != "w") %>% 
  mutate(lad2014_code = as.character(lad2014_code)) %>% 
  distinct(lad2014_code, .keep_all = TRUE)

phe_list <- map(c(11202, 10101), ~filter(clean_names(read.csv("PHE/PHE_LA_data.csv")), indicator_id == .x & category == "" & area_type == "District & UA")) %>% 
  map(., ~dplyr::select(., starts_with("indicator"), area_code, area_name, time_period, value, count, denominator)) %>% 
  map(~droplevels(.)) %>% 
  map(~left_join(.,look_up_code, by = c("area_code" = "lad2014_code"))) %>% 
  map(~mutate(., la_code = ifelse(is.na(la_code), as.character(area_code), as.character(la_code)),
              la_code = case_when(la_code == "E08000037" ~ "E08000020",
                                  la_code == "E06000057" ~ "E06000048",
                                  la_code == "E06000050" ~ "E06000049",
                                  TRUE ~ as.character(la_code)),
                 year = as.numeric(substr(as.character(time_period),1,4)))) %>% 
  map(~group_by(., year, la_code)) %>% 
  map(~mutate_at(., vars("count", "denominator"), funs(sum = sum(., na.rm = TRUE)))) %>% 
  map(~ungroup(.)) %>% 
  map(~distinct(., year, la_code, .keep_all = TRUE)) %>% 
  map(~mutate(., value_new = count_sum*100/denominator_sum)) %>% 
  map2(.,c("violent_crime", "child_lowinc"), ~dplyr::select(.x,la_code, year, !!.y := value_new)) 

violent_crime <- phe_list[[1]] %>% 
  filter(year == 2010) %>% 
  dplyr::select(-year)

child_lowinc <- phe_list[[2]]  


#-------------------------------#
# highest qual (2011 census) ----
#-------------------------------#
# highest_qual <- read.csv("census/highest_qual_by_sex_la.csv") %>% 
#   clean_names() %>% 
#   dplyr::select(contains("geography"), contains("all_persons")) %>% 
#   dplyr::select(la_code = geography_code, qual_denom = 3, none = 4, lvl1 = 5, lvl2 = 6) %>% 
#   mutate(la_code = case_when(la_code == "E08000037" ~ "E08000020",
#                              la_code == "E06000057" ~ "E06000048",
#                              la_code == "E06000050" ~ "E06000049",
#                              TRUE ~ as.character(la_code))) %>% 
#   group_by(la_code) %>% 
#   summarise_at(vars(qual_denom, none, lvl1, lvl2), funs(sum(.))) %>% 
#   mutate(qual_lt2 = none + lvl1 + lvl2,
#          qual_lt2_pct = qual_lt2*100/qual_denom,
#          qual_lt1 = none + lvl1,
#          qual_lt1_pct = qual_lt1*100/qual_denom) %>% 
#   dplyr::select(la_code, qual_lt1_pct) %>% 
#   ungroup()


#----------------------------------------#
# household composition (2011 census) ----
#----------------------------------------#
household_comp <- read.csv("census/household_composition_la.csv") %>% 
  clean_names() %>% 
  dplyr::select(contains("geography"), contains("age_all"), -contains("sex_males"), -contains("sex_females")) %>% 
  dplyr::select(contains("geography"), contains("dependent"), -contains("non_dependent"), 
                sex_all_persons_age_all_categories_age_household_composition_all_categories_household_composition_measures_value) %>% 
  dplyr::select(la_code = geography_code, married_couple = 3, cohab_couple = 4, lone_parent = 5, other = 6, all_households = 7) %>% 
  mutate(dependent_denom = married_couple + cohab_couple + lone_parent + other,
         la_code = case_when(la_code == "E08000037" ~ "E08000020",
                             la_code == "E06000057" ~ "E06000048",
                             la_code == "E06000050" ~ "E06000049",
                             TRUE ~ as.character(la_code))) %>% 
  group_by(la_code) %>% 
  summarise_all(funs(sum(.))) %>% 
  mutate(lone_parent_pct = lone_parent*100/dependent_denom,
         depend_house_pct = dependent_denom*100/all_households) %>% 
  ungroup() %>% 
  dplyr::select(la_code, lone_parent_pct, depend_house_pct)





#-------------------------#
# get dsh data ----
#-------------------------#

hes_dat <- readRDS("N:/Documents/Data & Programming/DSH data/RDS files/hes_dat.rds")
hes_child_dat <- readRDS("N:/Documents/Data & Programming/DSH data/RDS files/hes_child_dat.rds")
cla_dat <- readRDS("N:/Documents/Data & Programming/DSH data/RDS files/cla_dat.rds")
hes_bt_dat <- readRDS("N:/Documents/Data & Programming/DSH data/RDS files/hes_bt_dat.rds")
  
#---------------------------#
# LA CSC ofsted results ----
#---------------------------#
#ofsted <- read_xlsx("N:/Documents/Data & Programming/Public data/gov uk/LA_csc_inspection.xlsx") %>% 
#  left_join(la_lkup, by = "la_name") %>% 
#  dplyr::select(la_code = new_la_code, inspect_date, ends_with("score")) # latest inspection


#-------------------------#
# region look-up table ----
#-------------------------#
region_lkup <- readRDS("N:/Documents/Data & Programming/RDS files/region_lkup.rds") %>% 
  dplyr::select(la_code, la_name = uaccnm, contains("region")) %>% 
  mutate(la_name = ifelse(la_code == "E06000049", "Cheshire (pre-2009)", as.character(la_name))) %>% 
  filter(la_code != "E06000050")


#----------------------------------#
# add all data sources together ----
#----------------------------------#
all_dat <- list(hes_dat, hes_child_dat, cla_dat,
                pop_est_dat, csc_spend, 
                ADM_dat, child_lowinc) %>% 
  reduce(left_join, by = c("la_code","year")) %>% 
  left_join(IMD_2010_dat, by = "la_code") %>% # IMD 2010 data 
  left_join(ethnicity, by = "la_code") %>%   # ethnicity (2011 census data)
  left_join(violent_crime, by = "la_code") %>% #for the year 2010/11 only
  #left_join(highest_qual, by = "la_code") %>% #for the year 2011 only
  left_join(household_comp, by = "la_code") %>% #for the year 2011 only
  #left_join(ofsted, by = "la_code") %>% 
  left_join(ru11, by = "la_code") %>% # census 2011 data 
  left_join(hes_bt_dat, by = "la_code") %>% # hes birthweight data 2012/13 only
  #dplyr::select(-old_la_code, -old_la_name, -new_la_code) %>% 
  mutate(early_csc_spend = csc_centre_spend + csc_youth_spend,
         early_csc_spend_pc = early_csc_spend/pop_child_tot) %>% 
  left_join(region_lkup, by = "la_code") 
  

saveRDS(all_dat, "N:/Documents/Data & Programming/RDS files/la_analysis_dat.rds")
