#----------------------------#
# LA variation analysis data #
#----------------------------#

library(dplyr)
library(purrr)

analysis_dat <- readRDS("N:/Documents/Data & Programming/RDS files/la_analysis_dat.rds") %>% 
  filter(!la_code %in% c("E09000001", "E06000053") & year > 2005) %>%  # remove city of london & isles of scilly
  mutate(n_child_pct = n_child*100/pop_U1,
         n_child_10000 = n_child*10000/pop_U1,
         pop_U1_1000 = pop_U1/1000,     
         any_ara_pct = any_ara_sum*100/n_mother,
         teen_birth_pct = teen_birth_sum*100/n_mother,
         #imd_20pc_pct = imd_20pc_sum*100/n_mother,
         imd_10pc_pct = imd_10pc_sum*100/n_mother,
         prop10_imd_pct = prop10_imd*100,
         #prop10_idaci_pct = prop10_idaci*100,
         #violence_1000 = 
         #imd_quint = as.factor(case_when(decile_imd %in% c(1,2) ~ 1,
                                         # decile_imd %in% c(3,4) ~ 2,
                                         # decile_imd %in% c(5,6) ~ 3,
                                         # decile_imd %in% c(7,8) ~ 4,
                                         # decile_imd %in% c(9,10) ~ 5)),
         total_1000 = total/1000) %>% 
  ungroup()%>% 
  mutate(year_std = year - 2006,
         ofsted_protect = factor(case_when(protect_score == "Inadequate" ~ 4,
                                           protect_score == "Requires improvement" ~ 3,
                                           protect_score == "Good" ~ 2,
                                           protect_score == "Outstanding" ~ 1)),
         ofsted_prt_bad = ifelse(protect_score %in% c("Inadequate", "Requires improvement"),1,0),
         ofsted_lscb = factor(case_when(lscb_score == "Inadequate" ~ 4,
                                        lscb_score == "Requires improvement" ~ 3,
                                        lscb_score == "Good" ~ 2,
                                        lscb_score == "Outstanding" ~ 1)))


# taking out bad la data 
dat_2010 <- filter(analysis_dat, year == 2010)

#dat_2010_cc <- filter(analysis_dat, year == 2010 & bad_la_linkrate == 0 & bad_bwmiss_rate == 0)

#------------------------------------------#
# adapt data for longitudinal modelling ----
#------------------------------------------#

# split out within and between LA effect ----
model_dat <- analysis_dat %>% 
  group_by(la_code) %>% 
  dplyr::mutate_at(vars(total_1000,
                        any_ara_pct,
                        teen_birth_pct,
                        #imd_20pc_pct,
                        imd_10pc_pct,
                        lowbw_pct,
                        preterm_pct,
                        child_lowinc,
                        starts_with("cong_anom")), 
                   list(wt = ~(. - mean(., na.rm = TRUE) ), # within-LA effect
                        bt = ~mean(., na.rm = TRUE))) %>%    # between-LA effect
  mutate(any_bad_linkrate = as.numeric(any(bad_la_linkrate == 1)),
         any_bad_bwrate = as.numeric(any(bad_bw_missrate == 1)),
         any_bad_gestatrate = as.numeric(any(bad_gestat_missrate == 1))) %>% 
  ungroup() %>% 
  left_join(dplyr::select(dat_2010, la_code, teenbirth_2010 = teen_birth_pct, lowbw_2010 = lowbw_pct,
                          child_lowinc_2010 = child_lowinc, imd_10pc_2010 = imd_10pc_pct,
                          ca_feud_2010 = cong_anom_feud_sum_pct, ca_hardelid_2010 = cong_anom_sum_pct, 
                          total_2010 = total_1000, any_ara_2010 = any_ara_pct, preterm_2010 = preterm_pct,
                          early_csc_2010 = early_csc_spend_pc), by = c("la_code"))


model_dat_cc <- model_dat %>% 
  filter(any_bad_linkrate == 0 & any_bad_bwrate == 0) %>% 
  group_by(la_code) %>% 
  dplyr::mutate_at(vars(total_1000,
                        any_ara_pct,
                        teen_birth_pct,
                        #imd_20pc_pct,
                        imd_10pc_pct,
                        lowbw_pct,
                        preterm_pct,
                        child_lowinc,
                        starts_with("cong_anom")), 
                   list(wt = ~(. - mean(., na.rm = TRUE) ), # within-LA effect
                        bt = ~mean(., na.rm = TRUE))) %>%    # between-LA effect
  ungroup() %>% 
  dplyr::select(contains("total_1000"),
                contains("any_ara_pct"),
                contains("teen_birth_pct"),
                contains("pc_pct"),
                contains("lowbw_pct"),
                contains("preterm_pct"),
                contains("child_lowinc"),
                starts_with("cong_anom"),
                contains("n_child"),
                lone_parent_pct,
                violent_crime,
                prop10_imd_pct,
                pop_U1,
                la_code,
                la_name,
                region_name,
                year_std,
                ends_with("2010")) 
