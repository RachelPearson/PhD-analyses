#----------------------------#
# latent transition analysis #
#----------------------------#

#----------------------------#
# get packages ----
#----------------------------#
library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(tibble)
library(lubridate)
library(forcats)
library(janitor)
library(lcmm)
library(ggplot2)
library(splines)
library(stringr)
library(purrr)
library(ggpubr)

#------------------------------#
# get Cafcass SLaM cohort ----
#------------------------------#
source("R scripts/creating cohorts.R")

# filter to just women who link and had their index proceedings in SLaM catchment LA between Apr 2008 to Mar 2019:
get_cohort <- 
  filter(full_cohort, any_case_slam4 == 1 & match_status == 1) %>%
  group_by(new_person_id) %>%
  dplyr::mutate(
    core4_fl = ifelse(case_seq == 1 & case_fyear > 2008 & case_fyear < 2019 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark"), 1, 0),
    core4 = max(core4_fl),
    infant_fl = ifelse(case_seq == 1 & case_fyear > 2008 & case_fyear < 2019 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark") & (!is.na(new_case_yngst_age) & new_case_yngst_age < 1), 1, 0),
    infant = max(infant_fl)
  ) %>%
  ungroup() %>%
  filter(core4 == 1) 

# get all Cafcass Person IDs associated with women in get_cohort:
core4_cohort <- get_cohort %>%
  dplyr::select(person_id, dup_person_id, new_person_id) %>%
  gather(key = "id_type", value = "id", person_id:new_person_id) %>%
  distinct(id) %>%
  unlist(use.names = FALSE)
  
# get all Cafcass Person IDs associated with women in get_cohort with an infant in their index proceedings:  
infant_cohort <- filter(get_cohort, infant == 1)  %>%
  dplyr::select(person_id, dup_person_id, new_person_id) %>%
  gather(key = "id_type", value = "id", person_id:new_person_id) %>%
  distinct(id) %>%
  unlist(use.names = FALSE)

# Get de-duplicated Cafcass Person IDs for use with CRIS data:
study_person_ids <- purrr::map(c("1_cafcass_cris_dataset_main.xlsx", 
                             "2_cafcass_cris_dataset_diagnoses.xlsx", 
                             "3_cafcass_cris_dataset_services.xlsx"),
                           ~read_xlsx(paste0("Data/Cafcass cohort + CRIS data/", .x),
                                      na= c("", "NA", "NULL"))) %>%
  reduce(left_join, by = "person_id") %>%
  filter(person_id %in% core4_cohort) %>%
  distinct(person_id) %>%
  unlist(use.names = FALSE)

#------------------------------#
# Cohort selection figures ----
#------------------------------#
n_w_infants <- length(study_person_ids[study_person_ids %in% infant_cohort]) 

n_w_unlinked <- filter(full_cohort, any_case_slam4 == 1 & case_seq == 1) %>%
  filter(case_fyear > 2008 & case_fyear < 2019 & tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark")) %>%
  distinct(new_person_id) %>%
  nrow()

n_date_excl <-  filter(full_cohort, any_case_slam4 == 1 & case_seq == 1 ) %>%
  filter(case_fyear <= 2008 | case_fyear >= 2019) %>%
  dplyr::select(new_person_id) %>%
  distinct(new_person_id) %>%
  nrow()

n_loc_excl <-  filter(full_cohort, any_case_slam4 == 1 & case_seq == 1) %>%
  filter(!tolower(la_name) %in% c("croydon", "lambeth", "lewisham", "southwark")) %>%
  dplyr::select(new_person_id) %>%
  distinct(new_person_id) %>%
  nrow()

data.frame(linked_cohort = length(study_person_ids),
           unlinked_cohort = n_w_unlinked - length(study_person_ids),
           linked_excl_loc = n_loc_excl,
           linked_excl_date = n_date_excl) %>%
write_csv("Outputs/Core4/descriptive/LTA_cohorts.csv")

#--------------------------------------#
# Get CRIS attended events data  ----
#--------------------------------------#
attd_dat <- read_xlsx("Data/Cafcass cohort + CRIS data/events and inpatient data/all attended events.xlsx",
                      na = c("", "NA", "NULL"),
                      col_types = c("numeric", "date", "date", "date", "numeric", "numeric")) %>%
  filter(person_id %in% study_person_ids & as.Date(event_date, format = "%Y-%m-%d") <= "2020-03-31") %>%
  mutate_if(is.POSIXct, as.Date) %>%
  mutate(discharge_date = if_else(inpt == 1 & is.na(discharge_date), 
                                  as.Date("31/03/2020", "%d/%m/%Y"), 
                                  as.Date(discharge_date)))


# format as one row per bed day for inpatient stay records:
inpt_dates <- ddply(filter(attd_dat, inpt == 1), .(person_id, case_start_date, event_date, discharge_date), 
                    plyr::summarise, 
                    new_date = seq(event_date, discharge_date, by = 1))
  

#----------------------------------------#
# Restrict events to observation window ---- 
#----------------------------------------#
final_attd <- bind_rows(dplyr::select(filter(attd_dat, inpt == 0), person_id, case_start_date, new_date = event_date), 
                        dplyr::select(inpt_dates, person_id, case_start_date, new_date)) %>%
  filter(new_date <= case_start_date + 1*365.25 & new_date >= case_start_date -2*365.25) %>%
  mutate(time_from_case = as.numeric(new_date - case_start_date),
         time_cat = case_when(time_from_case >= -2*365.25              & time_from_case < (-2*365.25 + 365.25/4) ~ 1,
                              time_from_case >= (-2*365.25 + 365.25/4) & time_from_case < (-2*365.25 + 365.25/2) ~ 2,
                              time_from_case >= (-2*365.25 + 365.25/2) & time_from_case < (-365.25 - 365.2/4)    ~ 3,
                              time_from_case >= (-365.25 - 365.2/4)    & time_from_case < -365.25                ~ 4,
                              time_from_case >= -365.25                & time_from_case < -365.25 + 365.25/4     ~ 5,
                              time_from_case >= -365.25 + 365.25/4     & time_from_case < -365.25/2              ~ 6,
                              time_from_case >= -365.25/2              & time_from_case < -365.25/4              ~ 7,
                              time_from_case >= -365.25/4              & time_from_case < 0                      ~ 8,
                              time_from_case >= 0                      & time_from_case < 365.25/4               ~ 9,
                              time_from_case >= 365.25/4               & time_from_case < 365.25/2               ~ 10,
                              time_from_case >= 365.25/2               & time_from_case < (365.25 - 365.25/4)    ~ 11,
                              time_from_case >= (365.25 - 365.25/4)    & time_from_case <= 365.25                ~ 12)) %>%
  distinct(person_id, new_date, time_cat) %>% # take only one appt per date
  group_by(person_id, time_cat) %>%
  dplyr::summarise(n_event = n()) %>%
  ungroup() %>%
  spread(key = time_cat, value = n_event, sep = "_", fill = 0) %>%
  full_join(data.frame(person_id = study_person_ids), by = "person_id") %>% 
  mutate_at(vars(starts_with("time_cat")), list(~ifelse(is.na(.), 0, .))) %>%
  gather(key = "time_cat", value = "n_event", time_cat_1:time_cat_12) %>%
  mutate(time_cat = as.numeric(substr(time_cat,10,length(time_cat)))) %>%
  arrange(person_id, time_cat)

#----------------------------------#
# Check sparsity ----
#----------------------------------#

final_attd %>%
  group_by(person_id) %>%
  summarise(total = sum(n_event)) %>%
  filter(total == 0) %>%
  summarise(n = n(), 
            pct = n()*100/length(study_person_ids))

#----------------------------------#
# Descriptive plots of n_event  ----
#----------------------------------#
ggplot(data = final_attd, aes(x = time_cat, y = n_event, group = person_id)) +
  geom_line(colour = "grey90") +
  theme_classic() 

## HISTOGRAM ##
ggplot(final_attd, aes(x = n_event)) +
  geom_histogram(binwidth = 3) +
  theme_classic() +
  scale_x_continuous("\nNumber of days with a SlaM contact within quarter", 
                     expand = expansion(mult = c(0,0.05)),
                     breaks = seq(0, 90, 15)) +
  scale_y_continuous("Frequency\n",
                     expand = expansion(mult = c(0,0.05)),
                     breaks = seq(0, 20000, 5000),
                     limits = c(0, 20000)) +
  theme(axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))


#------------------------------#
# Get CRIS referrals data ----
#------------------------------#
ref_dat <- read_xlsx("Data/Cafcass cohort + CRIS data/LCTM/all referrals.xlsx",
                     na = c("", "NA", "NULL")) %>%
  filter(person_id %in% study_person_ids) %>%
  mutate_if(is.POSIXct, as_date) %>%
  group_by(person_id) %>%
  arrange(person_id, ref_date) %>%
  mutate(ref_num = row_number(),
         over_index = ifelse(ref_date <= (case_start_date + 92) & (is.na(dis_date) | dis_date > case_start_date), 1, 0),
         time_to_ref = difftime(as.Date(ref_date), as.Date(case_start_date), units = "days"),
         time_to_dis = difftime(as.Date(dis_date), as.Date(case_start_date), units = "days"),
         ref_dur = ifelse(!is.na(dis_date) & dis_status != "rejected", 
                          difftime(as.Date(dis_date), as.Date(ref_date), units = "days"),
                          NA)) %>%
  ungroup() %>%
  filter(is.na(dis_date) | ref_date < dis_date)

# median duration (non-missing end dates):
quantile(ref_dat$ref_dur, na.rm = TRUE, c(0.25, 0.5, 0.75, 0.99))
hist(ref_dat$ref_dur)

# how many had an active referral over their index set of proceedings?
filter(ref_dat, dis_status != "rejected") %>%
  group_by(person_id) %>%
  summarise(index = any(over_index == 1)) %>%
  ungroup() %>%
  summarise(n = sum(index == 1),
            pct = sum(index == 1)*100/length(study_person_ids))


# split by time-cat:
quantile(final_attd$n_event, probs = c(0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8))

# across all time_cat:
with(final_attd %>% group_by(person_id) %>% summarise(n = sum(n_event)) %>% ungroup(), quantile(n, probs = c(0.25, 0.5, 0.75)))


#------------------------------#
# Perform LCTM  ----
#------------------------------#

# load functions from LCTMtools package:
source("LCTMtools-master/R/appa.r")
source("LCTMtools-master/R/class_assignment.r")
source("LCTMtools-master/R/occ.r")
source("LCTMtools-master/R/mismatch.r")
source("LCTMtools-master/R/entropy.r")
source("LCTMtools-master/R/relative_entropy.r")
source("LCTMtools-master/R/LCTMcompare.r")
source("LCTMtools-master/R/LCTMtoolkit.r")
source("LCTMtools-master/R/residualplot_step1.r")


#-------------------------------------------------------------------------#
# Step 1: test one-class model with different link functions ----
#-------------------------------------------------------------------------#

# attended events only:
model_lin <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                        random = ~time_cat, 
                        subject = "person_id",
                        link = "linear", # continuous but non-normal outcome
                        ng = 1,
                        idiag = TRUE,
                        range = c(0, 92),
                        maxiter = 200,
                        data = data.frame(final_attd))

model_beta <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), 
                        random = ~time_cat, 
                        subject = "person_id",
                        link = "beta", # continuous but non-normal outcome
                        ng = 1,
                        idiag = TRUE,
                        range = c(0, 92),
                        maxiter = 400,
                        data = data.frame(final_attd))

model_2qspl <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                          random = ~time_cat, 
                          subject = "person_id",
                          link = "2-quant-splines", # max quant we can have
                          ng = 1,
                          idiag = TRUE,
                          range = c(0, 92),
                          maxiter = 200,
                          data = data.frame(final_attd))

model_5espl <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                          random = ~time_cat, 
                          subject = "person_id",
                          link = "5-equi-splines", # max quant we can have
                          ng = 1,
                          idiag = TRUE,
                          range = c(0, 92),
                          maxiter = 200,
                          data = data.frame(final_attd))
                              

model_5mspl <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), 
                          random = ~time_cat, 
                          subject = "person_id",
                          link = "5-manual-splines", # continuous but non-normal outcome
                          ng = 1,
                          idiag = TRUE,
                          intnodes = c(1,3,15), # 70, 80, 95% quantile
                          range = c(0, 92),
                          maxiter = 200,
                          data = data.frame(final_attd))

# check model convergence:
summarytable(model_lin, model_beta, model_2qspl, model_5espl, model_5mspl,
             which = c("loglik", "conv", "npm", "AIC", "BIC")) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "model") %>%
write_csv("Outputs/Core4/descriptive/lta_link_table.csv")

# plot link functions:
col <- rainbow(5)
plot(model_lin, which = "linkfunction", bty = "l", ylab = "Number of SLaM contacts", col = col[1], lwd = 2, xlab = "Underlying latent process")
plot(model_beta,  which = "linkfunction", add = TRUE, col = col[2], lwd = 2)
plot(model_2qspl, which = "linkfunction", add = TRUE, col = col[3], lwd = 2)
plot(model_5espl, which = "linkfunction", add = TRUE, col = col[4], lwd = 2)
plot(model_5mspl, which = "linkfunction", add = TRUE, col = col[5], lwd = 2)
legend(x = "topleft", legend = c("linear",
                                 "beta",
                                 "splines (2 at quantiles)",
                                 "splines (5 equidistant)",
                                 "splines (5 at 0, 1, 3, 15, 92)"),
       col = col, bty = "n", lwd = 2)


#--------------------------------------------------------------------------------#
# Step 2: run model specs for 1-6 class solutions ----
#--------------------------------------------------------------------------------#

get_lcmm <- function(i){
  
  # set seed:
  set.seed(12345)
  
  
  if(i == 1) {
  model <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                      random = ~time_cat,
                      subject = "person_id",
                      link = "5-manual-splines",
                      intnodes = c(1,3,15),
                      idiag = TRUE,
                      ng = i,
                      data = data.frame(final_attd))
  } else{
  
  model <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), 
                      mixture = ~ splines::ns(time_cat, df = 3), 
                      random = ~time_cat,
                      subject = "person_id",
                      link = "5-manual-splines",
                      intnodes = c(1,3,15),
                      ng = i,
                      idiag = TRUE,
                      maxiter = 500,
                      data = data.frame(final_attd))
  }
             
  return(model)
}

# run LCMMS:
model_list <- map(seq(1,7), ~get_lcmm(i = .))


# get info about model convergence and relative fit:
get_gfit <- function(model){
  fit <- data.frame(k = model$ng, converge = model$conv, AIC = model$AIC, BIC = model$BIC)
  return(fit)
}

map_df(model_list, ~get_gfit(.)) %>%
  knitr::kable(col.names = c("k", "converge", "AIC", "BIC"), row.names = FALSE, align = "c")


# Get numbers assigned to classes for each model:
map(model_list, "pprob") %>%
  map(., "class") %>%
  map_df(., ~as.data.frame(table(.), stringsAsFactors = FALSE), .id = "model") %>%
  dplyr::select(model, class_num = 2, n = 3)

# avepp:
map(model_list[-1], ~LCTMtoolkit(.)) %>%
  map(., "appa") %>%
  map_df(., ~as.data.frame(., stringsAsFactors = FALSE), .id = "model") %>%
  clean_names() %>%
  mutate(model = as.numeric(model) + 1) 

# OCCs:
map(model_list[-1], ~LCTMtoolkit(.)) %>%
  map(., "occ") %>%
  map_df(., ~as.data.frame(., stringsAsFactors = FALSE), .id = "model") %>%
  clean_names() %>%
  mutate(model = as.numeric(model) + 1)

# entropy:
map(model_list[-1], ~LCTMtoolkit(.)) %>%
  map(., "entropy") %>%
  map_df(., ~as.data.frame(., stringsAsFactors = FALSE), .id = "model") %>%
  clean_names() %>%
  mutate(model = as.numeric(model) + 1)

# relative entropy:
map(model_list[-1], ~LCTMtoolkit(.)) %>%
  map(., "relativeentropy") %>%
  map_df(., ~as.data.frame(., stringsAsFactors = FALSE), .id = "model") %>%
  clean_names() %>%
  mutate(model = as.numeric(model) + 1) 


# create function to plot the predicted latent trajectories for each model:
get_pred_plot <- function(model) {
  
  new_dat <- data.frame(time_cat = seq(1, 12, 1))
  
  pred_dat <- predictY(model, newdata = new_dat, var.time = "time_cat", draw = TRUE)
  
  plot(pred_dat, lwd = c(2,1), type = "l", xlab = "quarters (3-month periods)",
       ylab = "Number of SLaM contacts", bty = "l", legend = NULL, shades = TRUE)
}



par(mfrow = c(3,2))
get_pred_plot(model_list[[1]])
get_pred_plot(model_list[[2]])
get_pred_plot(model_list[[3]])
get_pred_plot(model_list[[4]])
get_pred_plot(model_list[[5]])
get_pred_plot(model_list[[6]])
#get_pred_plot(model_list[[7]]) # did not converge
dev.off()


# Re-fit any that did not converge with gridsearch:

# NA

#-------------------------------------------------------------------------#
# Step 3: test for optimal model structure (6-class model only) ----
#-------------------------------------------------------------------------#

# A) allow random intercept and random slope to be correlated & allow var-covar matrix to vary by class:
model_a <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                      mixture = ~splines::ns(time_cat, df = 3),
                      random = ~time_cat,
                      subject = "person_id",
                      link = "5-manual-splines",
                      intnodes = c(1,3,15),
                      idiag = FALSE, # allow REs to be correlated
                      nwg = FALSE, # allow var-covar matrix to vary across classes
                      ng = 6,
                      range = c(0,92),
                      data = data.frame(final_attd))
                      
# check convergence?
model_a$conv

# try again with gridsearch:
model_a1 <- gridsearch(rep = 10,
                       maxiter = 10,
                       minit = model_a,
                       m = lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                                      mixture = ~splines::ns(time_cat, df = 3),
                                      random = ~time_cat,
                                      subject = "person_id",
                                      link = "5-manual-splines",
                                      intnodes = c(1,3,15),
                                      idiag = FALSE, # allow REs to be correlated
                                      nwg = FALSE, # allow var-covar matrix to vary across classes
                                      ng = 6,
                                      range = c(0,92),
                                      data = data.frame(final_attd))
                        )

# check convergence?
model_a1$conv

# B) # allow random intercept and random slope to be correlated & do not allow var-covar matrix to vary by class:
model_b <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), # + I(time_cat^2),
                      mixture = ~ splines::ns(time_cat, df = 3), # + I(time_cat^2),
                      random = ~time_cat,
                      subject = "person_id",
                      link = "5-manual-splines",
                      intnodes = c(1,3,15),
                      ng = 6,
                      range = c(0,92),
                      idiag = FALSE,
                      nwg = TRUE,
                      data = data.frame(final_attd))
                    
# check convergence?
model_a$conv
                      
# try again with gridsearch:
model_b1 <- gridsearch(rep = 10,
                       maxiter = 10,
                       minit = model_b,
                       m = lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3),
                                      mixture = ~splines::ns(time_cat, df = 3),
                                      random = ~time_cat,
                                      subject = "person_id",
                                      link = "5-manual-splines",
                                      intnodes = c(1,3,15),
                                      idiag = FALSE, 
                                      nwg = TRUE, 
                                      ng = 6,
                                      range = c(0,92),
                                      data = data.frame(final_attd))
                        )
                        
# check convergence?
model_b1$conv                      

# C) do not allow random intercept and random slope to be correlated & do not allow var-covar matrix to vary by class:
model_c <- lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), # + I(time_cat^2),
                      mixture = n_event ~ splines::ns(time_cat, df = 3), # + I(time_cat^2),
                      random = ~time_cat,
                      subject = "person_id",
                      link = "5-manual-splines",
                      intnodes = c(1,3,15),
                      range = c(0,92),
                      ng = 6,
                      idiag = TRUE,
                      nwg = TRUE,
                      maxiter = 500,
                      data = data.frame(final_attd))


# check convergence?
model_c$conv

model_c1 <- gridsearch(rep = 10,
                       maxiter = 20,
                       minit = model_list[[1]],
                       m = lcmm::lcmm(fixed = n_event ~ splines::ns(time_cat, df = 3), # + I(time_cat^2),
                                      mixture = ~splines::ns(time_cat, df = 3), # + I(time_cat^2),
                                      random = ~time_cat,
                                      subject = "person_id",
                                      link = "5-manual-splines",
                                      intnodes = c(1,3,15),
                                      ng = 6,
                                      idiag = TRUE,
                                      nwg = TRUE,
                                      data = data.frame(final_attd))
)


# none converged...

#------------------------------------------#
# Step 4: plot observed trajectories ----
#------------------------------------------#


# observed mean/median trajectories for final model:
final_model <- model_list[[6]]

plot(final_model, which = "fit", var.time = "time_cat", break.times = seq(1,12,1), 
     marg = TRUE, shades = TRUE, legend = NULL)


final_model_dat <- final_attd %>%
  left_join(as.data.frame(final_model$pprob), by = "person_id")

# observed mean by class and time:
obs_traj <- final_model_dat %>%
  group_by(class, time_cat) %>%
  summarise(median = median(n_event),
            q_025  = quantile(n_event,0.025),
            q_25   = quantile(n_event,0.25),
            q_75   = quantile(n_event,0.75),
            q_975  = quantile(n_event,0.975),
            mean   = mean(n_event),
            std    = sd(n_event)
            )

# observed mean by class:
final_model_dat %>%
  group_by(class) %>%
  summarise(median = median(n_event),
            q_025  = quantile(n_event,0.025),
            q_25   = quantile(n_event,0.25),
            q_75   = quantile(n_event,0.75),
            q_975  = quantile(n_event,0.975),
            mean   = mean(n_event),
            std    = sd(n_event)
            ) %>%
  mutate(time_cat = 99) %>%
  bind_rows(obs_traj)


#--------------------------------------------#
# Step 5: plot model predicted trajectories  ----
#--------------------------------------------#

# plot predicted trajectories:
new_dat <- data.frame(time_cat = seq(1,12,1))
plotpred <- predictY(final_model, new_dat, 
                      var.time = "time_cat", draws = TRUE, methInteg = 1, nsim = 500)

data.frame(time_cat = plotpred$times, 
           as.data.frame(plotpred$pred))
            

# data for plotting:


plotpred_df <- as.data.frame(plotpred$pred) %>%
  rowid_to_column(var = "time_cat") %>%
  gather(key = "Ypred", value = "value", Ypred_50_class1:Ypred_97.5_class6) %>%
  mutate(class = as.numeric(str_sub(Ypred, start = -1)),
         Ypred = gsub("_class.*$", "", Ypred)) %>%
  spread(key = Ypred, value = value) %>%
  left_join(as.data.frame(final_model$pprob) %>% group_by(class) %>% dplyr::summarise(n = n()), by = "class") %>%
  dplyr::mutate(class_lbl = as.factor(paste0("Group ", class, ", (n = ", n,", ", round(n*100/length(study_person_ids), 1), "%)")))
  
# GGPLOT:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = filter(plotpred_df, time_cat < 9), 
         aes(x = time_cat, y = Ypred_50, ymin = Ypred_2.5, ymax = Ypred_97.5, 
             group = class_lbl, colour = class_lbl, fill = class_lbl)) +
  facet_wrap(~class_lbl, nrow = 2, ncol = 3) +
  geom_ribbon(alpha = 0.25, colour = NA) +
  geom_line() +
  geom_point(shape = 21, fill = "white", size = 2.5) +
  geom_ribbon(data = filter(plotpred_df, time_cat >= 9),
              alpha = 0.25, colour = NA) +
  geom_line(data = filter(plotpred_df, time_cat >= 9)) +
  geom_point(data = filter(plotpred_df, time_cat >= 9),
             shape = 21, fill = "white", size = 2.5) +
  theme_classic() +
  geom_vline(xintercept = 8.5, linetype = "dashed", colour = "grey70") +
  geom_text(x = 8.5, y = 25, 
            label = " start of\n proceedings", colour = "black", hjust = 0, check_overlap = T,
            size = 3.5) +
  scale_x_continuous("\nQuarter (i.e. 3-month period)", breaks = c(seq(1,8,1), 8.5, seq(9,12,1)), labels = c(seq(-8,-1,1), "", seq(1,4,1))) +
  scale_y_continuous("Predicted number \nof days with \na SLaM contact ", limits = c(0,27), breaks = seq(0,25,5)) +
  scale_color_manual("", values = cbPalette[1:6]) +
  scale_fill_manual("", values = cbPalette[1:6])  +
  guides(fill = "none", colour = "none") +
  theme(axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 13),
        panel.grid.major.y = element_line(colour = "grey90")) #+
#ggtitle("Predicted trajectories of SLaM service use (outpatient and inpatient) within\nthe two years before and one year after the onset of care proceedings")


# join class assignment to original data and plot individual trajectories:
final_attd_wclass <- final_attd %>%
  left_join(as.data.frame(model_a$pprob), by = "person_id") %>%
  left_join(as.data.frame(model_a$pprob) %>% group_by(class) %>% dplyr::summarise(n = n()), by = "class") %>%
  mutate(class_lbl = as.factor(paste0(class, " (", round(n*100/length(study_person_ids), 1), "%)")))

ggplot(data = final_model_dat,
       aes(x = time_cat, y = n_event, colour = as.factor(class))) +
  facet_wrap(~class, nrow = 2, ncol = 3) +
  geom_line(data = filter(final_model_dat, time_cat < 9),
            aes(group = person_id),
            alpha = 0.1) +
  geom_line(data = filter(final_model_dat, time_cat >= 9), 
            aes(group = person_id),
            alpha = 0.1) +
  geom_smooth(data = filter(final_model_dat, time_cat < 9),
              aes(group = class), 
              se = FALSE,
              method = "loess",
              size = 2) +
  geom_smooth(data = filter(final_model_dat, time_cat >= 9), 
              aes(group = class),
              se = FALSE,
              method = "loess",
              size = 2) +  
  geom_vline(xintercept = 8.5, linetype = "dashed", colour = "grey70") +
  theme_classic() +
  scale_color_manual("", values = cbPalette[1:6]) +
  guides(colour = "none") +
  scale_x_continuous("\nQuarter", breaks = c(seq(1,8,1), 8.5, seq(9,12,1)), labels = c(seq(-8,-1,1), "", seq(1,4,1))) +
  scale_y_continuous("Predicted number of events", limits = c(0,25))



# Number with no events over follow-up, by class:
group_by(final_model_dat, person_id, class) %>%
  dplyr::summarise(tot_n_event = sum(n_event, na.rm = TRUE)) %>% 
  mutate(bin = ifelse(tot_n_event == 0, 0, 1)) %>%
  group_by(class, bin) %>%
  dplyr::summarise(n = n()) %>%
  group_by(class) %>%
  mutate(pct = n*100/sum(n))

#--------------------------------------------------#
# Plot observed and predicted for final LTA model ----
#--------------------------------------------------#

ggplot(data = filter(plotpred_df, time_cat < 9), 
       aes(x = time_cat, 
           group = as.factor(class), colour = as.factor(class), fill = as.factor(class))) +
  facet_wrap(~class, nrow = 3, ncol = 2) +
  geom_ribbon(aes(y = Ypred_50, ymin = Ypred_2.5, ymax = Ypred_97.5),
              alpha = 0.25, colour = NA) +
  geom_line(aes(y = Ypred_50)) +
  geom_point(aes(y = Ypred_50), 
             shape = 21, size = 2.5) +
  geom_ribbon(data = filter(plotpred_df, time_cat >= 9),
              aes(y = Ypred_50, ymin = Ypred_2.5, ymax = Ypred_97.5),
              alpha = 0.25, colour = NA) +
  geom_line(data = filter(plotpred_df, time_cat >= 9),
            aes(y = Ypred_50)) +
  geom_point(data = filter(plotpred_df, time_cat >= 9),
             aes(y = Ypred_50),
             shape = 21, size = 2.5) +
  
  #observed:
  geom_line(data = filter(obs_traj, time_cat < 9),
            aes(y = median, linetype = "median"),
            colour = "black", size = 0.75) + 
  geom_line(data = filter(obs_traj, time_cat >= 9),
            aes(y = median, linetype = "median"),
            colour = "black", size = 0.75) + 
  geom_line(data = filter(obs_traj, time_cat < 9),
            aes(y = q_025, linetype = "2.5% and 97.5% percentile"), 
            colour = "black", size = 0.75) + 
  geom_line(data = filter(obs_traj, time_cat >= 9),
            aes(y = q_025,
                linetype = "2.5% and 97.5% percentile"),
            colour = "black", size = 0.75) +
  geom_line(data = filter(obs_traj, time_cat < 9),
            aes(y = q_975,
                linetype = "2.5% and 97.5% percentile"),
            colour = "black", size = 0.75) + 
  geom_line(data = filter(obs_traj, time_cat >= 9),
            aes(y = q_975,
            linetype = "2.5% and 97.5% percentile"), 
            colour = "black", size = 0.75) +

  # format: 
  theme_classic() +
  scale_linetype_manual("Observed values:", values = c("dotted", "dashed")) +
  geom_vline(xintercept = 8.5, linetype = "dashed", colour = "grey70") +
  scale_x_continuous("\nQuarter (i.e. 3-month period)", 
                     breaks = c(seq(1,8,1), 8.5, seq(9,12,1)), 
                     labels = c(seq(-8,-1,1), "", seq(1,4,1))) +
  scale_y_continuous("Predicted number of days with a SLaM contact\n ", 
                     #limits = c(0,100), 
                     breaks = seq(0, 100, 20)) +
  scale_color_manual("", values = cbPalette[1:6]) +
  scale_fill_manual("", values = cbPalette[1:6])  +
  guides(fill = "none", colour = "none") +
  theme(#axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        axis.title   = element_text(size = 12),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 13),
        legend.text  = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.position = "top",
        legend.key.width = unit(2, "cm"))



#--------------------------------------------------#
# Step 6: summarise outcome by class ----
#--------------------------------------------------#
overall_summary <- final_attd %>%
  summarise(mean   = mean(n_event),
            std    = sd(n_event),
            median = median(n_event),
            q_25   = quantile(n_event, 0.25),
            q_75   = quantile(n_event, 0.75),
            n_0    = sum(n_event == 0),
            pct_0  = sum(n_event == 0)*100/nrow(final_attd)) %>%
  mutate(class = 999)

class_summary <- final_attd %>%
  left_join(as.data.frame(final_model$pprob), by = "person_id") %>%
  group_by(class) %>%
  summarise(mean   = mean(n_event),
            std    = sd(n_event),
            median = median(n_event),
            q_25   = quantile(n_event, 0.25),
            q_75   = quantile(n_event, 0.75),
            n_0    = sum(n_event == 0),
            pct_0  = sum(n_event == 0)*100/nrow(final_attd)) %>%
  ungroup() %>%
  bind_rows(overall_summary) %>%
  mutate_all(~round_half_up(., 2))


