
#--------------------------#
# LA variation - modelling #
#--------------------------#

# load packages ----
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(readr)
library(purrr)
library(ggplot2)
library(naniar)
library(broom)
library(lme4)
library(ggrepel)
library(reshape2)
library(janitor)
library(tableone)
library(glmnet)
library(doParallel)
library(MuMIn)
library(lmtest)
library(GGally)
library(car)
library(stringr)

#--------------#
# get data ----
#--------------#
source("N:/Documents/Data & Programming/R Scripts/la variation/la_variation_analysis_data.r", echo = FALSE)

#------------------------------------------#
# look at correlation between variables ----
#------------------------------------------#

# using only data from 2010/11 (i.e. mid-point)
dat_forcor <- model_dat %>% 
  filter(year == 2010) %>% 
  select(ends_with("_bt"), lone_parent_pct, violent_crime, prop10_imd_pct)

# create correlations (method = spearman's)
cormat <- round(cor(dat_forcor, use = "complete.obs", method = 'spearman'),2) %>% 
  melt()

# plot these in coloured tiles
ggplot(data = cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradientn(colours=rainbow(2))
ggsave("N:/Documents/Data & Programming/outputs/la variation/figures/correlation_analysis_variables.png")


# try again but with distribution of each var in plot (using GGpairs):
dat_forcor <- filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0) %>% 
       filter(year == 2010) %>% 
       dplyr::select(teenage_births_pct= teenbirth_2010, propmumsLSOA_in_10pct_mostdepr= imd_10pc_2010, 
              low_birweit_pct = lowbw_2010, 
              congenital_anoms_pct= ca_feud_2010,
              lone_parent_pct, ARA_pct= any_ara_pct_bt, violent_crime,
              propLSOA_in_10pct_mostdepr= prop10_imd_pct)

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "black") +
    geom_smooth(method = method, color = "red", ...)+
    theme_classic()
  p
}

ggpairs(dat_forcor, lower = list(continuous = wrap(lowerFn, method = "lm")))
ggsave("N:/Documents/Data & Programming/outputs/la variation/figures/correlation_analysis_variables_scatter.png", units = "mm",
       width = 510, height = 400)

#----------------------------------------------------------------------------------------------#
# fitting lmer models ----
#----------------------------------------------------------------------------------------------#
set.seed(1234)

# model diagnostic functions:
source("N:/Documents/Data & Programming/R Scripts/lmer_diagnostics.R", echo = FALSE)

# pseudo R-squared confidence interval bootstrapping function:
source("N:/Documents/Data & Programming/R Scripts/la variation/la_variation_lmer_bs.R", echo = FALSE)

# function to create dataframe of lmer object results:
res_2_df <- function(model, model_R2, model_name){
  lmer_null_tidy <- broom::tidy(model, conf.int = TRUE) %>% # get model estimates with confidence intervals
    mutate(R2m_actual = r.squaredGLMM(model)[1,1],# get actual marginal R-squared
           R2c_actual = r.squaredGLMM(model)[1,2],# get actual conditional R-squared
           AIC = AIC(model),
           BIC = BIC(model),
           model = model_name,
           R2m_median = distinct(filter(model_R2, type_R2 == "R2m"),median)$median, # from 10,000 bootstrapped samples
           R2m_ll = distinct(filter(model_R2, type_R2 == "R2m"),ll)$ll, 
           R2m_ul = distinct(filter(model_R2, type_R2 == "R2m"),ul)$ul,
           R2c_median = distinct(filter(model_R2, type_R2 == "R2c"),median)$median,
           R2c_ll = distinct(filter(model_R2, type_R2 == "R2c"),ll)$ll,
           R2c_ul = distinct(filter(model_R2, type_R2 == "R2c"),ul)$ul)
}

#-----------------#
# Null model   ----
#-----------------# 

lmer_null <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0), 
                  formula = as.formula("n_child_10000 ~ year_std + (year_std|la_code)"), REML = TRUE)

# get median and 95% CI for pseudo R-squared:
lmer_null_bs <- R2_bootstrap(model = lmer_null)

# create dataframe of model results:
lmer_null_tidy <- res_2_df(model = lmer_null, model_R2 = lmer_null_bs, model_name = "null")

# without random slope:
lmer_null_noslope <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0), 
                          formula = as.formula("n_child_10000 ~ year_std + (1|la_code)"), REML = TRUE)

G <- -2*logLik(lmer_null_noslope) + 2*logLik(lmer_null)
pchisq(as.numeric(G), df = 2, lower.tail = FALSE) # p < 0.001


#-----------------------------------#
# Unadjusted association model   ----
#-----------------------------------# 
lmer_aras <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0), 
                  formula = as.formula("n_child_10000 ~ year_std + any_ara_pct_wt + any_ara_pct_bt + (year_std|la_code)"), 
                  REML = TRUE)

# get median and 95% CI for pseudo R-squared:
lmer_aras_bs <- R2_bootstrap(model = lmer_aras)

# create dataframe of model results:
lmer_aras_tidy <- res_2_df(model = lmer_aras, model_R2 = lmer_aras_bs, model_name = "aras")

# without random slope:
lmer_aras_noslope <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0), 
                          formula = as.formula("n_child_10000 ~ year_std + any_ara_pct_wt + any_ara_pct_bt + (1|la_code)"), REML = TRUE)

G <- -2*logLik(lmer_aras_noslope) + 2*logLik(lmer_aras)
pchisq(as.numeric(G), df = 2, lower.tail = FALSE) # p < 0.001

#------------------------#
# Full models         ----
#------------------------#

# full model with random intercept for LA and random slope for year:
lmer_full <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                 formula = as.formula(paste("n_child_10000 ~ year_std",
                                            "any_ara_pct_wt", "any_ara_pct_bt",
                                            "teenbirth_2010",
                                            "imd_10pc_2010",
                                            "lowbw_2010",
                                            "ca_feud_2010",
                                            "total_2010",
                                            "lone_parent_pct",
                                            "violent_crime", 
                                            "(year_std|la_code)",
                                            sep = "+")), REML = T)

# get bootstrap estimate of pseudo R-squared measures (and confidence intervals): 
lmer_full_bs <- R2_bootstrap(model = lmer_full)

# create dataframe of model results:
lmer_full_tidy <- res_2_df(model = lmer_full, model_R2 = lmer_full_bs, model_name = "full")

# test equality of with/between effects
linearHypothesis(lmer_full, "any_ara_pct_wt = any_ara_pct_bt") # insufficient evidence to reject H0

# without random slope:
lmer_full_noslope <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0), 
                          formula = as.formula(paste("n_child_10000 ~ year_std",
                                                     "any_ara_pct_wt", "any_ara_pct_bt",
                                                     "teenbirth_2010",
                                                     "imd_10pc_2010",
                                                     "lowbw_2010",
                                                     "ca_feud_2010",
                                                     "total_2010",
                                                     "lone_parent_pct",
                                                     "violent_crime", 
                                                     "(1|la_code)",
                                                     sep = "+")), REML = TRUE)

# compare random intercept model with random coefficient model:
G <- -2*logLik(lmer_full_noslope) + 2*logLik(lmer_full)
pchisq(as.numeric(G), df = 2, lower.tail = FALSE)# p < 0.001


# full model without disagregation of within/between LA effects of maternal ARA:
lmer_full_nodisag <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                         formula = as.formula(paste("n_child_10000 ~ any_ara_pct + year_std",
                                                    "teenbirth_2010",
                                                    "imd_10pc_2010",
                                                    "lowbw_2010",
                                                    "ca_feud_2010",
                                                    "total_2010",
                                                    "lone_parent_pct",
                                                    "violent_crime", 
                                                    #"prop10_imd_pct",
                                                    "(year_std|la_code)",
                                                    sep = "+")), REML = T)

lmer_full_nodisag_bs <- R2_bootstrap(model = lmer_full_nodisag)

lmer_full_nodisag_tidy <- res_2_df(model = lmer_full_nodisag, model_R2 = lmer_full_nodisag_bs, 
                                   model_name = "full_nodisag")

# higher order year_std terms:
lmer_full_nodisag_2 <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                          formula = as.formula(paste("n_child_10000 ~ any_ara_pct + poly(year_std, 2, raw = TRUE)",
                                                     "teenbirth_2010",
                                                     "imd_10pc_2010",
                                                     "lowbw_2010",
                                                     "ca_feud_2010",
                                                     "total_2010",
                                                     "lone_parent_pct",
                                                     "violent_crime", 
                                                     #"prop10_imd_pct",
                                                     "(poly(year_std, 2, raw = TRUE)|la_code)",
                                                     sep = "+")), REML = T)

lmer_full_nodisag_2_bs <- R2_bootstrap(model = lmer_full_nodisag_2)

lmer_full_nodisag_2_tidy <- res_2_df(model = lmer_full_nodisag_2, model_R2 = lmer_full_nodisag_2_bs, 
                                   model_name = "full_nodisag_poly2")

lmer_full_nodisag_3 <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                            formula = as.formula(paste("n_child_10000 ~ any_ara_pct + poly(year_std, 3, raw = TRUE)",
                                                       "teenbirth_2010",
                                                       "imd_10pc_2010",
                                                       "lowbw_2010",
                                                       "ca_feud_2010",
                                                       "total_2010",
                                                       "lone_parent_pct",
                                                       "violent_crime", 
                                                       #"prop10_imd_pct",
                                                       "(poly(year_std, 3, raw = TRUE)|la_code)",
                                                       sep = "+")), REML = T)

lmer_full_nodisag_3_bs <- R2_bootstrap(model = lmer_full_nodisag_3)

lmer_full_nodisag_3_tidy <- res_2_df(model = lmer_full_nodisag_3, model_R2 = lmer_full_nodisag_3_bs, 
                                     model_name = "full_nodisag_poly3")

#-------------------------#
# save analysis models ----
#-------------------------#
model_list <- list(lmer_null,
                   lmer_null_noslope,
                   lmer_aras,
                   lmer_aras_noslope,
                   lmer_full,
                   lmer_full_noslope,
                   lmer_full_nodisag,
                   lmer_full_nodisag_2,
                   lmer_full_nodisag_3,
                   lmer_null_bs,
                   lmer_aras_bs,
                   lmer_full_bs,
                   lmer_full_nodisag_bs,
                   lmer_full_nodisag_2_bs,
                   lmer_full_nodisag_3_bs)
save(list = "model_list", file = "N:/Documents/Data & Programming/outputs/la variation/modelling/model_list.rda")

# save raw results:
write_csv(lmer_null_tidy, "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_null_results.csv")
write_csv(lmer_aras_tidy, "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_aras_results.csv")
write_csv(lmer_full_tidy, "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_wslope_results.csv")
write_csv(lmer_full_nodisag_tidy, "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_nodisag_results.csv")


#----------------------#
# model diagnostics ----
#----------------------#

# examine level-1 residuals using histograms and quantile-quantile plots---- 


# null model:
png("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_null_lvl1_dist.png",
    width = 300, height = 150, res = 300, units = "mm")
par(mfrow = c(1,2))
lmer1_hist(model = lmer_null)
lmer1_qnorm(model = lmer_null)
dev.off()

# aras only model:
png("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_aras_lvl1_dist.png",
    width = 300, height = 150, res = 300, units = "mm")
par(mfrow = c(1,2))
lmer1_hist(model = lmer_aras)
lmer1_qnorm(model = lmer_aras)
dev.off()

# full model:
png("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_lvl1_dist.png",
    width = 300, height = 150, res = 300, units = "mm")
par(mfrow = c(1,2))
lmer1_hist(model = lmer_full)
lmer1_qnorm(model = lmer_full)
dev.off()

# full model - ara disaggregated:
png("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_nodisag_lvl1_dist.png",
    width = 300, height = 150, res = 300, units = "mm")
par(mfrow = c(1,2))
lmer1_hist(model = lmer_full_nodisag)
lmer1_qnorm(model = lmer_full_nodisag)
dev.off()

#--------------------------------------#
# interaction between time and ARA ----
#--------------------------------------#

# extra packages:
library(interplot)

# interaction model:
lmer_full_int <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                      formula = as.formula(paste("n_child_10000 ~ year_std*any_ara_pct",
                                                 "teenbirth_2010",
                                                 "imd_10pc_2010",
                                                 "lowbw_2010",
                                                 "ca_feud_2010",
                                                 "total_2010",
                                                 "lone_parent_pct",
                                                 "violent_crime", 
                                                 "(1|la_code)",
                                                 sep = "+")), REML = T)

# get bootstrap estimate of pseudo R-squared measures (and confidence intervals): 
lmer_full_int_bs <- R2_bootstrap(model = lmer_full_int)

# collate results:
lmer_full_int_tidy <- res_2_df(model = lmer_full_int, model_R2 = lmer_full_int_bs, model_name = "full_int") #%>% 
  #mutate_at(vars(contains("R2"), contains("conf"), "estimate"),list(~sprintf("%.2f", round(.,2)))) %>% 
  #mutate(ci = paste0(conf.low," to ", conf.high))

# save results:
write_csv(lmer_full_int_tidy,
          "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_int_results.csv")

# model diagnostics:
lmer1_hist(model = lmer_full_int)
lmer1_qnorm(model = lmer_full_int)

# predictive values:
range(predict(lmer_full_int))


# plot interaction effect:
int_data <- interplot(lmer_full_int, var2= "year_std", var1 = "any_ara_pct",plot = FALSE) %>% 
  mutate_at(vars("coef", "ub", "lb"),list(~sprintf("%.2f", round(.,2)))) %>% 
  mutate(ci = paste0(ub," to ", lb))

write_csv(int_data, "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_full_int_results_byyr.csv")


interplot(lmer_full_int, var2= "year_std", var1 = "any_ara_pct",
                    point = TRUE) +
  scale_x_continuous("",
                     breaks = seq(0,7,1), labels = paste(seq(2006,2013,1), 
                                                         substr(seq(2007,2014),3,4), sep = "/")) +
  scale_y_continuous("Estimated coefficient\n",
                     breaks = seq(-2,4,2), limits = c(-2.2,5.2)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed",colour = "grey60") +
  #geom_text(data = int_data, aes(x = year_std, y = as.numeric(lb), label = paste0(coef, "\n(", ci, ")")),
  #          vjust = -1,
  #          size = 5) +
  theme_classic() +
  geom_errorbar(data = int_data, aes(x = year_std, ymin = as.numeric(ub), ymax = as.numeric(lb)),
                inherit.aes = FALSE, width = 0.2, colour = "#5379a9ff", size = 1.25) +
  geom_point(size = 5, data = int_data, aes(x = year_std, y = as.numeric(coef)),
             shape = 21, fill = "white", colour = "#5379a9ff") +
  #scale_fill_brewer(palette = "YlGnBu") +
  labs(caption = "") +
  guides(fill = "none") +
  #geom_text()
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        title = element_text(size = 18))

#require("svglite")
ggsave("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_interaction_plot_poster.png", units = "mm",
       height = 150, width = 300, dpi = 300)

#-----------------------------------------------#
# plot fixed-effect coeffs and their 95% CIs ----
#-----------------------------------------------#
# create variable labels:
lbls <- data.frame(label = c("any_ara_pct_bt" = "Between-cluster: % of live births with maternal history of ARA",
                             "any_ara_pct_wt" = "Within-cluster: % of live births with maternal history of ARA",
                             "any_ara_pct" = "% of live births with maternal history of ARA",
                             "lone_parent_pct" = "% of dependent child households with lone parent",
                             "teenbirth_2010" = "% of live births where mother < 20 years old",
                             "imd_10pc_2010" = "% of live births where maternal LSOA history within 10% most deprived LSOAs in England",
                             "prop10_imd_pct" = "Proportion of LSOAs in LA in 10% most deprived in England",
                             "imd_quint" = "LA IMD quintile - 2010",
                             "total_2010" = "LA population size (000s)",
                             "violent_crime" = "Rate of violent crime (per 100 LA residents)",
                             "lowbw_2010" = "% of live births with LBW",
                             "ca_feud_2010" = "% of live births where child has a CCC",
                             "year_std" = "Time (financial year)",
                             "year_std:any_ara_pct" = "Interaction: Time & % of live births with maternal history of ARA")) %>% 
  rownames_to_column(var = "term")

# create dataframe to plot: --------------------------------------------------------------------------------#
lmer_plot <- bind_rows(lmer_null_tidy,
                       lmer_aras_tidy,
                       lmer_full_tidy,
                       lmer_full_nodisag_tidy,
                       lmer_full_int_tidy) %>% 
  filter(group == "fixed" & !grepl("intercept", tolower(term))) %>% 
  rowwise() %>% 
  mutate(model_num = case_when(grepl("null", model) ~ "Model 1",
                               grepl("aras", model) ~ "Model 2",
                               model == "full" ~ "Model 3",
                               grepl("disag", model) ~ "Model 4",
                               grepl("full_int", model) ~ "Model 5"),
         model_ltr = case_when(grepl("null", model) ~ "a",
                               grepl("aras", model) ~ "b",
                               model == "full" ~ "c",
                               grepl("disag", model) ~ "d",
                               grepl("full_int", model) ~ "e"),
         # facet_lbl = paste(model_num,
         #                   paste("AIC = ",round(AIC)),
         #                   paste0("R2m = ", sprintf("%.2f", round(R2m_median,2)), 
         #                         " (95% CI: ", sprintf("%.2f", round(R2m_ll,2))," to ", sprintf("%.2f", round(R2m_ul,2)),")"),
         #                   paste0("R2c = ", sprintf("%.2f", round(R2c_median,2)), 
         #                         " (95% CI: ", sprintf("%.2f", round(R2c_ll,2))," to ", sprintf("%.2f", round(R2c_ul,2)),")"),
         #                   sep = "\n"),
         facet_lbl_noaic = ifelse(model_num %in% c("Model 3", "Model 4", "Model 5"), 
                                  paste0(model_num, "*"), model_num), #as.character(as.expression(bquote(.(model_num)^.(model_ltr)))),
         facet_lbl = ifelse(model_num %in% c("Model 3", "Model 4", "Model 5"), 
                            paste0(model_num, "*", "\nAIC = ", round(AIC)), 
                            paste0(model_num, "\nAIC = ", round(AIC))),
         aic_lbl = paste0("NULL[AIC:~", AIC,"]")
         ) %>% 
  arrange(model_num, term) %>% 
  droplevels()

# create rows for each coefficient in each model
dummy <- tidyr::expand(lmer_plot, nesting(term), facet_lbl)

# merge on dummy rows and create order for model terms:
lmer_plot <- left_join(dummy, lmer_plot, by = c("facet_lbl", "term")) %>% 
  left_join(lbls, by = "term") %>% 
  mutate(order = case_when(term == "year_std" ~ 12,
                           grepl("ARA", label ) & grepl("Within", label) ~ 11,
                           grepl("ARA", label ) & grepl("Between", label) ~ 10,
                           term == "any_ara_pct"  ~ 9,
                           term == "year_std:any_ara_pct" ~ 8,
                           grepl("< 20", label) ~ 7,
                           grepl("LSOA", label) ~ 6,
                           grepl("CCC", label) ~ 5,
                           grepl("LBW", label) ~ 4,
                           grepl("size", label) ~ 3,
                           grepl("lone", label) ~ 2,
                           TRUE ~ 1),
         tvc = ifelse(order > 7, 0, 1), # group terms by whether they are time-varying or not
         int = ifelse(model_num == "Model 5" & term != "year_std:any_ara_pct", 1, 0),
         ltr = case_when(model_num == "Model 5" & term == "year_std" ~ "a",
                         model_num == "Model 5" & term == "any_ara_pct" ~ "b",
                         TRUE ~ NA_character_),
         label = reorder(as.factor(as.character(label)), order),
         text = ifelse(is.na(estimate) | model_num != "Model 4", # create text label of estimate and CI for model 4
                       "", paste0("(", sprintf("%.2f", round(estimate,2)),", ", 
                                  sprintf("%.2f", round(conf.low,2)), " to ", 
                                          sprintf("%.2f", round(conf.high, 2)), ")")),
         text2 = ifelse(grepl("ara|year",term) & !is.na(estimate), # create text label of estimate and CI for model 4
                       paste0("'", sprintf("%.2f", round(estimate,2))," (", 
                                  sprintf("%.2f", round(conf.low,2)), " to ", 
                                  sprintf("%.2f", round(conf.high, 2)), ")'"), ""),
         text3 = ifelse(!is.na(ltr), paste(text2, "^", ltr), text2))

# save model plot data:
write.csv(lmer_plot, "N:/Documents/Data & Programming/outputs/la variation/modelling/final_results.csv")
#-------------------------------------------------------------------------------------------------------------#

    
# caption for plot ----
# lmer_caption <- lmer_plot %>%
#   mutate(cap  = paste(paste0(model_ltr, ": ", "AIC = ",round(AIC)),
#                       paste0("R2m = ", sprintf("%.2f", round(R2m_median,2)),
#                              " (95% CI: ", sprintf("%.2f", round(R2m_ll,2))," to ", sprintf("%.2f", round(R2m_ul,2)),")"),
#                       paste0("R2c = ", sprintf("%.2f", round(R2c_median,2)),
#                              " (95% CI: ", sprintf("%.2f", round(R2c_ll,2))," to ", sprintf("%.2f", round(R2c_ul,2)),")"),
#                       sep = "; ")) %>%
#   filter(!is.na(model_ltr)) %>%
#   distinct(cap) %>%
#   arrange(cap) %>% 
#   unlist()
  
# create plot object: -------------------------------------------------------------------
ggplot(data = lmer_plot, 
       aes(y = reorder(label, order), 
           x = estimate, 
           xmin = conf.low, 
           xmax = conf.high,
           color = as.factor(tvc))) +
  facet_wrap(~facet_lbl, 
             scales = "free_x", 
             ncol = 4,
             labeller = "label_parsed") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             size = 0.6, 
             colour = "grey60", 
             alpha = 0.5) +
  geom_errorbarh(height = 0.2, 
                 size = 1) +
  geom_point(shape = 19,
             fill = "white", 
             size = 2.5) +
  geom_text(aes(label = text),
            position = position_dodge(width = 1), 
            vjust = -1,
            show.legend = FALSE, 
            color = "black", 
            size = 5.5) +
  scale_x_continuous(limits = c(-9, 23),
                     breaks = c(-5,0,5,10,15,20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  ylab("") +
  xlab("\nDifference in the outcome per unit change in predictor") +
  labs(caption = paste0("\n\nAIC = Akaike information criterion; R2m = marginal pseudo R-squared; R2c = conditional pseudo R-squared.\n\n", 
                       paste(lmer_caption, collapse = "\n\n"))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.caption = element_text(hjust = 0, size = 16),
        panel.border = element_rect(color="black", fill=NA)) +
  scale_colour_manual("", 
                      labels = c("time-varying\n(2006/07 to 2013/14)\n ",
                                 "non-time-varying\n(2010/11 data only)\n "), 
                      values = c("#1F78B4", "#D55E00")) 


ggsave("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_coef_plots.png",
       units = "mm", height = 350, width = 550, dpi = "print")


#--------------------------------------#
# model diagnostic plot ----
#--------------------------------------#
model_check <- 
  ggplot(data = filter(distinct(lmer_plot, model_num, .keep_all = TRUE), !is.na(model_num)),
       aes(group = 1)) +
  # facet_wrap(~facet_lbl_noaic, 
  #            scales = "free_x", 
  #            ncol = 5) +
  # geom_point(aes(x = model_num,
  #                y = AIC/10000,
  #                shape = "AIC",
  #                colour = "AIC"),
  #            size = 3) +
  geom_line(aes(x = model_num,
                 y = R2m_median,
                 colour = "% variation in the outcome explained by the fixed-effects",
                group = 1)) +
  geom_line(aes(x = model_num,
                 y = R2c_median,
                 colour = "% variation in the outcome explained by the whole model",
                group = 1)) +
  # geom_line(aes(x = model_num,
  #                y = AIC/10000,
  #               colour = "AIC")) +
  geom_errorbar(aes(x = model_num,
                    ymin = R2m_ll,
                    ymax = R2m_ul),
                    #colour = "% variation in the outcome explained by the fixed-effects"),
                width = 0.02) +
  geom_errorbar(aes(x = model_num,
                    ymin = R2c_ll,
                    ymax = R2c_ul),
                    #colour = "% variation in the outcome explained by the whole model"),
                width = 0.02) +
  geom_point(aes(x = model_num,
                 y = R2m_median,
                 fill = "% variation in the outcome explained by the fixed-effects",
                 shape = "% variation in the outcome explained by the fixed-effects"),
             size = 3) +
  geom_point(aes(x = model_num,
                 y = R2c_median,
                 fill = "% variation in the outcome explained by the whole model",
                 shape = "% variation in the outcome explained by the whole model"),
             size = 3) +
  theme_classic() +
  geom_text(aes(x = model_num, y = R2m_ul, label = paste0(round(R2m_median*100),"% (", 
                                                          round(R2m_ll*100),
                                                          "-",
                                                          round(R2m_ul*100),"%)")),
                #colour = "% variation in the outcome explained by the fixed-effects"), 
            vjust = -1,
            show.legend = FALSE,
            size = 4) +
  geom_text(aes(x = model_num, y = R2c_ul, label = paste0(round(R2c_median*100),"% (", 
                                                          round(R2c_ll*100),
                                                          "-",
                                                          round(R2c_ul*100),"%)")),
                #colour = "% variation in the outcome explained by the whole model")
            vjust = -1,
            show.legend = FALSE,
            size = 4) +
  # geom_text(aes(x = model_num, y = AIC/10000, label = round(AIC),
  #               colour = "AIC"), vjust = -0.9,
  #           show.legend = FALSE,
  #           size = 6) +
  scale_shape_manual("", values = c(21, 22)) +
  #scale_color_manual("", values = c("#D95F02", "#1B9E77", "#7570B3")) +
  scale_color_manual("", values = c("#bbcc33", "dodgerblue4")) +
  scale_fill_manual("", values = c("#bbcc33", "dodgerblue4")) +
  scale_y_continuous("",labels = scales::percent, 
                     limits =c(0,1.10), breaks = seq(0,1,0.2)) +
                     #sec.axis = sec_axis(~.*10000, name = "AIC\n")) +
  scale_x_discrete("") +
  #labs(caption = "Note: 95% confidence intervals given in brackets.", hjust = 0) +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        #axis.text = element_text(size = 20),
        #plot.title = element_text(size = 26),
        #strip.text = element_text(size = 20),
        #axis.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(1.0, 'cm'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        legend.position = "bottom")
  

model_check
ggsave("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_prop_explained.png",
       units = "mm", height = 210, width = 250, dpi = "print")




#--------------------------------------#
# only tvc plot ----
#--------------------------------------#

tvc_plot <- ggplot(data = filter(lmer_plot, tvc == 0 & !is.na(model_num) ), 
                 aes(y = reorder(label, order), 
                     x = estimate, 
                     xmin = conf.low, 
                     xmax = conf.high#,
                     #colour = as.factor(int)
                 )) +
  facet_wrap(~ facet_lbl, 
             ncol = 5,
             scales = "free_x") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             size = 0.6, 
             colour = "grey60", 
             alpha = 0.3) +
  geom_errorbarh(height = 0.1) + 
                 #size = 1,
                 #colour = "#44bb99") +
  geom_point(size = 3,
             shape = 21,
             fill = "#44bb99") +
  geom_text(aes(label = text3),
            #position = position_dodge(width = 1), 
            vjust = -1.2,
            show.legend = FALSE, 
            color = "black", 
            size = 4,
            parse = TRUE) +
  scale_x_continuous(limits = c(-5, 18),
                     breaks = c(-5,0,5,10,15)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  ylab("") +
  xlab("\nDifference in the outcome per unit change in predictor\n") +
  #labs(caption = paste0("\n\nAIC = Akaike information criterion; R2m = marginal pseudo R-squared; R2c = conditional pseudo R-squared.\n\n", 
  #                      paste(lmer_caption, collapse = "\n\n"))) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
         panel.border = element_rect(color="black", fill=NA)) #+
# scale_colour_manual("", 
#                     labels = c("interaction model\n ",
#                                "non-time-varying\n(2010/11 data only)\n "), 
#                     values = c("#1F78B4", "#D55E00")) 

tvc_plot
ggsave( "N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_coef_plots_tvconly.png",
       units = "mm", height = 200, width = 550, dpi = "print")

# plot tvc only and model diagnostics together:
svg(filename = "N:/Documents/Data & Programming/outputs/la variation/modelling/figure2.svg", 
    width = 18, height = 11.5)
egg::ggarrange(tvc_plot + labs(title = "A"), model_check + labs(title = "B"), ncol=1)
dev.off()


#--------------------------------------#
# Final model only plot ----
#--------------------------------------#
ggplot(data = filter(lmer_plot, grepl("disag", model)),
       aes(y = reorder(label, order), 
           x = estimate, 
           xmin = conf.low, 
           xmax = conf.high,
           color = as.factor(tvc))) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             size = 0.6, 
             colour = "grey60", 
             alpha = 0.5) +
  geom_errorbarh(height = 0.2, 
                 size = 1) +
  geom_point(shape = 19,
             fill = "white", 
             size = 2.5) +
  geom_text(aes(label = ifelse(term == "any_ara_pct", text, "")),
            #position = position_dodge(width = 1), 
            vjust = -1,
            show.legend = FALSE, 
            color = "black", 
            size = 5.5) +
  scale_x_continuous(limits = c(-9, 23),
                     breaks = c(-5,0,5,10,15,20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  ylab("") +
  xlab("\nDifference in the outcome per unit change in predictor") +
  #labs(caption = paste0("\n\nAIC = Akaike information criterion; R2m = marginal pseudo R-squared; R2c = conditional pseudo R-squared.\n\n", 
  #                      paste(lmer_caption, collapse = "\n\n"))) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.caption = element_text(hjust = 0, size = 16),
        panel.border = element_rect(color="black", fill=NA)) +
  scale_colour_manual("", 
                      labels = c("time-varying\n(2006/07 to 2013/14)\n ",
                                 "non-time-varying\n(2010/11 data only)\n "), 
                      values = c("#1F78B4", "#D55E00")) 


ggsave("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_coef_plot_final.png",
       units = "mm", height = 300, width = 350, dpi = "print")


#---------------------------#
# plot for presentations ----
#---------------------------#
library(grid)
library(gridExtra)
library(gtable)
library(ggpubr)

ggarrange(ggplotGrob(tvc_plot + labs(title = "A")), 
          ggplotGrob(model_check + labs(title = "B")), 
          ncol =1, nrow = 2,
          align = "v",
          heights = c(1,0.9))
ggsave("N:/Documents/Data & Programming/outputs/la variation/modelling/lmer_plot.png",
       units = "mm", height = 410, width = 600, dpi = "print")
