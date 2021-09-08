#------------------------#
# lmer model diagnostics #
#------------------------#

library(dplyr)
library(lme4)
library(broom)

# 1 - qqplot of level 1 residuals:
lmer1_qnorm <- function(model) {
  p <- qqnorm(residuals(model))
  p <- p + qqline(residuals(model))
  return(p)
}

# 1 - histogram of level 1 residuals:
lmer1_hist <- function(model) {
  p <- hist(residuals(model, scaled = TRUE))
  return(p)
}


# 2 - linearity of relationship between outcome and response:
lmer_linrel <- function(model, var) {
  p <- plot(resid(model, scaled = TRUE), filter(model_dat, any_bad_linkrate == 0))[[var]]
  return(p)
} 


# 4 - qqplot of level 2 residuals:
lmer2_int_qnorm <- function(model) {
  
  re_sd <- unlist(filter(tidy(model), group == "la_code")$estimate)
  resid_sd <- unlist(filter(tidy(model), group == "Residual")$estimate)
  
  dat <- model@frame %>% 
    group_by(la_code) %>% 
    summarise(nobs = n()) %>% 
    ungroup() 
  
  std_lvl2 <- data.frame(nobs = dat$nobs, uhat_eb2 = model@u)%>% 
    mutate(R = (re_sd^2)/(re_sd^2 + (resid_sd^2))/nobs,
           var_eb = R*(re_sd^2),
           uhat_st2 = uhat_eb2/sqrt(var_eb))
  
  p <- qqnorm(std_lvl2$uhat_st2)
  qqline(std_lvl2$uhat_st2)
  
  return(p)
}  

# 5 - histogram of level 2 residuals:
lmer2_hist <- function(model) {
  
  re_sd <- unlist(filter(tidy(model), group == "la_code")$estimate)
  resid_sd <- unlist(filter(tidy(model), group == "Residual")$estimate)
  
  dat <- model@frame %>% 
    group_by(la_code) %>% 
    summarise(nobs = n()) %>% 
    ungroup() 
  
  std_lvl2 <- data.frame(nobs = dat$nobs, uhat_eb2 = model@u)%>% 
    mutate(R = (re_sd^2)/(re_sd^2 + (resid_sd^2))/nobs,
           var_eb = R*(re_sd^2),
           uhat_st2 = uhat_eb2/sqrt(var_eb))
  
  p <- hist(std_lvl2$uhat_st2, breaks = 15)
  return(p)
} 

icc_lmer <- function(model) {
  qtidy <- quietly(tidy)
  re_sd <- unlist(filter(qtidy(model)$result, group == "la_code")$estimate)
  resid_sd <- unlist(filter(qtidy(model)$result, group == "Residual")$estimate)
  
  icc <- (re_sd^2)/(re_sd^2 + resid_sd^2)
  return(icc)
}

# adj R-squared for LMM:
r2_lmer <- function(model) {
  qtidy <- quietly(tidy)
  re_sd <- unlist(filter(qtidy(model)$result, group == "la_code")$estimate)
  resid_sd <- unlist(filter(qtidy(model)$result, group == "Residual")$estimate)
}

