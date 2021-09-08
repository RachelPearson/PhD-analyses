#---------------------------------------------------#
# LA variation - 2010/11 model_adjusted funnel plot #
#---------------------------------------------------#

# packages ----
library(dplyr)
library(tidyr)
library(readr)
library(lme4)
library(broom)
library(gridExtra)
library(grid)
library(gtable)
library(ggpubr)
library(ggplot2)
library(ggrepel)

#----------------------#
# begin function ----
#----------------------#
abs_rate_funnel <- function(data, var, pop, pern = 1000, wlbls = FALSE){
  
  eval(data)
  
  # derive standard error ----
  dt <- data %>% 
    rename(var_unquo = var,
           pop_unquo = pop) %>% 
    mutate(rate = var_unquo/pop_unquo ,
           rate_pern = rate*pern, 
           rate_se = sqrt(rate*(1-rate)/pop_unquo),
           pop_round = round(pop_unquo, -1))

  # derive common (fixed) effect ----
  rate_fem <- weighted.mean(dt$rate, 1/dt$rate_se^2, na.rm = TRUE)

  if(is.nan(rate_fem)){
    rate_fem <- mean(dt$rate, na.rm = TRUE)
    warning("weighted means could not be calculated. Arithmetic mean used instead.")
  }
  
  
  ## lower and upper limits for 95% and 99.9% CI, based on FEM estimator ----
  p_fem <- rate_fem*(1-rate_fem)
	
	number.seq <- seq(10, max(dt$pop_unquo), 10)
	
	number.ll95 <- (rate_fem - 1.96 * (sqrt((p_fem * (1 - p_fem)) / number.seq))) * pern
	number.ul95 <- (rate_fem + 1.96 * (sqrt((p_fem * (1 - p_fem)) / number.seq))) * pern
	number.ll999 <- (rate_fem - 3.29 * (sqrt((p_fem * (1 - p_fem)) / number.seq))) * pern
	number.ul999 <- (rate_fem + 3.29 * (sqrt((p_fem * (1 - p_fem)) / number.seq))) * pern
	number.se <- ((sqrt((p_fem * (1 - p_fem)) / number.seq)) * pern)
	dfCI <- data.frame(number.ll95, number.ul95, number.ll999, number.ul999, number.seq, number.se, rate_fem)
  
  
  # create labels for 'outliers' ----
  labels <- left_join(dt, dfCI, by = c("pop_round" = "number.seq")) %>% 
    mutate(label_name = ifelse(rate_fem*pern > number.ul999 + 2*number.se | rate_fem*pern < number.ll999 - 2*number.se, la_name, NA),
           label_rate = ifelse(!is.na(label_name), round(rate_fem*pern,1), NA),
           label_comb = ifelse(!is.na(label_name) & !is.na(label_rate), paste(label_name, label_rate, sep = ": "), NA)) %>% 
    select(la_code, starts_with("label_"))  
  
  count <- left_join(dt, dfCI, by = c("pop_round" = "number.seq")) %>% 
    mutate(high_low = ifelse(rate*pern > number.ul999 | rate*pern < number.ll999, 1, 0)) %>% 
    summarise(extreme = sum(high_low ==1, na.rm = TRUE))
	
	final_dt <- left_join(dt, labels, by = c("la_code"))

	
if(wlbls == TRUE) {
	# draw funnel plot ----
	p <- ggplot(data = final_dt, aes(x = pop_unquo, y = rate_pern)) +
	  geom_point(alpha = 0.85, size = 2, shape = 21) +
	  geom_line(aes(x = number.seq, y = number.ll95, linetype = "dashed"), data = dfCI) +
	  geom_line(aes(x = number.seq, y = number.ul95), data = dfCI, linetype = "dashed") +
	  geom_line(aes(x = number.seq, y = number.ll999, linetype = "longdash"),  data = dfCI) +
	  geom_line(aes(x = number.seq, y = number.ul999), linetype = "longdash", data = dfCI) +
	  geom_hline(aes(yintercept = rate_fem*pern), data = dfCI) +
	  geom_text_repel(aes(label = label_name),size = 3.2, nudge_x = 11000, nudge_y = -0.05) +
	  scale_linetype_manual("",values = c("dashed", "longdash"), labels = c("95% control limit", "99.9% control limit")) +
	  theme(axis.line = element_line(colour = "black"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_blank(),
	        panel.background = element_blank(),
	        axis.text = element_text(colour = "black")) #+
	return(list(p, count, labels))
} else{
  # draw funnel plot ----
  p <- ggplot(data = final_dt, aes(x = pop_unquo, y = rate_pern)) +
    geom_point(alpha = 0.85, size = 2, shape = 21) +
    geom_line(aes(x = number.seq, y = number.ll95, linetype = "dashed"), data = dfCI) +
    geom_line(aes(x = number.seq, y = number.ul95), data = dfCI, linetype = "dashed") +
    geom_line(aes(x = number.seq, y = number.ll999, linetype = "longdash"),  data = dfCI) +
	  geom_line(aes(x = number.seq, y = number.ul999), linetype = "longdash", data = dfCI) +
	  geom_hline(aes(yintercept = rate_fem*pern), data = dfCI) +
	  scale_linetype_manual("",values = c("dashed", "longdash"), labels = c("95% control limit", "99.9% control limit")) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black")) #+
  return(list(p, count))
}
  
  
}

#----------------------#
# get analysis data ----
#----------------------#
source("N:/Documents/Data & Programming/R Scripts/la variation/la_variation_analysis_data.r", echo = FALSE)


#----------------------#
# get final model ---- 
#----------------------#

# linear mixed-effect model:
lmer_final <- lmer(data = filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0),
                         formula = as.formula(paste("n_child_10000 ~ year_std",
                                                    "any_ara_pct",
                                                    "teenbirth_2010",
                                                    "imd_10pc_2010",
                                                    "lowbw_2010",
                                                    "ca_feud_2010",
                                                    "total_2010",
                                                    "lone_parent_pct",
                                                    "violent_crime", 
                                                    #"prop10_idaci_pct",
                                                    "(year_std|la_code)",
                                                    sep = "+")), REML = TRUE)
lmer_final_predict <- predict(lmer_final, re.form = NA)


#-----------------------#
# funnel plots lmer ----
#-----------------------#
final_dat <- bind_cols(filter(model_dat, any_bad_linkrate == 0 & any_bad_bwrate ==0) , 
                       "predict" = lmer_final_predict) %>%
  filter(year == 2010) %>% 
  mutate(adjusted_rate = (n_child_10000/predict)*mean(n_child_10000), # crude rate or mean of the rates? SMR 
         adj_count = round((adjusted_rate*pop_U1)/10000),
         mean_rate = mean(n_child_10000)) %>% 
  rowwise() %>% 
  mutate(y_min = min(n_child_10000, adjusted_rate), 
         y_max = max(n_child_10000, adjusted_rate),
         extreme = ifelse(abs(n_child_10000 - mean_rate) - abs(adjusted_rate - mean_rate) < 0, 1, 0)) %>% 
  dplyr::select(la_code, la_name, region_name, n_child_10000, predict,
                adjusted_rate, mean_rate, n_child, adj_count, y_min, y_max, pop_U1, extreme)

  
# Funnel plot based only on observed rates:
plot_act <- abs_rate_funnel(data = final_dat, var = "n_child", 
                            pop = "pop_U1", pern = 10000)[[1]] +
  scale_y_continuous("Number of infants (per 10000)",
                     limits = c(-5,250)) +
  scale_x_continuous("Local population size (Infants)") +
  labs(title = "Actual") 

# Funnel plot with both observed and expected (as per lmer final) rates :
final <- plot_act  +
  geom_linerange(data = final_dat, aes(x = pop_U1, ymin = y_min, ymax = y_max), size = 1, alpha = 0.2, inherit.aes = FALSE, colour = "grey") +
  geom_point(data= final_dat, aes(x = pop_U1, y = n_child_10000, colour = "actual rate", fill = "actual rate", shape = "actual rate"), size = 2) +
  geom_point(data= final_dat, aes(x = pop_U1, y = adjusted_rate, colour = "adjusted rate", shape = "adjusted rate", fill = "adjusted rate"), size = 2) +
  scale_colour_manual("", values = c("grey70", "#1B9E77")) +
  scale_shape_manual("", values = c(19,15)) +
  scale_fill_manual("", values = c("grey70", "#1B9E77")) +
  scale_y_continuous("Number of infants CLA (per 10,000)",
                     limits = c(-5,250)) +
  scale_x_continuous("Local population size (Infants)") +
  theme(axis.text = element_text(size = 14),
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        title = element_text(size = 16)) +
  labs(subtitle = "model prediction of rates uses fixed-effects only - final model") 
  
ggsave(final, "N:/Documents/Data & Programming/outputs/la variation/modelling/adjusted_lmer_funnel_plot_feonly.png")

#--------------------------------------------------------------------------------#
# add lines between observed and expected and use shapes to depict region of LA ----
#--------------------------------------------------------------------------------#
  final_shapes <- plot_act  +
    geom_linerange(data = final_dat_wlbls, aes(x = pop_U1, ymin = y_min, ymax = y_max), size = 1, alpha = 0.2, inherit.aes = FALSE, colour = "grey") +
    geom_point(data= final_dat_wlbls, aes(x = pop_U1, y = n_child_10000, colour = "actual rate", fill = "actual rate", shape = "actual rate"), size = 2) +
    geom_point(data= final_dat_wlbls, aes(colour = region_name, shape = region_name, fill = region_name,
                                    x = pop_U1, y = adjusted_rate), size = 3.2) +
    #geom_label_repel(data= final_dat_wlbls, aes(x = pop_U1, y = n_child_10000,label = lbl),size = 3.2, nudge_x = 3000, nudge_y = -0.05) +
    scale_colour_manual("Region", values = c("grey80", "#1F78B4",  "#D95F02", "#1B9E77", "#7570B3",  "#66A61E", "#E7298A", "#E6AB02", "#FB9A99", "#666666")) +
    scale_shape_manual("Region", values=c(19,2,0,8,15,21,17,4,23,7)) +
    scale_fill_manual("Region", values = c("grey80", "#1F78B4",  "#D95F02", "#1B9E77", "#7570B3",  "#66A61E", "#E7298A", "#E6AB02", "#FB9A99", "#666666")) +
    scale_y_continuous("Number of infants CLA (per 10,000)",
                       limits = c(-5,250)) +
    scale_x_continuous("Local population size (Infants)") +
    theme(#legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          title = element_text(size = 16)) +
    labs(title = "Local authority variation in Infants CLA - actual vs case-mix adjusted (2010/11 only)",
         subtitle = "linear mixed-effect model prediction of rates uses fixed-effects only"
      #title ="",
      #subtitle = "model prediction of rates uses fixed-effects only - final model"
      ) 

ggsave(final_shapes,"N:/Documents/Data & Programming/outputs/la variation/modelling/adjusted_lmer_funnel_plot_feonly_shapes.png",
       units = "mm", width = 350,height = 250)
