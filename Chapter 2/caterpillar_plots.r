#-----------------------------------#
# LA variation caterpillar plots ----
#-----------------------------------#

library(dplyr)
library(ggplot2)
library(ggsci)

lavar_cat_plot <- function(data, var, var_lbl,pal) {
  eval(data)

  dat <- data %>% 
    rename(var_unquo = var) %>% 
    group_by(la_code) %>% 
    mutate(mean_pct = mean(var_unquo),
           y_min = mean(var_unquo) - sd(var_unquo),
           y_max = mean(var_unquo) + sd(var_unquo)) %>% 
    ungroup() %>% 
    distinct(la_code, .keep_all = TRUE) %>% 
    arrange(mean_pct, la_code) %>% 
    mutate(id = row_number(),
           mean_tot = mean(mean_pct))

  p <- ggplot(data = dat, aes(x = id, y = mean_pct, colour = region_name)) +
    geom_point() +
    geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.75) +
    geom_hline(aes(yintercept = mean_tot, linetype = "English average")) +
    theme_classic() +
    scale_x_continuous("Local Authority", breaks = seq(1,max(dat$id), by = 1)) +
    scale_color_manual("Region", values = pal) +
    ylab(var_lbl) +
    scale_linetype_manual("", values = "dashed") +
    theme(axis.text.x = element_blank(),
          axis.title = element_text(size = 14),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) 
    
     
  
  return(p)
}


#---------------------------------------#
# ARAs over time - caterpillar plots ----
#---------------------------------------#

xlims <- c(1,149)

# Get colour palette:
cbPal <- c("#1F78B4",  "#D95F02", "#1B9E77", "#7570B3",  "#66A61E", "#E7298A", "#E6AB02", "#FB9A99", "#666666")

# Create caterpillar plot:
cat_plot <- 
lavar_cat_plot(data    = analysis_dat, var = "any_ara_pct", 
               var_lbl = "% of live births where \nmother had a history of ARA", pal = cbPal) +
  labs(title    = "Average % of live-births with maternal history of adversity, by local authority (2006/7 - 2013/14)",
       subtitle = "Mean % with +/- standard deviation error bars for each local authority, compared to England mean (black, dashed line)") +
  ylab("Percentage\nof\nlive births") +
  xlab("Local authority") +
  theme(axis.line.x  = element_blank(),
        axis.title.y = element_text(size = 11, angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 11),
        axis.text    = element_text(size = 11),
        legend.text  = element_text(size = 11),
        legend.title = element_text(size = 11))
  

# add barchart underneath:
bar_dat <- 
analysis_dat %>% 
  group_by(la_code) %>% 
  mutate(mean_pct = mean(any_ara_pct),
         y_min    = mean(any_ara_pct) - sd(any_ara_pct),
         y_max    = mean(any_ara_pct) + sd(any_ara_pct)) %>% 
  ungroup() %>% 
  distinct(la_code, .keep_all = TRUE) %>% 
  arrange(mean_pct, la_code) %>% 
  mutate(id = row_number()) 

bar_plot <- 
ggplot(data = bar_dat,
       aes(x = id, y = n_mother/100000, fill = region_name)) +
  geom_col() +
  scale_fill_manual(values = cbPal) +
  guides(fill = "none") +
  theme_classic() +
  xlab("") +
  ylab("Number\nof\nlive births") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_text(size = 11, angle = 0, vjust = 0.5),
        axis.ticks = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.text = element_text(size = 12)) 

# plot cat_plot and bar_plot together:
ggarrange(ggplotGrob(cat_plot), 
          gtable_add_cols(ggplotGrob(bar_plot), unit(rep(1,2), "null")), 
          ncol =1, nrow = 2, align = "v",
          heights = c(3,0.5))

ggsave("N:/Documents/Data & Programming/outputs/la variation/figures/ara_catplot.png", 
       units  = "mm",
       height = 200, 
       width  = 290)
