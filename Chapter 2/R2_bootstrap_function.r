set.seed(1234)

R2_bootstrap <- function(model, n.sim = 10000) {
  # bootstrap n.sim times to calc pseudo r-squared values:
  lmer_bs <- bootMer(model, FUN=function(x)r.squaredGLMM(x),
                nsim=n.sim) 
  
  # calculate median and 0.025,0.975 percentiles for R2c and R2m:
  lmer_bs_R2 <- as.data.frame(lmer_bs$t) %>% 
    rownames_to_column(var = "type_R2") %>% 
    rename(R2=V1) %>% 
    mutate(type_R2 = substr(type_R2,1,3)) %>% 
    group_by(type_R2) %>% 
    mutate(median = median(R2),
           ll = quantile(R2,0.025),
           ul = quantile(R2, 0.975))
  
  # return bootstrap data:
  return(lmer_bs_R2)
}
