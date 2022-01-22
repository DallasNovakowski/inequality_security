library(tidyverse)
library(lme4)
library(ordinal)

library(stevemisc) # my toy R package with various helper functions
library(modelr) # for data_grid
library(kableExtra) # for pretty tables


load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_joined.RData")

set.seed(1234)

#used for dotwhisker plots
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

sec_m100_ord_iv <- iv_joined %>% map(~ ordinal::clmm(sec_mord_min, data = .x))


sec_m100_iv_coef <- sec_m100_ord_iv %>%
  map(. %>% coef())

# extract
sec_m100_iv_vcov <- sec_m100_ord_iv %>%
  map(. %>% vcov()) 

sec_m100_iv_vcov <- sec_m100_iv_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, sec_m100_iv_coef, sec_m100_iv_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# sec_m100_iv_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

sec_m100_iv_sims <- list()

sec_m100_iv_sims <- lapply(seq_along(combined), function(i) {
  sec_m100_iv_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(sec_m100_iv_sims[[i]])
}) 

# save(sec_m100_iv_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/sec_m100_iv_sims.RData")
# cgwtools::resave(sec_m100_iv_sims, file = here::here("output","sec_m100_sims.RData"))
save(sec_m100_iv_sims, file = here::here("output","sec_m100_iv_sims.RData"))