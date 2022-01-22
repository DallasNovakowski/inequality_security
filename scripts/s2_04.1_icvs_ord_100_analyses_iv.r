library(tidyverse)
library(lme4)
library(ordinal)

library(stevemisc) # my toy R package with various helper functions
library(modelr) # for data_grid
library(kableExtra) # for pretty tables


load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_joined.RData")

load(file = here::here("output","icvs_output.RData"))

set.seed(1234)

#used for dotwhisker plots
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")



m100_ord_iv <- iv_joined %>% map(~ ordinal::clmm(victim_mord_min, data = .x))

m100_iv_coef <- m100_ord_iv %>%
  map(. %>% coef())

# extract
m100_iv_vcov <- m100_ord_iv %>%
  map(. %>% vcov()) 

m100_iv_vcov <- m100_iv_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, m100_iv_coef, m100_iv_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# m100_iv_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

m100_iv_sims <- list()

m100_iv_sims <- lapply(seq_along(combined), function(i) {
  m100_iv_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(m100_iv_sims[[i]])
}) 

# save(m100_iv_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_iv_sims.RData")
# cgwtools::resave(m100_iv_sims, file = here::here("output","m100_sims.RData"))
save(m100_iv_sims, file = here::here("output","m100_iv_sims.RData"))
