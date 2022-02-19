# This script is meant for the final (proposed) multilevel analysis, using security consumption as the DV

library(tidyverse)
library(lme4)
library(ordinal)

library(stevemisc) # my toy R package with various helper functions
library(modelr) # for data_grid
library(kableExtra) # for pretty tables


load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mod_joined.RData")

set.seed(1234)

#used for dotwhisker plots
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

load(here::here("data","m100_sims.RData"))
load(here::here("output","icvs_output.RData"))





sec_m100_ord <- mod_joined %>% map(~ ordinal::clmm(sec_mord_max, data = .x))

# http://svmiller.com/blog/2020/04/summarizing-ordinal-models-with-simulation-multivariate-normal/
# extract coefficients for each model
sec_m100_coef <- sec_m100_ord %>%
  map(. %>% coef())

# extract
sec_m100_vcov <- sec_m100_ord %>%
  map(. %>% vcov()) 

sec_m100_vcov <- sec_m100_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, sec_m100_coef, sec_m100_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# sec_m100_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

sec_m100_sims <- list()

sec_m100_sims <- lapply(seq_along(combined), function(i) {
  sec_m100_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(sec_m100_sims[[i]])
}) 

# save(sec_m100_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/sec_m100_sims.RData")
cgwtools::resave(sec_m100_sims, file = here::here("output","sec_m100_sims.RData"))

# load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/sec_m100_sims.RData")
load(file = here::here("output","icvs_output.RData"))



# > Chapter 7 of Gelman and Hill (2007) for how the multivariate normal distribution is a novel way of simulating uncertainty regarding the model output from a generalized linear model. I’ll only note here that simulating values from a multivariate normal distribution of the ordinal model requires only the vector of regression coefficients and the variance-covariance matrix of the fitted model. 



# weighted ordinal regression
sec_mw100_ord <- mod_joined %>% map(~ ordinal::clmm(sec_mord_max, data = .x, weights = individual_weight))


save(sec_mw100_ord, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mw100_ord.RData")

summary(sec_mw100_ord[[1]])$coefficients[,"Std. Error"]

is.nan(summary(mw100_ord[[9]])$coefficients[,"Std. Error"]["gini_2004_6_cent"])

sec_mw100_invalid <- list()

sec_mw100_invalid <- lapply(seq_along(sec_mw100_ord), function(i) {
  sec_mw100_invalid[[i]] <- is.nan(summary(sec_mw100_ord[[i]])$coefficients[,"Std. Error"]["gini_2004_6_cent"])
  return(sec_mw100_invalid[[i]])
}) 

sec_model_nonconvergence <- Reduce("+",sec_mw100_invalid)

cgwtools::resave(sec_model_nonconvergence, file = here::here("output", "icvs_output.RData"))


sec_mord_min
sec_m100_ord_min <- mod_joined %>% map(~ ordinal::clmm(sec_mord_min, data = .x))


sec_m100_min_coef <- sec_m100_ord_iv %>%
  map(. %>% coef())

# extract
sec_m100_min_vcov <- sec_m100_ord_iv %>%
  map(. %>% vcov()) 

sec_m100_min_vcov <- sec_m100_min_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, sec_m100_min_coef, sec_m100_min_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# sec_m100_min_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

sec_m100_min_sims <- list()

sec_m100_min_sims <- lapply(seq_along(combined), function(i) {
  sec_m100_min_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(sec_m100_min_sims[[i]])
}) 

# save(sec_m100_min_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/sec_m100_min_sims.RData")
# cgwtools::resave(sec_m100_min_sims, file = here::here("output","sec_m100_sims.RData"))
save(sec_m100_min_sims, file = here::here("output","sec_m100_min_sims.RData"))

