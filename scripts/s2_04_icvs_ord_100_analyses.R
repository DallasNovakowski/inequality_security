
library(tidyverse)
library(lme4)
library(ordinal)

library(stevemisc) # my toy R package with various helper functions
library(modelr) # for data_grid
library(kableExtra) # for pretty tables

load(file = here::here("output","icvs_output.RData"))

load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mod_joined.RData")


set.seed(1234)

#used for dotwhisker plots
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")


# takes about an hour
m100_ord <- mod_joined %>% map(~ ordinal::clmm(victim_ass_mord_max, data = .x))


# http://svmiller.com/blog/2020/04/summarizing-ordinal-models-with-simulation-multivariate-normal/
# extract coefficients for each model
m100_coef <- m100_ord %>%
  map(. %>% coef())

# extract
m100_vcov <- m100_ord %>%
  map(. %>% vcov()) 

m100_vcov <- m100_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, m100_coef, m100_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# m100_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

m100_sims <- list()

m100_sims <- lapply(seq_along(combined), function(i) {
  m100_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(m100_sims[[i]])
}) 

save(m100_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")

save(m100_sims, file = here::here("output","m100_sims.RData"))


# cgwtools::resave(m100_sims, file = here::here("output","m100_sims.RData"))


rm(m100_ord)

m100_ord_min <- mod_joined %>% map(~ ordinal::clmm(victim_mord_min, data = .x))


m100_min_coef <- m100_ord_min %>%
  map(. %>% coef())

# extract
m100_min_vcov <- m100_ord_min %>%
  map(. %>% vcov()) 

m100_min_vcov <- m100_min_vcov %>%
  map(. %>% .[-nrow(.), -ncol(.)])

combined <- list()
combined <- mapply(data.frame, m100_min_coef, m100_min_vcov, SIMPLIFY=FALSE)

combined <- lapply(seq_along(combined), function(i) {
  colnames(combined[[i]])[1] <- "estimates"
  return(combined[[i]])
}) 

# m100_min_sims2 <- combined %>%
#   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))

m100_min_sims <- list()

m100_min_sims <- lapply(seq_along(combined), function(i) {
  m100_min_sims[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
  return(m100_min_sims[[i]])
}) 

save(m100_min_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_min_sims.RData")
cgwtools::resave(m100_min_sims, file = here::here("output","m100_sims.RData"))
save(m100_min_sims, file = here::here("output","m100_min_sims.RData"))


rm(m100_ord_min)


# load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")



# > Chapter 7 of Gelman and Hill (2007) for how the multivariate normal distribution is a novel way of simulating uncertainty regarding the model output from a generalized linear model. I’ll only note here that simulating values from a multivariate normal distribution of the ordinal model requires only the vector of regression coefficients and the variance-covariance matrix of the fitted model. 


# weighted ordinal regression
mw100_ord <- mod_joined %>% map(~ ordinal::clmm(victim_ass_mord_min, data = .x, weights = individual_weight))

save(mw100_ord, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mw100_ord.RData")

summary(mw100_ord[[1]])$coefficients[,"Std. Error"]

is.nan(summary(mw100_ord[[9]])$coefficients[,"Std. Error"]["gini_2004_6_cent"])

mw100_invalid <- list()

mw100_invalid <- lapply(seq_along(mw100_ord), function(i) {
  mw100_invalid[[i]] <- is.nan(summary(mw100_ord[[i]])$coefficients[,"Std. Error"]["gini_2004_6_cent"])
  return(mw100_invalid[[i]])
}) 

model_nonconvergence <- Reduce("+",mw100_invalid)

cgwtools::resave(model_nonconvergence, file = here::here("output", "icvs_output.RData"))

