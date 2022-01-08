
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

# m1 <- mod_joined %>% map(~ lmer(num_victim_5yr ~ gini_2004_6_cent + gdppc_2004_6_scale + 
#                      age_cent + employed + male + police_effective + income_quartile + (1 | country),
# data = .x))

# m1_sims <- m1 %>%
#   map(. %>% arm::sim(n.sims = 1000)) %>%
#   map_df(. %>% slot("fixef") %>% as_tibble())
# 
# 
# # Generate a tidy dataframe of results (cf. broom::tidy())
# m1_tidy <- tibble(
#   term = names(m1_sims),
#   estimate = summarise_all(m1_sims, list(mean)) %>%
#     t() %>%
#     as.vector(),
#   std.error = m1_sims %>%
#     summarise_all(list(sd)) %>%
#     t() %>%
#     as.vector()
# )
# 
# 
# library(dotwhisker)
# # Plot the results
# m1_tidy %>%
#   by_2sd(mod_joined1) %>%
#   relabel_predictors(
#     c(
#       gini_2004_6_cent = "Income Inequality",
#       gdppc_2004_6_scale = "GDP/capita",
#       age_cent = "Age",
#       employed1 = "Employed",
#       male1 = "Male",
#       police_effective = "Perceived police effectiveness",
#       income_quartile = "Income quartile"
#     )
#   ) %>%
#   dwplot() +
#   theme_bw() +
#   geom_vline(xintercept = 0,
#              colour = "grey60",
#              linetype = "dashed") +
#   theme(legend.position = "none") +
#   labs(title = "Predicting crime victimization",
#        x = "Coefficient Estimate")


# takes about an hour
m100_ord <- mod_joined %>% map(~ ordinal::clmm(ordered(num_victim_5yr_assault_winz) ~  gdppc_2004_6_scale + gini_2004_6_cent + age_cent + employed + male + police_effective + income_quartile + (1 | country), data = .x))
# 
# m100_ord1 <- m100_ord[1:10]
# 
# save(m100_ord1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord1.RData")
# 
# m100_ord2 <- m100_ord[11:20]
# 
# save(m100_ord2, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord2.RData")
# 
# m100_ord3 <- m100_ord[21:30]
# 
# save(m100_ord3, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord3.RData")
# 
# m100_ord4 <- m100_ord[31:40]
# 
# save(m100_ord4, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord4.RData")
# 
# m100_ord5 <- m100_ord[41:50]
# 
# save(m100_ord5, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord5.RData")
# 
# m100_ord6 <- m100_ord[51:60]
# 
# save(m100_ord6, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord6.RData")
# 
# m100_ord7 <- m100_ord[61:70]
# 
# save(m100_ord7, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord7.RData")
# 
# m100_ord8 <- m100_ord[71:80]
# 
# save(m100_ord8, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord8.RData")
# 
# m100_ord9 <- m100_ord[81:90]
# 
# save(m100_ord9, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord9.RData")
# 
# m100_ord10 <- m100_ord[91:100]
# 
# save(m100_ord10, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord10.RData")


# m100_ord2 <- mod_joined2 %>% map(~ ordinal::clmm(ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale + age_cent + employed + male + police_effective + income_quartile + (1 | country), data = .x))
# 
# save(m100_ord2, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord2.RData")


list_100 <- list()

# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord1.RData")
# 


# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord2.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord3.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord4.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord5.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord6.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord7.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord8.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord9.RData")
# 
# load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_ord10.RData")




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

# save(m100_sims, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")
cgwtools::resave(m100_sims, file = here::here("output","m100_sims.RData"))

# load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")
load(file = here::here("output","icvs_output.RData"))



# > Chapter 7 of Gelman and Hill (2007) for how the multivariate normal distribution is a novel way of simulating uncertainty regarding the model output from a generalized linear model. I’ll only note here that simulating values from a multivariate normal distribution of the ordinal model requires only the vector of regression coefficients and the variance-covariance matrix of the fitted model. 


# weighted ordinal regression
mw100_ord <- mod_joined %>% map(~ ordinal::clmm(ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale + age_cent + employed + male + police_effective + income_quartile + (1 | country), data = .x, weights = individual_weight))

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



# # extract
# 
# mw100_coef2 <- mw100_ord %>%
#   map(. %>% coef())
# 
# 
# mw100_vcov2 <- mw100_ord %>%
#   map(. %>% vcov()) 
#   
# combined <- list()
# combined <- mapply(data.frame, mw100_coef2, mw100_vcov2, SIMPLIFY=FALSE)
# 
# combined <- lapply(seq_along(combined), function(i) {
#   colnames(combined[[i]])[1] <- "estimates"
#   return(combined[[i]])
# }) 
# 
# # mw100_sims2 <- combined %>%
# #   map(. %>% smvrnorm(1000, .$estimates, as.matrix(.[,-1])))
# 
# mw100_sims2 <- list()
# 
# mw100_sims2 <- lapply(seq_along(combined), function(i) {
#  mw100_sims2[[i]] <- smvrnorm(1000, combined[[i]]$estimates, as.matrix(combined[[i]][,-1]))
#   return(mw100_sims2[[i]])
# }) 
# 
# 
# save(mw100_sims2, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mw100_sims2.RData")

