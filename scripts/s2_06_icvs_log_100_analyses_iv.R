library(tidyverse)
library(lme4)

# library(stevemisc) # Steve's toy R package with various helper functions
# library(modelr) # for data_grid
# library(kableExtra) # for pretty tables


# Load data and prep environment ____________________
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_joined.RData")
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")
load(file = here::here("output","icvs_output.RData"))
source(here::here("scripts", "manuscript_funs.r"), local = knitr::knit_global())

set.seed(1234)


# Run analyses ________________________________________________________________________________________

# Log analysis 0|1 _______________________________________

m100_log_01_iv <- iv_joined %>% map(~ lme4::glmer(victim_mlog_01_min, data = .x,family = binomial, control = lme4::glmerControl(optimizer = "bobyqa"),
                                                    nAGQ = 10))

# extract estimates simulation
m100_log_01_sims_iv <- m100_log_01_iv %>%
  map(. %>% arm::sim(n.sims = 3000)) %>%
  map_df(. %>% slot("fixef") %>% as_tibble())

# condense simulated values into a single dataset with estimate and SE
m100_log_01_tidy_iv <- tibble(term = names(m100_log_01_sims_iv),
                  estimate = summarise_all(m100_log_01_sims_iv, list(mean)) %>%
                    t() %>%
                    as.vector(),
                  std.error = m100_log_01_sims_iv %>%
                    summarise_all(list(sd)) %>%
                    t() %>%
                    as.vector())

# define level for regression term
m100_log_01_tidy_iv$obs_lvl <- NA
m100_log_01_tidy_iv$obs_lvl[1] <- 1
m100_log_01_tidy_iv$obs_lvl[2:3] <- 2
m100_log_01_tidy_iv$obs_lvl[4:6] <- 1


# add df values
  #DF for level-1 predictors is M-r-1, where M is number of level-1 units and r is the total number of explanatory variables
  #DF for  level-2 predictors is N-q-1, where N is the number of clusters and q is the number of level-2 predictors
m100_log_01_tidy_iv <- make_df(m100_log_01_tidy_iv,m_log_01_iv1)

# Adds 95% cis, z-scores, p-values
m100_log_01_tidy_iv <- make_ci(m100_log_01_tidy_iv)

# saves first model only
# save(m100_log_01_tidy_iv, file = here::here("output","m100_log_iv_sims.RData"))


# Log analysis 1|2 ______________________________________________

m100_log_12_iv <- iv_joined %>% map(~ lme4::glmer(victim_mlog_12_min, data = .x,family = binomial, control = lme4::glmerControl(optimizer = "bobyqa"),
                                                  nAGQ = 10))

m100_log_12_sims_iv <- m100_log_12_iv %>%
  map(. %>% arm::sim(n.sims = 3000)) %>%
  map_df(. %>% slot("fixef") %>% as_tibble())

# identify observation/cluster level for different parameters
m100_log_12_tidy_iv <- tibble(term = names(m100_log_12_sims_iv),
                              estimate = summarise_all(m100_log_12_sims_iv, list(mean)) %>%
                                t() %>%
                                as.vector(),
                              std.error = m100_log_12_sims_iv %>%
                                summarise_all(list(sd)) %>%
                                t() %>%
                                as.vector())

# identify observation/cluster level for different parameters
m100_log_12_tidy_iv$obs_lvl <- NA
m100_log_12_tidy_iv$obs_lvl[1] <- 1
m100_log_12_tidy_iv$obs_lvl[2:3] <- 2
m100_log_12_tidy_iv$obs_lvl[4:6] <- 1

m100_log_12_tidy_iv$df <- NA

m100_log_12_tidy_iv <- make_df(m100_log_12_tidy_iv,m_log_12_iv1)

# Adds 95% cis, z-scores, p-values 
m100_log_12_tidy_iv <- make_ci(m100_log_12_tidy_iv)


# Compare Coefficients ________________________________________________________________________________________
compare_coefs_z<- function(m1,m2,coef_name){
  m1_row <- m1[m1$term %in% coef_name,]
  m2_row <- m2[m2$term %in% coef_name,]
  
  (m1_row$estimate - m2_row$estimate)/sqrt(m1_row$std.error + m2_row$std.error)
}

compare_coefs_diff<- function(m1,m2,coef_name){
  m1_row <- m1[m1$term %in% coef_name,]
  m2_row <- m2[m2$term %in% coef_name,]
  
  c(m1_row$estimate - m2_row$estimate,
    (m1_row$estimate-m1_row$std.error*1.96-m2_row$estimate-m2_row$std.error*1.96),
    (m1_row$estimate+m1_row$std.error*1.96-m2_row$estimate+m2_row$std.error*1.96))
}

#should yield a z-score
log_comp_01_12_iv <- compare_coefs_z(m100_log_01_tidy_iv,m100_log_12_tidy_iv,"gini_2004_6_cent")

log_comp_01_12_diff_iv <- compare_coefs_diff(m100_log_01_tidy_iv,m100_log_12_tidy_iv,"gini_2004_6_cent")


log_comp_01_12_pval_iv <- 2*pnorm(log_comp_01_12_iv, mean = 0, sd = 1, lower.tail = TRUE)

# save both dataframes _______________________________________________________________________________________
save(m100_log_01_tidy_iv, m100_log_12_tidy_iv,log_comp_01_12_diff_iv,log_comp_01_12_iv,log_comp_01_12_pval_iv, file = here::here("output","m100_log_iv_sims.RData"))
