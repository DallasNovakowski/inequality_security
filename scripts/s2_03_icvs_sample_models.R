# load data files
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

set.seed(1234)

#larger dataset
# Estimate linear model for larger dataset 
lme1 <- lme4::lmer(num_victim_5yr ~ gini_2004_6_cent + gdppc_2004_6_scale +
                     age_cent + employed + male + (1 | country), data = iv_joined1, REML=FALSE)

cgwtools::resave(lme1, file = here::here("output","icvs_output.RData"))



#smaller dataset
#for comparability with lme1
lme_mod1 <- lme4::lmer(num_victim_5yr ~ gini_2004_6_cent + gdppc_2004_6_scale +
                        age_cent + employed + male + (1 | country), data = mod_joined1, REML=FALSE)

#manuscript-included variables
lme_mod_max1 <- lme4::lmer(num_victim_5yr ~ gdppc_2004_6_scale + gini_2004_6_cent + 
                            age_cent + employed + male + police_effective + income_quartile + (1 | country), 
                          data = mod_joined1, REML=FALSE)

#reduced ordinal model
m_ord_mod1 <- ordinal::clmm(ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +
                              age_cent + employed + male + (1 | country), data = mod_joined1)

#full model for analyses
m_ord_mod_max1 <- ordinal::clmm(ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale + 
                                  age_cent + employed + male + police_effective + income_quartile + (1 | country), 
                                data = mod_joined1)

# estimate weighted model - fails to converge even with all numeric variables scaled
# m1_weight <- lme4::lmer(scale(num_victim_5yr) ~ scale(av_gini) + scale(gdppc_2004_6) + 
#                           scale(age_num) + employed +   male + (1 | country),
#                         data = mod_joined1, REML=FALSE, weights = individual_weight)



 
# cgwtools::resave(lme1,
# lme_mod1,lme_mod_max1,m_ord_mod1,m_ord_mod_max1, file = here::here("output","icvs_output.RData"))
