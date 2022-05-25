# load data files
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

set.seed(1234)

victim_lmer_min <- num_victim_5yr ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + (1 | country)

victim_lmer_max <- num_victim_5yr ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + police_effective + income_quartile + (1 | country)

#Ordinal models

victim_mord_min_l1 <- ordered(num_victim_5yr_winz) ~ 
  age_cent + employed + male

victim_mord_min <- ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + (1 | country)

victim_mord_max <- ordered(num_victim_5yr_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + police_effective + income_quartile + (1 | country)

victim_ass_mord_min <- ordered(num_victim_5yr_assault_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + (1 | country)

victim_ass_mord_max <- ordered(num_victim_5yr_assault_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + police_effective + income_quartile + (1 | country)

cgwtools::resave(victim_lmer_min,victim_lmer_max, victim_mord_min,victim_mord_max,victim_ass_mord_min,victim_ass_mord_max,
                 file = here::here("output","icvs_output.RData"))


#Logistic models

victim_mlog_01_min <- num_victim_5yr_0_1 ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + (1 | country)

victim_mlog_12_min <- num_victim_5yr_1_2 ~ gini_2004_6_cent + gdppc_2004_6_scale +
  age_cent + employed + male + (1 | country)

cgwtools::resave(victim_mlog_01_min,victim_mlog_12_min, file = here::here("output","icvs_output.RData"))


# actual security consumption variables

sec_lmer_min <- total_security ~ gini_2004_6_cent + gdppc_2004_6_scale + victim_nation + 
  num_victim_5yr + age_cent + employed + male + (1 | country)

sec_lmer_max <- total_security ~ gini_2004_6_cent + gdppc_2004_6_scale + victim_nation_assault + 
  num_victim_5yr_assault + age_cent + employed + male + police_effective + income_quartile + (1 | country)

sec_mord_min <- ordered(security_winz) ~  gini_2004_6_cent + gdppc_2004_6_scale +  victim_nation + 
  num_victim_5yr + age_cent + employed + male + (1 | country)

sec_mord_max <- ordered(security_winz) ~ gini_2004_6_cent + gdppc_2004_6_scale +  victim_nation_assault + 
  num_victim_5yr_assault + age_cent + employed + male + police_effective + income_quartile + (1 | country)

cgwtools::resave(sec_lmer_min,sec_lmer_max, sec_mord_min,sec_mord_max, file = here::here("output","icvs_output.RData"))


# Logistic models
sec_mlog_01_min <- security_0_1 ~ gini_2004_6_cent + gdppc_2004_6_scale + victim_nation + 
  num_victim_5yr + age_cent + employed + male + (1 | country)

sec_mlog_12_min <- security_1_2 ~ gini_2004_6_cent + gdppc_2004_6_scale + victim_nation + 
  num_victim_5yr + age_cent + employed + male + (1 | country)

sec_mlog_23_min <- security_2_3 ~ gini_2004_6_cent + gdppc_2004_6_scale + victim_nation + 
  num_victim_5yr + age_cent + employed + male + (1 | country)

cgwtools::resave(sec_mlog_01_min,sec_mlog_12_min, sec_mlog_23_min, file = here::here("output","icvs_output.RData"))


#larger dataset
# Estimate linear model for larger dataset 
lme1 <- lme4::lmer(victim_lmer_min, data = iv_joined1, REML=FALSE)

cgwtools::resave(lme1, file = here::here("output","icvs_output.RData"))


#smaller dataset
#for comparability with lme1
lme_mod1 <- lme4::lmer(victim_lmer_min, data = mod_joined1, REML=FALSE)

#manuscript-included variables
lme_mod_max1 <- lme4::lmer(victim_lmer_max, 
                           data = mod_joined1, REML=FALSE)


# ordinal models ______________
#reduced ordinal model
m_ord_iv1 <- summary(ordinal::clmm(victim_mord_min, 
                                   data = iv_joined1))


m_ord_mod1 <- summary(ordinal::clmm(victim_mord_min, 
                                    data = mod_joined1))

m_ord_iv_nom <- ordinal::clmm(victim_mord_min, nominal = ~ employed, data=iv_joined1)


# Logistic models
m_log_01_iv1 <- summary(lme4::glmer(victim_mlog_01_min, data = iv_joined1, family = binomial, control = lme4::glmerControl(optimizer = "bobyqa"),
           nAGQ = 10))

# se_01 <- sqrt(diag(stats::vcov(m_log_01_iv1)))
# 
# # table of estimates with 95% CI
# (tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
#                 se))

m_log_12_iv1 <- summary(lme4::glmer(victim_mlog_12_min, data = iv_joined1, family = binomial, control = lme4::glmerControl(optimizer = "bobyqa"),
                            nAGQ = 10))

# anova(m_ord_iv_nom,m_ord_iv_big)

mord_iv_l1 <- ordinal::clm(victim_mord_min_l1, data=iv_joined1)

l1_propodds <- ordinal::nominal_test(mord_iv_l1)


#full model for analyses
m_ord_mod_max1 <- summary(ordinal::clmm(victim_ass_mord_max, 
                                        data = mod_joined1))

# estimate weighted model - fails to converge even with all numeric variables scaled
# m1_weight <- lme4::lmer(scale(num_victim_5yr) ~ scale(av_gini) + scale(gdppc_2004_6) + 
#                           scale(age_num) + employed +   male + (1 | country),
#                         data = mod_joined1, REML=FALSE, weights = individual_weight)


# cgwtools::resave(lme1,
# lme_mod1,lme_mod_max1,m_ord_mod1,m_ord_mod_max1, file = here::here("output","icvs_output.RData"))


cgwtools::resave(m_ord_mod1,m_ord_mod_max1,m_ord_iv1, file = here::here("output","icvs_output.RData"))

cgwtools::resave(m_log_01_iv1,m_log_12_iv1, file = here::here("output","icvs_output.RData"))

